"""JSON-API-Endpunkte (Live-Daten, Statistiken, Rekorde, Backup/Restore, ...)."""
import csv
import io
import json
import os
from datetime import datetime, timedelta

from flask import (
    Blueprint, Response, jsonify, request, stream_with_context,
)
from sqlalchemy import func, text

from .extensions import db, log, socketio
from .models import (
    Event, Lap, Penalty, PersonalRecord, RaceResult, RaceStatus,
    SessionName, Track, TrackRecord,
)
from .utils import (
    _calc_stddev, _get_active_track_name, _parse_sector_ms,
    _session_display_name, fmt_ms, reset_active_track_override,
    set_active_track_name,
)


def _resolve_active_track():
    """Aktuell aktive Strecke als Track-Objekt (oder None).

    Nutzt dieselbe Logik wie die Runden-Zuordnung (Override -> is_active ->
    zuletzt benutzt), damit UI und Zuordnung immer konsistent sind.
    """
    name = _get_active_track_name()
    if not name:
        return None
    return Track.query.filter_by(name=name).first()

api_bp = Blueprint('api', __name__)


@api_bp.route('/api/live-data')
def api_live_data():
    """Live-Daten gruppiert nach Controller."""
    try:
        q = Lap.query
        sid = request.args.get('session_id')
        if sid and sid != 'all':
            q = q.filter(Lap.session_id == sid)
        else:
            # Ohne explizite Session: nur letzte Session laden (Performance)
            latest_event = db.session.query(Event.session_id).order_by(
                Event.id.desc()).first()
            if latest_event and latest_event.session_id:
                sid = latest_event.session_id
                q = q.filter(Lap.session_id == sid)
            else:
                # Fallback: letzte 2 Stunden
                cutoff = datetime.utcnow() - timedelta(hours=2)
                q = q.filter(Lap.created_at >= cutoff)

        laps = q.order_by(Lap.created_at.desc()).all()
        if not laps:
            return jsonify({})

        # Penalties pro Controller zaehlen
        penalty_counts = {}
        try:
            penalty_q = Penalty.query
            if sid and sid != 'all':
                penalty_q = penalty_q.filter(Penalty.session_id == sid)
            for p in penalty_q.all():
                penalty_counts[p.controller_id] = penalty_counts.get(p.controller_id, 0) + 1
        except Exception as e:
            log.warning(f"Penalty query failed (migration pending?): {e}")
            db.session.rollback()

        # DNF/DQ Status + Pitstops pruefen
        dnf_dq = {}
        pitstop_counts = {}
        try:
            result_q = RaceResult.query
            if sid and sid != 'all':
                result_q = result_q.filter(RaceResult.session_id == sid)
            for r in result_q.all():
                try:
                    if r.pitstops:
                        pitstop_counts[r.controller_id] = r.pitstops
                except Exception:
                    pass
                if r.retired or r.disqualified:
                    dnf_dq[r.controller_id] = {
                        'retired': r.retired,
                        'disqualified': r.disqualified,
                    }
        except Exception as e:
            log.warning(f"RaceResult query failed (migration pending?): {e}")
            db.session.rollback()

        ctrls = {}
        for lap in laps:
            cid = str(lap.controller_id)
            if cid not in ctrls:
                ctrls[cid] = {
                    'name': lap.driver_name,
                    'car': lap.car_name,
                    'color': lap.controller_color or lap.car_color,
                    'laps': [],
                    'best_time_raw': None,
                    'penalties': penalty_counts.get(cid, 0),
                    'pitstops': pitstop_counts.get(cid, 0),
                    'retired': dnf_dq.get(cid, {}).get('retired', False),
                    'disqualified': dnf_dq.get(cid, {}).get('disqualified', False),
                }
            ctrls[cid]['laps'].append({
                'lap': lap.lap_number or 0,
                'laptime_raw': lap.laptime_ms or 0,
                'laptime_formatted': lap.laptime_display or fmt_ms(lap.laptime_ms) or '--:--.---',
                'timestamp': lap.created_at.isoformat() if lap.created_at else None,
                'is_pb': lap.is_personal_best,
                'session_id': lap.session_id,
                'sector_1': lap.sector_1,
                'sector_2': lap.sector_2,
                'sector_3': lap.sector_3,
            })
            if lap.laptime_ms and lap.laptime_ms > 0:
                best = ctrls[cid]['best_time_raw']
                if best is None or lap.laptime_ms < best:
                    ctrls[cid]['best_time_raw'] = lap.laptime_ms

        for c in ctrls.values():
            nums = [l['lap'] for l in c['laps'] if l['lap']]
            c['lap_count'] = max(nums) if nums else 0

        return jsonify(ctrls)
    except Exception as e:
        log.error(f"live-data: {e}")
        return jsonify({}), 500


@api_bp.route('/api/sessions')
def api_sessions():
    """Alle Sessions mit Statistiken."""
    try:
        rows = db.session.query(
            Lap.session_id,
            Lap.session_type,
            func.count(Lap.id).label('total_laps'),
            func.count(func.distinct(Lap.controller_id)).label('drivers'),
            func.min(Lap.created_at).label('start_time'),
            func.max(Lap.created_at).label('end_time'),
        ).group_by(Lap.session_id, Lap.session_type
        ).order_by(func.max(Lap.created_at).desc()).all()

        return jsonify([{
            'session_id': r.session_id,
            'session_type': r.session_type,
            'display_name': _session_display_name(r.session_id, r.session_type),
            'total_laps': r.total_laps,
            'drivers': r.drivers,
            'start_time': r.start_time.isoformat() if r.start_time else None,
            'end_time': r.end_time.isoformat() if r.end_time else None,
        } for r in rows])
    except Exception as e:
        log.error(f"sessions: {e}")
        return jsonify([])


@api_bp.route('/api/laps')
def api_laps():
    """Rundenzeiten mit Filtern und Pagination."""
    try:
        q = Lap.query

        sid = request.args.get('session_id')
        if sid and sid != 'all':
            q = q.filter(Lap.session_id == sid)

        driver = request.args.get('driver')
        if driver:
            q = q.filter(Lap.driver_name.ilike(f'%{driver}%'))

        car = request.args.get('car')
        if car:
            q = q.filter(Lap.car_name.ilike(f'%{car}%'))

        track = request.args.get('track')
        if track:
            q = q.filter(Lap.track_name == track)

        df = request.args.get('date_from')
        if df:
            try:
                date_from = datetime.strptime(df, '%Y-%m-%d')
                q = q.filter(Lap.created_at >= date_from)
            except ValueError:
                pass

        dt = request.args.get('date_to')
        if dt:
            try:
                date_to = datetime.strptime(dt, '%Y-%m-%d').replace(
                    hour=23, minute=59, second=59)
                q = q.filter(Lap.created_at <= date_to)
            except ValueError:
                pass

        # Sortierung
        sort_col = request.args.get('sort', 'created_at')
        sort_dir = request.args.get('dir', 'desc')
        sort_map = {
            'session_id': Lap.session_id,
            'session_type': Lap.session_type,
            'driver_name': Lap.driver_name,
            'car_name': Lap.car_name,
            'lap': Lap.lap_number,
            'laptime_raw': Lap.laptime_ms,
            'timestamp': Lap.created_at,
            'created_at': Lap.created_at,
        }
        col = sort_map.get(sort_col, Lap.created_at)
        q = q.order_by(col.asc() if sort_dir == 'asc' else col.desc())

        # Pagination
        page = max(1, int(request.args.get('page', 1)))
        per_page = min(200, max(10, int(request.args.get('per_page', 50))))
        total = q.count()
        laps = q.offset((page - 1) * per_page).limit(per_page).all()

        return jsonify({
            'data': [{
                'id': l.id,
                'session_id': l.session_id,
                'session_type': l.session_type,
                'controller_id': l.controller_id,
                'driver_name': l.driver_name,
                'car_name': l.car_name,
                'lap': l.lap_number,
                'laptime_raw': l.laptime_ms,
                'laptime_formatted': l.laptime_display or fmt_ms(l.laptime_ms) or '--:--.---',
                'sector_1': l.sector_1,
                'sector_2': l.sector_2,
                'sector_3': l.sector_3,
                'car_color': l.car_color,
                'controller_color': l.controller_color,
                'is_pb': l.is_personal_best,
                'track_name': l.track_name,
                'timestamp': l.created_at.isoformat() if l.created_at else None,
            } for l in laps],
            'total': total,
            'page': page,
            'per_page': per_page,
            'pages': max(1, (total + per_page - 1) // per_page),
        })
    except Exception as e:
        log.error(f"laps: {e}")
        return jsonify({'data': [], 'total': 0, 'page': 1, 'per_page': 50, 'pages': 1})


@api_bp.route('/api/laps/<int:lap_id>', methods=['DELETE'])
def api_delete_lap(lap_id):
    """Einzelne Runde loeschen."""
    try:
        lap = Lap.query.get(lap_id)
        if not lap:
            return jsonify({'error': 'Runde nicht gefunden'}), 404
        db.session.delete(lap)
        db.session.commit()
        return jsonify({'status': 'ok', 'deleted_id': lap_id})
    except Exception as e:
        db.session.rollback()
        log.error(f"delete-lap: {e}")
        return jsonify({'error': str(e)}), 500


@api_bp.route('/api/analytics')
def api_analytics():
    """Aggregierte Daten fuer Charts (SQL-Aggregation)."""
    try:
        sid = request.args.get('session_id')

        # 1. Aggregierte Stats per SQL (schnell, auch bei 500K+ Rows)
        q = db.session.query(
            Lap.driver_name,
            func.count(Lap.id).label('total_laps'),
            func.min(Lap.laptime_ms).label('best_time'),
            func.avg(Lap.laptime_ms).label('avg_time'),
        ).filter(Lap.laptime_ms > 0, Lap.driver_name.isnot(None))

        if sid and sid != 'all':
            q = q.filter(Lap.session_id == sid)

        agg = {r.driver_name: {
            'total_laps': r.total_laps,
            'best_time': r.best_time or 0,
            'avg_time': round(r.avg_time) if r.avg_time else 0,
        } for r in q.group_by(Lap.driver_name).all()}

        # 2. Letzte 50 Rundenzeiten pro Fahrer (ein Query statt N)
        ranked = db.session.query(
            Lap.driver_name,
            Lap.laptime_ms,
            func.row_number().over(
                partition_by=Lap.driver_name,
                order_by=Lap.created_at.desc(),
            ).label('rn'),
        ).filter(Lap.laptime_ms > 0, Lap.driver_name.isnot(None))

        if sid and sid != 'all':
            ranked = ranked.filter(Lap.session_id == sid)

        ranked_sub = ranked.subquery()
        recent_laps = db.session.query(
            ranked_sub.c.driver_name,
            ranked_sub.c.laptime_ms,
        ).filter(ranked_sub.c.rn <= 50).order_by(
            ranked_sub.c.driver_name, ranked_sub.c.rn.desc(),
        ).all()

        laps_by_driver = {}
        for row in recent_laps:
            laps_by_driver.setdefault(row.driver_name, []).append(row.laptime_ms)

        result = {}
        for name, s in agg.items():
            result[name] = {
                'avg_time': s['avg_time'],
                'best_time': s['best_time'],
                'total_laps': s['total_laps'],
                'laps': laps_by_driver.get(name, []),
            }

        return jsonify(result)
    except Exception as e:
        log.error(f"analytics: {e}")
        return jsonify({})


@api_bp.route('/api/penalties')
def api_penalties():
    """Strafen mit optionalem Session-Filter."""
    try:
        q = Penalty.query
        sid = request.args.get('session_id')
        if sid and sid != 'all':
            q = q.filter(Penalty.session_id == sid)

        penalties = []
        for p in q.order_by(Penalty.created_at.desc()).all():
            entry = {
                'id': p.id,
                'session_id': p.session_id,
                'controller_id': p.controller_id,
                'driver_name': p.driver_name,
                'penalty_type': p.penalty_type,
                'timestamp': p.created_at.isoformat() if p.created_at else None,
            }
            try:
                entry['penalty_seconds'] = p.penalty_seconds or 0
            except Exception:
                entry['penalty_seconds'] = 0
            penalties.append(entry)
        return jsonify(penalties)
    except Exception as e:
        log.error(f"penalties: {e}")
        db.session.rollback()
        return jsonify([])


@api_bp.route('/api/race-status')
def api_race_status():
    """Aktueller Rennstatus (nur aktuelle Session)."""
    try:
        # Aktuelle Session ermitteln (wie in api_live_data)
        latest_event = db.session.query(Event.session_id).order_by(
            Event.id.desc()).first()
        current_sid = latest_event.session_id if latest_event else None

        if current_sid:
            rs = RaceStatus.query.filter_by(session_id=current_sid).order_by(
                RaceStatus.id.desc()).first()
            if rs:
                return jsonify({
                    'session_id': rs.session_id,
                    'status': rs.status,
                    'race_type': rs.race_type,
                    'updated_at': rs.updated_at.isoformat() if rs.updated_at else None,
                })
        return jsonify({'status': 'waiting', 'race_type': '', 'session_id': current_sid or ''})
    except Exception as e:
        log.error(f"race-status: {e}")
        return jsonify({'status': 'unknown'})


@api_bp.route('/api/driver-stats')
def api_driver_stats():
    """Fahrer-Statistiken ueber alle Sessions."""
    try:
        lap_rows = db.session.query(
            Lap.driver_name,
            func.count(Lap.id).label('total_laps'),
            func.count(func.distinct(Lap.session_id)).label('total_sessions'),
            func.min(Lap.laptime_ms).label('best_time'),
            func.avg(Lap.laptime_ms).label('avg_time'),
            func.stddev_samp(Lap.laptime_ms).label('stddev_time'),
        ).filter(
            Lap.laptime_ms > 0,
            Lap.driver_name.isnot(None),
            Lap.driver_name != '',
        ).group_by(Lap.driver_name).all()

        # Siege und Podien aus Ergebnissen
        result_stats = {}
        for row in db.session.query(
            RaceResult.driver_name,
            RaceResult.position,
        ).filter(
            RaceResult.position > 0,
            RaceResult.retired == False,
            RaceResult.disqualified == False,
        ).all():
            name = row.driver_name
            if name not in result_stats:
                result_stats[name] = {'wins': 0, 'podiums': 0}
            if row.position == 1:
                result_stats[name]['wins'] += 1
            if row.position <= 3:
                result_stats[name]['podiums'] += 1

        # Penalty-Counts
        penalty_rows = db.session.query(
            Penalty.driver_name,
            func.count(Penalty.id).label('cnt'),
        ).group_by(Penalty.driver_name).all()
        penalty_map = {r.driver_name: r.cnt for r in penalty_rows}

        # Konsistenz-Score: direkt aus SQL-Aggregation (stddev_samp)
        consistency_map = {}
        for r in lap_rows:
            avg_val = r.avg_time or 0
            stddev = r.stddev_time or 0
            if avg_val > 0 and r.total_laps >= 2 and stddev > 0:
                consistency_map[r.driver_name] = {
                    'score': round((1 - stddev / avg_val) * 100, 1),
                    'stddev_ms': round(stddev),
                }
            else:
                consistency_map[r.driver_name] = {'score': 0, 'stddev_ms': 0}

        drivers = []
        for r in lap_rows:
            rs = result_stats.get(r.driver_name, {'wins': 0, 'podiums': 0})
            avg = round(r.avg_time) if r.avg_time else 0
            cons = consistency_map.get(r.driver_name, {'score': 0, 'stddev_ms': 0})
            drivers.append({
                'driver_name': r.driver_name,
                'total_laps': r.total_laps,
                'total_sessions': r.total_sessions,
                'best_time': r.best_time,
                'best_time_formatted': fmt_ms(r.best_time) or '--',
                'avg_time': avg,
                'avg_time_formatted': fmt_ms(avg) or '--',
                'wins': rs['wins'],
                'podiums': rs['podiums'],
                'penalties': penalty_map.get(r.driver_name, 0),
                'consistency_score': cons['score'],
                'stddev_ms': cons['stddev_ms'],
            })

        return jsonify(sorted(drivers, key=lambda d: d['best_time'] or 999999))
    except Exception as e:
        log.error(f"driver-stats: {e}")
        return jsonify([])


@api_bp.route('/api/car-stats')
def api_car_stats():
    """Auto-Statistiken ueber alle Sessions."""
    try:
        rows = db.session.query(
            Lap.car_name,
            func.count(Lap.id).label('total_laps'),
            func.count(func.distinct(Lap.session_id)).label('total_sessions'),
            func.count(func.distinct(Lap.driver_name)).label('total_drivers'),
            func.min(Lap.laptime_ms).label('best_time'),
            func.avg(Lap.laptime_ms).label('avg_time'),
        ).filter(
            Lap.laptime_ms > 0,
            Lap.car_name.isnot(None),
            Lap.car_name != '',
        ).group_by(Lap.car_name).all()

        cars = []
        for r in rows:
            avg = round(r.avg_time) if r.avg_time else 0
            cars.append({
                'car_name': r.car_name,
                'total_laps': r.total_laps,
                'total_sessions': r.total_sessions,
                'total_drivers': r.total_drivers,
                'best_time': r.best_time,
                'best_time_formatted': fmt_ms(r.best_time) or '--',
                'avg_time': avg,
                'avg_time_formatted': fmt_ms(avg) or '--',
            })

        return jsonify(sorted(cars, key=lambda c: c['best_time'] or 999999))
    except Exception as e:
        log.error(f"car-stats: {e}")
        return jsonify([])


@api_bp.route('/api/filters')
def api_filters():
    """Filterwerte fuer Dropdowns."""
    try:
        drivers = [r[0] for r in db.session.query(Lap.driver_name.distinct()).filter(
            Lap.driver_name.isnot(None), Lap.driver_name != ''
        ).all() if r[0]]

        cars = [r[0] for r in db.session.query(Lap.car_name.distinct()).filter(
            Lap.car_name.isnot(None), Lap.car_name != ''
        ).all() if r[0]]

        return jsonify({'drivers': sorted(drivers), 'cars': sorted(cars)})
    except Exception as e:
        log.error(f"filters: {e}")
        return jsonify({'drivers': [], 'cars': []})


@api_bp.route('/api/export/csv')
def api_export_csv():
    """CSV-Export."""
    try:
        q = Lap.query
        for key, col in [('driver', Lap.driver_name), ('car', Lap.car_name)]:
            val = request.args.get(key)
            if val:
                q = q.filter(col.ilike(f'%{val}%'))

        event = request.args.get('event')
        if event:
            q = q.filter(Lap.session_id == event)

        df = request.args.get('date_from')
        if df:
            try:
                date_from = datetime.strptime(df, '%Y-%m-%d')
                q = q.filter(Lap.created_at >= date_from)
            except ValueError:
                pass

        dt = request.args.get('date_to')
        if dt:
            try:
                date_to = datetime.strptime(dt, '%Y-%m-%d').replace(
                    hour=23, minute=59, second=59)
                q = q.filter(Lap.created_at <= date_to)
            except ValueError:
                pass

        laps = q.order_by(Lap.created_at.desc()).all()

        buf = io.StringIO()
        w = csv.writer(buf)
        w.writerow(['Session', 'Typ', 'Zeitstempel', 'Controller', 'Fahrer',
                     'Auto', 'Runde', 'Zeit (ms)', 'Zeit', 'S1', 'S2', 'S3', 'PB'])
        for l in laps:
            w.writerow([
                l.session_id or '', l.session_type or '',
                l.created_at.strftime('%Y-%m-%d %H:%M:%S') if l.created_at else '',
                l.controller_id or '', l.driver_name or '', l.car_name or '',
                l.lap_number or 0, l.laptime_ms or 0,
                l.laptime_display or fmt_ms(l.laptime_ms) or '',
                l.sector_1 or '', l.sector_2 or '', l.sector_3 or '',
                'Ja' if l.is_personal_best else 'Nein',
            ])

        buf.seek(0)
        ts = datetime.now().strftime('%Y%m%d_%H%M%S')
        return Response(buf.getvalue(), mimetype='text/csv', headers={
            'Content-Disposition': f'attachment; filename=smartrace_{ts}.csv',
        })
    except Exception as e:
        log.error(f"csv-export: {e}")
        return jsonify({'error': str(e)}), 500


@api_bp.route('/api/track-record')
def api_track_record():
    """Aktueller Streckenrekord fuer aktive Strecke."""
    try:
        track = request.args.get('track') or _get_active_track_name()
        q = TrackRecord.query
        if track:
            q = q.filter_by(track_name=track)
        rec = q.order_by(TrackRecord.laptime_ms.asc()).first()
        if rec:
            return jsonify({
                'id': rec.id,
                'laptime_ms': rec.laptime_ms,
                'laptime_formatted': fmt_ms(rec.laptime_ms),
                'driver_name': rec.driver_name,
                'car_name': rec.car_name,
                'session_id': rec.session_id,
                'track_name': rec.track_name,
                'created_at': rec.created_at.isoformat() if rec.created_at else None,
            })
        return jsonify(None)
    except Exception as e:
        log.error(f"track-record: {e}")
        return jsonify(None)


@api_bp.route('/api/track-records')
def api_track_records():
    """Streckenrekorde mit Sektorzeiten. Optional ?track= Filter."""
    try:
        track = request.args.get('track')
        q = TrackRecord.query
        if track:
            q = q.filter_by(track_name=track)
        records = q.order_by(TrackRecord.laptime_ms.asc()).all()
        result = []
        for r in records:
            lap = Lap.query.filter_by(
                laptime_ms=r.laptime_ms, driver_name=r.driver_name,
            ).order_by(Lap.created_at.desc()).first() if r.driver_name else None
            result.append({
                'id': r.id,
                'laptime_ms': r.laptime_ms,
                'laptime_formatted': fmt_ms(r.laptime_ms),
                'driver_name': r.driver_name,
                'car_name': r.car_name,
                'session_id': r.session_id,
                'track_name': r.track_name,
                'created_at': r.created_at.isoformat() if r.created_at else None,
                'sector_1': lap.sector_1 if lap else None,
                'sector_2': lap.sector_2 if lap else None,
                'sector_3': lap.sector_3 if lap else None,
            })
        return jsonify(result)
    except Exception as e:
        log.error(f"track-records: {e}")
        return jsonify([])


@api_bp.route('/api/virtual-best')
def api_virtual_best():
    """Virtuelle schnellste Runde aus den besten Sektorzeiten. Optional ?track= Filter."""
    try:
        track = request.args.get('track')
        q = Lap.query.filter(
            Lap.laptime_ms > 1000,
            Lap.sector_1.isnot(None), Lap.sector_1 != '',
            Lap.sector_2.isnot(None), Lap.sector_2 != '',
            Lap.sector_3.isnot(None), Lap.sector_3 != '',
        )
        if track:
            q = q.filter(Lap.track_name == track)
        laps = q.all()

        if not laps:
            return jsonify(None)

        best_s1 = None
        best_s2 = None
        best_s3 = None
        s1_driver = s2_driver = s3_driver = ''

        for lap in laps:
            s1 = _parse_sector_ms(lap.sector_1)
            s2 = _parse_sector_ms(lap.sector_2)
            s3 = _parse_sector_ms(lap.sector_3)
            if s1 and (best_s1 is None or s1 < best_s1):
                best_s1 = s1
                s1_driver = lap.driver_name or ''
                s1_display = lap.sector_1
            if s2 and (best_s2 is None or s2 < best_s2):
                best_s2 = s2
                s2_driver = lap.driver_name or ''
                s2_display = lap.sector_2
            if s3 and (best_s3 is None or s3 < best_s3):
                best_s3 = s3
                s3_driver = lap.driver_name or ''
                s3_display = lap.sector_3

        if not all([best_s1, best_s2, best_s3]):
            return jsonify(None)

        total_ms = best_s1 + best_s2 + best_s3
        return jsonify({
            'laptime_ms': total_ms,
            'laptime_formatted': fmt_ms(total_ms),
            'sector_1': s1_display,
            'sector_1_ms': best_s1,
            'sector_1_driver': s1_driver,
            'sector_2': s2_display,
            'sector_2_ms': best_s2,
            'sector_2_driver': s2_driver,
            'sector_3': s3_display,
            'sector_3_ms': best_s3,
            'sector_3_driver': s3_driver,
        })
    except Exception as e:
        log.error(f"virtual-best: {e}")
        return jsonify(None)


@api_bp.route('/api/track-records/<int:record_id>', methods=['DELETE'])
def api_delete_track_record(record_id):
    """Einzelnen Streckenrekord loeschen."""
    try:
        rec = TrackRecord.query.get(record_id)
        if not rec:
            return jsonify({'error': 'Nicht gefunden'}), 404
        track_name = rec.track_name
        db.session.delete(rec)
        db.session.commit()
        q = TrackRecord.query
        if track_name:
            q = q.filter_by(track_name=track_name)
        new_best = q.order_by(TrackRecord.laptime_ms.asc()).first()
        if new_best:
            socketio.emit('track_record', {
                'laptime_ms': new_best.laptime_ms,
                'laptime_formatted': fmt_ms(new_best.laptime_ms),
                'driver_name': new_best.driver_name,
                'car_name': new_best.car_name,
                'track_name': new_best.track_name,
            })
        return jsonify({'status': 'ok', 'deleted_id': record_id})
    except Exception as e:
        db.session.rollback()
        log.error(f"delete-track-record: {e}")
        return jsonify({'error': str(e)}), 500


@api_bp.route('/api/personal-records')
def api_personal_records():
    """Persoenliche Rekorde. Optional ?track= Filter."""
    try:
        track = request.args.get('track')
        q = PersonalRecord.query
        if track:
            q = q.filter_by(track_name=track)
        records = q.order_by(PersonalRecord.laptime_ms.asc()).all()
        return jsonify([{
            'id': r.id,
            'driver_name': r.driver_name,
            'laptime_ms': r.laptime_ms,
            'laptime_formatted': fmt_ms(r.laptime_ms),
            'car_name': r.car_name,
            'track_name': r.track_name,
        } for r in records])
    except Exception as e:
        log.error(f"personal-records: {e}")
        return jsonify([])


@api_bp.route('/api/personal-records/<int:record_id>', methods=['DELETE'])
def api_delete_personal_record(record_id):
    """Einzelnen persoenlichen Rekord loeschen."""
    try:
        rec = PersonalRecord.query.get(record_id)
        if not rec:
            return jsonify({'error': 'Nicht gefunden'}), 404
        db.session.delete(rec)
        db.session.commit()
        return jsonify({'status': 'ok', 'deleted_id': record_id})
    except Exception as e:
        db.session.rollback()
        log.error(f"delete-personal-record: {e}")
        return jsonify({'error': str(e)}), 500


@api_bp.route('/api/live-feed')
def api_live_feed():
    """Letzte Events als Live-Feed / Ticker."""
    try:
        limit = min(int(request.args.get('limit', 30)), 100)
        items = []

        # Letzte Runden
        for lap in Lap.query.order_by(Lap.created_at.desc()).limit(limit).all():
            items.append({
                'type': 'lap',
                'timestamp': lap.created_at.isoformat() if lap.created_at else None,
                'controller_id': lap.controller_id,
                'driver_name': lap.driver_name,
                'car_name': lap.car_name,
                'lap_number': lap.lap_number,
                'laptime_ms': lap.laptime_ms,
                'laptime_formatted': lap.laptime_display or fmt_ms(lap.laptime_ms) or '--',
                'is_pb': lap.is_personal_best,
                'color': lap.controller_color or lap.car_color,
            })

        # Letzte Penalties
        try:
            for p in Penalty.query.order_by(Penalty.created_at.desc()).limit(10).all():
                entry = {
                    'type': 'penalty',
                    'timestamp': p.created_at.isoformat() if p.created_at else None,
                    'controller_id': p.controller_id,
                    'driver_name': p.driver_name,
                    'penalty_type': p.penalty_type,
                }
                try:
                    entry['penalty_seconds'] = p.penalty_seconds or 0
                except Exception:
                    entry['penalty_seconds'] = 0
                items.append(entry)
        except Exception as e:
            log.warning(f"live-feed penalties: {e}")
            db.session.rollback()

        # Letzte Statusaenderungen
        for s in RaceStatus.query.order_by(RaceStatus.updated_at.desc()).limit(5).all():
            items.append({
                'type': 'status',
                'timestamp': s.updated_at.isoformat() if s.updated_at else None,
                'status': s.status,
                'race_type': s.race_type,
            })

        # DNF/DQ
        try:
            for r in RaceResult.query.filter(
                (RaceResult.retired == True) | (RaceResult.disqualified == True)
            ).order_by(RaceResult.created_at.desc()).limit(5).all():
                items.append({
                    'type': 'dnf' if r.retired else 'dq',
                    'timestamp': r.created_at.isoformat() if r.created_at else None,
                    'controller_id': r.controller_id,
                    'driver_name': r.driver_name,
                })
        except Exception as e:
            log.warning(f"live-feed dnf/dq: {e}")
            db.session.rollback()

        # Streckenrekorde
        for tr in TrackRecord.query.order_by(TrackRecord.created_at.desc()).limit(5).all():
            items.append({
                'type': 'record',
                'timestamp': tr.created_at.isoformat() if tr.created_at else None,
                'driver_name': tr.driver_name,
                'car_name': tr.car_name,
                'laptime_ms': tr.laptime_ms,
                'laptime_formatted': fmt_ms(tr.laptime_ms),
            })

        items.sort(key=lambda x: x.get('timestamp') or '', reverse=True)
        return jsonify(items[:limit])
    except Exception as e:
        log.error(f"live-feed: {e}")
        return jsonify([])


@api_bp.route('/api/backup')
def api_backup():
    """Komplettes Datenbank-Backup als JSON (Streaming fuer grosse Datenmengen)."""
    try:
        ts = datetime.now().strftime('%Y%m%d_%H%M%S')
        batch_size = 500  # Rows pro Batch um RAM zu schonen

        def generate():
            yield '{\n"version": 1,\n'
            yield f'"created_at": "{datetime.utcnow().isoformat()}",\n'

            # Laps (groesste Tabelle - in Batches streamen)
            yield '"laps": [\n'
            first = True
            offset = 0
            while True:
                batch = Lap.query.order_by(Lap.id).offset(offset).limit(batch_size).all()
                if not batch:
                    break
                for l in batch:
                    row = json.dumps({
                        'session_id': l.session_id, 'session_type': l.session_type,
                        'controller_id': l.controller_id, 'driver_name': l.driver_name,
                        'car_name': l.car_name, 'lap_number': l.lap_number,
                        'laptime_ms': l.laptime_ms, 'laptime_display': l.laptime_display,
                        'sector_1': l.sector_1, 'sector_2': l.sector_2,
                        'sector_3': l.sector_3, 'car_color': l.car_color,
                        'controller_color': l.controller_color,
                        'is_personal_best': l.is_personal_best,
                        'track_name': l.track_name,
                        'created_at': l.created_at.isoformat() if l.created_at else None,
                    }, ensure_ascii=False)
                    yield ('  ' if first else ',\n  ') + row
                    first = False
                offset += batch_size
                db.session.expire_all()  # RAM freigeben
            yield '\n],\n'

            # Events (auch gross - in Batches)
            yield '"events": [\n'
            first = True
            offset = 0
            while True:
                batch = Event.query.order_by(Event.id).offset(offset).limit(batch_size).all()
                if not batch:
                    break
                for e in batch:
                    row = json.dumps({
                        'session_id': e.session_id, 'event_type': e.event_type,
                        'raw_json': e.raw_json,
                        'created_at': e.created_at.isoformat() if e.created_at else None,
                    }, ensure_ascii=False)
                    yield ('  ' if first else ',\n  ') + row
                    first = False
                offset += batch_size
                db.session.expire_all()
            yield '\n],\n'

            # Kleinere Tabellen direkt
            yield '"results": ' + json.dumps([{
                'session_id': r.session_id, 'position': r.position,
                'controller_id': r.controller_id, 'driver_name': r.driver_name,
                'total_laps': r.total_laps, 'best_laptime_ms': r.best_laptime_ms,
                'gap': r.gap, 'pitstops': r.pitstops,
                'disqualified': r.disqualified, 'retired': r.retired,
                'created_at': r.created_at.isoformat() if r.created_at else None,
            } for r in RaceResult.query.all()], ensure_ascii=False) + ',\n'

            yield '"penalties": ' + json.dumps([{
                'session_id': p.session_id, 'controller_id': p.controller_id,
                'driver_name': p.driver_name, 'penalty_type': p.penalty_type,
                'penalty_seconds': p.penalty_seconds,
                'created_at': p.created_at.isoformat() if p.created_at else None,
            } for p in Penalty.query.all()], ensure_ascii=False) + ',\n'

            yield '"race_status": ' + json.dumps([{
                'session_id': s.session_id, 'status': s.status,
                'race_type': s.race_type,
                'updated_at': s.updated_at.isoformat() if s.updated_at else None,
            } for s in RaceStatus.query.all()], ensure_ascii=False) + ',\n'

            yield '"track_records": ' + json.dumps([{
                'laptime_ms': t.laptime_ms, 'driver_name': t.driver_name,
                'car_name': t.car_name, 'session_id': t.session_id,
                'controller_id': t.controller_id, 'track_name': t.track_name,
                'created_at': t.created_at.isoformat() if t.created_at else None,
            } for t in TrackRecord.query.all()], ensure_ascii=False) + ',\n'

            yield '"personal_records": ' + json.dumps([{
                'driver_name': r.driver_name, 'laptime_ms': r.laptime_ms,
                'car_name': r.car_name, 'session_id': r.session_id,
                'controller_id': r.controller_id, 'track_name': r.track_name,
                'created_at': r.created_at.isoformat() if r.created_at else None,
                'updated_at': r.updated_at.isoformat() if r.updated_at else None,
            } for r in PersonalRecord.query.all()], ensure_ascii=False) + ',\n'

            yield '"session_names": ' + json.dumps([{
                'session_id': s.session_id, 'display_name': s.display_name,
                'created_at': s.created_at.isoformat() if s.created_at else None,
            } for s in SessionName.query.all()], ensure_ascii=False) + '\n'

            yield '}'

        return Response(
            stream_with_context(generate()),
            mimetype='application/json',
            headers={'Content-Disposition': f'attachment; filename=smartrace_backup_{ts}.json'},
        )
    except Exception as e:
        log.error(f"backup: {e}")
        return jsonify({'error': str(e)}), 500


@api_bp.route('/api/restore', methods=['POST'])
def api_restore():
    """Datenbank aus JSON-Backup wiederherstellen."""
    try:
        data = request.get_json(silent=True)
        if not data or 'version' not in data:
            return jsonify({'error': 'Ungueltiges Backup-Format'}), 400

        counts = {}

        if data.get('laps'):
            for l in data['laps']:
                db.session.add(Lap(
                    session_id=l.get('session_id'), session_type=l.get('session_type'),
                    controller_id=l.get('controller_id'), driver_name=l.get('driver_name'),
                    car_name=l.get('car_name'), lap_number=l.get('lap_number'),
                    laptime_ms=l.get('laptime_ms'), laptime_display=l.get('laptime_display'),
                    sector_1=l.get('sector_1'), sector_2=l.get('sector_2'),
                    sector_3=l.get('sector_3'), car_color=l.get('car_color'),
                    controller_color=l.get('controller_color'),
                    is_personal_best=l.get('is_personal_best', False),
                    track_name=l.get('track_name'),
                    created_at=datetime.fromisoformat(l['created_at']) if l.get('created_at') else None,
                ))
            counts['laps'] = len(data['laps'])

        if data.get('events'):
            for e in data['events']:
                db.session.add(Event(
                    session_id=e.get('session_id'), event_type=e.get('event_type'),
                    raw_json=e.get('raw_json'),
                    created_at=datetime.fromisoformat(e['created_at']) if e.get('created_at') else None,
                ))
            counts['events'] = len(data['events'])

        if data.get('results'):
            for r in data['results']:
                db.session.add(RaceResult(
                    session_id=r.get('session_id'), position=r.get('position'),
                    controller_id=r.get('controller_id'), driver_name=r.get('driver_name'),
                    total_laps=r.get('total_laps'), best_laptime_ms=r.get('best_laptime_ms'),
                    gap=r.get('gap'), pitstops=r.get('pitstops', 0),
                    disqualified=r.get('disqualified', False),
                    retired=r.get('retired', False),
                    created_at=datetime.fromisoformat(r['created_at']) if r.get('created_at') else None,
                ))
            counts['results'] = len(data['results'])

        if data.get('penalties'):
            for p in data['penalties']:
                db.session.add(Penalty(
                    session_id=p.get('session_id'), controller_id=p.get('controller_id'),
                    driver_name=p.get('driver_name'), penalty_type=p.get('penalty_type'),
                    penalty_seconds=p.get('penalty_seconds', 0),
                    created_at=datetime.fromisoformat(p['created_at']) if p.get('created_at') else None,
                ))
            counts['penalties'] = len(data['penalties'])

        if data.get('race_status'):
            for s in data['race_status']:
                db.session.add(RaceStatus(
                    session_id=s.get('session_id'), status=s.get('status'),
                    race_type=s.get('race_type'),
                    updated_at=datetime.fromisoformat(s['updated_at']) if s.get('updated_at') else None,
                ))
            counts['race_status'] = len(data['race_status'])

        if data.get('track_records'):
            for t in data['track_records']:
                db.session.add(TrackRecord(
                    laptime_ms=t.get('laptime_ms'), driver_name=t.get('driver_name'),
                    car_name=t.get('car_name'), session_id=t.get('session_id'),
                    controller_id=t.get('controller_id'),
                    track_name=t.get('track_name'),
                    created_at=datetime.fromisoformat(t['created_at']) if t.get('created_at') else None,
                ))
            counts['track_records'] = len(data['track_records'])

        if data.get('personal_records'):
            for r in data['personal_records']:
                existing = PersonalRecord.query.filter_by(
                    driver_name=r.get('driver_name'),
                    track_name=r.get('track_name'),
                ).first()
                if existing:
                    if r.get('laptime_ms') and r['laptime_ms'] < existing.laptime_ms:
                        existing.laptime_ms = r['laptime_ms']
                        existing.car_name = r.get('car_name')
                        existing.updated_at = datetime.utcnow()
                else:
                    db.session.add(PersonalRecord(
                        driver_name=r.get('driver_name'),
                        laptime_ms=r.get('laptime_ms'),
                        car_name=r.get('car_name'),
                        session_id=r.get('session_id'),
                        controller_id=r.get('controller_id'),
                        track_name=r.get('track_name'),
                        created_at=datetime.fromisoformat(r['created_at']) if r.get('created_at') else None,
                        updated_at=datetime.fromisoformat(r['updated_at']) if r.get('updated_at') else None,
                    ))
            counts['personal_records'] = len(data['personal_records'])

        if data.get('session_names'):
            for s in data['session_names']:
                existing = SessionName.query.filter_by(
                    session_id=s.get('session_id')).first()
                if not existing:
                    db.session.add(SessionName(
                        session_id=s.get('session_id'),
                        display_name=s.get('display_name'),
                        created_at=datetime.fromisoformat(s['created_at']) if s.get('created_at') else None,
                    ))
            counts['session_names'] = len(data['session_names'])

        db.session.commit()
        return jsonify({'status': 'ok', 'imported': counts})
    except Exception as e:
        db.session.rollback()
        log.error(f"restore: {e}")
        return jsonify({'error': str(e)}), 500


@api_bp.route('/api/tracks')
def api_tracks():
    """Alle Strecken mit per-Track Rekorden und Statistiken."""
    try:
        tracks = Track.query.order_by(Track.last_used.desc()).all()
        active_track = _resolve_active_track()
        active_name = active_track.name if active_track else None
        result = []
        for t in tracks:
            track_records = TrackRecord.query.filter_by(
                track_name=t.name,
            ).order_by(TrackRecord.laptime_ms.asc()).limit(10).all()

            track_pbs = PersonalRecord.query.filter_by(
                track_name=t.name,
            ).order_by(PersonalRecord.laptime_ms.asc()).all()

            recs_data = []
            for r in track_records:
                lap = Lap.query.filter_by(
                    laptime_ms=r.laptime_ms, driver_name=r.driver_name,
                ).order_by(Lap.created_at.desc()).first() if r.driver_name else None
                recs_data.append({
                    'laptime_ms': r.laptime_ms,
                    'laptime_formatted': fmt_ms(r.laptime_ms),
                    'driver_name': r.driver_name,
                    'car_name': r.car_name,
                    'date': r.created_at.isoformat() if r.created_at else None,
                    'sector_1': lap.sector_1 if lap else None,
                    'sector_2': lap.sector_2 if lap else None,
                    'sector_3': lap.sector_3 if lap else None,
                })

            pbs_data = [{
                'driver_name': p.driver_name,
                'laptime_ms': p.laptime_ms,
                'laptime_formatted': fmt_ms(p.laptime_ms),
                'car_name': p.car_name,
                'date': p.updated_at.isoformat() if p.updated_at else None,
            } for p in track_pbs]

            track_data = {
                'id': t.id,
                'name': t.name,
                'length': t.length,
                'pitstop_delta': t.pitstop_delta,
                'svg_layout': t.svg_layout,
                'is_active': (t.name == active_name),
                'last_used': t.last_used.isoformat() if t.last_used else None,
                'created_at': t.created_at.isoformat() if t.created_at else None,
                'records': recs_data,
                'personal_records': pbs_data,
            }
            result.append(track_data)

        # Rekorde ohne Streckenzuordnung (Legacy-Daten)
        unassigned_records = TrackRecord.query.filter(db.or_(
            TrackRecord.track_name == None,
            TrackRecord.track_name == '',
        )).order_by(TrackRecord.laptime_ms.asc()).limit(10).all()
        unassigned_pbs = PersonalRecord.query.filter(db.or_(
            PersonalRecord.track_name == None,
            PersonalRecord.track_name == '',
        )).order_by(PersonalRecord.laptime_ms.asc()).all()

        legacy_records = []
        for r in unassigned_records:
            lap = Lap.query.filter_by(
                laptime_ms=r.laptime_ms, driver_name=r.driver_name,
            ).order_by(Lap.created_at.desc()).first() if r.driver_name else None
            legacy_records.append({
                'laptime_ms': r.laptime_ms,
                'laptime_formatted': fmt_ms(r.laptime_ms),
                'driver_name': r.driver_name,
                'car_name': r.car_name,
                'date': r.created_at.isoformat() if r.created_at else None,
                'sector_1': lap.sector_1 if lap else None,
                'sector_2': lap.sector_2 if lap else None,
                'sector_3': lap.sector_3 if lap else None,
            })

        return jsonify({
            'tracks': result,
            'records': legacy_records,
            'personal_records': [{
                'driver_name': p.driver_name,
                'laptime_ms': p.laptime_ms,
                'laptime_formatted': fmt_ms(p.laptime_ms),
                'car_name': p.car_name,
                'date': p.updated_at.isoformat() if p.updated_at else None,
            } for p in unassigned_pbs],
        })
    except Exception as e:
        log.error(f"tracks api: {e}")
        db.session.rollback()
        return jsonify({'tracks': [], 'records': [], 'personal_records': []})


@api_bp.route('/api/tracks', methods=['POST'])
def api_create_track():
    """Strecke manuell anlegen."""
    try:
        data = request.get_json(silent=True) or {}
        name = (data.get('name') or '').strip()
        if not name:
            return jsonify({'error': 'Name ist erforderlich'}), 400
        existing = Track.query.filter_by(name=name).first()
        if existing:
            return jsonify({'error': 'Strecke existiert bereits', 'track_id': existing.id}), 409
        length = data.get('length')
        pitstop_delta = data.get('pitstop_delta')
        svg_layout = (data.get('svg_layout') or '').strip() or None
        if svg_layout and '<svg' not in svg_layout.lower():
            return jsonify({'error': 'Kein gueltiges SVG'}), 400
        track = Track(
            name=name,
            length=float(length) if length else None,
            pitstop_delta=float(pitstop_delta) if pitstop_delta else None,
            svg_layout=svg_layout,
        )
        db.session.add(track)
        db.session.commit()
        return jsonify({'ok': True, 'track_id': track.id}), 201
    except Exception as e:
        log.error(f"create track: {e}")
        db.session.rollback()
        return jsonify({'error': str(e)}), 500


@api_bp.route('/api/tracks/<int:track_id>/layout', methods=['PUT'])
def api_track_layout(track_id):
    """SVG-Layout fuer eine Strecke speichern oder loeschen."""
    try:
        track = Track.query.get(track_id)
        if not track:
            return jsonify({'error': 'Strecke nicht gefunden'}), 404
        data = request.get_json(silent=True) or {}
        svg = (data.get('svg_layout') or '').strip()
        # Einfache Validierung: muss SVG-artig sein oder leer
        if svg and '<svg' not in svg.lower():
            return jsonify({'error': 'Kein gueltiges SVG'}), 400
        track.svg_layout = svg if svg else None
        db.session.commit()
        return jsonify({'ok': True, 'track_id': track_id})
    except Exception as e:
        log.error(f"track layout save: {e}")
        db.session.rollback()
        return jsonify({'error': str(e)}), 500


@api_bp.route('/api/active-track-layout')
def api_active_track_layout():
    """SVG-Layout der aktuell aktiven Strecke zurueckgeben."""
    try:
        track = _resolve_active_track()
        if track and track.svg_layout:
            return jsonify({
                'name': track.name,
                'svg_layout': track.svg_layout,
                'track_id': track.id,
            })
        return jsonify({'name': track.name if track else None, 'svg_layout': None})
    except Exception as e:
        log.error(f"active track layout: {e}")
        db.session.rollback()
        return jsonify({'name': None, 'svg_layout': None})


@api_bp.route('/api/active-track')
def api_active_track():
    """Nur der Name der aktuell aktiven Strecke (leicht, fuer Navbar)."""
    try:
        track = _resolve_active_track()
        return jsonify({'name': track.name if track else None})
    except Exception as e:
        log.error(f"active-track: {e}")
        return jsonify({'name': None})


@api_bp.route('/api/tracks/<int:track_id>/activate', methods=['POST'])
def api_activate_track(track_id):
    """Strecke manuell als aktiv markieren (nur eine ist aktiv)."""
    try:
        track = Track.query.get(track_id)
        if not track:
            return jsonify({'error': 'Strecke nicht gefunden'}), 404
        Track.query.update({'is_active': False}, synchronize_session=False)
        track.is_active = True
        track.last_used = datetime.utcnow()
        db.session.commit()
        set_active_track_name(track.name)
        socketio.emit('active_track', {
            'name': track.name,
            'length': track.length or '',
            'pitstop_delta': track.pitstop_delta or '',
        })
        return jsonify({'ok': True, 'track_id': track_id, 'active': True})
    except Exception as e:
        db.session.rollback()
        log.error(f"activate track: {e}")
        return jsonify({'error': str(e)}), 500


@api_bp.route('/api/tracks/<int:track_id>/deactivate', methods=['POST'])
def api_deactivate_track(track_id):
    """Strecke deaktivieren — danach ist keine Strecke aktiv."""
    try:
        track = Track.query.get(track_id)
        if not track:
            return jsonify({'error': 'Strecke nicht gefunden'}), 404
        Track.query.update({'is_active': False}, synchronize_session=False)
        db.session.commit()
        set_active_track_name(None)  # erzwungen: keine aktive Strecke
        socketio.emit('active_track', {'name': '', 'deactivated': True})
        return jsonify({'ok': True, 'track_id': track_id, 'active': False})
    except Exception as e:
        db.session.rollback()
        log.error(f"deactivate track: {e}")
        return jsonify({'error': str(e)}), 500


@api_bp.route('/api/tracks/<int:track_id>', methods=['DELETE'])
def api_delete_track(track_id):
    """Strecke loeschen. Rundendaten/Rekorde bleiben erhalten (per Name zugeordnet)."""
    try:
        track = Track.query.get(track_id)
        if not track:
            return jsonify({'error': 'Strecke nicht gefunden'}), 404
        was_active = track.is_active
        db.session.delete(track)
        db.session.commit()
        # War es die aktive Strecke, Override aufheben und Clients informieren
        if was_active:
            reset_active_track_override()
            new_active = _resolve_active_track()
            socketio.emit('active_track', {
                'name': new_active.name if new_active else '',
            })
        return jsonify({'ok': True, 'deleted_id': track_id})
    except Exception as e:
        db.session.rollback()
        log.error(f"delete track: {e}")
        return jsonify({'error': str(e)}), 500


@api_bp.route('/api/driver-profile')
def api_driver_profile():
    """Detailliertes Fahrerprofil mit Statistiken, Trend, Rekorden."""
    try:
        name = request.args.get('name', '')
        if not name:
            return jsonify({'error': 'Fahrername angeben'}), 400

        laps = Lap.query.filter(
            Lap.driver_name == name,
            Lap.laptime_ms > 0,
        ).order_by(Lap.created_at.asc()).all()

        if not laps:
            return jsonify({'error': 'Fahrer nicht gefunden'}), 404

        times = [l.laptime_ms for l in laps]
        sessions = list(set(l.session_id for l in laps))

        # Meistgefahrenes Auto
        cars = {}
        for l in laps:
            cars[l.car_name] = cars.get(l.car_name, 0) + 1
        favorite_car = max(cars, key=cars.get) if cars else ''

        # Siege / Podien
        wins = RaceResult.query.filter(
            RaceResult.driver_name == name,
            RaceResult.position == 1,
            RaceResult.retired == False,
            RaceResult.disqualified == False,
        ).count()
        podiums = RaceResult.query.filter(
            RaceResult.driver_name == name,
            RaceResult.position.between(1, 3),
            RaceResult.retired == False,
            RaceResult.disqualified == False,
        ).count()
        penalties = Penalty.query.filter(Penalty.driver_name == name).count()

        # Persoenliche Rekorde (bester ueber alle Strecken)
        pr = PersonalRecord.query.filter_by(
            driver_name=name,
        ).order_by(PersonalRecord.laptime_ms.asc()).first()

        # Konsistenz
        stddev = _calc_stddev(times)
        avg = sum(times) / len(times) if times else 0
        consistency_pct = round((1 - stddev / avg) * 100, 1) if avg > 0 and len(times) >= 2 else 0

        # Letzte 100 Runden fuer Trend-Chart
        recent_laps = [{
            'lap': l.lap_number,
            'time': l.laptime_ms,
            'session': l.session_id,
            'car': l.car_name,
        } for l in laps[-100:]]

        # Session-Historie
        session_stats = []
        for sid in sorted(sessions, key=lambda s: s, reverse=True)[:20]:
            s_laps = [l for l in laps if l.session_id == sid]
            s_times = [l.laptime_ms for l in s_laps]
            session_stats.append({
                'session_id': sid,
                'display_name': _session_display_name(sid),
                'laps': len(s_laps),
                'best_time': min(s_times),
                'best_time_formatted': fmt_ms(min(s_times)),
                'avg_time': round(sum(s_times) / len(s_times)),
                'avg_time_formatted': fmt_ms(round(sum(s_times) / len(s_times))),
                'stddev_ms': round(_calc_stddev(s_times)),
            })

        return jsonify({
            'driver_name': name,
            'total_laps': len(times),
            'total_sessions': len(sessions),
            'best_time': min(times),
            'best_time_formatted': fmt_ms(min(times)),
            'avg_time': round(avg),
            'avg_time_formatted': fmt_ms(round(avg)),
            'wins': wins,
            'podiums': podiums,
            'penalties': penalties,
            'favorite_car': favorite_car,
            'favorite_car_laps': cars.get(favorite_car, 0),
            'consistency_score': consistency_pct,
            'stddev_ms': round(stddev),
            'personal_record': {
                'laptime_ms': pr.laptime_ms,
                'laptime_formatted': fmt_ms(pr.laptime_ms),
                'car_name': pr.car_name,
            } if pr else None,
            'recent_laps': recent_laps,
            'session_history': session_stats,
            'cars': [{'name': k, 'laps': v} for k, v in sorted(
                cars.items(), key=lambda x: -x[1])],
        })
    except Exception as e:
        log.error(f"driver-profile: {e}")
        return jsonify({'error': str(e)}), 500


@api_bp.route('/api/session-name', methods=['PUT'])
def api_session_name():
    """Session umbenennen."""
    try:
        data = request.get_json(silent=True) or {}
        sid = data.get('session_id', '')
        name = data.get('display_name', '').strip()
        if not sid or not name:
            return jsonify({'error': 'session_id und display_name angeben'}), 400

        sn = SessionName.query.filter_by(session_id=sid).first()
        if sn:
            sn.display_name = name
        else:
            db.session.add(SessionName(session_id=sid, display_name=name))
        db.session.commit()
        return jsonify({'status': 'ok', 'session_id': sid, 'display_name': name})
    except Exception as e:
        db.session.rollback()
        log.error(f"session-name: {e}")
        return jsonify({'error': str(e)}), 500


@api_bp.route('/api/cleanup', methods=['POST'])
def api_cleanup():
    """Alte Events bereinigen. Behaelt nur Events der letzten X Tage."""
    try:
        days = int(request.args.get('days', 90))
        cutoff = datetime.utcnow() - timedelta(days=days)

        # Nur sr_events bereinigen (raw JSON, groesste Tabelle)
        deleted = Event.query.filter(Event.created_at < cutoff).delete()
        db.session.commit()

        return jsonify({
            'status': 'ok',
            'deleted_events': deleted,
            'cutoff_date': cutoff.isoformat(),
        })
    except Exception as e:
        db.session.rollback()
        log.error(f"cleanup: {e}")
        return jsonify({'error': str(e)}), 500


@api_bp.route('/api/debug/events')
def api_debug_events():
    """Letzte rohe Events anzeigen - zum Debuggen der SmartRace-Daten."""
    try:
        limit = min(int(request.args.get('limit', 20)), 100)
        etype = request.args.get('type', '')
        q = Event.query.order_by(Event.id.desc())
        if etype:
            q = q.filter(Event.event_type == etype)
        events = q.limit(limit).all()
        result = []
        for e in events:
            raw = {}
            try:
                raw = json.loads(e.raw_json) if e.raw_json else {}
            except Exception:
                raw = {'_raw': e.raw_json}
            result.append({
                'id': e.id,
                'session_id': e.session_id,
                'event_type': e.event_type,
                'timestamp': e.created_at.isoformat() if e.created_at else None,
                'event_data': raw.get('event_data', {}),
                'raw_keys': list(raw.keys()),
            })
        return jsonify(result)
    except Exception as e:
        log.error(f"debug-events: {e}")
        return jsonify({'error': str(e)}), 500


@api_bp.route('/api/db-stats')
def api_db_stats():
    """Datenbank-Statistiken fuer Monitoring."""
    try:
        stats = {
            'laps': Lap.query.count(),
            'events': Event.query.count(),
            'results': RaceResult.query.count(),
            'penalties': Penalty.query.count(),
            'track_records': TrackRecord.query.count(),
            'sessions': db.session.query(
                func.count(func.distinct(Lap.session_id))).scalar(),
        }

        # Aeltester und neuester Eintrag
        oldest = Lap.query.order_by(Lap.created_at.asc()).first()
        newest = Lap.query.order_by(Lap.created_at.desc()).first()
        stats['oldest_lap'] = oldest.created_at.isoformat() if oldest and oldest.created_at else None
        stats['newest_lap'] = newest.created_at.isoformat() if newest and newest.created_at else None

        return jsonify(stats)
    except Exception as e:
        log.error(f"db-stats: {e}")
        return jsonify({})


@api_bp.route('/api/laps/count')
def api_laps_count():
    """Gesamtanzahl Runden (ohne Filter, fuer Datenbank-Seite)."""
    try:
        return jsonify({'count': Lap.query.count()})
    except Exception as e:
        log.error(f"laps-count: {e}")
        return jsonify({'count': 0})


@api_bp.route('/api/health')
def api_health():
    """Health-Check fuer Docker."""
    try:
        db.session.execute(text('SELECT 1'))
        return jsonify({'status': 'ok'})
    except Exception as e:
        return jsonify({'status': 'error', 'detail': str(e)}), 503


@api_bp.route('/api/version')
def api_version():
    """Versionsinformationen aus VERSION-Datei und Build-Datum."""
    app_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
    version = '?'
    try:
        with open(os.path.join(app_dir, 'VERSION')) as f:
            version = f.read().strip()
    except FileNotFoundError:
        pass

    build_date = ''
    try:
        with open(os.path.join(app_dir, 'build-date.txt')) as f:
            build_date = f.read().strip()[:10]
    except FileNotFoundError:
        pass

    return jsonify({
        'version': version,
        'build_date': build_date,
    })
