from flask import Flask, render_template, request, jsonify, Response
from flask_sqlalchemy import SQLAlchemy
from flask_socketio import SocketIO
from flask_cors import CORS
from sqlalchemy import func, text
from datetime import datetime
import json
import os
import csv
import io
import re
import logging

# =============================================================================
# App
# =============================================================================

app = Flask(__name__)
CORS(app)

app.config['SQLALCHEMY_DATABASE_URI'] = os.getenv(
    'DATABASE_URL', 'sqlite:///smartrace.db'
)
app.config['SQLALCHEMY_TRACK_MODIFICATIONS'] = False
app.config['SECRET_KEY'] = os.getenv('SECRET_KEY', 'sr-dashboard-secret')

db = SQLAlchemy(app)
socketio = SocketIO(app, cors_allowed_origins="*", async_mode='eventlet')
log = logging.getLogger('smartrace')


# =============================================================================
# Modelle  (Prefix "sr_" um Konflikte mit alten Tabellen zu vermeiden)
# =============================================================================

class Event(db.Model):
    __tablename__ = 'sr_events'
    id = db.Column(db.Integer, primary_key=True)
    session_id = db.Column(db.String(100), index=True)
    event_type = db.Column(db.String(50))
    created_at = db.Column(db.DateTime, default=datetime.utcnow)
    raw_json = db.Column(db.Text)


class Lap(db.Model):
    __tablename__ = 'sr_laps'
    id = db.Column(db.Integer, primary_key=True)
    session_id = db.Column(db.String(100), index=True)
    session_type = db.Column(db.String(50))
    controller_id = db.Column(db.String(10), index=True)
    driver_name = db.Column(db.String(100))
    car_name = db.Column(db.String(100))
    lap_number = db.Column(db.Integer)
    laptime_ms = db.Column(db.Integer)
    laptime_display = db.Column(db.String(20))
    sector_1 = db.Column(db.String(20))
    sector_2 = db.Column(db.String(20))
    sector_3 = db.Column(db.String(20))
    car_color = db.Column(db.String(20))
    controller_color = db.Column(db.String(20))
    is_personal_best = db.Column(db.Boolean, default=False)
    created_at = db.Column(db.DateTime, default=datetime.utcnow, index=True)


class RaceResult(db.Model):
    __tablename__ = 'sr_results'
    id = db.Column(db.Integer, primary_key=True)
    session_id = db.Column(db.String(100), index=True)
    position = db.Column(db.Integer)
    controller_id = db.Column(db.String(10))
    driver_name = db.Column(db.String(100))
    total_laps = db.Column(db.Integer)
    best_laptime_ms = db.Column(db.Integer)
    gap = db.Column(db.String(50))
    disqualified = db.Column(db.Boolean, default=False)
    retired = db.Column(db.Boolean, default=False)
    created_at = db.Column(db.DateTime, default=datetime.utcnow)


class Penalty(db.Model):
    __tablename__ = 'sr_penalties'
    id = db.Column(db.Integer, primary_key=True)
    session_id = db.Column(db.String(100), index=True)
    controller_id = db.Column(db.String(10))
    driver_name = db.Column(db.String(100))
    penalty_type = db.Column(db.String(50))
    created_at = db.Column(db.DateTime, default=datetime.utcnow)


class RaceStatus(db.Model):
    __tablename__ = 'sr_race_status'
    id = db.Column(db.Integer, primary_key=True)
    session_id = db.Column(db.String(100), index=True)
    status = db.Column(db.String(50))
    race_type = db.Column(db.String(50))
    updated_at = db.Column(db.DateTime, default=datetime.utcnow)


# =============================================================================
# DB Init  (robust: erst versuchen, bei Fehler alles bereinigen)
# =============================================================================

def init_db():
    try:
        db.create_all()
        log.info("DB-Tabellen bereit.")
    except Exception as e:
        log.warning(f"create_all fehlgeschlagen: {e}")
        db.session.rollback()
        try:
            db.session.execute(text("""
                DO $$ DECLARE r RECORD;
                BEGIN
                    FOR r IN (SELECT tablename FROM pg_tables
                              WHERE schemaname = 'public') LOOP
                        EXECUTE 'DROP TABLE IF EXISTS '
                                || quote_ident(r.tablename) || ' CASCADE';
                    END LOOP;
                END $$;
            """))
            db.session.commit()
        except Exception:
            db.session.rollback()
        db.create_all()
        log.info("DB nach Bereinigung neu erstellt.")


with app.app_context():
    init_db()


# =============================================================================
# Hilfsfunktionen
# =============================================================================

def rgb_to_hex(val):
    """'rgb(r,g,b)' -> '#rrggbb', gibt None bei ungueltigem Input."""
    if not val:
        return None
    if val.startswith('#'):
        return val
    nums = re.findall(r'\d+', val)
    if len(nums) >= 3:
        return f"#{int(nums[0]):02x}{int(nums[1]):02x}{int(nums[2]):02x}"
    return None


def fmt_ms(ms):
    """Millisekunden -> 'M:SS.mmm'"""
    if not ms or ms <= 0:
        return None
    s = ms / 1000
    return f"{int(s // 60)}:{s % 60:06.3f}"


# Events die eine NEUE Session starten (neues Rennen / Training)
_NEW_SESSION_EVENTS = {'ui.reset', 'ui.status_change'}


def _get_session_id(etype):
    """Session-ID ermitteln.

    Neue Session bei ui.reset / ui.status_change.
    Alle anderen Events (lap_update, penalty, car_removed, race_result)
    gehoeren zur aktuellen Session.
    """
    if etype in _NEW_SESSION_EVENTS:
        return f"session_{datetime.now().strftime('%Y%m%d_%H%M%S')}"

    latest = db.session.query(Event.session_id).order_by(Event.id.desc()).first()
    if latest and latest.session_id:
        return latest.session_id

    return f"session_{datetime.now().strftime('%Y%m%d_%H%M%S')}"


def _get_current_race_type():
    """Aktuellen Renntyp aus letztem Status-Event ermitteln."""
    rs = RaceStatus.query.order_by(RaceStatus.id.desc()).first()
    if rs and rs.race_type:
        return rs.race_type
    return 'Training'


# =============================================================================
# Seiten
# =============================================================================

@app.route('/')
def page_dashboard():
    return render_template('dashboard.html')

@app.route('/leaderboard')
def page_leaderboard():
    return render_template('leaderboard.html')

@app.route('/analytics')
def page_analytics():
    return render_template('analytics.html')

@app.route('/database')
def page_database():
    return render_template('database.html')

@app.route('/stats')
def page_stats():
    return render_template('stats.html')


# =============================================================================
# SmartRace Datenschnittstelle
# =============================================================================

@app.route('/api/smartrace', methods=['POST', 'OPTIONS'])
def receive_smartrace():
    """Empfaengt Events von SmartRace (POST mit JSON).

    Format: { "time": ..., "event_type": "ui.lap_update", "event_data": { ... } }
    """
    if request.method == 'OPTIONS':
        return '', 200

    data = request.get_json(silent=True)
    if not data:
        return jsonify({'error': 'Kein JSON'}), 400

    try:
        etype = data.get('event_type', 'unknown')
        ed = data.get('event_data') or {}

        sid = _get_session_id(etype)

        db.session.add(Event(
            session_id=sid, event_type=etype, raw_json=json.dumps(data),
        ))

        if etype == 'ui.lap_update':
            _save_lap(sid, ed)
        elif etype == 'ui.race_result':
            _save_result(sid, ed)
        elif etype == 'ui.status_change':
            _save_status(sid, ed)
        elif etype == 'ui.penalty':
            _save_penalty(sid, ed)
        elif etype == 'ui.car_removed':
            _save_car_removed(sid, ed)

        db.session.commit()
        log.info(f"Event: {etype} | Session: {sid}")

        # WebSocket: alle verbundenen Clients benachrichtigen
        socketio.emit('new_data', {
            'event_type': etype,
            'session_id': sid,
        })

        if etype == 'ui.status_change':
            socketio.emit('race_status', {
                'session_id': sid,
                'status': ed.get('status') or ed.get('new_status') or 'unknown',
                'race_type': (ed.get('type') or ed.get('race_type')
                              or ed.get('mode') or ''),
            })

        if etype == 'ui.penalty':
            socketio.emit('penalty', {
                'session_id': sid,
                'controller_id': str(ed.get('controller_id', '')),
                'driver_name': (ed.get('driver_data') or {}).get('name', ''),
            })

        return jsonify({'status': 'ok', 'event_type': etype})

    except Exception as e:
        db.session.rollback()
        log.error(f"SmartRace-Fehler: {e}")
        return jsonify({'error': str(e)}), 500


def _save_lap(sid, ed):
    cid = str(ed.get('controller_id', '0'))
    dd = ed.get('driver_data') or {}
    cd = ed.get('car_data') or {}
    ctrl = ed.get('controller_data') or {}

    db.session.add(Lap(
        session_id=sid,
        session_type=_get_current_race_type(),
        controller_id=cid,
        driver_name=dd.get('name') or f"Fahrer {cid}",
        car_name=cd.get('name') or f"Auto {cid}",
        lap_number=ed.get('lap'),
        laptime_ms=ed.get('laptime_raw'),
        laptime_display=ed.get('laptime'),
        sector_1=ed.get('sector_1'),
        sector_2=ed.get('sector_2'),
        sector_3=ed.get('sector_3'),
        car_color=rgb_to_hex(cd.get('color')),
        controller_color=rgb_to_hex(ctrl.get('color_bg')),
        is_personal_best=bool(ed.get('lap_pb', False)),
    ))


def _save_result(sid, ed):
    for pos, r in (ed.get('result') or {}).items():
        db.session.add(RaceResult(
            session_id=sid,
            position=int(pos),
            controller_id=str(r.get('controller_id', '')),
            driver_name=r.get('driver_name', ''),
            total_laps=r.get('laps', 0),
            best_laptime_ms=r.get('best_laptime'),
            gap=r.get('gap', ''),
            disqualified=bool(r.get('disqualified', False)),
            retired=bool(r.get('retired', False)),
        ))


def _save_status(sid, ed):
    status = (ed.get('status') or ed.get('new_status')
              or ed.get('state') or 'unknown')
    race_type = (ed.get('type') or ed.get('race_type')
                 or ed.get('mode') or ed.get('session_type') or '')

    db.session.add(RaceStatus(
        session_id=sid,
        status=status,
        race_type=race_type,
    ))


def _save_penalty(sid, ed):
    cid = str(ed.get('controller_id', '0'))
    dd = ed.get('driver_data') or {}
    penalty_type = (ed.get('type') or ed.get('penalty')
                    or ed.get('penalty_type') or 'Strafe')

    db.session.add(Penalty(
        session_id=sid,
        controller_id=cid,
        driver_name=dd.get('name') or f"Fahrer {cid}",
        penalty_type=penalty_type,
    ))


def _save_car_removed(sid, ed):
    cid = str(ed.get('controller_id', '0'))
    dd = ed.get('driver_data') or {}

    db.session.add(RaceResult(
        session_id=sid,
        position=0,
        controller_id=cid,
        driver_name=dd.get('name') or f"Fahrer {cid}",
        total_laps=0,
        retired=True,
    ))


# =============================================================================
# WebSocket
# =============================================================================

@socketio.on('connect')
def handle_connect():
    log.info("WebSocket-Client verbunden")


@socketio.on('disconnect')
def handle_disconnect():
    log.info("WebSocket-Client getrennt")


# =============================================================================
# API
# =============================================================================

@app.route('/api/live-data')
def api_live_data():
    """Live-Daten gruppiert nach Controller."""
    try:
        q = Lap.query
        sid = request.args.get('session_id')
        if sid and sid != 'all':
            q = q.filter(Lap.session_id == sid)

        laps = q.order_by(Lap.created_at.desc()).all()
        if not laps:
            return jsonify({})

        # Penalties pro Controller zaehlen
        penalty_q = Penalty.query
        if sid and sid != 'all':
            penalty_q = penalty_q.filter(Penalty.session_id == sid)
        penalty_counts = {}
        for p in penalty_q.all():
            penalty_counts[p.controller_id] = penalty_counts.get(p.controller_id, 0) + 1

        # DNF/DQ Status pruefen
        dnf_dq = {}
        result_q = RaceResult.query
        if sid and sid != 'all':
            result_q = result_q.filter(RaceResult.session_id == sid)
        for r in result_q.filter(
            (RaceResult.retired == True) | (RaceResult.disqualified == True)
        ).all():
            dnf_dq[r.controller_id] = {
                'retired': r.retired,
                'disqualified': r.disqualified,
            }

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


@app.route('/api/sessions')
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
            'total_laps': r.total_laps,
            'drivers': r.drivers,
            'start_time': r.start_time.isoformat() if r.start_time else None,
            'end_time': r.end_time.isoformat() if r.end_time else None,
        } for r in rows])
    except Exception as e:
        log.error(f"sessions: {e}")
        return jsonify([])


@app.route('/api/laps')
def api_laps():
    """Rundenzeiten mit Filtern."""
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

        df = request.args.get('date_from')
        if df:
            q = q.filter(Lap.created_at >= f"{df} 00:00:00")

        dt = request.args.get('date_to')
        if dt:
            q = q.filter(Lap.created_at <= f"{dt} 23:59:59")

        laps = q.order_by(Lap.created_at.desc()).limit(2000).all()

        return jsonify([{
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
            'timestamp': l.created_at.isoformat() if l.created_at else None,
        } for l in laps])
    except Exception as e:
        log.error(f"laps: {e}")
        return jsonify([])


@app.route('/api/analytics')
def api_analytics():
    """Aggregierte Daten fuer Charts."""
    try:
        q = Lap.query
        sid = request.args.get('session_id')
        if sid and sid != 'all':
            q = q.filter(Lap.session_id == sid)

        stats = {}
        for lap in q.all():
            name = lap.driver_name or f"Fahrer {lap.controller_id}"
            if name not in stats:
                stats[name] = {'laps': [], 'best': None, 'count': 0}

            if lap.laptime_ms and lap.laptime_ms > 0:
                stats[name]['laps'].append(lap.laptime_ms)
                stats[name]['count'] += 1
                b = stats[name]['best']
                if b is None or lap.laptime_ms < b:
                    stats[name]['best'] = lap.laptime_ms

        return jsonify({
            name: {
                'avg_time': round(sum(s['laps']) / len(s['laps'])),
                'best_time': s['best'] or 0,
                'total_laps': s['count'],
                'laps': s['laps'][:50],
            }
            for name, s in stats.items() if s['laps']
        })
    except Exception as e:
        log.error(f"analytics: {e}")
        return jsonify({})


@app.route('/api/penalties')
def api_penalties():
    """Strafen mit optionalem Session-Filter."""
    try:
        q = Penalty.query
        sid = request.args.get('session_id')
        if sid and sid != 'all':
            q = q.filter(Penalty.session_id == sid)

        return jsonify([{
            'id': p.id,
            'session_id': p.session_id,
            'controller_id': p.controller_id,
            'driver_name': p.driver_name,
            'penalty_type': p.penalty_type,
            'timestamp': p.created_at.isoformat() if p.created_at else None,
        } for p in q.order_by(Penalty.created_at.desc()).all()])
    except Exception as e:
        log.error(f"penalties: {e}")
        return jsonify([])


@app.route('/api/race-status')
def api_race_status():
    """Aktueller Rennstatus."""
    try:
        rs = RaceStatus.query.order_by(RaceStatus.id.desc()).first()
        if rs:
            return jsonify({
                'session_id': rs.session_id,
                'status': rs.status,
                'race_type': rs.race_type,
                'updated_at': rs.updated_at.isoformat() if rs.updated_at else None,
            })
        return jsonify({'status': 'waiting', 'race_type': '', 'session_id': ''})
    except Exception as e:
        log.error(f"race-status: {e}")
        return jsonify({'status': 'unknown'})


@app.route('/api/driver-stats')
def api_driver_stats():
    """Fahrer-Statistiken ueber alle Sessions."""
    try:
        lap_rows = db.session.query(
            Lap.driver_name,
            func.count(Lap.id).label('total_laps'),
            func.count(func.distinct(Lap.session_id)).label('total_sessions'),
            func.min(Lap.laptime_ms).label('best_time'),
            func.avg(Lap.laptime_ms).label('avg_time'),
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

        drivers = []
        for r in lap_rows:
            rs = result_stats.get(r.driver_name, {'wins': 0, 'podiums': 0})
            avg = round(r.avg_time) if r.avg_time else 0
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
            })

        return jsonify(sorted(drivers, key=lambda d: d['best_time'] or 999999))
    except Exception as e:
        log.error(f"driver-stats: {e}")
        return jsonify([])


@app.route('/api/car-stats')
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


@app.route('/api/filters')
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


@app.route('/api/export/csv')
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
            q = q.filter(Lap.created_at >= f"{df} 00:00:00")

        dt = request.args.get('date_to')
        if dt:
            q = q.filter(Lap.created_at <= f"{dt} 23:59:59")

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


@app.route('/api/health')
def api_health():
    """Health-Check fuer Docker."""
    try:
        db.session.execute(text('SELECT 1'))
        return jsonify({'status': 'ok'})
    except Exception as e:
        return jsonify({'status': 'error', 'detail': str(e)}), 503


# =============================================================================
# Start
# =============================================================================

if __name__ == '__main__':
    socketio.run(
        app,
        host='0.0.0.0',
        port=int(os.getenv('PORT', 5000)),
        debug=os.getenv('FLASK_DEBUG', 'false').lower() == 'true',
    )
