from flask import Flask, render_template, request, jsonify, Response, stream_with_context
from flask_sqlalchemy import SQLAlchemy
from flask_socketio import SocketIO
from flask_cors import CORS
from sqlalchemy import func, text, Index
from datetime import datetime, timedelta
import json
import os
import csv
import io
import re
import math
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
app.config['SQLALCHEMY_ENGINE_OPTIONS'] = {
    'pool_size': 5,
    'max_overflow': 10,
    'pool_recycle': 300,
    'pool_pre_ping': True,
}
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
    __table_args__ = (
        Index('ix_sr_laps_session_controller', 'session_id', 'controller_id'),
        Index('ix_sr_laps_driver_name', 'driver_name'),
        Index('ix_sr_laps_car_name', 'car_name'),
        Index('ix_sr_laps_session_created', 'session_id', 'created_at'),
    )
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


class TrackRecord(db.Model):
    __tablename__ = 'sr_track_records'
    id = db.Column(db.Integer, primary_key=True)
    laptime_ms = db.Column(db.Integer, nullable=False)
    driver_name = db.Column(db.String(100))
    car_name = db.Column(db.String(100))
    session_id = db.Column(db.String(100))
    controller_id = db.Column(db.String(10))
    created_at = db.Column(db.DateTime, default=datetime.utcnow)


class PersonalRecord(db.Model):
    __tablename__ = 'sr_personal_records'
    id = db.Column(db.Integer, primary_key=True)
    driver_name = db.Column(db.String(100), unique=True, index=True)
    laptime_ms = db.Column(db.Integer, nullable=False)
    car_name = db.Column(db.String(100))
    session_id = db.Column(db.String(100))
    controller_id = db.Column(db.String(10))
    created_at = db.Column(db.DateTime, default=datetime.utcnow)
    updated_at = db.Column(db.DateTime, default=datetime.utcnow)


class SessionName(db.Model):
    __tablename__ = 'sr_session_names'
    id = db.Column(db.Integer, primary_key=True)
    session_id = db.Column(db.String(100), unique=True, index=True)
    display_name = db.Column(db.String(200))
    created_at = db.Column(db.DateTime, default=datetime.utcnow)


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


def _calc_stddev(times):
    """Standardabweichung einer Liste von Zeiten berechnen."""
    if len(times) < 2:
        return 0
    avg = sum(times) / len(times)
    variance = sum((t - avg) ** 2 for t in times) / len(times)
    return math.sqrt(variance)


def _session_display_name(session_id, session_type=None):
    """Lesbaren Session-Namen generieren oder gespeicherten zurueckgeben."""
    sn = SessionName.query.filter_by(session_id=session_id).first()
    if sn:
        return sn.display_name
    # Typ ermitteln: uebergebener Wert, oder aus RaceStatus/Laps der Session
    type_label = session_type
    if not type_label:
        rs = RaceStatus.query.filter_by(session_id=session_id).order_by(
            RaceStatus.id.desc()).first()
        if rs and rs.race_type:
            type_label = rs.race_type
    if not type_label:
        lap = Lap.query.filter(
            Lap.session_id == session_id,
            Lap.session_type.isnot(None),
            Lap.session_type != '',
            Lap.session_type != 'Training',
        ).first()
        if lap:
            type_label = lap.session_type
    type_label = type_label or 'Training'
    # Auto-generieren aus session_id Format "session_YYYYMMDD_HHMMSS"
    try:
        parts = session_id.replace('session_', '')
        dt = datetime.strptime(parts, '%Y%m%d_%H%M%S')
        date_str = dt.strftime('%d.%m.%Y %H:%M')
        return f"{type_label} - {date_str}"
    except Exception:
        return session_id


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
    """Aktuellen Renntyp aus letztem Status-Event oder Race-Result ermitteln."""
    rs = RaceStatus.query.order_by(RaceStatus.id.desc()).first()
    if rs and rs.race_type:
        return rs.race_type
    # Fallback: letztes race_result Event in raw_json pruefen
    evt = Event.query.filter_by(event_type='ui.race_result').order_by(
        Event.id.desc()).first()
    if evt and evt.raw_json:
        try:
            ed = json.loads(evt.raw_json).get('event_data', {})
            rt = ed.get('type') or ed.get('race_type') or ''
            if rt:
                return rt
        except Exception:
            pass
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

@app.route('/head-to-head')
def page_head_to_head():
    return render_template('head-to-head.html')

@app.route('/results')
def page_results():
    return render_template('results.html')

@app.route('/session-compare')
def page_session_compare():
    return render_template('session-compare.html')

@app.route('/driver/<name>')
def page_driver_profile(name):
    return render_template('driver-profile.html', driver_name=name)


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
            race_type_ws = _get_current_race_type()
            socketio.emit('race_status', {
                'session_id': sid,
                'status': ed.get('status') or ed.get('new_status') or 'unknown',
                'race_type': race_type_ws if race_type_ws != 'Training' else '',
            })

        if etype == 'ui.race_result':
            result_type = (ed.get('type') or ed.get('race_type') or '')
            socketio.emit('race_status', {
                'session_id': sid,
                'status': 'ended',
                'race_type': result_type,
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

    laptime_ms = ed.get('laptime_raw')
    driver_name = dd.get('name') or f"Fahrer {cid}"
    car_name = cd.get('name') or f"Auto {cid}"

    db.session.add(Lap(
        session_id=sid,
        session_type=_get_current_race_type(),
        controller_id=cid,
        driver_name=driver_name,
        car_name=car_name,
        lap_number=ed.get('lap'),
        laptime_ms=laptime_ms,
        laptime_display=ed.get('laptime'),
        sector_1=ed.get('sector_1'),
        sector_2=ed.get('sector_2'),
        sector_3=ed.get('sector_3'),
        car_color=rgb_to_hex(cd.get('color')),
        controller_color=rgb_to_hex(ctrl.get('color_bg')),
        is_personal_best=bool(ed.get('lap_pb', False)),
    ))

    # Streckenrekord pruefen
    if laptime_ms and laptime_ms > 0:
        current_record = TrackRecord.query.order_by(TrackRecord.laptime_ms.asc()).first()
        if not current_record or laptime_ms < current_record.laptime_ms:
            db.session.add(TrackRecord(
                laptime_ms=laptime_ms,
                driver_name=driver_name,
                car_name=car_name,
                session_id=sid,
                controller_id=cid,
            ))
            socketio.emit('track_record', {
                'laptime_ms': laptime_ms,
                'laptime_formatted': fmt_ms(laptime_ms),
                'driver_name': driver_name,
                'car_name': car_name,
            })

        # Persoenlicher Rekord pruefen
        pr = PersonalRecord.query.filter_by(driver_name=driver_name).first()
        if not pr:
            db.session.add(PersonalRecord(
                driver_name=driver_name, laptime_ms=laptime_ms,
                car_name=car_name, session_id=sid, controller_id=cid,
            ))
        elif laptime_ms < pr.laptime_ms:
            pr.laptime_ms = laptime_ms
            pr.car_name = car_name
            pr.session_id = sid
            pr.controller_id = cid
            pr.updated_at = datetime.utcnow()


def _save_result(sid, ed):
    # Race-Type aus dem Result-Event extrahieren und in RaceStatus speichern
    race_type = (ed.get('type') or ed.get('race_type') or '').strip()
    if race_type:
        rs = RaceStatus.query.filter_by(session_id=sid).order_by(
            RaceStatus.id.desc()).first()
        if rs:
            rs.race_type = race_type
        else:
            db.session.add(RaceStatus(
                session_id=sid, status='ended', race_type=race_type,
            ))
        # Auch bestehende Laps dieser Session aktualisieren
        Lap.query.filter_by(session_id=sid).update(
            {'session_type': race_type}, synchronize_session=False)

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
        else:
            # Ohne explizite Session: nur letzte Session laden (Performance)
            latest_event = db.session.query(Event.session_id).order_by(
                Event.id.desc()).first()
            if latest_event and latest_event.session_id:
                q = q.filter(Lap.session_id == latest_event.session_id)
            else:
                # Fallback: letzte 2 Stunden
                cutoff = datetime.utcnow() - timedelta(hours=2)
                q = q.filter(Lap.created_at >= cutoff)

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
            'display_name': _session_display_name(r.session_id, r.session_type),
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

        # 2. Letzte 50 Rundenzeiten pro Fahrer (fuer Chart)
        result = {}
        for name, s in agg.items():
            lap_q = Lap.query.filter(
                Lap.driver_name == name,
                Lap.laptime_ms > 0,
            )
            if sid and sid != 'all':
                lap_q = lap_q.filter(Lap.session_id == sid)

            recent = lap_q.order_by(Lap.created_at.desc()).limit(50).all()
            # Umkehren fuer chronologische Reihenfolge
            laps_list = [l.laptime_ms for l in reversed(recent)]

            result[name] = {
                'avg_time': s['avg_time'],
                'best_time': s['best_time'],
                'total_laps': s['total_laps'],
                'laps': laps_list,
            }

        return jsonify(result)
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

        # Konsistenz-Score: Stddev pro Fahrer berechnen
        consistency_map = {}
        for r in lap_rows:
            times = [l.laptime_ms for l in Lap.query.filter(
                Lap.driver_name == r.driver_name,
                Lap.laptime_ms > 0,
            ).with_entities(Lap.laptime_ms).all()]
            stddev = _calc_stddev(times)
            avg_val = sum(times) / len(times) if times else 0
            if avg_val > 0 and len(times) >= 2:
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


@app.route('/api/track-record')
def api_track_record():
    """Aktueller Streckenrekord (persistent)."""
    try:
        rec = TrackRecord.query.order_by(TrackRecord.laptime_ms.asc()).first()
        if rec:
            return jsonify({
                'laptime_ms': rec.laptime_ms,
                'laptime_formatted': fmt_ms(rec.laptime_ms),
                'driver_name': rec.driver_name,
                'car_name': rec.car_name,
                'session_id': rec.session_id,
                'created_at': rec.created_at.isoformat() if rec.created_at else None,
            })
        return jsonify(None)
    except Exception as e:
        log.error(f"track-record: {e}")
        return jsonify(None)


@app.route('/api/live-feed')
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
        for p in Penalty.query.order_by(Penalty.created_at.desc()).limit(10).all():
            items.append({
                'type': 'penalty',
                'timestamp': p.created_at.isoformat() if p.created_at else None,
                'controller_id': p.controller_id,
                'driver_name': p.driver_name,
                'penalty_type': p.penalty_type,
            })

        # Letzte Statusaenderungen
        for s in RaceStatus.query.order_by(RaceStatus.updated_at.desc()).limit(5).all():
            items.append({
                'type': 'status',
                'timestamp': s.updated_at.isoformat() if s.updated_at else None,
                'status': s.status,
                'race_type': s.race_type,
            })

        # DNF/DQ
        for r in RaceResult.query.filter(
            (RaceResult.retired == True) | (RaceResult.disqualified == True)
        ).order_by(RaceResult.created_at.desc()).limit(5).all():
            items.append({
                'type': 'dnf' if r.retired else 'dq',
                'timestamp': r.created_at.isoformat() if r.created_at else None,
                'controller_id': r.controller_id,
                'driver_name': r.driver_name,
            })

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


@app.route('/api/head-to-head')
def api_head_to_head():
    """Vergleich zweier Fahrer."""
    try:
        d1 = request.args.get('driver1', '')
        d2 = request.args.get('driver2', '')
        sid = request.args.get('session_id')

        if not d1 or not d2:
            return jsonify({'error': 'Zwei Fahrer angeben'}), 400

        # Beide Fahrer in einem Query laden (statt N+1)
        q = Lap.query.filter(
            Lap.driver_name.in_([d1, d2]),
            Lap.laptime_ms > 0,
        )
        if sid and sid != 'all':
            q = q.filter(Lap.session_id == sid)

        all_laps = q.order_by(Lap.lap_number.asc()).all()

        # Nach Fahrer gruppieren
        laps_by_driver = {d1: [], d2: []}
        for lap in all_laps:
            if lap.driver_name in laps_by_driver:
                laps_by_driver[lap.driver_name].append(lap)

        # Bestzeiten pro Fahrer pro Session (fuer Siege)
        bests = {}  # {(driver, session): best_time}
        for lap in all_laps:
            key = (lap.driver_name, lap.session_id)
            if key not in bests or lap.laptime_ms < bests[key]:
                bests[key] = lap.laptime_ms

        # Penalties fuer beide Fahrer in einem Query
        penalty_counts = {}
        for row in db.session.query(
            Penalty.driver_name,
            func.count(Penalty.id).label('cnt'),
        ).filter(
            Penalty.driver_name.in_([d1, d2])
        ).group_by(Penalty.driver_name).all():
            penalty_counts[row.driver_name] = row.cnt

        result = {}
        for name in [d1, d2]:
            laps = laps_by_driver[name]
            times = [l.laptime_ms for l in laps]
            sessions = set(l.session_id for l in laps)

            # Siege: Sessions wo dieser Fahrer schneller war
            opp = d2 if name == d1 else d1
            wins = 0
            for s in sessions:
                my_best = bests.get((name, s))
                opp_best = bests.get((opp, s))
                if my_best and opp_best and my_best < opp_best:
                    wins += 1

            result[name] = {
                'driver_name': name,
                'total_laps': len(laps),
                'best_time': min(times) if times else None,
                'best_time_formatted': fmt_ms(min(times)) if times else '--',
                'avg_time': round(sum(times) / len(times)) if times else None,
                'avg_time_formatted': fmt_ms(round(sum(times) / len(times))) if times else '--',
                'total_sessions': len(sessions),
                'wins': wins,
                'penalties': penalty_counts.get(name, 0),
                'lap_times': [
                    {
                        'lap': l.lap_number,
                        'time': l.laptime_ms,
                        'formatted': l.laptime_display or fmt_ms(l.laptime_ms) or '--',
                        'session_id': l.session_id,
                    }
                    for l in laps
                ],
            }

        return jsonify(result)
    except Exception as e:
        log.error(f"head-to-head: {e}")
        return jsonify({'error': str(e)}), 500


@app.route('/api/backup')
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
                'gap': r.gap, 'disqualified': r.disqualified, 'retired': r.retired,
                'created_at': r.created_at.isoformat() if r.created_at else None,
            } for r in RaceResult.query.all()], ensure_ascii=False) + ',\n'

            yield '"penalties": ' + json.dumps([{
                'session_id': p.session_id, 'controller_id': p.controller_id,
                'driver_name': p.driver_name, 'penalty_type': p.penalty_type,
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
                'controller_id': t.controller_id,
                'created_at': t.created_at.isoformat() if t.created_at else None,
            } for t in TrackRecord.query.all()], ensure_ascii=False) + ',\n'

            yield '"personal_records": ' + json.dumps([{
                'driver_name': r.driver_name, 'laptime_ms': r.laptime_ms,
                'car_name': r.car_name, 'session_id': r.session_id,
                'controller_id': r.controller_id,
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


@app.route('/api/restore', methods=['POST'])
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
                    gap=r.get('gap'), disqualified=r.get('disqualified', False),
                    retired=r.get('retired', False),
                    created_at=datetime.fromisoformat(r['created_at']) if r.get('created_at') else None,
                ))
            counts['results'] = len(data['results'])

        if data.get('penalties'):
            for p in data['penalties']:
                db.session.add(Penalty(
                    session_id=p.get('session_id'), controller_id=p.get('controller_id'),
                    driver_name=p.get('driver_name'), penalty_type=p.get('penalty_type'),
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
                    created_at=datetime.fromisoformat(t['created_at']) if t.get('created_at') else None,
                ))
            counts['track_records'] = len(data['track_records'])

        if data.get('personal_records'):
            for r in data['personal_records']:
                existing = PersonalRecord.query.filter_by(
                    driver_name=r.get('driver_name')).first()
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


@app.route('/api/personal-records')
def api_personal_records():
    """Persoenliche Rekorde aller Fahrer."""
    try:
        records = PersonalRecord.query.order_by(
            PersonalRecord.laptime_ms.asc()).all()
        return jsonify([{
            'driver_name': r.driver_name,
            'laptime_ms': r.laptime_ms,
            'laptime_formatted': fmt_ms(r.laptime_ms),
            'car_name': r.car_name,
            'session_id': r.session_id,
            'created_at': r.created_at.isoformat() if r.created_at else None,
            'updated_at': r.updated_at.isoformat() if r.updated_at else None,
        } for r in records])
    except Exception as e:
        log.error(f"personal-records: {e}")
        return jsonify([])


@app.route('/api/session-compare')
def api_session_compare():
    """Vergleich zweier Sessions."""
    try:
        s1 = request.args.get('session1', '')
        s2 = request.args.get('session2', '')
        if not s1 or not s2:
            return jsonify({'error': 'Zwei Sessions angeben'}), 400

        result = {}
        for sid in [s1, s2]:
            laps = Lap.query.filter(
                Lap.session_id == sid, Lap.laptime_ms > 0
            ).all()

            drivers = {}
            for l in laps:
                name = l.driver_name or f"Fahrer {l.controller_id}"
                if name not in drivers:
                    drivers[name] = {'times': [], 'laps': 0}
                drivers[name]['times'].append(l.laptime_ms)
                drivers[name]['laps'] += 1

            driver_stats = []
            for name, d in drivers.items():
                driver_stats.append({
                    'driver_name': name,
                    'total_laps': d['laps'],
                    'best_time': min(d['times']),
                    'best_time_formatted': fmt_ms(min(d['times'])),
                    'avg_time': round(sum(d['times']) / len(d['times'])),
                    'avg_time_formatted': fmt_ms(
                        round(sum(d['times']) / len(d['times']))),
                })
            driver_stats.sort(key=lambda x: x['best_time'])

            # Session-Metadaten
            meta = db.session.query(
                Lap.session_type,
                func.min(Lap.created_at).label('start'),
                func.max(Lap.created_at).label('end'),
                func.count(Lap.id).label('total'),
            ).filter(Lap.session_id == sid).first()

            penalties = Penalty.query.filter(Penalty.session_id == sid).count()

            result[sid] = {
                'session_id': sid,
                'session_type': meta.session_type if meta else '',
                'start_time': meta.start.isoformat() if meta and meta.start else None,
                'end_time': meta.end.isoformat() if meta and meta.end else None,
                'total_laps': meta.total if meta else 0,
                'penalties': penalties,
                'drivers': driver_stats,
            }

        return jsonify(result)
    except Exception as e:
        log.error(f"session-compare: {e}")
        return jsonify({'error': str(e)}), 500


@app.route('/api/driver-profile')
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

        # Persoenlicher Rekord
        pr = PersonalRecord.query.filter_by(driver_name=name).first()

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


@app.route('/api/session-name', methods=['PUT'])
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


@app.route('/api/cleanup', methods=['POST'])
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


@app.route('/api/db-stats')
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


@app.route('/api/laps/count')
def api_laps_count():
    """Gesamtanzahl Runden (ohne Filter, fuer Datenbank-Seite)."""
    try:
        return jsonify({'count': Lap.query.count()})
    except Exception as e:
        log.error(f"laps-count: {e}")
        return jsonify({'count': 0})


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
