from flask import Flask, render_template, request, jsonify, Response
from flask_sqlalchemy import SQLAlchemy
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

db = SQLAlchemy(app)
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
        # Alle alten Tabellen/Sequenzen entfernen (PostgreSQL)
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

    # Aktuelle Session aus letztem Event uebernehmen
    latest = db.session.query(Event.session_id).order_by(Event.id.desc()).first()
    if latest and latest.session_id:
        return latest.session_id

    # Allererster Event ueberhaupt -> neue Session
    return f"session_{datetime.now().strftime('%Y%m%d_%H%M%S')}"


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
            _save_lap(sid, etype, ed)
        elif etype == 'ui.race_result':
            _save_result(sid, ed)

        db.session.commit()
        log.info(f"Event: {etype} | Session: {sid}")
        return jsonify({'status': 'ok', 'event_type': etype})

    except Exception as e:
        db.session.rollback()
        log.error(f"SmartRace-Fehler: {e}")
        return jsonify({'error': str(e)}), 500


def _save_lap(sid, etype, ed):
    cid = str(ed.get('controller_id', '0'))
    dd = ed.get('driver_data') or {}
    cd = ed.get('car_data') or {}
    ctrl = ed.get('controller_data') or {}

    db.session.add(Lap(
        session_id=sid,
        session_type=etype,
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
                }
            ctrls[cid]['laps'].append({
                'lap': lap.lap_number or 0,
                'laptime_raw': lap.laptime_ms or 0,
                'laptime_formatted': lap.laptime_display or fmt_ms(lap.laptime_ms) or '--:--.---',
                'timestamp': lap.created_at.isoformat() if lap.created_at else None,
                'is_pb': lap.is_personal_best,
                'session_id': lap.session_id,
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
    app.run(
        host='0.0.0.0',
        port=int(os.getenv('PORT', 5000)),
        debug=os.getenv('FLASK_DEBUG', 'false').lower() == 'true',
    )
