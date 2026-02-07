from flask import Flask, render_template, request, jsonify, Response
from flask_sqlalchemy import SQLAlchemy
from flask_cors import CORS
from sqlalchemy import func
from datetime import datetime
import json
import os
import csv
import io
import re

app = Flask(__name__)
CORS(app)

# Konfiguration
app.config['SQLALCHEMY_DATABASE_URI'] = os.getenv(
    'DATABASE_URL', 'sqlite:///smartrace.db'
)
app.config['SQLALCHEMY_TRACK_MODIFICATIONS'] = False
app.config['SECRET_KEY'] = os.getenv('SECRET_KEY', 'smartrace-secret-key')

db = SQLAlchemy(app)


# =============================================================================
# Datenbank-Modelle
# =============================================================================

class Event(db.Model):
    __tablename__ = 'events'
    id = db.Column(db.Integer, primary_key=True)
    event_id = db.Column(db.String(100), index=True)
    event_type = db.Column(db.String(50))
    timestamp = db.Column(db.DateTime, default=datetime.utcnow)
    data = db.Column(db.Text)


class LapTime(db.Model):
    __tablename__ = 'lap_times'
    id = db.Column(db.Integer, primary_key=True)
    event_id = db.Column(db.String(100), index=True)
    event_type = db.Column(db.String(50))
    controller_id = db.Column(db.String(10), index=True)
    driver_name = db.Column(db.String(100))
    car_name = db.Column(db.String(100))
    lap = db.Column(db.Integer)
    laptime_raw = db.Column(db.Integer)
    laptime = db.Column(db.String(20))
    sector_1 = db.Column(db.String(20))
    sector_2 = db.Column(db.String(20))
    sector_3 = db.Column(db.String(20))
    car_color = db.Column(db.String(20))
    controller_color = db.Column(db.String(20))
    timestamp = db.Column(db.DateTime, default=datetime.utcnow, index=True)
    is_pb = db.Column(db.Boolean, default=False)


class RaceResult(db.Model):
    __tablename__ = 'race_results'
    id = db.Column(db.Integer, primary_key=True)
    event_id = db.Column(db.String(100), index=True)
    position = db.Column(db.Integer)
    controller_id = db.Column(db.String(10))
    driver_name = db.Column(db.String(100))
    laps = db.Column(db.Integer)
    best_laptime = db.Column(db.Integer)
    gap = db.Column(db.String(50))
    pitstops = db.Column(db.Integer, default=0)
    disqualified = db.Column(db.Boolean, default=False)
    retired = db.Column(db.Boolean, default=False)
    timestamp = db.Column(db.DateTime, default=datetime.utcnow)


# Datenbank initialisieren
with app.app_context():
    db.create_all()


# =============================================================================
# Hilfsfunktionen
# =============================================================================

def rgb_to_hex(rgb_string):
    """Konvertiert rgb(r,g,b) zu #rrggbb"""
    if not rgb_string:
        return '#333333'
    if rgb_string.startswith('#'):
        return rgb_string
    try:
        numbers = re.findall(r'\d+', rgb_string)
        if len(numbers) >= 3:
            r, g, b = int(numbers[0]), int(numbers[1]), int(numbers[2])
            return f"#{r:02x}{g:02x}{b:02x}"
    except (ValueError, IndexError):
        pass
    return '#333333'


def format_time(milliseconds):
    """Formatiert Millisekunden zu M:SS.mmm"""
    if not milliseconds or milliseconds <= 0:
        return "--:--.---"
    seconds = milliseconds / 1000
    minutes = int(seconds // 60)
    remaining = seconds % 60
    return f"{minutes}:{remaining:06.3f}"


# =============================================================================
# Frontend-Routen
# =============================================================================

@app.route('/')
def dashboard():
    return render_template('dashboard.html')


@app.route('/leaderboard')
def leaderboard():
    return render_template('leaderboard.html')


@app.route('/analytics')
def analytics():
    return render_template('analytics.html')


@app.route('/database')
def database_view():
    return render_template('database.html')


# =============================================================================
# SmartRace Datenschnittstelle - Empfangs-Endpoint
# =============================================================================

@app.route('/api/smartrace', methods=['POST', 'OPTIONS'])
def smartrace_endpoint():
    """Empfaengt Daten von der SmartRace Datenschnittstelle.

    SmartRace sendet POST-Requests mit JSON-Body bei verschiedenen Events:
    - ui.lap_update: Runde abgeschlossen
    - ui.race_result: Rennergebnis
    - ui.status_change: Statusaenderung (prepare_for_start, running, ended, etc.)
    - ui.penalty: Strafe
    - ui.reset: UI zurueckgesetzt
    - ui.car_removed: Auto entfernt
    - ui.fuel_update: Tankstand
    - ui.weather_change: Wetter
    - ui.vsc: Virtual Safety Car
    - ui.damage: Schaden
    """
    if request.method == 'OPTIONS':
        return '', 200

    try:
        data = request.get_json()
        if not data:
            return jsonify({'error': 'No JSON data received'}), 400

        event_type = data.get('event_type', 'unknown')
        event_data = data.get('event_data', {})
        timestamp = data.get('time')

        # Event-ID bestimmen
        event_id = (
            data.get('event_id')
            or event_data.get('event_id')
            or data.get('session_id')
            or f"session_{datetime.now().strftime('%Y%m%d_%H%M%S')}"
        )

        # Roh-Event speichern
        event = Event(
            event_id=event_id,
            event_type=event_type,
            data=json.dumps(data)
        )
        db.session.add(event)

        # Event-spezifische Verarbeitung
        if event_type == 'ui.lap_update':
            _process_lap_update(event_id, event_type, event_data)
        elif event_type == 'ui.race_result':
            _process_race_result(event_id, event_data)

        db.session.commit()
        return jsonify({'status': 'success', 'event_type': event_type})

    except Exception as e:
        db.session.rollback()
        app.logger.error(f"SmartRace endpoint error: {e}")
        return jsonify({'error': str(e)}), 400


def _process_lap_update(event_id, event_type, event_data):
    """Verarbeitet ein Runden-Update von SmartRace."""
    controller_id = str(event_data.get('controller_id', '0'))

    # Fahrer-Daten
    driver_data = event_data.get('driver_data', {}) or {}
    driver_name = driver_data.get('name', f"Fahrer {controller_id}")

    # Auto-Daten
    car_data = event_data.get('car_data', {}) or {}
    car_name = car_data.get('name', f"Auto {controller_id}")
    car_color = rgb_to_hex(car_data.get('color', '#000000'))

    # Controller-Daten
    controller_data = event_data.get('controller_data', {}) or {}
    controller_color = rgb_to_hex(controller_data.get('color_bg', '#333333'))

    lap_time = LapTime(
        event_id=event_id,
        event_type=event_type,
        controller_id=controller_id,
        driver_name=driver_name,
        car_name=car_name,
        lap=event_data.get('lap'),
        laptime_raw=event_data.get('laptime_raw'),
        laptime=event_data.get('laptime'),
        sector_1=event_data.get('sector_1'),
        sector_2=event_data.get('sector_2'),
        sector_3=event_data.get('sector_3'),
        car_color=car_color,
        controller_color=controller_color,
        is_pb=event_data.get('lap_pb', False),
    )
    db.session.add(lap_time)


def _process_race_result(event_id, event_data):
    """Verarbeitet ein Rennergebnis von SmartRace."""
    result_data = event_data.get('result', {})
    for position_str, result in result_data.items():
        race_result = RaceResult(
            event_id=event_id,
            position=int(position_str),
            controller_id=str(result.get('controller_id', '')),
            driver_name=result.get('driver_name', ''),
            laps=result.get('laps', 0),
            best_laptime=result.get('best_laptime'),
            gap=result.get('gap', ''),
            pitstops=result.get('pitstops', 0),
            disqualified=result.get('disqualified', False),
            retired=result.get('retired', False),
        )
        db.session.add(race_result)


# =============================================================================
# API-Endpunkte
# =============================================================================

@app.route('/api/laps')
def get_laps():
    """Gibt alle Rundenzeiten zurueck, optional gefiltert."""
    try:
        query = LapTime.query

        session_id = request.args.get('session_id')
        if session_id and session_id != 'all':
            query = query.filter(LapTime.event_id == session_id)

        driver = request.args.get('driver')
        if driver:
            query = query.filter(LapTime.driver_name.ilike(f'%{driver}%'))

        car = request.args.get('car')
        if car:
            query = query.filter(LapTime.car_name.ilike(f'%{car}%'))

        date_from = request.args.get('date_from')
        if date_from:
            query = query.filter(LapTime.timestamp >= f"{date_from} 00:00:00")

        date_to = request.args.get('date_to')
        if date_to:
            query = query.filter(LapTime.timestamp <= f"{date_to} 23:59:59")

        laps = query.order_by(LapTime.timestamp.desc()).limit(2000).all()

        return jsonify([{
            'id': lap.id,
            'event_id': lap.event_id,
            'event_type': lap.event_type,
            'controller_id': lap.controller_id,
            'driver_name': lap.driver_name,
            'car_name': lap.car_name,
            'lap': lap.lap,
            'laptime_raw': lap.laptime_raw,
            'laptime_formatted': format_time(lap.laptime_raw),
            'sector_1': lap.sector_1,
            'sector_2': lap.sector_2,
            'sector_3': lap.sector_3,
            'car_color': lap.car_color,
            'controller_color': lap.controller_color,
            'is_pb': lap.is_pb,
            'timestamp': lap.timestamp.isoformat() if lap.timestamp else None,
        } for lap in laps])

    except Exception as e:
        app.logger.error(f"Get laps error: {e}")
        return jsonify({'error': str(e)}), 500


@app.route('/api/sessions')
def get_sessions():
    """Gibt alle verfuegbaren Sessions mit Metadaten zurueck."""
    try:
        sessions = db.session.query(
            LapTime.event_id,
            LapTime.event_type,
            func.count(LapTime.id).label('total_laps'),
            func.count(func.distinct(LapTime.controller_id)).label('drivers'),
            func.min(LapTime.timestamp).label('start_time'),
            func.max(LapTime.timestamp).label('end_time'),
        ).group_by(
            LapTime.event_id,
            LapTime.event_type,
        ).order_by(
            func.max(LapTime.timestamp).desc()
        ).all()

        return jsonify([{
            'event_id': s.event_id,
            'event_type': s.event_type,
            'total_laps': s.total_laps,
            'drivers': s.drivers,
            'start_time': s.start_time.isoformat() if s.start_time else None,
            'end_time': s.end_time.isoformat() if s.end_time else None,
        } for s in sessions])

    except Exception as e:
        app.logger.error(f"Get sessions error: {e}")
        return jsonify({'error': str(e)}), 500


@app.route('/api/live-data')
def live_data():
    """Gibt Live-Daten fuer das Dashboard zurueck, gruppiert nach Controller."""
    try:
        session_id = request.args.get('session_id')

        query = LapTime.query
        if session_id and session_id != 'all':
            query = query.filter(LapTime.event_id == session_id)

        laps = query.order_by(LapTime.timestamp.desc()).all()

        if not laps:
            return jsonify({})

        controller_data = {}

        for lap in laps:
            cid = str(lap.controller_id)

            if cid not in controller_data:
                controller_data[cid] = {
                    'name': lap.driver_name or f'Fahrer {cid}',
                    'car': lap.car_name or f'Auto {cid}',
                    'color': lap.controller_color or '#333333',
                    'laps': [],
                    'lap_count': 0,
                    'best_time_raw': None,
                    'best_time_formatted': '--:--.---',
                }

            controller_data[cid]['laps'].append({
                'lap': lap.lap or 0,
                'laptime_raw': lap.laptime_raw or 0,
                'laptime_formatted': lap.laptime or format_time(lap.laptime_raw),
                'timestamp': lap.timestamp.isoformat() if lap.timestamp else None,
                'is_pb': lap.is_pb or False,
                'event_id': lap.event_id,
            })

            if lap.laptime_raw and lap.laptime_raw > 0:
                current_best = controller_data[cid]['best_time_raw']
                if current_best is None or lap.laptime_raw < current_best:
                    controller_data[cid]['best_time_raw'] = lap.laptime_raw
                    controller_data[cid]['best_time_formatted'] = (
                        lap.laptime or format_time(lap.laptime_raw)
                    )

        for cid in controller_data:
            lap_list = controller_data[cid]['laps']
            if lap_list:
                controller_data[cid]['lap_count'] = max(
                    l['lap'] for l in lap_list
                )

        return jsonify(controller_data)

    except Exception as e:
        app.logger.error(f"Live data error: {e}")
        return jsonify({}), 500


@app.route('/api/analytics')
def analytics_data():
    """Gibt aggregierte Analytics-Daten fuer Charts zurueck."""
    try:
        session_id = request.args.get('session_id')

        query = LapTime.query
        if session_id and session_id != 'all':
            query = query.filter(LapTime.event_id == session_id)

        laps = query.all()

        driver_stats = {}
        for lap in laps:
            name = lap.driver_name or f"Fahrer {lap.controller_id}"
            if name not in driver_stats:
                driver_stats[name] = {
                    'laps': [],
                    'best_time': float('inf'),
                    'total_laps': 0,
                }

            if lap.laptime_raw and lap.laptime_raw > 0:
                driver_stats[name]['laps'].append(lap.laptime_raw)
                driver_stats[name]['total_laps'] += 1
                if lap.laptime_raw < driver_stats[name]['best_time']:
                    driver_stats[name]['best_time'] = lap.laptime_raw

        result = {}
        for name, stats in driver_stats.items():
            if stats['laps']:
                avg_time = sum(stats['laps']) / len(stats['laps'])
                best = stats['best_time']
                result[name] = {
                    'avg_time': round(avg_time),
                    'best_time': best if best != float('inf') else 0,
                    'total_laps': stats['total_laps'],
                    'laps': stats['laps'][:50],
                }

        return jsonify(result)

    except Exception as e:
        app.logger.error(f"Analytics error: {e}")
        return jsonify({'error': str(e)}), 500


@app.route('/api/filters')
def get_filters():
    """Gibt verfuegbare Filter-Optionen (Fahrer, Autos, Events) zurueck."""
    try:
        drivers = db.session.query(LapTime.driver_name.distinct()).filter(
            LapTime.driver_name.isnot(None),
            LapTime.driver_name != '',
        ).all()

        cars = db.session.query(LapTime.car_name.distinct()).filter(
            LapTime.car_name.isnot(None),
            LapTime.car_name != '',
        ).all()

        events = db.session.query(LapTime.event_id.distinct()).filter(
            LapTime.event_id.isnot(None),
            LapTime.event_id != '',
        ).all()

        return jsonify({
            'drivers': sorted([d[0] for d in drivers if d[0]]),
            'cars': sorted([c[0] for c in cars if c[0]]),
            'events': sorted([e[0] for e in events if e[0]]),
        })

    except Exception as e:
        app.logger.error(f"Filters error: {e}")
        return jsonify({'drivers': [], 'cars': [], 'events': []})


@app.route('/api/export/csv')
def export_csv():
    """Exportiert Rundenzeiten als CSV-Datei."""
    try:
        query = LapTime.query

        driver = request.args.get('driver')
        if driver:
            query = query.filter(LapTime.driver_name.ilike(f'%{driver}%'))

        car = request.args.get('car')
        if car:
            query = query.filter(LapTime.car_name.ilike(f'%{car}%'))

        event = request.args.get('event')
        if event:
            query = query.filter(LapTime.event_id == event)

        date_from = request.args.get('date_from')
        if date_from:
            query = query.filter(LapTime.timestamp >= f"{date_from} 00:00:00")

        date_to = request.args.get('date_to')
        if date_to:
            query = query.filter(LapTime.timestamp <= f"{date_to} 23:59:59")

        laps = query.order_by(LapTime.timestamp.desc()).all()

        output = io.StringIO()
        writer = csv.writer(output)
        writer.writerow([
            'Event ID', 'Event Type', 'Zeitstempel', 'Controller',
            'Fahrer', 'Auto', 'Runde', 'Rundenzeit (ms)', 'Rundenzeit',
            'Sektor 1', 'Sektor 2', 'Sektor 3', 'Personal Best',
        ])

        for lap in laps:
            writer.writerow([
                lap.event_id or '',
                lap.event_type or '',
                lap.timestamp.strftime('%Y-%m-%d %H:%M:%S') if lap.timestamp else '',
                lap.controller_id or '',
                lap.driver_name or '',
                lap.car_name or '',
                lap.lap or 0,
                lap.laptime_raw or 0,
                lap.laptime or format_time(lap.laptime_raw),
                lap.sector_1 or '',
                lap.sector_2 or '',
                lap.sector_3 or '',
                'Ja' if lap.is_pb else 'Nein',
            ])

        output.seek(0)
        filename = f"smartrace_export_{datetime.now().strftime('%Y%m%d_%H%M%S')}.csv"

        return Response(
            output.getvalue(),
            mimetype='text/csv',
            headers={
                'Content-Disposition': f'attachment; filename={filename}',
                'Content-Type': 'text/csv; charset=utf-8',
            },
        )

    except Exception as e:
        app.logger.error(f"CSV export error: {e}")
        return jsonify({'error': str(e)}), 500


@app.route('/api/health')
def health_check():
    """Health-Check fuer Docker/Portainer."""
    try:
        db.session.execute(db.text('SELECT 1'))
        return jsonify({
            'status': 'healthy',
            'database': 'connected',
            'timestamp': datetime.utcnow().isoformat(),
        })
    except Exception as e:
        return jsonify({
            'status': 'unhealthy',
            'database': str(e),
        }), 503


# =============================================================================
# App starten
# =============================================================================

if __name__ == '__main__':
    debug = os.getenv('FLASK_DEBUG', 'false').lower() == 'true'
    port = int(os.getenv('PORT', 5000))
    app.run(host='0.0.0.0', port=port, debug=debug)
