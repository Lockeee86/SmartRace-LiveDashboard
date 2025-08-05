from flask import Flask, render_template, request, jsonify
from flask_sqlalchemy import SQLAlchemy
from flask_cors import CORS
from datetime import datetime
import json
import os

app = Flask(__name__)
CORS(app)

# Konfiguration
app.config['SQLALCHEMY_DATABASE_URI'] = os.getenv('DATABASE_URL', 'sqlite:///smartrace.db')
app.config['SQLALCHEMY_TRACK_MODIFICATIONS'] = False

db = SQLAlchemy(app)

# Modelle
class Event(db.Model):
    id = db.Column(db.Integer, primary_key=True)
    event_id = db.Column(db.String(100))
    event_type = db.Column(db.String(50))
    timestamp = db.Column(db.DateTime, default=datetime.utcnow)
    data = db.Column(db.Text)

class LapTime(db.Model):
    id = db.Column(db.Integer, primary_key=True)
    event_id = db.Column(db.String(100))
    controller_id = db.Column(db.String(10))
    driver_name = db.Column(db.String(100))
    car_name = db.Column(db.String(100))
    lap = db.Column(db.Integer)
    laptime_raw = db.Column(db.Integer)
    laptime = db.Column(db.String(20))
    sector_1 = db.Column(db.String(20))
    sector_2 = db.Column(db.String(20))
    sector_3 = db.Column(db.String(20))
    car_color = db.Column(db.String(20))
    timestamp = db.Column(db.DateTime, default=datetime.utcnow)
    is_pb = db.Column(db.Boolean, default=False)

# Datenbank initialisieren
with app.app_context():
    db.create_all()

# Routen
@app.route('/')
def dashboard():
    return render_template('dashboard.html')

@app.route('/analytics')
def analytics():
    return render_template('analytics.html')

@app.route('/database')
def database_view():
    return render_template('database.html')

# SmartRace Datenschnittstelle
@app.route('/api/smartrace', methods=['POST', 'OPTIONS'])
def smartrace_endpoint():
    if request.method == 'OPTIONS':
        return '', 200
    
    try:
        data = request.get_json()
        
        # Event speichern
        event = Event(
            event_type=data.get('event_type'),
            data=json.dumps(data)
        )
        db.session.add(event)
        
        # Rundeninformationen verarbeiten
        if data.get('event_type') == 'ui.lap_update':
            event_data = data.get('event_data', {})
            
            lap_time = LapTime(
                controller_id=event_data.get('controller_id'),
                driver_name=event_data.get('driver_data', {}).get('name'),
                car_name=event_data.get('car_data', {}).get('name'),
                lap=event_data.get('lap'),
                laptime_raw=event_data.get('laptime_raw'),
                laptime=event_data.get('laptime'),
                sector_1=event_data.get('sector_1'),
                sector_2=event_data.get('sector_2'),
                sector_3=event_data.get('sector_3'),
                car_color=event_data.get('car_data', {}).get('color'),
                is_pb=event_data.get('lap_pb', False)
            )
            db.session.add(lap_time)
        
        db.session.commit()
        return jsonify({'status': 'success'})
    
    except Exception as e:
        return jsonify({'error': str(e)}), 400

# API für Live-Daten
@app.route('/api/live-data')
def live_data():
    latest_laps = db.session.query(LapTime).order_by(LapTime.timestamp.desc()).limit(60).all()
    
    # Gruppiere nach Controller ID
    controller_data = {}
    for lap in latest_laps:
        if lap.controller_id not in controller_data:
            controller_data[lap.controller_id] = []
        controller_data[lap.controller_id].append({
            'driver': lap.driver_name,
            'car': lap.car_name,
            'lap': lap.lap,
            'laptime': lap.laptime,
            'sector_1': lap.sector_1,
            'sector_2': lap.sector_2,
            'sector_3': lap.sector_3,
            'color': lap.car_color,
            'is_pb': lap.is_pb
        })
    
    return jsonify(controller_data)

# API für Analytics
@app.route('/api/analytics')
def analytics_data():
    laps = LapTime.query.all()
    
    # Aggregate Daten für Charts
    driver_stats = {}
    for lap in laps:
        if lap.driver_name not in driver_stats:
            driver_stats[lap.driver_name] = {
                'laps': [],
                'best_time': float('inf'),
                'avg_time': 0
            }
        
        if lap.laptime_raw:
            driver_stats[lap.driver_name]['laps'].append(lap.laptime_raw)
            if lap.laptime_raw < driver_stats[lap.driver_name]['best_time']:
                driver_stats[lap.driver_name]['best_time'] = lap.laptime_raw
    
    # Berechne Durchschnittswerte
    for driver in driver_stats:
        laps = driver_stats[driver]['laps']
        if laps:
            driver_stats[driver]['avg_time'] = sum(laps) / len(laps)
    
    return jsonify(driver_stats)

# API für Datenbank-View
@app.route('/api/database')
def database_data():
    driver_filter = request.args.get('driver')
    car_filter = request.args.get('car')
    
    query = LapTime.query
    
    if driver_filter:
        query = query.filter(LapTime.driver_name.ilike(f'%{driver_filter}%'))
    if car_filter:
        query = query.filter(LapTime.car_name.ilike(f'%{car_filter}%'))
    
    laps = query.order_by(LapTime.timestamp.desc()).limit(1000).all()
    
    result = []
    for lap in laps:
        result.append({
            'driver': lap.driver_name,
            'car': lap.car_name,
            'lap': lap.lap,
            'laptime': lap.laptime,
            'sector_1': lap.sector_1,
            'sector_2': lap.sector_2,
            'sector_3': lap.sector_3,
            'timestamp': lap.timestamp.isoformat(),
            'is_pb': lap.is_pb
        })
    
    return jsonify(result)

# API für Filter-Optionen
@app.route('/api/filters')
def get_filters():
    drivers = db.session.query(LapTime.driver_name.distinct()).all()
    cars = db.session.query(LapTime.car_name.distinct()).all()
    
    return jsonify({
        'drivers': [d[0] for d in drivers if d[0]],
        'cars': [c[0] for c in cars if c[0]]
    })

if __name__ == '__main__':
    app.run(host='0.0.0.0', port=5000, debug=True)
