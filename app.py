from flask import Flask, request, jsonify, render_template, make_response
from flask_sqlalchemy import SQLAlchemy
from flask_cors import CORS
from datetime import datetime
import json
import csv
import io
import os
from dotenv import load_dotenv

load_dotenv()

app = Flask(__name__)
app.config['SQLALCHEMY_DATABASE_URI'] = os.getenv('DATABASE_URL', 'sqlite:///smartrace.db')
app.config['SQLALCHEMY_TRACK_MODIFICATIONS'] = False

db = SQLAlchemy(app)
CORS(app)

# Erweiterte Datenbank-Tabelle mit allen Feldern
class LapData(db.Model):
    id = db.Column(db.Integer, primary_key=True)
    timestamp = db.Column(db.DateTime, default=datetime.utcnow)
    event_type = db.Column(db.String(50))
    controller_id = db.Column(db.String(10))
    
    # Fahrer und Auto Daten
    driver = db.Column(db.String(100))
    car = db.Column(db.String(100))
    
    # Lap Daten
    lap = db.Column(db.Integer)
    laptime = db.Column(db.String(20))
    laptime_raw = db.Column(db.Integer)
    
    # Sektor Zeiten
    sector_1 = db.Column(db.String(20))
    sector_1_pb = db.Column(db.Boolean)
    sector_2 = db.Column(db.String(20))
    sector_2_pb = db.Column(db.Boolean)
    sector_3 = db.Column(db.String(20))
    sector_3_pb = db.Column(db.Boolean)
    
    # Event Details
    event_id = db.Column(db.String(100))
    track = db.Column(db.String(100))
    session_type = db.Column(db.String(50))
    position = db.Column(db.Integer)
    gap = db.Column(db.String(20))
    
    # Zusätzliche Daten
    weather = db.Column(db.String(20))
    temperature = db.Column(db.Integer)

@app.route('/')
def home():
    return render_template('index.html')

@app.route('/dashboard')
def dashboard():
    return render_template('dashboard.html')

@app.route('/analytics')
def analytics():
    return render_template('analytics.html')

@app.route('/database')
def database():
    return render_template('database.html')

@app.route('/api/smartrace', methods=['POST', 'OPTIONS'])
def smartrace_webhook():
    if request.method == 'OPTIONS':
        response = make_response()
        response.headers.add("Access-Control-Allow-Origin", "*")
        response.headers.add('Access-Control-Allow-Headers', "*")
        response.headers.add('Access-Control-Allow-Methods', "*")
        return response
    
    try:
        data = request.get_json()
        
        if not data:
            return jsonify({"error": "No JSON data received"}), 400
        
        # Nur lap_update Events in Datenbank speichern
        if data.get('event_type') == 'ui.lap_update':
            event_data = data.get('event_data', {})
            
            # Neuen Lap-Eintrag erstellen mit ALLEN Daten
            lap_entry = LapData(
                timestamp=datetime.fromtimestamp(data.get('time', 0) / 1000) if data.get('time') else datetime.utcnow(),
                event_type=data.get('event_type'),
                controller_id=event_data.get('controller_id'),
                
                # Fahrer und Auto - das war das Problem!
                driver=event_data.get('driver', f"Driver {event_data.get('controller_id', 'Unknown')}"),
                car=event_data.get('car', f"Car {event_data.get('controller_id', 'Unknown')}"),
                
                # Lap Daten
                lap=event_data.get('lap'),
                laptime=event_data.get('laptime'),
                laptime_raw=event_data.get('laptime_raw'),
                
                # Sektor Zeiten
                sector_1=event_data.get('sector_1'),
                sector_1_pb=event_data.get('sector_1_pb', False),
                sector_2=event_data.get('sector_2'),
                sector_2_pb=event_data.get('sector_2_pb', False),
                sector_3=event_data.get('sector_3'),
                sector_3_pb=event_data.get('sector_3_pb', False),
                
                # Event Details
                event_id=event_data.get('event_id'),
                track=event_data.get('track'),
                session_type=event_data.get('session_type'),
                position=event_data.get('position'),
                gap=event_data.get('gap'),
                
                # Zusätzliche Daten
                weather=event_data.get('weather'),
                temperature=event_data.get('temperature')
            )
            
            db.session.add(lap_entry)
            db.session.commit()
            
            print(f"✅ Saved lap: {lap_entry.driver} ({lap_entry.car}) - {lap_entry.laptime} at {lap_entry.track}")
        
        return jsonify({"status": "success", "message": "Data received"}), 200
        
    except Exception as e:
        print(f"❌ Error processing SmartRace data: {str(e)}")
        return jsonify({"error": str(e)}), 500

@app.route('/api/dashboard')
def get_dashboard_data():
    try:
        # Letzte 50 Runden mit Driver und Car Daten
        recent_laps = db.session.query(LapData)\
            .filter(LapData.event_type == 'ui.lap_update')\
            .order_by(LapData.timestamp.desc())\
            .limit(50).all()
        
        dashboard_data = {
            "recent_laps": [{
                "id": lap.id,
                "timestamp": lap.timestamp.isoformat(),
                "driver": lap.driver or f"Driver {lap.controller_id}",
                "car": lap.car or f"Car {lap.controller_id}",
                "controller_id": lap.controller_id,
                "lap": lap.lap,
                "laptime": lap.laptime,
                "laptime_raw": lap.laptime_raw,
                "track": lap.track,
                "session_type": lap.session_type,
                "position": lap.position,
                "sector_1": lap.sector_1,
                "sector_2": lap.sector_2,
                "sector_3": lap.sector_3,
                "sector_1_pb": lap.sector_1_pb,
                "sector_2_pb": lap.sector_2_pb,
                "sector_3_pb": lap.sector_3_pb
            } for lap in recent_laps],
            
            "total_laps": db.session.query(LapData).filter(LapData.event_type == 'ui.lap_update').count(),
            "unique_drivers": db.session.query(LapData.driver).filter(LapData.driver.isnot(None)).distinct().count(),
            "unique_tracks": db.session.query(LapData.track).filter(LapData.track.isnot(None)).distinct().count()
        }
        
        return jsonify(dashboard_data)
        
    except Exception as e:
        print(f"❌ Dashboard API error: {str(e)}")
        return jsonify({"error": str(e)}), 500

@app.route('/api/database')
def get_database_data():
    try:
        # Alle Lap-Daten mit Paginierung
        page = request.args.get('page', 1, type=int)
        per_page = request.args.get('per_page', 100, type=int)
        
        # Filter Parameter
        driver_filter = request.args.get('driver', '')
        track_filter = request.args.get('track', '')
        session_filter = request.args.get('session', '')
        
        # Base Query
        query = db.session.query(LapData).filter(LapData.event_type == 'ui.lap_update')
        
        # Filter anwenden
        if driver_filter:
            query = query.filter(LapData.driver.ilike(f'%{driver_filter}%'))
        if track_filter:
            query = query.filter(LapData.track.ilike(f'%{track_filter}%'))
        if session_filter:
            query = query.filter(LapData.session_type.ilike(f'%{session_filter}%'))
        
        # Sortierung und Paginierung
        total = query.count()
        laps = query.order_by(LapData.timestamp.desc())\
                   .offset((page - 1) * per_page)\
                   .limit(per_page).all()
        
        # Filter-Optionen für Frontend
        all_drivers = db.session.query(LapData.driver).filter(LapData.driver.isnot(None)).distinct().all()
        all_tracks = db.session.query(LapData.track).filter(LapData.track.isnot(None)).distinct().all()
        all_sessions = db.session.query(LapData.session_type).filter(LapData.session_type.isnot(None)).distinct().all()
        
        return jsonify({
            "laps": [{
                "id": lap.id,
                "timestamp": lap.timestamp.isoformat(),
                "driver": lap.driver or f"Driver {lap.controller_id}",
                "car": lap.car or f"Car {lap.controller_id}",
                "controller_id": lap.controller_id,
                "lap": lap.lap,
                "laptime": lap.laptime,
                "laptime_raw": lap.laptime_raw,
                "track": lap.track or "Unknown",
                "session_type": lap.session_type or "Unknown",
                "position": lap.position,
                "gap": lap.gap,
                "event_id": lap.event_id,
                "sector_1": lap.sector_1,
                "sector_2": lap.sector_2,
                "sector_3": lap.sector_3,
                "sector_1_pb": lap.sector_1_pb,
                "sector_2_pb": lap.sector_2_pb,
                "sector_3_pb": lap.sector_3_pb,
                "weather": lap.weather,
                "temperature": lap.temperature
            } for lap in laps],
            "pagination": {
                "page": page,
                "per_page": per_page,
                "total": total,
                "pages": (total + per_page - 1) // per_page
            },
            "filters": {
                "drivers": [d[0] for d in all_drivers if d[0]],
                "tracks": [t[0] for t in all_tracks if t[0]],
                "sessions": [s[0] for s in all_sessions if s[0]]
            }
        })
        
    except Exception as e:
        print(f"❌ Database API error: {str(e)}")
        return jsonify({"error": str(e)}), 500

@app.route('/api/stats')
def get_stats():
    try:
        # Umfangreiche Statistiken
        total_laps = db.session.query(LapData).filter(LapData.event_type == 'ui.lap_update').count()
        
        # Driver Statistiken mit korrekten Namen
        driver_stats = db.session.query(
            LapData.driver,
            db.func.count(LapData.id).label('lap_count'),
            db.func.min(LapData.laptime_raw).label('best_time'),
            db.func.avg(LapData.laptime_raw).label('avg_time')
        ).filter(
            LapData.event_type == 'ui.lap_update',
            LapData.driver.isnot(None),
            LapData.laptime_raw.isnot(None)
        ).group_by(LapData.driver).all()
        
        # Track Statistiken 
        track_stats = db.session.query(
            LapData.track,
            db.func.count(LapData.id).label('lap_count'),
            db.func.min(LapData.laptime_raw).label('best_time')
        ).filter(
            LapData.event_type == 'ui.lap_update',
            LapData.track.isnot(None),
            LapData.laptime_raw.isnot(None)
        ).group_by(LapData.track).all()
        
        def ms_to_timestring(ms):
            if not ms:
                return "N/A"
            minutes = int(ms // 60000)
            seconds = (ms % 60000) / 1000
            return f"{minutes}:{seconds:06.3f}"
        
        return jsonify({
            "total_laps": total_laps,
            "unique_drivers": len(driver_stats),
            "unique_tracks": len(track_stats),
            "driver_stats": [{
                "driver": stat.driver,
                "lap_count": stat.lap_count,
                "best_time": ms_to_timestring(stat.best_time),
                "best_time_raw": stat.best_time,
                "avg_time": ms_to_timestring(stat.avg_time),
                "avg_time_raw": stat.avg_time
            } for stat in sorted(driver_stats, key=lambda x: x.lap_count, reverse=True)],
            "track_stats": [{
                "track": stat.track,
                "lap_count": stat.lap_count,
                "best_time": ms_to_timestring(stat.best_time),
                "best_time_raw": stat.best_time
            } for stat in sorted(track_stats, key=lambda x: x.lap_count, reverse=True)]
        })
        
    except Exception as e:
        print(f"❌ Stats API error: {str(e)}")
        return jsonify({"error": str(e)}), 500

@app.route('/api/export/csv')
def export_csv():
    try:
        # Alle Lap-Daten für Export
        laps = db.session.query(LapData)\
            .filter(LapData.event_type == 'ui.lap_update')\
            .order_by(LapData.timestamp.desc()).all()
        
        output = io.StringIO()
        writer = csv.writer(output)
        
        # CSV Header mit allen Feldern
        writer.writerow([
            'ID', 'Timestamp', 'Driver', 'Car', 'Controller_ID', 
            'Lap', 'Laptime', 'Laptime_Raw', 'Track', 'Session_Type',
            'Position', 'Gap', 'Event_ID',
            'Sector_1', 'Sector_1_PB', 'Sector_2', 'Sector_2_PB', 
            'Sector_3', 'Sector_3_PB', 'Weather', 'Temperature'
        ])
        
        # Daten-Zeilen
        for lap in laps:
            writer.writerow([
                lap.id,
                lap.timestamp.isoformat(),
                lap.driver or f"Driver {lap.controller_id}",
                lap.car or f"Car {lap.controller_id}",
                lap.controller_id,
                lap.lap,
                lap.laptime,
                lap.laptime_raw,
                lap.track or "Unknown",
                lap.session_type or "Unknown",
                lap.position,
                lap.gap,
                lap.event_id,
                lap.sector_1,
                lap.sector_1_pb,
                lap.sector_2,
                lap.sector_2_pb,
                lap.sector_3,
                lap.sector_3_pb,
                lap.weather,
                lap.temperature
            ])
        
        output.seek(0)
        
        # CSV Response mit korrektem Encoding
        response = make_response(output.getvalue())
        response.headers['Content-Type'] = 'text/csv; charset=utf-8'
        response.headers['Content-Disposition'] = f'attachment; filename=smartrace_export_{datetime.now().strftime("%Y%m%d_%H%M%S")}.csv'
        
        return response
        
    except Exception as e:
        print(f"❌ CSV Export error: {str(e)}")
        return jsonify({"error": str(e)}), 500

if __name__ == '__main__':
    with app.app_context():
        # Datenbank Tabellen erstellen
        db.create_all()
        print("🚀 SmartRace Dashboard starting...")
        print("📊 Database tables created/updated")
        print("🔗 Endpoints available:")
        print("   - POST /api/smartrace (SmartRace webhook)")
        print("   - GET /api/dashboard (Dashboard data)")
        print("   - GET /api/database (Database with filters)")
        print("   - GET /api/stats (Statistics)")
        print("   - GET /api/export/csv (CSV export)")
        
    app.run(host='0.0.0.0', port=5000, debug=True)
