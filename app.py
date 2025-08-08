from flask import Flask, render_template, request, jsonify
from flask_sqlalchemy import SQLAlchemy
from flask_cors import CORS
from datetime import datetime
import json
import os
import csv
import re

app = Flask(__name__)
CORS(app)

# Konfiguration
app.config['SQLALCHEMY_DATABASE_URI'] = os.getenv('DATABASE_URL', 'sqlite:///smartrace.db')
app.config['SQLALCHEMY_TRACK_MODIFICATIONS'] = False

db = SQLAlchemy(app)

def rgb_to_hex(rgb_string):
    """Konvertiert rgb(r,g,b) zu #rrggbb"""
    try:
        if rgb_string and rgb_string.startswith('rgb'):
            numbers = re.findall(r'\d+', rgb_string)
            if len(numbers) >= 3:
                r, g, b = int(numbers[0]), int(numbers[1]), int(numbers[2])
                hex_color = f"#{r:02x}{g:02x}{b:02x}"
                print(f"🎨 RGB→HEX: {rgb_string} → {hex_color}")
                return hex_color
        return rgb_string or '#333333'
    except Exception as e:
        print(f"❌ RGB conversion error: {e}")
        return '#333333'

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
    controller_color = db.Column(db.String(20))  # ✅ NEUE Spalte für Controller-Farbe!
    timestamp = db.Column(db.DateTime, default=datetime.utcnow)
    is_pb = db.Column(db.Boolean, default=False)

# Datenbank initialisieren
with app.app_context():
    db.create_all()

# ✅ FEHLENDE FUNKTIONEN ERGÄNZEN:

# Format-Funktion für Zeiten
def format_time(milliseconds):
    """Formatiert Millisekunden zu MM:SS.mmm"""
    if not milliseconds or milliseconds == 0:
        return "--:--.---"
    
    seconds = milliseconds / 1000
    minutes = int(seconds // 60)
    remaining_seconds = seconds % 60
    
    return f"{minutes}:{remaining_seconds:06.3f}"

# Driver Cache (einfache Version)
driver_cache = {}

def get_current_event():
    """Gibt das neueste Event zurück (vereinfacht)"""
    try:
        # ✅ EINFACH: Nimm die neueste LapTime und verwende deren event_id
        latest_lap = db.session.query(LapTime).order_by(LapTime.timestamp.desc()).first()
        
        if latest_lap and latest_lap.event_id:
            print(f"🎯 Current Event gefunden: {latest_lap.event_id}")
            return {'id': latest_lap.event_id}
        
        print("❌ Kein Current Event gefunden")
        return None
        
    except Exception as e:
        print(f"❌ get_current_event Fehler: {e}")
        return None

def update_driver_cache():
    """Aktualisiert den Driver-Cache aus der DB"""
    global driver_cache
    try:
        # Hole die neuesten Fahrer-Daten pro Controller
        latest_laps = db.session.query(LapTime).order_by(LapTime.timestamp.desc()).limit(50).all()
        
        for lap in latest_laps:
            controller_id = str(lap.controller_id)
            if controller_id not in driver_cache:
                driver_cache[controller_id] = {
                    'name': lap.driver_name or f'Driver {controller_id}',
                    'car': lap.car_name or f'Car {controller_id}',
                    'color': lap.controller_color or '#333333'
                }
        
        print(f"🗂️ Driver Cache aktualisiert: {list(driver_cache.keys())}")
        
    except Exception as e:
        print(f"❌ Driver Cache Update Fehler: {e}")

# Cache beim Start initialisieren
with app.app_context():
    db.create_all()
    update_driver_cache()  # ✅ Cache initialisieren


# Routen
@app.route('/')
def dashboard():
    return render_template('dashboard.html')

@app.route('/analytics')
def analytics():
    return render_template('analytics.html')

@app.route('/leaderboard')
def leaderboard():
    """Live Race Leaderboard"""
    return render_template('leaderboard.html')

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
        print(f"📨 Received data: {json.dumps(data, indent=2)}")  # Debug
        
        # Event speichern
        event = Event(
            event_type=data.get('event_type'),
            data=json.dumps(data)
        )
        db.session.add(event)
        
        # Rundeninformationen verarbeiten
        if data.get('event_type') == 'ui.lap_update':
            event_data = data.get('event_data', {})
            
            # ✅ CONTROLLER-FARBE aus controller_data extrahieren!
            controller_color = '#333333'  # Fallback
            if 'controller_data' in event_data and event_data['controller_data']:
                color_bg = event_data['controller_data'].get('color_bg')
                if color_bg:
                    controller_color = rgb_to_hex(color_bg)
                    print(f"🎨 Controller-Farbe gefunden: {color_bg} → {controller_color}")
            
            # Fahrer und Auto Namen extrahieren
            driver_name = None
            car_name = None
            
            # Prüfe verschiedene Datenquellen
            if 'driver_data' in event_data and event_data['driver_data']:
                driver_name = event_data['driver_data'].get('name')
            elif 'driver' in event_data:
                driver_name = event_data['driver']
            elif 'driver' in data:
                driver_name = data['driver']
            
            if 'car_data' in event_data and event_data['car_data']:
                car_name = event_data['car_data'].get('name')
            elif 'car' in event_data:
                car_name = event_data['car']
            elif 'car' in data:
                car_name = data['car']
            
            # Fallback wenn keine Namen gefunden
            controller_id = event_data.get('controller_id', 'Unknown')
            if not driver_name:
                driver_name = f"Driver {controller_id}"
            if not car_name:
                car_name = f"Car {controller_id}"
            
            lap_time = LapTime(
                controller_id=controller_id,
                driver_name=driver_name,
                car_name=car_name,
                lap=event_data.get('lap'),
                laptime_raw=event_data.get('laptime_raw'),
                laptime=event_data.get('laptime'),
                sector_1=event_data.get('sector_1'),
                sector_2=event_data.get('sector_2'),
                sector_3=event_data.get('sector_3'),
                car_color=event_data.get('car_data', {}).get('color') if event_data.get('car_data') else '#000000',
                controller_color=controller_color,  # ✅ Controller-Farbe speichern!
                is_pb=event_data.get('lap_pb', False)
            )
            db.session.add(lap_time)
            
            print(f"💾 Saved lap: Driver={driver_name}, Car={car_name}, Controller-Color={controller_color}")  # Debug
        
        db.session.commit()
        return jsonify({'status': 'success'})
    
    except Exception as e:
        print(f"❌ Error: {str(e)}")  # Debug
        import traceback
        traceback.print_exc()
        return jsonify({'error': str(e)}), 400

@app.route('/api/live-data')
def live_data():
    try:
        print("🔍 === LIVE-DATA START ===")
        
        # ✅ Hole alle LapTimes wie database.html es macht
        laps = db.session.query(LapTime).order_by(LapTime.timestamp.desc()).all()
        print(f"📊 Gesamt Runden in DB: {len(laps)}")
        
        if len(laps) == 0:
            return jsonify({})
        
        # ✅ Organisiere nach Controller (wie database.html)
        controller_data = {}
        
        for lap in laps:
            controller_id = str(lap.controller_id)
            
            if controller_id not in controller_data:
                controller_data[controller_id] = {
                    'name': lap.driver_name or f'Driver {controller_id}',
                    'car': lap.car_name or f'Car {controller_id}',
                    'color': lap.controller_color or '#FF6B6B',
                    'laps': [],
                    'lap_count': 0,
                    'best_time_raw': None,
                    'best_time_formatted': '--:--.---',
                    'total_time': 0
                }
            
            # ✅ Runde hinzufügen (GLEICHE Struktur wie database.html)
            controller_data[controller_id]['laps'].append({
                'lap': lap.lap or 0,
                'laptime_raw': lap.laptime_raw or 0,
                'laptime_formatted': lap.laptime or '--:--.---',
                'timestamp': lap.timestamp.isoformat() if lap.timestamp else None,
                'is_pb': lap.is_pb or False,
                'driver': lap.driver_name,
                'car': lap.car_name
            })
            
            # ✅ Bestzeit aktualisieren
            if lap.laptime_raw and lap.laptime_raw > 0:
                if (controller_data[controller_id]['best_time_raw'] is None or 
                    lap.laptime_raw < controller_data[controller_id]['best_time_raw']):
                    controller_data[controller_id]['best_time_raw'] = lap.laptime_raw
                    controller_data[controller_id]['best_time_formatted'] = lap.laptime or '--:--.---'
        
        # ✅ Lap Count richtig berechnen (höchste Rundennummer)
        for controller_id in controller_data:
            if controller_data[controller_id]['laps']:
                controller_data[controller_id]['lap_count'] = max(
                    lap['lap'] for lap in controller_data[controller_id]['laps']
                )
        
        print(f"✅ Controller-Daten: {len(controller_data)} Controller")
        return jsonify(controller_data)
        
    except Exception as e:
        print(f"❌ Live-Data Error: {e}")
        return jsonify({}), 500

        
# API für Analytics
@app.route('/api/analytics')
def analytics_data():
    laps = LapTime.query.all()
    
    # Aggregate Daten für Charts
    driver_stats = {}
    for lap in laps:
        # Fahrername fallback - verwende driver_name oder "Unknown Driver"
        driver_name = lap.driver_name or f"Driver {lap.controller_id}" or "Unknown Driver"
        
        if driver_name not in driver_stats:
            driver_stats[driver_name] = {
                'laps': [],
                'best_time': float('inf'),
                'avg_time': 0,
                'total_laps': 0
            }
        
        if lap.laptime_raw and lap.laptime_raw > 0:
            driver_stats[driver_name]['laps'].append(lap.laptime_raw)
            driver_stats[driver_name]['total_laps'] += 1
            if lap.laptime_raw < driver_stats[driver_name]['best_time']:
                driver_stats[driver_name]['best_time'] = lap.laptime_raw
    
    # Berechne Durchschnittswerte und bereinige Daten
    clean_driver_stats = {}
    for driver in driver_stats:
        laps = driver_stats[driver]['laps']
        if laps and driver:  # Nur wenn Fahrer und Rundenzeiten existieren
            avg_time = sum(laps) / len(laps)
            best_time = driver_stats[driver]['best_time']
            
            clean_driver_stats[driver] = {
                'avg_time': round(avg_time, 0),
                'best_time': best_time if best_time != float('inf') else 0,
                'total_laps': len(laps),
                'laps': laps[:50]  # Limitiere auf letzte 50 Runden für Performance
            }
    
    return jsonify(clean_driver_stats)


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
        # Sichere Fallback-Werte für alle Felder
        result.append({
            'driver': lap.driver_name or f"Driver {lap.controller_id}" or "Unknown",
            'car': lap.car_name or f"Car {lap.controller_id}" or "Unknown",
            'lap': lap.lap or 0,
            'laptime': lap.laptime or "0:00.000",
            'sector_1': lap.sector_1 or "0:00.000",
            'sector_2': lap.sector_2 or "0:00.000", 
            'sector_3': lap.sector_3 or "0:00.000",
            'timestamp': lap.timestamp.isoformat() if lap.timestamp else "",
            'is_pb': lap.is_pb or False,
            'controller_id': lap.controller_id or "0"
        })
    
    return jsonify(result)


# API für Filter-Optionen
@app.route('/api/filters')
def get_filters():
    try:
        # Nur nicht-leere Fahrernamen holen
        drivers = db.session.query(LapTime.driver_name.distinct()).filter(
            LapTime.driver_name.isnot(None),
            LapTime.driver_name != ''
        ).all()
        
        # Nur nicht-leere Autonamen holen
        cars = db.session.query(LapTime.car_name.distinct()).filter(
            LapTime.car_name.isnot(None),
            LapTime.car_name != ''
        ).all()
        
        return jsonify({
            'drivers': sorted([d[0] for d in drivers if d[0]]),
            'cars': sorted([c[0] for c in cars if c[0]])
        })
    except Exception as e:
        print(f"Error in get_filters: {str(e)}")
        return jsonify({
            'drivers': [],
            'cars': []
        })

# CSV Export
@app.route('/api/export/csv')
def export_csv():
    try:
        driver_filter = request.args.get('driver')
        car_filter = request.args.get('car')
        
        query = LapTime.query
        
        if driver_filter:
            query = query.filter(LapTime.driver_name.ilike(f'%{driver_filter}%'))
        if car_filter:
            query = query.filter(LapTime.car_name.ilike(f'%{car_filter}%'))
        
        laps = query.order_by(LapTime.timestamp.desc()).all()
        
        # CSV Daten erstellen
        import io
        output = io.StringIO()
        writer = csv.writer(output)
        
        # Header schreiben
        writer.writerow([
            'Timestamp', 'Driver', 'Car', 'Lap', 'Laptime', 
            'Sector 1', 'Sector 2', 'Sector 3', 'Controller ID', 'Personal Best'
        ])
        
        # Daten schreiben
        for lap in laps:
            writer.writerow([
                lap.timestamp.strftime('%Y-%m-%d %H:%M:%S') if lap.timestamp else '',
                lap.driver_name or f"Driver {lap.controller_id}",
                lap.car_name or f"Car {lap.controller_id}",
                lap.lap or 0,
                lap.laptime or "0:00.000",
                lap.sector_1 or "0:00.000",
                lap.sector_2 or "0:00.000",
                lap.sector_3 or "0:00.000",
                lap.controller_id or "0",
                'Yes' if lap.is_pb else 'No'
            ])
        
        output.seek(0)
        
        # Response mit korrekten Headers
        from flask import Response
        return Response(
            output.getvalue(),
            mimetype='text/csv',
            headers={
                'Content-Disposition': f'attachment; filename=smartrace_export_{datetime.now().strftime("%Y%m%d_%H%M%S")}.csv',
                'Content-Type': 'text/csv; charset=utf-8'
            }
        )
        
    except Exception as e:
        print(f"CSV Export Error: {str(e)}")
        return jsonify({'error': f'Export failed: {str(e)}'}), 500
        
# Globale Variable für Dropbox Token
DROPBOX_TOKEN = None

@app.route('/api/dropbox/setup', methods=['POST'])
def setup_dropbox():
    global DROPBOX_TOKEN
    
    try:
        data = request.get_json()
        token = data.get('token')
        
        if not token:
            return jsonify({'success': False, 'error': 'No token provided'})
        
        # Test the token
        try:
            import dropbox
            dbx = dropbox.Dropbox(token)
            account_info = dbx.users_get_current_account()
            
            # Token is valid, save it
            DROPBOX_TOKEN = token
            
            return jsonify({
                'success': True,
                'message': f'Connected to Dropbox account: {account_info.name.display_name}'
            })
            
        except Exception as e:
            return jsonify({'success': False, 'error': f'Invalid token: {str(e)}'})
            
    except Exception as e:
        return jsonify({'success': False, 'error': f'Setup error: {str(e)}'})

@app.route('/api/dropbox/test', methods=['GET'])
def test_dropbox():
    global DROPBOX_TOKEN
    
    if not DROPBOX_TOKEN:
        return jsonify({'success': False, 'error': 'No Dropbox token configured'})
    
    try:
        import dropbox
        dbx = dropbox.Dropbox(DROPBOX_TOKEN)
        account_info = dbx.users_get_current_account()
        
        return jsonify({
            'success': True,
            'account': account_info.name.display_name
        })
        
    except Exception as e:
        return jsonify({'success': False, 'error': f'Connection failed: {str(e)}'})

@app.route('/api/dropbox/backup', methods=['POST'])
def backup_to_dropbox():
    global DROPBOX_TOKEN
    
    if not DROPBOX_TOKEN:
        return jsonify({'success': False, 'error': 'Dropbox not configured'})
    
    try:
        import dropbox
        import csv
        import io
        from datetime import datetime
        
        # Get all lap data
        laps = LapTime.query.order_by(LapTime.timestamp.desc()).all()
        
        if not laps:
            return jsonify({'success': False, 'error': 'No lap data to export'})
        
        # Create CSV in memory
        output = io.StringIO()
        writer = csv.writer(output)
        
        # Write header with correct column names
        writer.writerow([
            'Event_ID', 'Controller_ID', 'Driver', 'Car', 'Lap', 
            'Laptime_Raw', 'Laptime', 'Sector_1', 'Sector_2', 'Sector_3', 
            'Car_Color', 'Timestamp', 'Is_PB'
        ])
        
        # Write data using the correct attributes
        for lap in laps:
            writer.writerow([
                lap.event_id or '',
                lap.controller_id or '',
                lap.driver_name or '',
                lap.car_name or '',
                lap.lap or '',
                lap.laptime_raw or '',
                lap.laptime or '',
                lap.sector_1 or '',
                lap.sector_2 or '',
                lap.sector_3 or '',
                lap.car_color or '',
                lap.timestamp.strftime('%Y-%m-%d %H:%M:%S') if lap.timestamp else '',
                'Yes' if lap.is_pb else 'No'
            ])
        
        # Upload to Dropbox
        dbx = dropbox.Dropbox(DROPBOX_TOKEN)
        filename = f"/Carrera/BackUp_DB/SmartRace_WEB_DB_Backup_{datetime.now().strftime('%Y%m%d_%H%M%S')}.csv"
        
        dbx.files_upload(
            output.getvalue().encode('utf-8'),
            filename,
            mode=dropbox.files.WriteMode('overwrite')
        )
        
        return jsonify({
            'success': True,
            'records': len(laps),
            'filename': filename,
            'message': f'Successfully uploaded {len(laps)} records to Dropbox as {filename}'
        })
        
    except Exception as e:
        import traceback
        print("Dropbox backup error:", traceback.format_exc())
        return jsonify({'success': False, 'error': f'Backup failed: {str(e)}'})

if __name__ == '__main__':
    app.run(host='0.0.0.0', port=5000, debug=True)
