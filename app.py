from flask import Flask, render_template, request, jsonify
from flask_sqlalchemy import SQLAlchemy
from flask_cors import CORS
from datetime import datetime
import json
import os
import csv


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
                is_pb=event_data.get('lap_pb', False)
            )
            db.session.add(lap_time)
            
            print(f"💾 Saved lap: Driver={driver_name}, Car={car_name}")  # Debug
        
        db.session.commit()
        return jsonify({'status': 'success'})
    
    except Exception as e:
        print(f"❌ Error: {str(e)}")  # Debug
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

# Dropbox Setup - Robuste Version
@app.route('/api/dropbox/setup', methods=['POST'])
def setup_dropbox():
    try:
        data = request.get_json()
        if not data:
            return jsonify({'success': False, 'error': 'No data received'}), 400
            
        token = data.get('token', '').strip()
        
        if not token:
            return jsonify({'success': False, 'error': 'Dropbox token is required'}), 400
            
        if len(token) < 10:
            return jsonify({'success': False, 'error': 'Invalid token format'}), 400
            
        # Versuche Dropbox Import
        try:
            import dropbox
        except ImportError:
            return jsonify({
                'success': False, 
                'error': 'Dropbox library not installed. Run: pip install dropbox'
            }), 500
            
        # Test Dropbox Verbindung mit verschiedenen Timeouts
        for timeout in [5, 10, 15]:
            try:
                print(f"Testing Dropbox connection with {timeout}s timeout...")
                dbx = dropbox.Dropbox(token, timeout=timeout)
                
                # Einfacher Test-Call
                account_info = dbx.users_get_current_account()
                
                # Wenn wir hier ankommen, funktioniert die Verbindung
                print(f"Dropbox connection successful for account: {account_info.name.display_name}")
                
                # Token in Datenbank speichern
                try:
                    config = Config.query.filter_by(key='dropbox_token').first()
                    if config:
                        config.value = token
                    else:
                        config = Config(key='dropbox_token', value=token)
                        db.session.add(config)
                    
                    db.session.commit()
                    print("Dropbox token saved to database")
                    
                except Exception as db_error:
                    print(f"Database error: {str(db_error)}")
                    return jsonify({
                        'success': False, 
                        'error': f'Database error while saving token: {str(db_error)}'
                    }), 500
                
                return jsonify({
                    'success': True, 
                    'message': f'Dropbox successfully connected! Account: {account_info.name.display_name}'
                })
                
            except dropbox.exceptions.AuthError as auth_error:
                print(f"Auth error: {str(auth_error)}")
                return jsonify({
                    'success': False, 
                    'error': 'Invalid Dropbox token. Please check your access token and try again.'
                }), 401
                
            except dropbox.exceptions.ApiError as api_error:
                print(f"API error: {str(api_error)}")
                return jsonify({
                    'success': False, 
                    'error': f'Dropbox API error: {str(api_error)}'
                }), 400
                
            except (ConnectionError, TimeoutError, OSError) as conn_error:
                print(f"Connection error with {timeout}s timeout: {str(conn_error)}")
                if timeout == 15:  # Letzter Versuch
                    return jsonify({
                        'success': False, 
                        'error': 'Network connection failed. Please check internet connection and firewall settings.'
                    }), 408
                continue  # Versuche nächsten Timeout
                
            except Exception as e:
                error_str = str(e).lower()
                print(f"Unexpected error with {timeout}s timeout: {str(e)}")
                
                if any(word in error_str for word in ['timeout', 'connection', 'network', 'unreachable']):
                    if timeout == 15:  # Letzter Versuch
                        return jsonify({
                            'success': False, 
                            'error': f'Network timeout after {timeout} seconds. Please try again or check your connection.'
                        }), 408
                    continue  # Versuche nächsten Timeout
                else:
                    return jsonify({
                        'success': False, 
                        'error': f'Unexpected error: {str(e)}'
                    }), 500
        
        # Falls alle Timeouts fehlschlagen
        return jsonify({
            'success': False, 
            'error': 'Connection failed after multiple attempts. Please check your internet connection.'
        }), 408
        
    except Exception as e:
        print(f"General setup error: {str(e)}")
        return jsonify({
            'success': False, 
            'error': f'Setup failed: {str(e)}'
        }), 500

# Dropbox Status - Vereinfacht
@app.route('/api/dropbox/status')
def dropbox_status():
    try:
        config = Config.query.filter_by(key='dropbox_token').first()
        if not config or not config.value:
            return jsonify({
                'connected': False, 
                'message': 'No Dropbox token configured'
            })
            
        # Nur prüfen ob Token existiert, nicht testen (für bessere Performance)
        return jsonify({
            'connected': True,
            'message': 'Dropbox token configured',
            'token_preview': config.value[:10] + "..." if len(config.value) > 10 else config.value
        })
            
    except Exception as e:
        print(f"Status error: {str(e)}")
        return jsonify({
            'connected': False, 
            'message': f'Error checking status: {str(e)}'
        })

# Dropbox Test-Verbindung (separater Endpoint)
@app.route('/api/dropbox/test')
def test_dropbox():
    try:
        config = Config.query.filter_by(key='dropbox_token').first()
        if not config or not config.value:
            return jsonify({'success': False, 'error': 'No token configured'})
            
        import dropbox
        dbx = dropbox.Dropbox(config.value, timeout=10)
        
        account_info = dbx.users_get_current_account()
        return jsonify({
            'success': True, 
            'account': account_info.name.display_name,
            'email': account_info.email
        })
        
    except Exception as e:
        return jsonify({'success': False, 'error': str(e)})

@app.route('/api/dropbox/setup', methods=['POST'])
def setup_dropbox():
    try:
        data = request.get_json()
        token = data.get('token', '').strip()
        
        if not token:
            return jsonify({'success': False, 'error': 'Token is required'}), 400
            
        # Test Dropbox Verbindung mit Timeout
        import dropbox
        from dropbox.exceptions import AuthError, ApiError
        
        try:
            dbx = dropbox.Dropbox(token, timeout=10)  # 10 Sekunden Timeout
            
            # Test API call
            account_info = dbx.users_get_current_account()
            
            # Token speichern
            config = Config.query.filter_by(key='dropbox_token').first()
            if config:
                config.value = token
            else:
                config = Config(key='dropbox_token', value=token)
                db.session.add(config)
            
            db.session.commit()
            
            return jsonify({
                'success': True, 
                'message': f'Dropbox connected successfully! Account: {account_info.name.display_name}'
            })
            
        except AuthError:
            return jsonify({
                'success': False, 
                'error': 'Invalid Dropbox token. Please check your token and try again.'
            }), 401
            
        except ApiError as e:
            return jsonify({
                'success': False, 
                'error': f'Dropbox API error: {str(e)}'
            }), 400
            
        except Exception as e:
            error_msg = str(e)
            if 'timeout' in error_msg.lower() or 'connection' in error_msg.lower():
                return jsonify({
                    'success': False, 
                    'error': 'Network timeout. Please check your internet connection and try again.'
                }), 408
            else:
                return jsonify({
                    'success': False, 
                    'error': f'Connection failed: {error_msg}'
                }), 400
                
    except Exception as e:
        return jsonify({
            'success': False, 
            'error': f'Setup failed: {str(e)}'
        }), 500

# Dropbox Status
@app.route('/api/dropbox/status')
def dropbox_status():
    try:
        config = Config.query.filter_by(key='dropbox_token').first()
        if not config or not config.value:
            return jsonify({'connected': False, 'message': 'No token configured'})
            
        import dropbox
        dbx = dropbox.Dropbox(config.value, timeout=5)
        
        try:
            account_info = dbx.users_get_current_account()
            return jsonify({
                'connected': True, 
                'account': account_info.name.display_name,
                'message': 'Connected successfully'
            })
        except:
            return jsonify({'connected': False, 'message': 'Token invalid or expired'})
            
    except Exception as e:
        return jsonify({'connected': False, 'message': f'Error: {str(e)}'})

# Dropbox Upload
@app.route('/api/dropbox/upload', methods=['POST'])
def upload_to_dropbox():
    try:
        config = Config.query.filter_by(key='dropbox_token').first()
        if not config or not config.value:
            return jsonify({'success': False, 'error': 'Dropbox not configured'}), 400
            
        # CSV Daten generieren
        query = LapTime.query.order_by(LapTime.timestamp.desc())
        laps = query.all()
        
        import io
        output = io.StringIO()
        writer = csv.writer(output)
        
        # Header
        writer.writerow([
            'Timestamp', 'Driver', 'Car', 'Lap', 'Laptime', 
            'Sector 1', 'Sector 2', 'Sector 3', 'Controller ID', 'Personal Best'
        ])
        
        # Daten
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
        
        # Upload zu Dropbox
        import dropbox
        dbx = dropbox.Dropbox(config.value, timeout=30)
        
        filename = f"/SmartRace_Backup_{datetime.now().strftime('%Y%m%d_%H%M%S')}.csv"
        file_data = output.getvalue().encode('utf-8')
        
        dbx.files_upload(file_data, filename, mode=dropbox.files.WriteMode('overwrite'))
        
        return jsonify({
            'success': True, 
            'message': f'Backup uploaded successfully as {filename}',
            'filename': filename
        })
        
    except Exception as e:
        return jsonify({'success': False, 'error': f'Upload failed: {str(e)}'}), 500


if __name__ == '__main__':
    app.run(host='0.0.0.0', port=5000, debug=True)
