from flask import Flask, render_template, jsonify
import mysql.connector
import json
import random
import time

app = Flask(__name__)

# Database connection
def get_db_connection():
    return mysql.connector.connect(
        host='mysql',
        user='smartrace',
        password='smartrace123',
        database='smartrace'
    )

# Routes
@app.route('/api/laps')
def get_laps():
    try:
        cursor = conn.cursor()
        cursor.execute('''
            SELECT controller_id, driver, car, sector1, sector2, sector3, 
                   lap_number, total_time, timestamp
            FROM laps 
            ORDER BY timestamp DESC 
            LIMIT 50
        ''')
        
        laps = []
        for row in cursor.fetchall():
            laps.append({
                'controller_id': row[0],
                'driver': row[1] or 'Unbekannt',
                'car': row[2] or 'Unbekannt',
                'sector1': f"{row[3]:.3f}s" if row[3] else '-',
                'sector2': f"{row[4]:.3f}s" if row[4] else '-',
                'sector3': f"{row[5]:.3f}s" if row[5] else '-',
                'lap_number': row[6] or 0,
                'total_time': f"{row[7]:.3f}s" if row[7] else '-'
            })
        
        return jsonify(laps)
    except Exception as e:
        return jsonify({'error': str(e)}), 500

@app.route('/')
def dashboard():
    return render_template('dashboard.html')

@app.route('/live')
def live_stats():
    return render_template('live.html')

@app.route('/charts')
def charts():
    return render_template('charts.html')

@app.route('/database')
def database():
    return render_template('database.html')

@app.route('/api/stats')
def api_stats():
    # Demo data - später durch SmartRacer API ersetzen
    return jsonify({
        'total_laps': 156,
        'active_drivers': 5,
        'fastest_lap': '1:23.456',
        'current_race': 'Monaco GP'
    })

@app.route('/api/live-data')
def api_live_data():
    return jsonify({
        'drivers': [
            {'name': 'Driver 1', 'position': 1, 'lap_time': '1:23.456'},
            {'name': 'Driver 2', 'position': 2, 'lap_time': '1:24.123'},
            {'name': 'Driver 3', 'position': 3, 'lap_time': '1:24.789'}
        ]
    })

if __name__ == '__main__':
    app.run(host='0.0.0.0', port=5000, debug=True)
