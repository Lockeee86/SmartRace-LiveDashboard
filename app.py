from flask import Flask, render_template, jsonify
import mysql.connector
import os
from datetime import datetime

app = Flask(__name__)

# MySQL Verbindung
def get_db_connection():
    try:
        conn = mysql.connector.connect(
            host='mysql',  # Docker service name
            user='root',
            password='smartrace123',
            database='smartrace'
        )
        return conn
    except Exception as e:
        print(f"DB Error: {e}")
        return None

@app.route('/')
def dashboard():
    return render_template('dashboard.html')

@app.route('/database')
def database():
    return render_template('database.html')

@app.route('/api/live-data')
def get_live_data():
    try:
        conn = get_db_connection()
        if not conn:
            return jsonify([])
            
        cursor = conn.cursor()
        cursor.execute('''
            SELECT controller_id, driver, car, lap_number, total_time, 
                   sector1, sector2, sector3, timestamp
            FROM laps 
            ORDER BY timestamp DESC 
            LIMIT 100
        ''')
        
        laps = []
        for row in cursor.fetchall():
            laps.append({
                'controller_id': row[0],
                'driver': row[1] or 'Driver',
                'car': row[2] or 'Car',
                'lap_number': row[3] or 0,
                'total_time': row[4] or 0,
                'sector1': row[5],
                'sector2': row[6],  
                'sector3': row[7]
            })
        
        conn.close()
        return jsonify(laps)
    except Exception as e:
        print(f"API Error: {e}")
        return jsonify([])

if __name__ == '__main__':
    print('🏁 SmartRace Analytics starting...')
    print('🔗 Access: http://localhost:5000')
    app.run(host='0.0.0.0', port=5000, debug=True)
