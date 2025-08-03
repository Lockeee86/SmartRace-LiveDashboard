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
