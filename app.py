from flask import Flask, render_template, request, redirect, url_for, jsonify, send_file
import mysql.connector
import csv
import io
import os
import requests
import time
from datetime import datetime

app = Flask(__name__)

# Database configuration
db_config = {
    'host': 'mysql',
    'user': 'root',
    'password': 'smartrace123',
    'database': 'smartrace'
}

def get_db_connection():
    max_retries = 30  # 30 Versuche = 30 Sekunden
    retry_count = 0
    
    while retry_count < max_retries:
        try:
            print(f"Verbindungsversuch {retry_count + 1}/{max_retries} zu MySQL...")
            conn = mysql.connector.connect(**db_config)
            print("✅ MySQL Verbindung erfolgreich!")
            return conn
        except mysql.connector.Error as e:
            retry_count += 1
            print(f"❌ MySQL Verbindung fehlgeschlagen: {e}")
            if retry_count < max_retries:
                print("⏳ Warte 1 Sekunde und versuche erneut...")
                time.sleep(1)
            else:
                print("💥 Maximale Anzahl der Versuche erreicht!")
                raise

def init_db():
    conn = get_db_connection()
    cursor = conn.cursor()
    cursor.execute('''
        CREATE TABLE IF NOT EXISTS race_times (
            id INT AUTO_INCREMENT PRIMARY KEY,
            fahrer_name VARCHAR(100) NOT NULL,
            zeit_sekunden DECIMAL(6,3) NOT NULL,
            datum TIMESTAMP DEFAULT CURRENT_TIMESTAMP
        )
    ''')
    conn.commit()
    cursor.close()
    conn.close()
    print("✅ Datenbank initialisiert!")

@app.route('/')
def index():
    return render_template('index.html')

@app.route('/database')
def database():
    conn = get_db_connection()
    cursor = conn.cursor()
    cursor.execute('SELECT * FROM race_times ORDER BY zeit_sekunden ASC')
    race_times = cursor.fetchall()
    cursor.close()
    conn.close()
    return render_template('database.html', race_times=race_times)

@app.route('/add_time', methods=['POST'])
def add_time():
    fahrer_name = request.form['fahrer_name']
    zeit_sekunden = float(request.form['zeit_sekunden'])
    
    conn = get_db_connection()
    cursor = conn.cursor()
    cursor.execute('INSERT INTO race_times (fahrer_name, zeit_sekunden) VALUES (%s, %s)', 
                   (fahrer_name, zeit_sekunden))
    conn.commit()
    cursor.close()
    conn.close()
    
    return redirect(url_for('database'))

@app.route('/export_csv')
def export_csv():
    conn = get_db_connection()
    cursor = conn.cursor()
    cursor.execute('SELECT fahrer_name, zeit_sekunden, datum FROM race_times ORDER BY zeit_sekunden ASC')
    race_times = cursor.fetchall()
    cursor.close()
    conn.close()
    
    # CSV in Memory erstellen
    output = io.StringIO()
    writer = csv.writer(output)
    writer.writerow(['Fahrer', 'Zeit (Sekunden)', 'Datum'])
    
    for race_time in race_times:
        writer.writerow([race_time[0], race_time[1], race_time[2]])
    
    # StringIO zu BytesIO konvertieren
    mem = io.BytesIO()
    mem.write(output.getvalue().encode('utf-8'))
    mem.seek(0)
    output.close()
    
    return send_file(
        mem,
        as_attachment=True,
        download_name=f'smartrace_export_{datetime.now().strftime("%Y%m%d_%H%M%S")}.csv',
        mimetype='text/csv'
    )

@app.route('/export_dropbox')
def export_dropbox():
    # Dropbox Access Token (muss konfiguriert werden)
    DROPBOX_ACCESS_TOKEN = os.environ.get('DROPBOX_TOKEN', 'YOUR_DROPBOX_TOKEN_HERE')
    
    if DROPBOX_ACCESS_TOKEN == 'YOUR_DROPBOX_TOKEN_HERE':
        return jsonify({'error': 'Dropbox Token nicht konfiguriert'}), 400
    
    # CSV Daten erstellen
    conn = get_db_connection()
    cursor = conn.cursor()
    cursor.execute('SELECT fahrer_name, zeit_sekunden, datum FROM race_times ORDER BY zeit_sekunden ASC')
    race_times = cursor.fetchall()
    cursor.close()
    conn.close()
    
    # CSV erstellen
    csv_content = "Fahrer,Zeit (Sekunden),Datum\n"
    for race_time in race_times:
        csv_content += f"{race_time[0]},{race_time[1]},{race_time[2]}\n"
    
    # Zu Dropbox hochladen
    filename = f'/smartrace_export_{datetime.now().strftime("%Y%m%d_%H%M%S")}.csv'
    
    headers = {
        'Authorization': f'Bearer {DROPBOX_ACCESS_TOKEN}',
        'Content-Type': 'application/octet-stream',
        'Dropbox-API-Arg': f'{{"path":"{filename}"}}'
    }
    
    try:
        response = requests.post(
            'https://content.dropboxapi.com/2/files/upload',
            headers=headers,
            data=csv_content.encode('utf-8')
        )
        
        if response.status_code == 200:
            return jsonify({'success': True, 'message': f'Erfolgreich zu Dropbox exportiert: {filename}'})
        else:
            return jsonify({'error': f'Dropbox Upload Fehler: {response.text}'}), 400
            
    except Exception as e:
        return jsonify({'error': f'Fehler beim Upload: {str(e)}'}), 500

if __name__ == '__main__':
    print("🚀 SmartRace startet...")
    init_db()
    print("🌐 Flask Server startet auf Port 5000...")
    app.run(host='0.0.0.0', port=5000, debug=True)
