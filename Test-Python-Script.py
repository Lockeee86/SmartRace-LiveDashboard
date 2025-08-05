#!/usr/bin/env python3
"""
SmartRace Dashboard Test Suite
Testet alle Funktionen des SmartRace Dashboards
"""

import requests
import json
import time
import random
import threading
from datetime import datetime
import csv
import os

class SmartRaceTestSuite:
    def __init__(self, base_url="http://localhost:5000"):
        self.base_url = base_url
        self.session = requests.Session()
        self.test_results = []
        self.test_data_created = []
        
    def log(self, message, status="INFO"):
        timestamp = datetime.now().strftime("%H:%M:%S")
        status_emoji = {
            "INFO": "ℹ️",
            "SUCCESS": "✅",
            "ERROR": "❌",
            "WARNING": "⚠️"
        }
        print(f"[{timestamp}] {status_emoji.get(status, '')} {message}")
        
    def test_connection(self):
        """Teste Verbindung zum Server"""
        self.log("Testing server connection...")
        try:
            response = self.session.get(f"{self.base_url}/")
            if response.status_code == 200:
                self.log("Server is reachable", "SUCCESS")
                return True
            else:
                self.log(f"Server returned status {response.status_code}", "ERROR")
                return False
        except requests.exceptions.ConnectionError:
            self.log("Cannot connect to server", "ERROR")
            return False
            
    def test_smartrace_endpoint(self):
        """Teste SmartRace API Endpoint"""
        self.log("Testing SmartRace API endpoint...")
        
        # Test data representing SmartRace lap update
        test_payload = {
            "time": int(time.time() * 1000),
            "event_type": "ui.lap_update",
            "event_data": {
                "controller_id": "1",
                "lap": 1,
                "laptime": "0:13.861",
                "laptime_raw": 13861,
                "sector_1": "0:06.305",
                "sector_1_pb": True,
                "sector_2": "0:03.256",
                "sector_2_pb": False,
                "sector_3": "0:04.300",
                "sector_3_pb": True,
                "event_id": "test_event_001",
                "driver": "Test Driver 1",
                "car": "Test Car Red"
            }
        }
        
        try:
            response = self.session.post(
                f"{self.base_url}/api/smartrace",
                json=test_payload,
                headers={'Content-Type': 'application/json'}
            )
            
            if response.status_code == 200:
                self.log("SmartRace endpoint working", "SUCCESS")
                return True
            else:
                self.log(f"SmartRace endpoint failed: {response.status_code}", "ERROR")
                return False
        except Exception as e:
            self.log(f"SmartRace endpoint error: {str(e)}", "ERROR")
            return False

    def generate_test_data(self, num_events=50):
    """Generiere Testdaten für verschiedene Szenarien"""
    self.log(f"Generating {num_events} test events...")
    
    # Erweiterte F1 2024 Grid
    controllers = [
        # Red Bull Racing
        {"id": "1", "driver": "Max Verstappen", "car": "Red Bull RB20", "color": "#3671C6", "skill": 0.92},
        {"id": "2", "driver": "Sergio Perez", "car": "Red Bull RB20", "color": "#3671C6", "skill": 0.96},
        
        # Mercedes
        {"id": "3", "driver": "Lewis Hamilton", "car": "Mercedes W15", "color": "#27F4D2", "skill": 0.94},
        {"id": "4", "driver": "George Russell", "car": "Mercedes W15", "color": "#27F4D2", "skill": 0.95},
        
        # Ferrari
        {"id": "5", "driver": "Charles Leclerc", "car": "Ferrari SF-24", "color": "#E8002D", "skill": 0.93},
        {"id": "6", "driver": "Carlos Sainz", "car": "Ferrari SF-24", "color": "#E8002D", "skill": 0.95},
        
        # McLaren
        {"id": "7", "driver": "Lando Norris", "car": "McLaren MCL38", "color": "#FF8000", "skill": 0.94},
        {"id": "8", "driver": "Oscar Piastri", "car": "McLaren MCL38", "color": "#FF8000", "skill": 0.97},
        
        # Aston Martin
        {"id": "9", "driver": "Fernando Alonso", "car": "Aston Martin AMR24", "color": "#229971", "skill": 0.95},
        {"id": "10", "driver": "Lance Stroll", "car": "Aston Martin AMR24", "color": "#229971", "skill": 0.98},
        
        # Alpine
        {"id": "11", "driver": "Pierre Gasly", "car": "Alpine A524", "color": "#0093CC", "skill": 0.97},
        {"id": "12", "driver": "Esteban Ocon", "car": "Alpine A524", "color": "#0093CC", "skill": 0.97},
        
        # Williams
        {"id": "13", "driver": "Alex Albon", "car": "Williams FW46", "color": "#64C4FF", "skill": 0.98},
        {"id": "14", "driver": "Logan Sargeant", "car": "Williams FW46", "color": "#64C4FF", "skill": 1.02},
        
        # RB F1 Team
        {"id": "15", "driver": "Yuki Tsunoda", "car": "RB VCARB 01", "color": "#6692FF", "skill": 0.98},
        {"id": "16", "driver": "Daniel Ricciardo", "car": "RB VCARB 01", "color": "#6692FF", "skill": 0.96},
        
        # Haas
        {"id": "17", "driver": "Nico Hulkenberg", "car": "Haas VF-24", "color": "#B6BABD", "skill": 0.99},
        {"id": "18", "driver": "Kevin Magnussen", "car": "Haas VF-24", "color": "#B6BABD", "skill": 0.99},
        
        # Kick Sauber
        {"id": "19", "driver": "Valtteri Bottas", "car": "Sauber C44", "color": "#52E252", "skill": 0.98},
        {"id": "20", "driver": "Zhou Guanyu", "car": "Sauber C44", "color": "#52E252", "skill": 1.00},
        
        # Zusätzliche historische Fahrer für Abwechslung
        {"id": "21", "driver": "Michael Schumacher", "car": "Ferrari F2004", "color": "#DC143C", "skill": 0.91},
        {"id": "22", "driver": "Ayrton Senna", "car": "McLaren MP4/4", "color": "#FF8700", "skill": 0.90},
        {"id": "23", "driver": "Sebastian Vettel", "car": "Red Bull RB19", "color": "#3671C6", "skill": 0.93},
        {"id": "24", "driver": "Kimi Raikkonen", "car": "Ferrari SF70H", "color": "#E8002D", "skill": 0.95}
    ]
    
    # Verschiedene Strecken mit unterschiedlichen Base-Zeiten
    tracks = [
        {"name": "Monaco", "base_time": 78000, "sector_split": [0.42, 0.28, 0.30]},
        {"name": "Silverstone", "base_time": 92000, "sector_split": [0.35, 0.35, 0.30]},
        {"name": "Spa-Francorchamps", "base_time": 107000, "sector_split": [0.33, 0.40, 0.27]},
        {"name": "Monza", "base_time": 82000, "sector_split": [0.30, 0.35, 0.35]},
        {"name": "Suzuka", "base_time": 91000, "sector_split": [0.38, 0.32, 0.30]},
        {"name": "Interlagos", "base_time": 75000, "sector_split": [0.36, 0.34, 0.30]},
        {"name": "Nürburgring", "base_time": 95000, "sector_split": [0.34, 0.33, 0.33]}
    ]
    
    current_track = random.choice(tracks)
    event_types = ["ui.lap_update", "event.start", "event.end", "ui.remove_car_from_session"]
    
    # Session Types für mehr Realismus
    session_types = ["practice", "qualifying", "sprint", "race"]
    current_session = random.choice(session_types)
    
    self.log(f"Simulating session: {current_session} at {current_track['name']}")
    
    for i in range(num_events):
        controller = random.choice(controllers)
        event_type = random.choices(event_types, weights=[80, 8, 8, 4], k=1)[0]
        
        payload = {
            "time": int((time.time() - random.randint(0, 86400)) * 1000),  # Last 24 hours
            "event_type": event_type,
            "event_data": {}
        }
        
        if event_type == "ui.lap_update":
            # Realistische Rundenzeiten basierend auf Fahrer-Skill und Strecke
            base_laptime = current_track["base_time"]
            skill_factor = controller["skill"]
            
            # Session-spezifische Modifikationen
            session_factor = {
                "practice": random.uniform(1.02, 1.08),
                "qualifying": random.uniform(0.98, 1.02),
                "sprint": random.uniform(1.01, 1.04),
                "race": random.uniform(1.03, 1.07)
            }
            
            laptime_ms = int(base_laptime * skill_factor * session_factor[current_session] * random.uniform(0.98, 1.05))
            
            # Sektor-Zeiten basierend auf Strecken-Layout
            sector_splits = current_track["sector_split"]
            sector1_ms = int(laptime_ms * sector_splits[0] * random.uniform(0.95, 1.05))
            sector2_ms = int(laptime_ms * sector_splits[1] * random.uniform(0.95, 1.05))
            sector3_ms = laptime_ms - sector1_ms - sector2_ms
            
            # Bessere PB-Wahrscheinlichkeit für bessere Fahrer
            pb_chance = max(0.05, 0.25 - (controller["skill"] - 0.9) * 2)
            
            def ms_to_timestring(ms):
                if ms >= 60000:
                    minutes = ms // 60000
                    seconds = (ms % 60000) / 1000
                    return f"{minutes}:{seconds:06.3f}"
                else:
                    seconds = ms / 1000
                    return f"0:{seconds:06.3f}"
            
            # Event ID basierend auf Session und Strecke
            event_id = f"{current_session}_{current_track['name'].lower()}_{random.randint(1, 5)}"
            
            payload["event_data"] = {
                "controller_id": controller["id"],
                "lap": random.randint(1, 65),  # Längere Rennen
                "laptime": ms_to_timestring(laptime_ms),
                "laptime_raw": laptime_ms,
                "sector_1": ms_to_timestring(max(1000, sector1_ms)),
                "sector_1_pb": random.random() < pb_chance,
                "sector_2": ms_to_timestring(max(1000, sector2_ms)),
                "sector_2_pb": random.random() < pb_chance,
                "sector_3": ms_to_timestring(max(1000, sector3_ms)),
                "sector_3_pb": random.random() < pb_chance,
                "event_id": event_id,
                "driver": controller["driver"],
                "car": controller["car"],
                "track": current_track["name"],
                "session_type": current_session
            }
            
        elif event_type == "event.start":
            payload["event_data"] = {
                "event_id": f"{current_session}_{current_track['name'].lower()}_{random.randint(1, 5)}",
                "session_type": current_session,
                "track": current_track["name"],
                "weather": random.choice(["dry", "wet", "intermediate"]),
                "temperature": random.randint(15, 35)
            }
            
        elif event_type == "event.end":
            payload["event_data"] = {
                "event_id": f"{current_session}_{current_track['name'].lower()}_{random.randint(1, 5)}",
                "session_type": current_session,
                "winner": controller["driver"] if current_session in ["race", "sprint"] else None,
                "total_laps": random.randint(20, 70)
            }
            
        elif event_type == "ui.remove_car_from_session":
            payload["event_data"] = {
                "controller_id": controller["id"],
                "reason": random.choice(["manual", "dnf", "technical", "accident"])
            }
        
        # Send to API
        try:
            response = self.session.post(
                f"{self.base_url}/api/smartrace",
                json=payload,
                headers={'Content-Type': 'application/json'}
            )
            
            if response.status_code == 200:
                if i % 10 == 0:
                    self.log(f"Generated {i+1}/{num_events} events... (Last: {controller['driver']} - {current_track['name']})")
            else:
                self.log(f"Failed to send event {i+1}: {response.status_code}", "ERROR")
                
        except Exception as e:
            self.log(f"Error sending event {i+1}: {str(e)}", "ERROR")
        
        # Small delay to simulate real data
        time.sleep(0.05)
    
    self.log(f"Generated {num_events} test events for {current_session} at {current_track['name']}", "SUCCESS")

def simulate_live_race(self, duration_seconds=30):
    """Simuliere ein Live-Rennen mit mehr Fahrern"""
    self.log(f"Simulating live race for {duration_seconds} seconds...")
    
    # Top 8 Fahrer für Live-Simulation
    controllers = [
        {"id": "1", "driver": "Max Verstappen", "car": "Red Bull RB20", "skill": 0.92, "position": 1},
        {"id": "3", "driver": "Lewis Hamilton", "car": "Mercedes W15", "skill": 0.94, "position": 2},
        {"id": "5", "driver": "Charles Leclerc", "car": "Ferrari SF-24", "skill": 0.93, "position": 3},
        {"id": "7", "driver": "Lando Norris", "car": "McLaren MCL38", "skill": 0.94, "position": 4},
        {"id": "9", "driver": "Fernando Alonso", "car": "Aston Martin AMR24", "skill": 0.95, "position": 5},
        {"id": "4", "driver": "George Russell", "car": "Mercedes W15", "skill": 0.95, "position": 6},
        {"id": "6", "driver": "Carlos Sainz", "car": "Ferrari SF-24", "skill": 0.95, "position": 7},
        {"id": "8", "driver": "Oscar Piastri", "car": "McLaren MCL38", "skill": 0.97, "position": 8}
    ]
    
    # Silverstone als Beispielstrecke
    base_time = 92000
    track_name = "Silverstone"
    
    start_time = time.time()
    lap_count = {controller["id"]: random.randint(1, 5) for controller in controllers}
    
    def send_lap_update(controller):
        # Realistische Rundenzeiten mit Skill-Faktor
        laptime_ms = int(base_time * controller["skill"] * random.uniform(0.98, 1.04))
        
        # Reifen-Degradation simulieren (längere Stints = langsamere Zeiten)
        tire_factor = 1 + (lap_count[controller["id"]] * 0.002)
        laptime_ms = int(laptime_ms * tire_factor)
        
        # Sektor-Zeiten
        sector1_ms = int(laptime_ms * 0.35 * random.uniform(0.98, 1.02))
        sector2_ms = int(laptime_ms * 0.35 * random.uniform(0.98, 1.02))
        sector3_ms = laptime_ms - sector1_ms - sector2_ms
        
        def ms_to_timestring(ms):
            minutes = ms // 60000
            seconds = (ms % 60000) / 1000
            return f"{minutes}:{seconds:06.3f}"
        
        # Personal Best Chancen
        pb_chance = 0.15 if lap_count[controller["id"]] < 10 else 0.05
        
        payload = {
            "time": int(time.time() * 1000),
            "event_type": "ui.lap_update",
            "event_data": {
                "controller_id": controller["id"],
                "lap": lap_count[controller["id"]],
                "laptime": ms_to_timestring(laptime_ms),
                "laptime_raw": laptime_ms,
                "sector_1": ms_to_timestring(sector1_ms),
                "sector_1_pb": random.random() < pb_chance,
                "sector_2": ms_to_timestring(sector2_ms),
                "sector_2_pb": random.random() < pb_chance,
                "sector_3": ms_to_timestring(max(1000, sector3_ms)),
                "sector_3_pb": random.random() < pb_chance,
                "event_id": "live_race_silverstone",
                "driver": controller["driver"],
                "car": controller["car"],
                "track": track_name,
                "session_type": "race",
                "position": controller["position"],
                "gap": f"+{random.uniform(0.1, 30.5):.3f}s" if controller["position"] > 1 else "Leader"
            }
        }
        
        try:
            self.session.post(f"{self.base_url}/api/smartrace", json=payload)
            gap_info = payload["event_data"]["gap"] if controller["position"] > 1 else "🏁"
            self.log(f"P{controller['position']} {controller['driver']}: {payload['event_data']['laptime']} ({gap_info})")
            lap_count[controller["id"]] += 1
            
            # Position changes simulation
            if random.random() < 0.1:  # 10% chance for position change
                other_controller = random.choice([c for c in controllers if c != controller])
                controller["position"], other_controller["position"] = other_controller["position"], controller["position"]
                
        except Exception as e:
            self.log(f"Error in live simulation: {str(e)}", "ERROR")
    
    # Race start event
    try:
        start_payload = {
            "time": int(time.time() * 1000),
            "event_type": "event.start",
            "event_data": {
                "event_id": "live_race_silverstone",
                "session_type": "race",
                "track": track_name,
                "weather": "dry",
                "temperature": 22,
                "participants": len(controllers)
            }
        }
        self.session.post(f"{self.base_url}/api/smartrace", json=start_payload)
        self.log(f"🏁 RACE START at {track_name}! ({len(controllers)} drivers)", "SUCCESS")
    except Exception as e:
        self.log(f"Error sending race start: {str(e)}", "ERROR")
    
    # Live race loop
    while time.time() - start_time < duration_seconds:
        # Weighted selection - leaders complete laps more frequently
        weights = [1.0 / (controller["position"] * 0.5 + 0.5) for controller in controllers]
        controller = random.choices(controllers, weights=weights, k=1)[0]
        send_lap_update(controller)
        
        # Wait between 1-6 seconds for next lap
        time.sleep(random.uniform(1, 6))
    
    # Race end event
    try:
        winner = min(controllers, key=lambda x: x["position"])
        end_payload = {
            "time": int(time.time() * 1000),
            "event_type": "event.end",
            "event_data": {
                "event_id": "live_race_silverstone",
                "session_type": "race", 
                "winner": winner["driver"],
                "total_laps": max(lap_count.values()),
                "track": track_name
            }
        }
        self.session.post(f"{self.base_url}/api/smartrace", json=end_payload)
        self.log(f"🏆 RACE END! Winner: {winner['driver']} ({winner['car']})", "SUCCESS")
    except Exception as e:
        self.log(f"Error sending race end: {str(e)}", "ERROR")
    
    self.log("Live race simulation completed", "SUCCESS")

    def generate_test_data(self, num_events=50):
        """Generiere Testdaten für verschiedene Szenarien"""
        self.log(f"Generating {num_events} test events...")
        
        controllers = [
            {"id": "1", "driver": "Max Verstappen", "car": "Red Bull RB19", "color": "#FF0000"},
            {"id": "2", "driver": "Lewis Hamilton", "car": "Mercedes W14", "color": "#00D2BE"},
            {"id": "3", "driver": "Charles Leclerc", "car": "Ferrari SF-23", "color": "#DC143C"},
            {"id": "4", "driver": "Lando Norris", "car": "McLaren MCL60", "color": "#FF8700"},
            {"id": "5", "driver": "Fernando Alonso", "car": "Aston Martin AMR23", "color": "#006F62"},
            {"id": "6", "driver": "George Russell", "car": "Mercedes W14", "color": "#00D2BE"}
        ]
        
        event_types = ["ui.lap_update", "event.start", "event.end", "ui.remove_car_from_session"]
        
        base_laptime = 75000  # 1:15.000 in ms
        
        for i in range(num_events):
            controller = random.choice(controllers)
            event_type = random.choices(event_types, weights=[85, 5, 5, 5], k=1)[0]
            
            payload = {
                "time": int((time.time() - random.randint(0, 86400)) * 1000),  # Last 24 hours
                "event_type": event_type,
                "event_data": {}
            }
            
            if event_type == "ui.lap_update":
                # Generate realistic lap times with some variation
                controller_skill = {"1": 0.95, "2": 0.97, "3": 0.96, "4": 0.98, "5": 0.99, "6": 0.97}
                skill_factor = controller_skill.get(controller["id"], 1.0)
                
                laptime_ms = int(base_laptime * skill_factor * random.uniform(0.95, 1.1))
                sector1_ms = int(laptime_ms * random.uniform(0.35, 0.45))
                sector2_ms = int(laptime_ms * random.uniform(0.25, 0.35))
                sector3_ms = laptime_ms - sector1_ms - sector2_ms
                
                def ms_to_timestring(ms):
                    minutes = ms // 60000
                    seconds = (ms % 60000) / 1000
                    return f"{minutes}:{seconds:06.3f}"
                
                payload["event_data"] = {
                    "controller_id": controller["id"],
                    "lap": random.randint(1, 50),
                    "laptime": ms_to_timestring(laptime_ms),
                    "laptime_raw": laptime_ms,
                    "sector_1": ms_to_timestring(sector1_ms),
                    "sector_1_pb": random.choice([True, False]),
                    "sector_2": ms_to_timestring(sector2_ms),
                    "sector_2_pb": random.choice([True, False]),
                    "sector_3": ms_to_timestring(sector3_ms),
                    "sector_3_pb": random.choice([True, False]),
                    "event_id": f"test_event_{random.randint(1, 10)}",
                    "driver": controller["driver"],
                    "car": controller["car"]
                }
            elif event_type == "event.start":
                payload["event_data"] = {
                    "event_id": f"test_event_{random.randint(1, 10)}",
                    "session_type": random.choice(["qualifying", "race", "practice"])
                }
            elif event_type == "ui.remove_car_from_session":
                payload["event_data"] = {
                    "controller_id": controller["id"]
                }
            
            # Send to API
            try:
                response = self.session.post(
                    f"{self.base_url}/api/smartrace",
                    json=payload,
                    headers={'Content-Type': 'application/json'}
                )
                
                if response.status_code == 200:
                    if i % 10 == 0:
                        self.log(f"Generated {i+1}/{num_events} events...")
                else:
                    self.log(f"Failed to send event {i+1}: {response.status_code}", "ERROR")
                    
            except Exception as e:
                self.log(f"Error sending event {i+1}: {str(e)}", "ERROR")
            
            # Small delay to simulate real data
            time.sleep(0.1)
        
        self.log(f"Generated {num_events} test events", "SUCCESS")
        
    def test_dashboard_api(self):
        """Teste Dashboard API Endpoints"""
        self.log("Testing Dashboard API endpoints...")
        
        endpoints = [
            "/api/dashboard",
            "/api/analytics",
            "/api/database",
            "/api/filters"
        ]
        
        for endpoint in endpoints:
            try:
                response = self.session.get(f"{self.base_url}{endpoint}")
                if response.status_code == 200:
                    data = response.json()
                    self.log(f"✓ {endpoint}: {len(data) if isinstance(data, list) else 'OK'}", "SUCCESS")
                else:
                    self.log(f"✗ {endpoint}: Status {response.status_code}", "ERROR")
            except Exception as e:
                self.log(f"✗ {endpoint}: {str(e)}", "ERROR")
                
    def test_export_functionality(self):
        """Teste Export-Funktionen"""
        self.log("Testing export functionality...")
        
        # Test CSV Export
        try:
            response = self.session.get(f"{self.base_url}/api/export/csv")
            if response.status_code == 200:
                # Save CSV to test
                with open("test_export.csv", "wb") as f:
                    f.write(response.content)
                
                # Verify CSV content
                with open("test_export.csv", "r", encoding="utf-8") as f:
                    reader = csv.reader(f)
                    rows = list(reader)
                    if len(rows) > 1:
                        self.log(f"✓ CSV Export: {len(rows)-1} rows exported", "SUCCESS")
                    else:
                        self.log("✗ CSV Export: No data", "WARNING")
                
                os.remove("test_export.csv")
            else:
                self.log(f"✗ CSV Export failed: {response.status_code}", "ERROR")
        except Exception as e:
            self.log(f"✗ CSV Export error: {str(e)}", "ERROR")
            
    def test_frontend_pages(self):
        """Teste Frontend-Seiten"""
        self.log("Testing frontend pages...")
        
        pages = [
            ("/", "Dashboard"),
            ("/analytics", "Analytics"),
            ("/database", "Database")
        ]
        
        for url, name in pages:
            try:
                response = self.session.get(f"{self.base_url}{url}")
                if response.status_code == 200 and "SmartRace" in response.text:
                    self.log(f"✓ {name} page loaded", "SUCCESS")
                else:
                    self.log(f"✗ {name} page failed", "ERROR")
            except Exception as e:
                self.log(f"✗ {name} page error: {str(e)}", "ERROR")
                
    def simulate_live_race(self, duration_seconds=30):
        """Simuliere ein Live-Rennen"""
        self.log(f"Simulating live race for {duration_seconds} seconds...")
        
        controllers = [
            {"id": "1", "driver": "Hamilton", "car": "Mercedes", "position": 1},
            {"id": "2", "driver": "Verstappen", "car": "Red Bull", "position": 2},
            {"id": "3", "driver": "Leclerc", "car": "Ferrari", "position": 3}
        ]
        
        start_time = time.time()
        lap_count = {controller["id"]: 1 for controller in controllers}
        
        def send_lap_update(controller):
            base_time = 75000 + random.randint(-5000, 5000)  # Base lap time with variation
            
            payload = {
                "time": int(time.time() * 1000),
                "event_type": "ui.lap_update",
                "event_data": {
                    "controller_id": controller["id"],
                    "lap": lap_count[controller["id"]],
                    "laptime": f"1:{(base_time/1000)%60:06.3f}",
                    "laptime_raw": base_time,
                    "sector_1": f"0:{random.uniform(25, 35):06.3f}",
                    "sector_2": f"0:{random.uniform(20, 30):06.3f}",
                    "sector_3": f"0:{random.uniform(20, 30):06.3f}",
                    "event_id": "live_race_test",
                    "driver": controller["driver"],
                    "car": controller["car"]
                }
            }
            
            try:
                self.session.post(f"{self.base_url}/api/smartrace", json=payload)
                self.log(f"Lap {lap_count[controller['id']]} - {controller['driver']}: {payload['event_data']['laptime']}")
                lap_count[controller["id"]] += 1
            except Exception as e:
                self.log(f"Error in live simulation: {str(e)}", "ERROR")
        
        while time.time() - start_time < duration_seconds:
            # Random controller completes a lap
            controller = random.choice(controllers)
            send_lap_update(controller)
            
            # Wait between 2-8 seconds for next lap
            time.sleep(random.uniform(2, 8))
        
        self.log("Live race simulation completed", "SUCCESS")
        
    def test_cors_headers(self):
        """Teste CORS Headers"""
        self.log("Testing CORS headers...")
        
        try:
            # OPTIONS request
            response = self.session.options(f"{self.base_url}/api/smartrace")
            headers = response.headers
            
            required_cors = [
                'Access-Control-Allow-Origin',
                'Access-Control-Allow-Methods',
                'Access-Control-Allow-Headers'
            ]
            
            cors_ok = all(header in headers for header in required_cors)
            
            if cors_ok:
                self.log("✓ CORS headers present", "SUCCESS")
            else:
                self.log("✗ Missing CORS headers", "ERROR")
                
        except Exception as e:
            self.log(f"✗ CORS test error: {str(e)}", "ERROR")
            
    def performance_test(self, concurrent_requests=10):
        """Teste Performance mit mehreren gleichzeitigen Anfragen"""
        self.log(f"Running performance test with {concurrent_requests} concurrent requests...")
        
        def make_request(thread_id):
            try:
                start_time = time.time()
                response = requests.get(f"{self.base_url}/api/dashboard")
                duration = time.time() - start_time
                
                if response.status_code == 200:
                    self.log(f"Thread {thread_id}: {duration:.3f}s", "SUCCESS")
                else:
                    self.log(f"Thread {thread_id}: Failed {response.status_code}", "ERROR")
                    
            except Exception as e:
                self.log(f"Thread {thread_id}: Error {str(e)}", "ERROR")
        
        threads = []
        start_time = time.time()
        
        for i in range(concurrent_requests):
            thread = threading.Thread(target=make_request, args=(i+1,))
            threads.append(thread)
            thread.start()
        
        for thread in threads:
            thread.join()
            
        total_time = time.time() - start_time
        self.log(f"Performance test completed in {total_time:.3f}s", "SUCCESS")
        
    def cleanup_test_data(self):
        """Cleanup test data (optional)"""
        self.log("Cleaning up test data...")
        try:
            # This would require a cleanup endpoint in your API
            response = self.session.delete(f"{self.base_url}/api/cleanup-test-data")
            if response.status_code == 200:
                self.log("Test data cleaned up", "SUCCESS")
        except:
            self.log("No cleanup endpoint available", "WARNING")
            
    def run_all_tests(self):
        """Führe alle Tests aus"""
        self.log("=" * 60)
        self.log("STARTING SMARTRACE DASHBOARD TEST SUITE")
        self.log("=" * 60)
        
        tests = [
            ("Connection Test", self.test_connection),
            ("SmartRace Endpoint", self.test_smartrace_endpoint),
            ("Generate Test Data", lambda: self.generate_test_data(30)),
            ("Dashboard APIs", self.test_dashboard_api),
            ("Export Functions", self.test_export_functionality),
            ("Frontend Pages", self.test_frontend_pages),
            ("CORS Headers", self.test_cors_headers),
            ("Performance Test", lambda: self.performance_test(5)),
            ("Live Race Simulation", lambda: self.simulate_live_race(15))
        ]
        
        passed = 0
        failed = 0
        
        for test_name, test_func in tests:
            self.log(f"\n🧪 Running: {test_name}")
            self.log("-" * 40)
            
            try:
                result = test_func()
                if result is not False:
                    passed += 1
                else:
                    failed += 1
            except Exception as e:
                self.log(f"Test failed with exception: {str(e)}", "ERROR")
                failed += 1
                
            time.sleep(1)  # Brief pause between tests
        
        self.log("\n" + "=" * 60)
        self.log("TEST SUMMARY")
        self.log("=" * 60)
        self.log(f"✅ Passed: {passed}")
        self.log(f"❌ Failed: {failed}")
        self.log(f"📊 Success Rate: {(passed/(passed+failed)*100):.1f}%")
        
        if failed == 0:
            self.log("\n🎉 ALL TESTS PASSED! Your SmartRace Dashboard is working perfectly!", "SUCCESS")
        else:
            self.log(f"\n⚠️ {failed} tests failed. Check the logs above.", "WARNING")

def main():
    import argparse
    
    parser = argparse.ArgumentParser(description="SmartRace Dashboard Test Suite")
    parser.add_argument("--url", default="http://localhost:5000", help="Base URL of the dashboard")
    parser.add_argument("--quick", action="store_true", help="Run quick tests only")
    parser.add_argument("--generate-data", type=int, default=0, help="Only generate test data (specify number)")
    parser.add_argument("--live-race", type=int, default=0, help="Only simulate live race (specify duration in seconds)")
    
    args = parser.parse_args()
    
    test_suite = SmartRaceTestSuite(args.url)
    
    if args.generate_data:
        test_suite.generate_test_data(args.generate_data)
    elif args.live_race:
        test_suite.simulate_live_race(args.live_race)
    elif args.quick:
        test_suite.test_connection()
        test_suite.test_smartrace_endpoint()
        test_suite.test_dashboard_api()
        test_suite.test_frontend_pages()
    else:
        test_suite.run_all_tests()

if __name__ == "__main__":
    main()
