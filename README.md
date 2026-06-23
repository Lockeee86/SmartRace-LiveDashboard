# SmartRace LiveDashboard

Ein Echtzeit-Dashboard fuer [Carrera Digital](https://www.carrera-toys.com/) Slotcar-Rennen. Empfaengt Live-Daten ueber die [SmartRace-Datenschnittstelle](https://www.smartrace.de/anleitung/datenschnittstelle/), speichert alle Runden, Ergebnisse und Rekorde in einer PostgreSQL-Datenbank und zeigt alles in Echtzeit im Browser an.

![Live Leaderboard](https://raw.githubusercontent.com/Lockeee86/SmartRace-LiveDashboard/main/docs/leaderboard.png)

## Features

**Live-Ansichten**
- **Leaderboard** — Sortierbar nach Runden (Rennen) oder Bestzeit (Training), mit Sektoranalyse, Sparkline-Trends und Tankfuellstand
- **Dashboard** — Controller-Karten mit Streckenposition, Streckenrekord und Live-Ticker
- **TV-Modus** — Vollbild-Ansicht fuer den Renn-Monitor mit automatisch rotierenden Views (Leaderboard, Streckenmap, Statistiken)
- **Live-Ticker** — Horizontaler Ticker mit einstellbarer Groesse (S/M/L) fuer gute Lesbarkeit auf Racing-Bildschirmen

**Analyse & Statistiken**
- **Race Analytics** — Rundenzeiten-Verlauf, Fahrer-Radar, Auto-Vergleich
- **Live Timing** — Sektor- und Zwischenzeiten
- **Head-to-Head** — Direkter Fahrer-Vergleich
- **Fahrer-Statistiken** — Persoenliche Rekorde, Konsistenz-Score
- **Strecken** — Streckenrekorde, SVG-Layouts mit Live-Positionen

**Datenbank**
- Alle Runden, Sektoren, Strafen und Ergebnisse werden dauerhaft in PostgreSQL gespeichert
- Session-Verwaltung mit automatischer Erkennung (Training / Rennen / Qualifying)
- Strecken- und persoenliche Rekorde mit Verwaltungsoberflaeche (ungueltige Zeiten loeschen)
- Datenbank-Browser mit Filter, Sortierung und Export (CSV/JSON)
- Backup & Restore

**Renn-Features**
- Automatische Status-Erkennung (Training, Rennen, Qualifying)
- Countdown-Timer bei Zeitrennen
- Virtual Safety Car (VSC) Banner
- Strafen-Tracking mit Live-Anzeige
- Tankfuellstand-Anzeige
- Ueberholungs-Erkennung

## Voraussetzungen

- Docker & Docker Compose
- [SmartRace App](https://www.smartrace.de/) mit aktivierter Datenschnittstelle
- Beide Geraete im selben Netzwerk

## Installation

### 1. Repository klonen

```bash
git clone https://github.com/Lockeee86/SmartRace-LiveDashboard.git
cd SmartRace-LiveDashboard
```

### 2. Umgebungsvariablen konfigurieren

```bash
cp .env.example .env
```

Die `.env` Datei anpassen:

```env
POSTGRES_DB=smartrace
POSTGRES_USER=smartrace
POSTGRES_PASSWORD=ein_sicheres_passwort
SECRET_KEY=ein_zufaelliger_schluessel
FLASK_DEBUG=false
```

### 3. Starten

```bash
docker compose up -d
```

Das Dashboard ist dann erreichbar unter `http://<SERVER-IP>:5000`.

### 4. SmartRace verbinden

In der SmartRace App unter **Einstellungen > Datenschnittstelle** diese URL eintragen:

```
http://<SERVER-IP>:5000/api/smartrace
```

Ab sofort werden alle Renn-Daten automatisch an das Dashboard gesendet.

## Architektur

```
SmartRace App  ──POST──>  Flask Backend  ──WebSocket──>  Browser
                            │
                            ▼
                        PostgreSQL
                     (alle Runden, Ergebnisse,
                      Rekorde, Strafen)
```

| Komponente | Technologie |
|---|---|
| Backend | Flask 3.0 + Gunicorn + eventlet |
| Datenbank | PostgreSQL 16 |
| Echtzeit | Flask-SocketIO (WebSocket) |
| Frontend | Bootstrap 5.3, Chart.js 4.4, Socket.IO |
| Deployment | Docker Compose |

## Screenshots

### Race Analytics
![Analytics](https://raw.githubusercontent.com/Lockeee86/SmartRace-LiveDashboard/main/docs/analytics.png)

### Datenbank
![Datenbank](https://raw.githubusercontent.com/Lockeee86/SmartRace-LiveDashboard/main/docs/database.png)

## API

| Endpunkt | Beschreibung |
|---|---|
| `POST /api/smartrace` | Webhook fuer SmartRace-Daten |
| `GET /api/health` | Health-Check |
| `GET /api/live-data` | Aktuelle Live-Daten nach Controller |
| `GET /api/race-status` | Aktueller Rennstatus |
| `GET /api/laps` | Runden mit Filter & Pagination |
| `GET /api/track-record` | Aktueller Streckenrekord |
| `GET /api/analytics` | Analyse-Daten |

## Portainer

Das Projekt ist fuer den Betrieb mit [Portainer](https://www.portainer.io/) optimiert. Einfach als Stack deployen und die Umgebungsvariablen in der Portainer-Oberflaeche setzen.

## Lizenz

MIT
