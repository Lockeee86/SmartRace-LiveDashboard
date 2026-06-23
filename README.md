# SmartRace LiveDashboard

**[Deutsch](README.de.md)** | English

A real-time dashboard for [Carrera Digital](https://www.carrera-toys.com/) slot car racing. Receives live data from the [SmartRace data interface](https://www.smartrace.de/en/the-smartrace-manual/data-interface/), stores all laps, results and records in a PostgreSQL database, and displays everything in real time in the browser.

![Live Leaderboard](https://raw.githubusercontent.com/Lockeee86/SmartRace-LiveDashboard/main/docs/leaderboard.png)

## Features

**Live Views**
- **Leaderboard** — Sortable by laps (race) or best time (practice), with sector analysis, sparkline trends and fuel level
- **Dashboard** — Controller cards with track position, track record and live ticker
- **TV Mode** — Fullscreen spectator view with auto-rotating panels (leaderboard, track map, statistics)
- **Live Ticker** — Horizontal ticker with adjustable size (S/M/L) for readability on race day screens

**Analysis & Statistics**
- **Race Analytics** — Lap time progression, driver radar chart, car comparison
- **Live Timing** — Sector and split times
- **Head-to-Head** — Direct driver comparison
- **Driver Statistics** — Personal records, consistency score
- **Tracks** — Track records, SVG layouts with live positions

**Database**
- All laps, sectors, penalties and results are stored persistently in PostgreSQL
- Session management with automatic detection (practice / race / qualifying)
- Track and personal records with management UI (delete invalid times)
- Database browser with filtering, sorting and export (CSV/JSON)
- Backup & restore

**Race Features**
- Automatic status detection (practice, race, qualifying)
- Countdown timer for timed races
- Virtual Safety Car (VSC) banner
- Penalty tracking with live display
- Fuel level indicator
- Overtake detection

## Requirements

- Docker & Docker Compose
- [SmartRace App](https://www.smartrace.de/) with data interface enabled
- Both devices on the same network

## Installation

### 1. Clone the repository

```bash
git clone https://github.com/Lockeee86/SmartRace-LiveDashboard.git
cd SmartRace-LiveDashboard
```

### 2. Configure environment variables

```bash
cp .env.example .env
```

Edit the `.env` file:

```env
POSTGRES_DB=smartrace
POSTGRES_USER=smartrace
POSTGRES_PASSWORD=a_secure_password
SECRET_KEY=a_random_secret_key
FLASK_DEBUG=false
```

### 3. Start

```bash
docker compose up -d
```

The dashboard will be available at `http://<SERVER-IP>:5000`.

### 4. Connect SmartRace

In the SmartRace app, go to **Settings > Data Interface** and enter this URL:

```
http://<SERVER-IP>:5000/api/smartrace
```

All race data will now be sent to the dashboard automatically.

## Architecture

```
SmartRace App  ──POST──>  Flask Backend  ──WebSocket──>  Browser
                            |
                            v
                        PostgreSQL
                     (all laps, results,
                      records, penalties)
```

| Component | Technology |
|---|---|
| Backend | Flask 3.0 + Gunicorn + eventlet |
| Database | PostgreSQL 16 |
| Real-time | Flask-SocketIO (WebSocket) |
| Frontend | Bootstrap 5.3, Chart.js 4.4, Socket.IO |
| Deployment | Docker Compose |

## Screenshots

### Race Analytics
![Analytics](https://raw.githubusercontent.com/Lockeee86/SmartRace-LiveDashboard/main/docs/analytics.png)

### Database
![Database](https://raw.githubusercontent.com/Lockeee86/SmartRace-LiveDashboard/main/docs/database.png)

## API

| Endpoint | Description |
|---|---|
| `POST /api/smartrace` | Webhook for SmartRace data |
| `GET /api/health` | Health check |
| `GET /api/live-data` | Current live data grouped by controller |
| `GET /api/race-status` | Current race status |
| `GET /api/laps` | Laps with filtering & pagination |
| `GET /api/track-record` | Current track record |
| `GET /api/analytics` | Analytics data |

## Portainer

This project is optimized for deployment with [Portainer](https://www.portainer.io/). Simply deploy as a stack and set the environment variables in the Portainer UI.

## License

MIT
