# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

SmartRace LiveDashboard - a real-time Flask dashboard for Carrera Digital slot car racing. It receives webhook POST data from the SmartRace app, stores it in PostgreSQL, and displays live race data via WebSocket-powered pages.

## Build & Run

```bash
# Production (Docker Compose)
docker compose up --build -d

# Local development (requires PostgreSQL or uses SQLite fallback)
pip install -r requirements.txt
python app.py

# Environment variables needed (see .env.example)
POSTGRES_DB, POSTGRES_USER, POSTGRES_PASSWORD, SECRET_KEY, FLASK_DEBUG
```

There are no tests, linters, or CI/CD pipelines configured.

## Architecture

**Single-file backend**: `app.py` (~1800 lines) contains everything - models, routes, helpers, WebSocket handlers. Uses Gunicorn with eventlet worker for WebSocket support in production.

**Data flow**: SmartRace app → `POST /api/smartrace` → parse event → store in DB → emit WebSocket → frontend updates

**Database models** (all prefixed `sr_`): Event, Lap, RaceResult, Penalty, RaceStatus, TrackRecord, PersonalRecord, SessionName

**Frontend**: Jinja2 templates extending `base.html`, Bootstrap 5 dark theme, Chart.js for charts, Socket.IO for real-time updates. All custom CSS in `static/css/style.css`. No build step, no npm.

**Key shared code in `base.html`**: WebSocket connection management, `formatTime()`, `getControllerColor()`, `showToast()`, race status badge updates, fullscreen toggle (F11). All page-specific JS is in `{% block scripts %}`.

## SmartRace Event Types (verified from real data)

The SmartRace Datenschnittstelle sends POST requests with `{time, event_type, event_data}`:

- `ui.lap_update` - lap data with nested `driver_data`, `car_data`, `controller_data`
- `event.change_status` - status changes with `{new, old}` fields (NOT `status`/`new_status`)
- `event.end` - race end with `{type: "qualifying"|"race", result: {...}}`
- `ui.reset`, `ui.penalty`, `ui.sector_for_car`, `cu.send_start_command`

**Important**: The docs suggest `ui.status_change` and `ui.race_result` but these do NOT exist in real data. Colors arrive as `rgb(r,g,b)` strings requiring conversion to hex.

## Deployment

Docker Compose with two services: `web` (Flask/Gunicorn on port 5000) and `db` (PostgreSQL 16). Health check at `/api/health`. Designed for Portainer with environment variables passed via stack config.
