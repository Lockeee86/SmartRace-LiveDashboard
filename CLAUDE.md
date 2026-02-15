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

## SmartRace Event Types (verified from official API docs)

The SmartRace Datenschnittstelle sends POST requests with `{time, event_type, event_data}`. Some events include an `event_id` field for session association.

**Race lifecycle:**
- `event.start` - race start: `{type: "race", laps: "50"}` or `{type: "race", duration: "600"}`
- `event.change_status` - status changes: `{new, old}` - valid statuses: `prepare_for_start`, `starting`, `jumpstart`, `running`, `suspended`, `restarting`, `ended`
- `event.end` - race end: `{type: "race", result: {1: {driver_id, car_id, controller_id, laps, best_laptime, pitstops, gap, disqualified, retired}, ...}}`

**Lap & car data:**
- `ui.lap_update` - lap data with nested `driver_data`, `car_data`, `controller_data`
- `ui.remove_car_from_session` - car removed manually (contains only `controller_id`)
- `ui.reset` - UI reset, triggers new session (no data)

**Race events:**
- `race.penalty_update` - penalty: `{controller_id, penalty: 10}` (seconds, 0 = served). Enriched with `driver_data`, `car_data`, `controller_data`
- `race.vsc_deployed` / `race.vsc_retracted` - Virtual Safety Car (no data)

**Utility:**
- `util.fuel_update` - fuel level: `{controller_id, fuel: 50}` (-1 = disabled). Enriched with `driver_data`, `car_data`, `controller_data`
- `util.set_active_track` - track info: `{name, length, pitstop_delta, ...}`
- `cu.send_esc_command` - ESC command sent to CU (no data)

**Important**: `event.end` result contains `driver_id`/`car_id` but NOT `driver_name` - names are resolved from lap data. Colors arrive as `rgb(r,g,b)` strings requiring conversion to hex.

## Deployment

Docker Compose with two services: `web` (Flask/Gunicorn on port 5000) and `db` (PostgreSQL 16). Health check at `/api/health`. Designed for Portainer with environment variables passed via stack config.
