"""Hilfsfunktionen: Formatierung, Sektor-Parsing, Session-Ermittlung, aktive Strecke."""
import math
import re
from datetime import datetime

from .extensions import db, log
from .models import Event, Lap, RaceStatus, SessionName, Track

# =============================================================================
# Aktive Strecke (wird von util.set_active_track gesetzt)
# =============================================================================

_active_track_name = None


def set_active_track_name(name):
    """Aktive Strecke setzen (aus util.set_active_track)."""
    global _active_track_name
    _active_track_name = name


def _get_active_track_name():
    """Aktive Strecke ermitteln — Modul-Variable oder zuletzt benutzte aus DB."""
    global _active_track_name
    if _active_track_name:
        return _active_track_name
    try:
        track = Track.query.order_by(Track.last_used.desc()).first()
        if track:
            _active_track_name = track.name
            return track.name
    except Exception:
        pass
    return None


# =============================================================================
# Formatierung / Parsing
# =============================================================================

def rgb_to_hex(val):
    """'rgb(r,g,b)' -> '#rrggbb', gibt None bei ungueltigem Input."""
    if not val:
        return None
    if val.startswith('#'):
        return val
    nums = re.findall(r'\d+', val)
    if len(nums) >= 3:
        return f"#{int(nums[0]):02x}{int(nums[1]):02x}{int(nums[2]):02x}"
    return None


def fmt_ms(ms):
    """Millisekunden -> 'M:SS.mmm'"""
    if not ms or ms <= 0:
        return None
    s = ms / 1000
    return f"{int(s // 60)}:{s % 60:06.3f}"


def _parse_sector_ms(s):
    """Sektor-String 'M:SS.mmm' -> Millisekunden (int), None bei Fehler."""
    if not s or not isinstance(s, str):
        return None
    try:
        if ':' in s:
            parts = s.split(':')
            mins = int(parts[0])
            secs = float(parts[1])
            return int((mins * 60 + secs) * 1000)
        return int(float(s) * 1000)
    except (ValueError, IndexError):
        return None


def _calc_stddev(times):
    """Standardabweichung einer Liste von Zeiten berechnen."""
    if len(times) < 2:
        return 0
    avg = sum(times) / len(times)
    variance = sum((t - avg) ** 2 for t in times) / len(times)
    return math.sqrt(variance)


# =============================================================================
# Session-Ermittlung
# =============================================================================

def _session_display_name(session_id, session_type=None):
    """Lesbaren Session-Namen generieren oder gespeicherten zurueckgeben."""
    sn = SessionName.query.filter_by(session_id=session_id).first()
    if sn:
        return sn.display_name
    # Typ ermitteln: uebergebener Wert, oder aus RaceStatus/Laps der Session
    type_label = session_type
    if not type_label:
        rs = RaceStatus.query.filter_by(session_id=session_id).order_by(
            RaceStatus.id.desc()).first()
        if rs and rs.race_type:
            type_label = rs.race_type
    if not type_label:
        lap = Lap.query.filter(
            Lap.session_id == session_id,
            Lap.session_type.isnot(None),
            Lap.session_type != '',
            Lap.session_type != 'Training',
        ).first()
        if lap:
            type_label = lap.session_type

    type_map = {
        'race': 'Rennen',
        'qualifying': 'Qualifying',
        'training': 'Training',
        'practice': 'Training',
        'free_practice': 'Freies Training',
    }
    raw_label = type_label or 'Training'
    type_label = type_map.get(raw_label.lower().strip(), raw_label)

    # Datum aus session_id Format "session_YYYYMMDD_HHMMSS"
    try:
        parts = session_id.replace('session_', '')
        dt = datetime.strptime(parts, '%Y%m%d_%H%M%S')
        date_str = dt.strftime('%d.%m.%Y %H:%M')
        return f"{type_label} — {date_str}"
    except Exception:
        pass

    # Fuer event_XXX IDs: Datum aus erstem Event der Session
    try:
        first_event = Event.query.filter_by(session_id=session_id).order_by(
            Event.created_at.asc()).first()
        if first_event and first_event.created_at:
            date_str = first_event.created_at.strftime('%d.%m.%Y %H:%M')
            return f"{type_label} — {date_str}"
    except Exception:
        pass

    return f"{type_label} — {session_id}"


# Events die eine NEUE Session starten (neues Rennen / Training)
_NEW_SESSION_EVENTS = {'ui.reset', 'event.start'}

_SESSION_TIMEOUT_SECONDS = 300  # 5 Minuten ohne Events = neue Session


def _get_session_id(etype, data=None):
    """Session-ID ermitteln.

    Primaer: event_id aus SmartRace-Daten (wird bei event.start generiert
    und an alle Folgeereignisse weitergereicht).
    Fallback: Timestamp-basierte Session-ID.

    Neue Session bei:
    - ui.reset / event.start (explizit)
    - Neue event_id (SmartRace hat neues Event gestartet)
    - Timeout: > 5 Minuten seit letztem Event
    """
    event_id = None
    if data:
        event_id = data.get('event_id') or (data.get('event_data') or {}).get('event_id')

    # Bei session-startenden Events: immer neue Session
    if etype in _NEW_SESSION_EVENTS:
        if event_id:
            return f"event_{event_id}"
        return f"session_{datetime.now().strftime('%Y%m%d_%H%M%S')}"

    # event_id vorhanden: SmartRace ordnet das Event einer Session zu
    if event_id:
        return f"event_{event_id}"

    # Kein expliziter Trigger und keine event_id:
    # Pruefen ob die letzte Session noch aktiv ist (Timeout)
    latest = db.session.query(Event.session_id, Event.created_at).order_by(
        Event.id.desc()).first()
    if latest and latest.session_id and latest.created_at:
        age = (datetime.utcnow() - latest.created_at).total_seconds()
        if age < _SESSION_TIMEOUT_SECONDS:
            return latest.session_id
        # Timeout: neue Session starten
        log.info(f"Session-Timeout ({age:.0f}s) — neue Session")
        return f"session_{datetime.now().strftime('%Y%m%d_%H%M%S')}"

    return f"session_{datetime.now().strftime('%Y%m%d_%H%M%S')}"


def _get_current_race_type(sid=None):
    """Aktuellen Renntyp fuer eine Session ermitteln."""
    if sid:
        rs = RaceStatus.query.filter_by(session_id=sid).order_by(
            RaceStatus.id.desc()).first()
        if rs and rs.race_type:
            return rs.race_type
        return 'Training'
    return 'Training'
