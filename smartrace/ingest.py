"""SmartRace Datenschnittstelle: Empfang der Webhook-Events und Speicher-Handler."""
import json
from datetime import datetime

from flask import Blueprint, jsonify, request

from .extensions import db, log, socketio
from .models import (
    Event, Lap, Penalty, PersonalRecord, RaceResult, RaceStatus,
    Track, TrackRecord,
)
from .utils import (
    _get_active_track_name, _get_current_race_type, _get_session_id,
    fmt_ms, rgb_to_hex, set_active_track_name,
)

ingest_bp = Blueprint('ingest', __name__)


@ingest_bp.route('/api/smartrace', methods=['POST', 'OPTIONS'])
def receive_smartrace():
    """Empfaengt Events von SmartRace (POST mit JSON).

    Format: { "time": ..., "event_type": "ui.lap_update", "event_data": { ... } }
    """
    if request.method == 'OPTIONS':
        return '', 200

    data = request.get_json(silent=True)
    if not data:
        return jsonify({'error': 'Kein JSON'}), 400

    try:
        etype = data.get('event_type', 'unknown')
        ed = data.get('event_data') or {}

        sid = _get_session_id(etype, data)

        db.session.add(Event(
            session_id=sid, event_type=etype, raw_json=json.dumps(data),
        ))

        # Race-Type aus JEDEM Event extrahieren wenn vorhanden
        _extract_race_type(sid, ed)

        if etype == 'ui.lap_update':
            _save_lap(sid, ed)
        elif etype == 'event.start':
            _save_start(sid, ed)
        elif etype == 'event.end':
            _save_result(sid, ed)
        elif etype == 'event.change_status':
            _save_status(sid, ed)
        elif etype == 'race.penalty_update':
            _save_penalty(sid, ed)
        elif etype == 'ui.remove_car_from_session':
            _save_car_removed(sid, ed)
        elif etype == 'util.fuel_update':
            _save_fuel_update(sid, ed)
        elif etype == 'util.set_active_track':
            _save_active_track(sid, ed)

        db.session.commit()
        log.info(f"Event: {etype} | Session: {sid}")

        # WebSocket: alle verbundenen Clients benachrichtigen
        ws_payload = {
            'event_type': etype,
            'session_id': sid,
        }

        if etype == 'ui.lap_update':
            ws_payload['lap'] = {
                'controller_id': str(ed.get('controller_id', '0')),
                'driver_name': (ed.get('driver_data') or {}).get('name', ''),
                'car_name': (ed.get('car_data') or {}).get('name', ''),
                'lap_number': ed.get('lap'),
                'laptime_ms': ed.get('laptime_raw'),
                'laptime_formatted': ed.get('laptime') or fmt_ms(ed.get('laptime_raw')),
                'sector_1': ed.get('sector_1'),
                'sector_2': ed.get('sector_2'),
                'sector_3': ed.get('sector_3'),
                'is_pb': bool(ed.get('lap_pb', False)),
                'color': rgb_to_hex((ed.get('controller_data') or {}).get('color_bg'))
                         or rgb_to_hex((ed.get('car_data') or {}).get('color')),
                'timestamp': datetime.utcnow().isoformat(),
            }

        socketio.emit('new_data', ws_payload)

        if etype == 'event.start':
            race_type_start = ed.get('type') or ''
            laps_total = ed.get('laps') or ''
            duration_total = ed.get('duration') or ''
            socketio.emit('race_status', {
                'session_id': sid,
                'status': 'starting',
                'race_type': race_type_start,
                'laps': laps_total,
                'duration': duration_total,
            })

        if etype == 'event.change_status':
            race_type_ws = _get_current_race_type(sid)
            socketio.emit('race_status', {
                'session_id': sid,
                'status': ed.get('new') or ed.get('status') or 'unknown',
                'race_type': race_type_ws,
            })

        if etype == 'event.end':
            socketio.emit('race_status', {
                'session_id': sid,
                'status': 'ended',
                'race_type': _get_current_race_type(sid),
            })

        if etype == 'race.penalty_update':
            penalty_secs = ed.get('penalty', 0)
            driver_name_p = (ed.get('driver_data') or {}).get('name', '')
            socketio.emit('penalty', {
                'session_id': sid,
                'controller_id': str(ed.get('controller_id', '')),
                'driver_name': driver_name_p,
                'penalty_seconds': penalty_secs,
            })

        if etype == 'ui.remove_car_from_session':
            socketio.emit('car_removed', {
                'session_id': sid,
                'controller_id': str(ed.get('controller_id', '')),
            })

        if etype in ('race.vsc_deployed', 'race.vsc_retracted'):
            vsc_active = etype == 'race.vsc_deployed'
            socketio.emit('vsc', {
                'session_id': sid,
                'active': vsc_active,
            })

        if etype == 'util.fuel_update':
            socketio.emit('fuel_update', {
                'session_id': sid,
                'controller_id': str(ed.get('controller_id', '')),
                'fuel': ed.get('fuel', -1),
                'driver_name': (ed.get('driver_data') or {}).get('name', ''),
            })

        if etype == 'util.set_active_track':
            socketio.emit('active_track', {
                'name': ed.get('name', ''),
                'length': ed.get('length', ''),
                'pitstop_delta': ed.get('pitstop_delta', ''),
            })

        return jsonify({'status': 'ok', 'event_type': etype})

    except Exception as e:
        db.session.rollback()
        log.error(f"SmartRace-Fehler: {e}")
        return jsonify({'error': str(e)}), 500


def _extract_race_type(sid, ed):
    """Race-Type aus beliebigem Event extrahieren und speichern.

    SmartRace kann den Typ in verschiedenen Feldern senden.
    Wir durchsuchen alle bekannten Felder in event_data.
    """
    # Alle moeglichen Feldnamen fuer den Race-Type durchprobieren
    race_type = ''
    for field in ('type', 'race_type', 'mode', 'session_type', 'event_type_name'):
        val = ed.get(field, '')
        if val and isinstance(val, str) and val.lower() in (
            'race', 'qualifying', 'practice', 'free_practice',
            'rennen', 'qualifikation', 'training',
        ):
            race_type = val
            break

    if not race_type:
        return

    # In RaceStatus speichern/aktualisieren (nur race_type, status wird
    # von den spezifischen Handlern gesetzt)
    rs = RaceStatus.query.filter_by(session_id=sid).order_by(
        RaceStatus.id.desc()).first()
    if rs:
        if not rs.race_type or rs.race_type == 'Training':
            rs.race_type = race_type
    # Laps dieser Session aktualisieren
    Lap.query.filter(
        Lap.session_id == sid,
        db.or_(Lap.session_type.is_(None), Lap.session_type == '',
               Lap.session_type == 'Training'),
    ).update({'session_type': race_type}, synchronize_session=False)

    log.info(f"Race-Type erkannt: {race_type} fuer Session {sid}")


def _save_start(sid, ed):
    """event.start verarbeiten.

    SmartRace sendet:
      Rundenrennen: {"type": "race", "laps": "50"}
      Zeitrennen:   {"type": "race", "duration": "600"}
      Qualifying:   {"type": "qualifying", "laps": "10"}
    """
    race_type = (ed.get('type') or '').strip()
    if not race_type:
        race_type = 'race'

    rs = RaceStatus.query.filter_by(session_id=sid).order_by(
        RaceStatus.id.desc()).first()
    if rs:
        rs.status = 'starting'
        rs.race_type = race_type
        rs.updated_at = datetime.utcnow()
    else:
        db.session.add(RaceStatus(
            session_id=sid, status='starting', race_type=race_type,
        ))

    # Bestehende Laps dieser Session aktualisieren (falls vorhanden)
    Lap.query.filter(
        Lap.session_id == sid,
        db.or_(Lap.session_type.is_(None), Lap.session_type == '',
               Lap.session_type == 'Training'),
    ).update({'session_type': race_type}, synchronize_session=False)

    log.info(f"event.start: type={race_type}, laps={ed.get('laps')}, "
             f"duration={ed.get('duration')}, session={sid}")


def _save_lap(sid, ed):
    cid = str(ed.get('controller_id', '0'))
    dd = ed.get('driver_data') or {}
    cd = ed.get('car_data') or {}
    ctrl = ed.get('controller_data') or {}

    laptime_ms = ed.get('laptime_raw')
    driver_name = dd.get('name') or f"Fahrer {cid}"
    car_name = cd.get('name') or f"Auto {cid}"

    # Wenn kein RaceStatus fuer diese Session existiert, Training annehmen
    rs = RaceStatus.query.filter_by(session_id=sid).first()
    if not rs:
        db.session.add(RaceStatus(
            session_id=sid, status='running', race_type='Training',
        ))
        socketio.emit('race_status', {
            'session_id': sid,
            'status': 'running',
            'race_type': 'Training',
        })
    elif rs.race_type and rs.race_type != 'Training':
        had_start = Event.query.filter_by(
            session_id=sid, event_type='event.start',
        ).first()
        if not had_start:
            rs.race_type = 'Training'
            if rs.status != 'running':
                rs.status = 'running'
            socketio.emit('race_status', {
                'session_id': sid,
                'status': rs.status,
                'race_type': 'Training',
            })
            log.info(f"Self-heal: session {sid} corrected to Training (no event.start found)")

    db.session.add(Lap(
        session_id=sid,
        session_type=_get_current_race_type(sid),
        controller_id=cid,
        driver_name=driver_name,
        car_name=car_name,
        lap_number=ed.get('lap'),
        laptime_ms=laptime_ms,
        laptime_display=ed.get('laptime'),
        sector_1=ed.get('sector_1'),
        sector_2=ed.get('sector_2'),
        sector_3=ed.get('sector_3'),
        car_color=rgb_to_hex(cd.get('color')),
        controller_color=rgb_to_hex(ctrl.get('color_bg')),
        is_personal_best=bool(ed.get('lap_pb', False)),
        track_name=_get_active_track_name(),
    ))

    # Streckenrekord pruefen (Minimum 1000ms = 1s als Plausibilitaetsfilter)
    if laptime_ms and laptime_ms > 1000:
        track_name = _get_active_track_name()

        rec_q = TrackRecord.query
        if track_name:
            rec_q = rec_q.filter_by(track_name=track_name)
        current_record = rec_q.order_by(TrackRecord.laptime_ms.asc()).first()
        if not current_record or laptime_ms < current_record.laptime_ms:
            db.session.add(TrackRecord(
                laptime_ms=laptime_ms,
                driver_name=driver_name,
                car_name=car_name,
                session_id=sid,
                controller_id=cid,
                track_name=track_name,
            ))
            socketio.emit('track_record', {
                'laptime_ms': laptime_ms,
                'laptime_formatted': fmt_ms(laptime_ms),
                'driver_name': driver_name,
                'car_name': car_name,
                'track_name': track_name,
            })

        # Persoenlicher Rekord pruefen (pro Strecke)
        pr_q = PersonalRecord.query.filter_by(driver_name=driver_name)
        if track_name:
            pr_q = pr_q.filter_by(track_name=track_name)
        pr = pr_q.first()
        if not pr:
            db.session.add(PersonalRecord(
                driver_name=driver_name, laptime_ms=laptime_ms,
                car_name=car_name, session_id=sid, controller_id=cid,
                track_name=track_name,
            ))
        elif laptime_ms < pr.laptime_ms:
            pr.laptime_ms = laptime_ms
            pr.car_name = car_name
            pr.session_id = sid
            pr.controller_id = cid
            pr.updated_at = datetime.utcnow()


def _save_result(sid, ed):
    # Race-Type aus dem Result-Event extrahieren und in RaceStatus speichern
    event_race_type = (ed.get('type') or ed.get('race_type') or '').strip()

    rs = RaceStatus.query.filter_by(session_id=sid).order_by(
        RaceStatus.id.desc()).first()
    if rs:
        rs.status = 'ended'
        if event_race_type:
            rs.race_type = event_race_type
        rs.updated_at = datetime.utcnow()
    else:
        db.session.add(RaceStatus(
            session_id=sid, status='ended',
            race_type=event_race_type or _get_current_race_type(sid),
        ))

    # Laps dieser Session aktualisieren wenn Typ bekannt
    final_type = event_race_type or (rs.race_type if rs else '')
    if final_type:
        Lap.query.filter_by(session_id=sid).update(
            {'session_type': final_type}, synchronize_session=False)

    # Driver-Namen aus Laps aufloesen (event.end liefert nur driver_id,
    # nicht driver_name)
    driver_names = {}
    for lap in Lap.query.filter_by(session_id=sid).all():
        cid = str(lap.controller_id)
        if cid not in driver_names and lap.driver_name:
            driver_names[cid] = lap.driver_name

    for pos, r in (ed.get('result') or {}).items():
        cid = str(r.get('controller_id', ''))
        # Fahrername: aus Laps der Session aufloesen (primaer),
        # Fallback auf driver_name falls doch vorhanden
        driver_name = (driver_names.get(cid)
                       or r.get('driver_name')
                       or f"Fahrer {cid}")
        db.session.add(RaceResult(
            session_id=sid,
            position=int(pos),
            controller_id=cid,
            driver_name=driver_name,
            total_laps=r.get('laps', 0),
            best_laptime_ms=r.get('best_laptime'),
            gap=r.get('gap', ''),
            pitstops=r.get('pitstops', 0),
            disqualified=bool(r.get('disqualified', False)),
            retired=bool(r.get('retired', False)),
        ))


def _save_status(sid, ed):
    # SmartRace sendet: {"new": "running", "old": "starting"}
    status = (ed.get('new') or ed.get('status') or ed.get('new_status')
              or ed.get('state') or 'unknown')
    event_race_type = (ed.get('type') or ed.get('race_type')
                       or ed.get('mode') or ed.get('session_type') or '')

    rs = RaceStatus.query.filter_by(session_id=sid).order_by(
        RaceStatus.id.desc()).first()
    if rs:
        rs.status = status
        if event_race_type:
            rs.race_type = event_race_type
        rs.updated_at = datetime.utcnow()
    else:
        had_start = Event.query.filter_by(
            session_id=sid, event_type='event.start',
        ).first()
        race_type = event_race_type or ('Training' if not had_start else _get_current_race_type(sid))
        db.session.add(RaceStatus(
            session_id=sid, status=status, race_type=race_type,
        ))


def _save_penalty(sid, ed):
    """race.penalty_update verarbeiten.

    SmartRace sendet: {"controller_id": "2", "penalty": 10, driver_data: {...}}
    penalty=0 bedeutet: Strafe abgesessen.
    """
    cid = str(ed.get('controller_id', '0'))
    dd = ed.get('driver_data') or {}
    penalty_secs = ed.get('penalty', 0)
    try:
        penalty_secs = int(penalty_secs)
    except (ValueError, TypeError):
        penalty_secs = 0

    if penalty_secs == 0:
        penalty_type = 'Strafe abgesessen'
    else:
        penalty_type = f'{penalty_secs}s Strafe'

    db.session.add(Penalty(
        session_id=sid,
        controller_id=cid,
        driver_name=dd.get('name') or f"Fahrer {cid}",
        penalty_type=penalty_type,
        penalty_seconds=penalty_secs,
    ))


def _save_car_removed(sid, ed):
    """ui.remove_car_from_session - Fahrzeug aus Session entfernt."""
    cid = str(ed.get('controller_id', '0'))
    # Fahrername aus Laps aufloesen (Event hat keine driver_data)
    lap = Lap.query.filter_by(
        session_id=sid, controller_id=cid
    ).order_by(Lap.id.desc()).first()
    driver_name = lap.driver_name if lap else f"Fahrer {cid}"

    db.session.add(RaceResult(
        session_id=sid,
        position=0,
        controller_id=cid,
        driver_name=driver_name,
        total_laps=0,
        retired=True,
    ))


def _save_fuel_update(sid, ed):
    """util.fuel_update - Tankstand.

    Wird nur geloggt/per WebSocket weitergegeben, nicht in DB gespeichert.
    fuel=-1 bedeutet Tankfunktion deaktiviert.
    """
    cid = str(ed.get('controller_id', '0'))
    fuel = ed.get('fuel', -1)
    log.info(f"Fuel update: Controller {cid}, fuel={fuel}")


def _save_active_track(sid, ed):
    """util.set_active_track - Aktive Strecke speichern."""
    name = ed.get('name', 'Unbekannt')
    set_active_track_name(name)
    length = ed.get('length')
    pitstop_delta = ed.get('pitstop_delta')
    log.info(f"Aktive Strecke: {name} (Laenge: {length})")

    try:
        # Andere Strecken deaktivieren (nur eine ist aktiv)
        Track.query.filter(Track.name != name).update(
            {'is_active': False}, synchronize_session=False)
        track = Track.query.filter_by(name=name).first()
        if track:
            track.last_used = datetime.utcnow()
            track.is_active = True
            if length:
                track.length = float(length)
            if pitstop_delta:
                track.pitstop_delta = float(pitstop_delta)
        else:
            track = Track(
                name=name,
                length=float(length) if length else None,
                pitstop_delta=float(pitstop_delta) if pitstop_delta else None,
                is_active=True,
            )
            db.session.add(track)
        db.session.commit()
    except Exception as e:
        log.warning(f"Track save failed: {e}")
        db.session.rollback()
