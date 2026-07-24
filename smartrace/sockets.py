"""WebSocket-Handler (Verbindung/Trennung)."""
from .extensions import log, socketio


@socketio.on('connect')
def handle_connect():
    log.info("WebSocket-Client verbunden")


@socketio.on('disconnect')
def handle_disconnect():
    log.info("WebSocket-Client getrennt")
