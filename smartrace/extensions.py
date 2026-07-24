"""Geteilte Flask-Erweiterungen (ungebunden, via init_app in __init__.py)."""
import logging

from flask_sqlalchemy import SQLAlchemy
from flask_socketio import SocketIO
from flask_cors import CORS

db = SQLAlchemy()
socketio = SocketIO()
cors = CORS()
log = logging.getLogger('smartrace')
