"""SmartRace LiveDashboard - Flask App Factory.

Das Backend ist in Module aufgeteilt:
  extensions.py  - db / socketio / cors / log (ungebunden)
  models.py      - SQLAlchemy-Modelle
  db_init.py     - Tabellen-Erstellung + Auto-Migration
  utils.py       - Formatierung, Session-Ermittlung, aktive Strecke
  ingest.py      - SmartRace Datenschnittstelle (Webhook + Speicher-Handler)
  api.py         - JSON-API-Endpunkte
  pages.py       - HTML-Seiten
  sockets.py     - WebSocket-Handler
"""
import os

from flask import Flask

from .extensions import cors, db, socketio


def create_app():
    """Flask-App erstellen und konfigurieren."""
    app = Flask(
        __name__,
        template_folder='../templates',
        static_folder='../static',
        static_url_path='/static',
    )

    app.config['SQLALCHEMY_DATABASE_URI'] = os.getenv(
        'DATABASE_URL', 'sqlite:///smartrace.db'
    )
    app.config['SQLALCHEMY_TRACK_MODIFICATIONS'] = False
    app.config['SQLALCHEMY_ENGINE_OPTIONS'] = {
        'pool_size': 5,
        'max_overflow': 10,
        'pool_recycle': 300,
        'pool_pre_ping': True,
    }
    app.config['SECRET_KEY'] = os.getenv('SECRET_KEY', 'sr-dashboard-secret')

    # Erweiterungen an App binden
    cors.init_app(app)
    db.init_app(app)
    socketio.init_app(app, cors_allowed_origins="*", async_mode='eventlet')

    # Modelle registrieren (fuer create_all)
    from . import models  # noqa: F401

    # Seiten-Routen direkt registrieren (unpraefixierte Endpoint-Namen)
    from .pages import register_pages
    register_pages(app)

    # Blueprints registrieren
    from .api import api_bp
    from .ingest import ingest_bp
    app.register_blueprint(api_bp)
    app.register_blueprint(ingest_bp)

    # WebSocket-Handler registrieren
    from . import sockets  # noqa: F401

    # DB initialisieren (Tabellen + Auto-Migration)
    from .db_init import init_db
    with app.app_context():
        init_db()

    return app


# Modul-Level App fuer Gunicorn (app:app -> smartrace-Paket via app.py Shim)
app = create_app()
