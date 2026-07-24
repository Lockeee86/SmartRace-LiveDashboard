"""Einstiegspunkt fuer Gunicorn (`app:app`) und lokale Entwicklung.

Das Backend wurde in das Paket `smartrace/` aufgeteilt. Dieses Modul haelt
nur noch den Einstiegspunkt bereit, damit `gunicorn ... app:app` und
`python app.py` weiterhin funktionieren.
"""
import os

from smartrace import app, socketio  # noqa: F401  (app wird von Gunicorn genutzt)

if __name__ == '__main__':
    socketio.run(
        app,
        host='0.0.0.0',
        port=int(os.getenv('PORT', 5000)),
        debug=os.getenv('FLASK_DEBUG', 'false').lower() == 'true',
    )
