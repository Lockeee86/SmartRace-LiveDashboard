"""DB-Initialisierung und Auto-Migration (robust: erst versuchen, bei Fehler bereinigen)."""
from sqlalchemy import text

from .extensions import db, log


def _add_column_if_missing(table, column, col_type):
    """Fuegt eine Spalte hinzu, falls sie noch nicht existiert."""
    try:
        db.session.execute(text(
            f"ALTER TABLE {table} ADD COLUMN {column} {col_type}"
        ))
        db.session.commit()
        log.info(f"Spalte {table}.{column} hinzugefuegt.")
    except Exception:
        db.session.rollback()


def init_db():
    try:
        db.create_all()
        log.info("DB-Tabellen bereit.")
    except Exception as e:
        log.error(f"create_all fehlgeschlagen: {e} — bitte DB manuell pruefen.")
        db.session.rollback()

    # Auto-Migration: fehlende Spalten hinzufuegen
    _add_column_if_missing('sr_results', 'pitstops', 'INTEGER DEFAULT 0')
    _add_column_if_missing('sr_penalties', 'penalty_seconds', 'INTEGER DEFAULT 0')
    _add_column_if_missing('sr_tracks', 'svg_layout', 'TEXT')
    _add_column_if_missing('sr_tracks', 'is_active', 'BOOLEAN DEFAULT FALSE')
    _add_column_if_missing('sr_laps', 'track_name', 'VARCHAR(200)')
    _add_column_if_missing('sr_track_records', 'track_name', 'VARCHAR(200)')
    _add_column_if_missing('sr_personal_records', 'track_name', 'VARCHAR(200)')

    # Unique-Constraint auf driver_name entfernen (jetzt pro Track)
    try:
        db.session.execute(text(
            "DROP INDEX IF EXISTS ix_sr_personal_records_driver_name"
        ))
        db.session.commit()
    except Exception:
        db.session.rollback()
    try:
        db.session.execute(text(
            "ALTER TABLE sr_personal_records DROP CONSTRAINT IF EXISTS sr_personal_records_driver_name_key"
        ))
        db.session.commit()
    except Exception:
        db.session.rollback()
