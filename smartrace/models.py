"""SQLAlchemy-Modelle (Prefix "sr_" um Konflikte mit alten Tabellen zu vermeiden)."""
from datetime import datetime

from sqlalchemy import Index

from .extensions import db


class Event(db.Model):
    __tablename__ = 'sr_events'
    id = db.Column(db.Integer, primary_key=True)
    session_id = db.Column(db.String(100), index=True)
    event_type = db.Column(db.String(50))
    created_at = db.Column(db.DateTime, default=datetime.utcnow)
    raw_json = db.Column(db.Text)


class Lap(db.Model):
    __tablename__ = 'sr_laps'
    __table_args__ = (
        Index('ix_sr_laps_session_controller', 'session_id', 'controller_id'),
        Index('ix_sr_laps_driver_name', 'driver_name'),
        Index('ix_sr_laps_car_name', 'car_name'),
        Index('ix_sr_laps_session_created', 'session_id', 'created_at'),
    )
    id = db.Column(db.Integer, primary_key=True)
    session_id = db.Column(db.String(100), index=True)
    session_type = db.Column(db.String(50))
    controller_id = db.Column(db.String(10), index=True)
    driver_name = db.Column(db.String(100))
    car_name = db.Column(db.String(100))
    lap_number = db.Column(db.Integer)
    laptime_ms = db.Column(db.Integer)
    laptime_display = db.Column(db.String(20))
    sector_1 = db.Column(db.String(20))
    sector_2 = db.Column(db.String(20))
    sector_3 = db.Column(db.String(20))
    car_color = db.Column(db.String(20))
    controller_color = db.Column(db.String(20))
    is_personal_best = db.Column(db.Boolean, default=False)
    track_name = db.Column(db.String(200), index=True)
    created_at = db.Column(db.DateTime, default=datetime.utcnow, index=True)


class RaceResult(db.Model):
    __tablename__ = 'sr_results'
    id = db.Column(db.Integer, primary_key=True)
    session_id = db.Column(db.String(100), index=True)
    position = db.Column(db.Integer)
    controller_id = db.Column(db.String(10))
    driver_name = db.Column(db.String(100))
    total_laps = db.Column(db.Integer)
    best_laptime_ms = db.Column(db.Integer)
    gap = db.Column(db.String(50))
    pitstops = db.Column(db.Integer, default=0)
    disqualified = db.Column(db.Boolean, default=False)
    retired = db.Column(db.Boolean, default=False)
    created_at = db.Column(db.DateTime, default=datetime.utcnow)


class Penalty(db.Model):
    __tablename__ = 'sr_penalties'
    id = db.Column(db.Integer, primary_key=True)
    session_id = db.Column(db.String(100), index=True)
    controller_id = db.Column(db.String(10))
    driver_name = db.Column(db.String(100))
    penalty_type = db.Column(db.String(50))
    penalty_seconds = db.Column(db.Integer, default=0)
    created_at = db.Column(db.DateTime, default=datetime.utcnow)


class RaceStatus(db.Model):
    __tablename__ = 'sr_race_status'
    id = db.Column(db.Integer, primary_key=True)
    session_id = db.Column(db.String(100), index=True)
    status = db.Column(db.String(50))
    race_type = db.Column(db.String(50))
    updated_at = db.Column(db.DateTime, default=datetime.utcnow)


class TrackRecord(db.Model):
    __tablename__ = 'sr_track_records'
    id = db.Column(db.Integer, primary_key=True)
    laptime_ms = db.Column(db.Integer, nullable=False)
    driver_name = db.Column(db.String(100))
    car_name = db.Column(db.String(100))
    session_id = db.Column(db.String(100))
    controller_id = db.Column(db.String(10))
    track_name = db.Column(db.String(200), index=True)
    created_at = db.Column(db.DateTime, default=datetime.utcnow)


class PersonalRecord(db.Model):
    __tablename__ = 'sr_personal_records'
    id = db.Column(db.Integer, primary_key=True)
    driver_name = db.Column(db.String(100), index=True)
    laptime_ms = db.Column(db.Integer, nullable=False)
    car_name = db.Column(db.String(100))
    session_id = db.Column(db.String(100))
    controller_id = db.Column(db.String(10))
    track_name = db.Column(db.String(200), index=True)
    created_at = db.Column(db.DateTime, default=datetime.utcnow)
    updated_at = db.Column(db.DateTime, default=datetime.utcnow)


class Track(db.Model):
    __tablename__ = 'sr_tracks'
    id = db.Column(db.Integer, primary_key=True)
    name = db.Column(db.String(200), unique=True, index=True)
    length = db.Column(db.Float, nullable=True)
    pitstop_delta = db.Column(db.Float, nullable=True)
    svg_layout = db.Column(db.Text, nullable=True)
    is_active = db.Column(db.Boolean, default=False)
    last_used = db.Column(db.DateTime, default=datetime.utcnow)
    created_at = db.Column(db.DateTime, default=datetime.utcnow)


class SessionName(db.Model):
    __tablename__ = 'sr_session_names'
    id = db.Column(db.Integer, primary_key=True)
    session_id = db.Column(db.String(100), unique=True, index=True)
    display_name = db.Column(db.String(200))
    created_at = db.Column(db.DateTime, default=datetime.utcnow)
