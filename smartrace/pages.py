"""HTML-Seiten (Jinja2-Templates)."""
from flask import Blueprint, render_template

pages_bp = Blueprint('pages', __name__)


@pages_bp.route('/')
def page_leaderboard():
    return render_template('leaderboard.html')


@pages_bp.route('/dashboard')
def page_dashboard():
    return render_template('dashboard.html')


@pages_bp.route('/analytics')
def page_analytics():
    return render_template('analytics.html')


@pages_bp.route('/database')
def page_database():
    return render_template('database.html')


@pages_bp.route('/stats')
def page_stats():
    return render_template('stats.html')


@pages_bp.route('/tracks')
def page_tracks():
    return render_template('tracks.html')


@pages_bp.route('/results')
def page_results():
    return render_template('results.html')


@pages_bp.route('/tv')
def page_tv():
    return render_template('tv.html')


@pages_bp.route('/driver/<name>')
def page_driver_profile(name):
    return render_template('driver-profile.html', driver_name=name)
