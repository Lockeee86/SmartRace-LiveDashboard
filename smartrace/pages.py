"""HTML-Seiten (Jinja2-Templates).

Direkt auf der App registriert (kein Blueprint), damit die Endpoint-Namen
unpraefixiert bleiben (`page_dashboard` statt `pages.page_dashboard`) und die
Templates weiterhin `url_for('page_...')` / `request.endpoint == 'page_...'`
verwenden koennen.
"""
from flask import render_template


def register_pages(app):
    @app.route('/')
    def page_leaderboard():
        return render_template('leaderboard.html')

    @app.route('/dashboard')
    def page_dashboard():
        return render_template('dashboard.html')

    @app.route('/analytics')
    def page_analytics():
        return render_template('analytics.html')

    @app.route('/database')
    def page_database():
        return render_template('database.html')

    @app.route('/stats')
    def page_stats():
        return render_template('stats.html')

    @app.route('/tracks')
    def page_tracks():
        return render_template('tracks.html')

    @app.route('/results')
    def page_results():
        return render_template('results.html')

    @app.route('/tv')
    def page_tv():
        return render_template('tv.html')

    @app.route('/driver/<name>')
    def page_driver_profile(name):
        return render_template('driver-profile.html', driver_name=name)
