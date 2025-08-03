FROM python:3.11-slim

WORKDIR /app

RUN pip install flask mysql-connector-python

COPY <<EOF /app/app.py
from flask import Flask, jsonify

app = Flask(__name__)

@app.route('/')
def dashboard():
    return '''
<!DOCTYPE html>
<html lang="de">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>🏁 SmartRace Analytics</title>
    <script src="https://cdn.tailwindcss.com"></script>
    <script src="https://cdn.jsdelivr.net/npm/chart.js"></script>
    <script>tailwind.config = {darkMode: 'class'}</script>
</head>
<body class="bg-gray-900 text-white min-h-screen">
    <!-- Navigation -->
    <nav class="bg-gray-800 shadow-lg sticky top-0 z-50">
        <div class="max-w-7xl mx-auto px-4">
            <div class="flex justify-between items-center h-16">
                <h1 class="text-xl font-bold text-blue-400">🏁 SmartRace Analytics</h1>
                <div class="hidden md:flex space-x-8">
                    <a href="#dashboard" onclick="showPage('dashboard')" class="nav-link hover:text-blue-400 transition-colors">📊 Dashboard</a>
                    <a href="#live" onclick="showPage('live')" class="nav-link hover:text-blue-400 transition-colors">📡 Live Stats</a>
                    <a href="#charts" onclick="showPage('charts')" class="nav-link hover:text-blue-400 transition-colors">📈 Charts</a>
                    <a href="#database" onclick="showPage('database')" class="nav-link hover:text-blue-400 transition-colors">🗄️ Database</a>
                </div>
                <div class="md:hidden">
                    <button id="mobile-menu-btn" class="text-white">☰</button>
                </div>
            </div>
        </div>
        <!-- Mobile Menu -->
        <div id="mobile-menu" class="md:hidden hidden bg-gray-700 px-4 py-2 space-y-2">
            <a href="#dashboard" onclick="showPage('dashboard')" class="block nav-link">📊 Dashboard</a>
            <a href="#live" onclick="showPage('live')" class="block nav-link">📡 Live Stats</a>
            <a href="#charts" onclick="showPage('charts')" class="block nav-link">📈 Charts</a>
            <a href="#database" onclick="showPage('database')" class="block nav-link">🗄️ Database</a>
        </div>
    </nav>

    <main class="max-w-7xl mx-auto px-4 py-8">
        <!-- Dashboard Page -->
        <div id="dashboard-page" class="page active">
            <div class="space-y-6">
                <h2 class="text-3xl font-bold text-center">📊 Race Dashboard</h2>
                
                <div class="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6">
                    <div class="bg-gray-800 p-6 rounded-lg shadow-lg hover:bg-gray-700 transition-colors">
                        <div class="text-sm text-gray-400">Total Laps</div>
                        <div class="text-2xl font-bold text-blue-400">156</div>
                    </div>
                    <div class="bg-gray-800 p-6 rounded-lg shadow-lg hover:bg-gray-700 transition-colors">
                        <div class="text-sm text-gray-400">Active Drivers</div>
                        <div class="text-2xl font-bold text-green-400">5</div>
                    </div>
                    <div class="bg-gray-800 p-6 rounded-lg shadow-lg hover:bg-gray-700 transition-colors">
                        <div class="text-sm text-gray-400">Fastest Lap</div>
                        <div class="text-2xl font-bold text-yellow-400">1:23.456</div>
                    </div>
                    <div class="bg-gray-800 p-6 rounded-lg shadow-lg hover:bg-gray-700 transition-colors">
                        <div class="text-sm text-gray-400">Current Race</div>
                        <div class="text-2xl font-bold text-purple-400">Monaco GP</div>
                    </div>
                </div>
                
                <div class="bg-gray-800 p-6 rounded-lg shadow-lg">
                    <h3 class="text-xl font-bold mb-4">🎮 Quick Actions</h3>
                    <div class="flex flex-wrap gap-4">
                        <button onclick="refreshData()" class="bg-blue-600 hover:bg-blue-700 px-6 py-2 rounded-lg transition-colors">
                            🔄 Refresh Data
                        </button>
                        <button onclick="showPage('charts')" class="bg-green-600 hover:bg-green-700 px-6 py-2 rounded-lg transition-colors">
                            📊 View Charts
                        </button>
                        <button onclick="showPage('live')" class="bg-purple-600 hover:bg-purple-700 px-6 py-2 rounded-lg transition-colors">
                            📱 Live View
                        </button>
                    </div>
                </div>
            </div>
        </div>

        <!-- Live Stats Page -->
        <div id="live-page" class="page hidden">
            <div class="space-y-6">
                <div class="flex items-center justify-center space-x-2">
                    <span class="w-3 h-3 bg-green-500 rounded-full animate-pulse"></span>
                    <h2 class="text-3xl font-bold">📡 Live Statistics</h2>
                </div>
                
                <div class="bg-gray-800 p-6 rounded-lg shadow-lg">
                    <h3 class="text-xl font-bold mb-4">🏁 Current Positions</h3>
                    <div class="space-y-2">
                        <div class="flex justify-between items-center p-3 bg-gray-700 rounded-lg hover:bg-gray-600 transition-colors">
                            <div class="flex items-center space-x-3">
                                <span class="px-2 py-1 bg-yellow-500 text-black rounded font-bold">1</span>
                                <span class="font-bold">Lewis Hamilton</span>
                            </div>
                            <span class="text-yellow-400 font-mono">1:23.456</span>
                        </div>
                        <div class="flex justify-between items-center p-3 bg-gray-700 rounded-lg hover:bg-gray-600 transition-colors">
                            <div class="flex items-center space-x-3">
                                <span class="px-2 py-1 bg-gray-400 text-black rounded font-bold">2</span>
                                <span class="font-bold">Max Verstappen</span>
                            </div>
                            <span class="text-yellow-400 font-mono">1:23.789</span>
                        </div>
                        <div class="flex justify-between items-center p-3 bg-gray-700 rounded-lg hover:bg-gray-600 transition-colors">
                            <div class="flex items-center space-x-3">
                                <span class="px-2 py-1 bg-orange-600 text-white rounded font-bold">3</span>
                                <span class="font-bold">Charles Leclerc</span>
                            </div>
                            <span class="text-yellow-400 font-mono">1:24.123</span>
                        </div>
                    </div>
                </div>
            </div>
        </div>

        <!-- Charts Page -->
        <div id="charts-page" class="page hidden">
            <div class="space-y-6">
                <h2 class="text-3xl font-bold text-center">📈 Interactive Charts</h2>
                <div class="bg-gray-800 p-6 rounded-lg shadow-lg">
                    <h3 class="text-lg font-bold mb-4">Lap Time Comparison</h3>
                    <canvas id="raceChart"></canvas>
                </div>
            </div>
        </div>

        <!-- Database Page -->
        <div id="database-page" class="page hidden">
            <div class="space-y-6">
                <h2 class="text-3xl font-bold text-center">🗄️ Database Overview</h2>
                <div class="bg-gray-800 p-6 rounded-lg shadow-lg overflow-x-auto">
                    <table class="w-full text-left">
                        <thead>
                            <tr class="border-b border-gray-600">
                                <th class="pb-3 text-gray-300">Driver</th>
                                <th class="pb-3 text-gray-300">Best Lap</th>
                                <th class="pb-3 text-gray-300">Total Laps</th>
                                <th class="pb-3 text-gray-300">Avg Time</th>
                                <th class="pb-3 text-gray-300">Status</th>
                            </tr>
                        </thead>
                        <tbody>
                            <tr class="border-b border-gray-700 hover:bg-gray-700">
                                <td class="py-3 font-bold">Lewis Hamilton</td>
                                <td class="py-3 text-yellow-400 font-mono">1:23.456</td>
                                <td class="py-3">45</td>
                                <td class="py-3">1:24.123</td>
                                <td class="py-3"><span class="px-2 py-1 bg-green-600 rounded text-xs">Active</span></td>
                            </tr>
                            <tr class="border-b border-gray-700 hover:bg-gray-700">
                                <td class="py-3 font-bold">Max Verstappen</td>
                                <td class="py-3 text-yellow-400 font-mono">1:23.789</td>
                                <td class="py-3">43</td>
                                <td class="py-3">1:24.456</td>
                                <td class="py-3"><span class="px-2 py-1 bg-green-600 rounded text-xs">Active</span></td>
                            </tr>
                            <tr class="border-b border-gray-700 hover:bg-gray-700">
                                <td class="py-3 font-bold">Charles Leclerc</td>
                                <td class="py-3 text-yellow-400 font-mono">1:24.123</td>
                                <td class="py-3">41</td>
                                <td class="py-3">1:24.789</td>
                                <td class="py-3"><span class="px-2 py-1 bg-green-600 rounded text-xs">Active</span></td>
                            </tr>
                        </tbody>
                    </table>
                </div>
            </div>
        </div>
    </main>

    <script>
        // Page Navigation
        function showPage(pageId) {
            // Hide all pages
            document.querySelectorAll('.page').forEach(page => {
                page.classList.add('hidden');
                page.classList.remove('active');
            });
            
            // Show selected page
            const targetPage = document.getElementById(pageId + '-page');
            targetPage.classList.remove('hidden');
            targetPage.classList.add('active');
            
            // Update nav links
            document.querySelectorAll('.nav-link').forEach(link => {
                link.classList.remove('text-blue-400');
            });
            
            // Initialize chart if charts page
            if (pageId === 'charts') {
                setTimeout(initChart, 100);
            }
            
            // Hide mobile menu
            document.getElementById('mobile-menu').classList.add('hidden');
        }

        // Initialize Chart
        function initChart() {
            const ctx = document.getElementById('raceChart');
            if (!ctx) return;
            
            new Chart(ctx, {
                type: 'line',
                data: {
                    labels: ['Lap 1', 'Lap 2', 'Lap 3', 'Lap 4', 'Lap 5', 'Lap 6'],
                    datasets: [{
                        label: 'Hamilton',
                        data: [82.5, 81.2, 80.9, 81.1, 80.7, 80.5],
                        borderColor: 'rgb(59, 130, 246)',
                        backgroundColor: 'rgba(59, 130, 246, 0.2)',
                        tension: 0.4
                    }, {
                        label: 'Verstappen',
                        data: [82.1, 81.0, 80.8, 80.9, 80.5, 80.3],
                        borderColor: 'rgb(34, 197, 94)',
                        backgroundColor: 'rgba(34, 197, 94, 0.2)',
                        tension: 0.4
                    }, {
                        label: 'Leclerc',
                        data: [82.8, 81.5, 81.2, 81.3, 81.0, 80.8],
                        borderColor: 'rgb(239, 68, 68)',
                        backgroundColor: 'rgba(239, 68, 68, 0.2)',
                        tension: 0.4
                    }]
                },
                options: {
                    responsive: true,
                    plugins: {
                        legend: {
                            labels: { color: 'white' }
                        }
                    },
                    scales: {
                        y: {
                            ticks: { color: 'white' },
                            grid: { color: 'rgba(255,255,255,0.1)' }
                        },
                        x: {
                            ticks: { color: 'white' },
                            grid: { color: 'rgba(255,255,255,0.1)' }
                        }
                    }
                }
            });
        }

        // Mobile Menu Toggle
        document.getElementById('mobile-menu-btn').addEventListener('click', function() {
            const menu = document.getElementById('mobile-menu');
            menu.classList.toggle('hidden');
        });

        // Refresh Data Function
        function refreshData() {
            location.reload();
        }

        // Initialize default page
        showPage('dashboard');
    </script>
</body>
</html>
    '''

@app.route('/api/stats')
def api_stats():
    return jsonify({
        'total_laps': 156,
        'active_drivers': 5,
        'fastest_lap': '1:23.456',
        'current_race': 'Monaco GP'
    })

if __name__ == '__main__':
    print('🏁 SmartRace Analytics starting...')
    print('🔗 Access: http://localhost:5000')
    app.run(host='0.0.0.0', port=5000, debug=True)
EOF

CMD ["python", "app.py"]
