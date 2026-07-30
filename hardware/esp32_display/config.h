// ============================================================================
// SmartRace ESP32 Display - Konfiguration
// Diese Werte an deine Umgebung anpassen.
// ============================================================================
#pragma once

// --- WLAN ---
#define WIFI_SSID      "DEIN_WLAN"
#define WIFI_PASSWORD  "DEIN_PASSWORT"

// --- SmartRace-Dashboard (Flask-Server) ---
// IP/Host des laufenden Dashboards (ohne abschliessenden Slash)
#define SERVER_BASE    "http://192.168.1.90:5000"

// --- Abfrage-Intervall (ms) ---
#define POLL_INTERVAL_MS      500   // wie oft die aktuellen Daten geholt werden
#define CTRL_POLL_INTERVAL_MS 3000  // wie oft Fahrernamen der Buttons aktualisiert werden

// --- Start-Controller (1..6) ---
#define DEFAULT_CONTROLLER  1

// --- Wie viele Runden in der Liste anzeigen (max 10, so viele liefert die API) ---
#define LAP_LIST_COUNT  10

// --- Display-Pixeltakt (gegen Flackern) ---
// Standard der GFX-Lib waeren nur 12 MHz -> ~42 Hz Bildrate = sichtbares Flimmern.
// Hoeher = mehr Hz = ruhiger. Bei Tearing (WLAN/Bandbreite) eher wieder senken.
// Sinnvoll: 12000000 .. 16000000. Bei Underrun-Flackern eher NIEDRIGER (14 MHz
// war in Tests am ruhigsten; 18 MHz war schlechter). Bei Flimmern durch zu wenig
// Bildrate eher hoeher. Sweet Spot meist 13..15 MHz.
#define RGB_PCLK_HZ  14000000
