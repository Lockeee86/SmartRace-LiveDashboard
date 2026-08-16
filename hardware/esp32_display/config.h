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

// --- Start-Ansicht: 0 = "Letzte Runden" (alle Fahrer), 1..6 = Controller C1..C6 ---
#define DEFAULT_CONTROLLER  1

// --- Startbildschirm beim Einschalten ---
//   0 = Timing-Ansicht (DEFAULT_CONTROLLER)
//   1 = Fahrer-Uebersicht (alle Fahrer; Zeile antippen -> Timing)
#define START_VIEW  1

// --- Wie viele Runden in der Liste anzeigen (max 10, so viele liefert die API) ---
#define LAP_LIST_COUNT  10

// --- Display-Pixeltakt ---
// HINWEIS: Der Pixeltakt (und alle Display-Timings/Pins) stehen jetzt in
// esp_panel_board_custom_conf.h -> ESP_PANEL_BOARD_LCD_RGB_CLK_HZ (Default 14 MHz).
// Das eigentliche Flackern loest der Doppel-Framebuffer (Anti-Tear) im LVGL-Port.
