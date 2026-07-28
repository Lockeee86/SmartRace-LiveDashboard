// ============================================================================
// SmartRace ESP32 Display
// ----------------------------------------------------------------------------
// Zeigt die Live-Rundenzeiten eines Controllers auf einem ESP32-S3 4"-Touch-LCD
// (Waveshare ESP32-S3-Touch-LCD-4). Unten ein fester Picker C1-C6 (Touch).
//
// Datenquelle: das SmartRace-Dashboard (Flask), Endpunkte:
//   GET /api/device/laps?controller=N   -> Fahrer, Best/Letzte-Zeit, Position,
//                                          Status, letzte 10 Runden + Sektoren
//   GET /api/device/controllers         -> Fahrernamen fuer die Buttons
//
// WICHTIG - Board-Setup (einmalig):
//   Diese Datei kuemmert sich um NETZWERK + UI (LVGL). Die Display-/Touch-/LVGL-
//   Initialisierung ist board-spezifisch und kommt aus dem Waveshare-Demo fuer
//   genau dieses Board. Vorgehen:
//     1) Waveshares Arduino-LVGL-Demo fuer ESP32-S3-Touch-LCD-4 zum Laufen
//        bringen (Display + Touch + lv_timer_handler laufen).
//     2) Aus deren setup() den Display/Touch/LVGL-Init hierher in board_init()
//        uebernehmen (oder deren Init-Funktion dort aufrufen).
//     3) lv_conf.h wie im Waveshare-Demo (LVGL 8.x) verwenden.
//
// Bibliotheken (Arduino IDE -> Bibliotheksverwalter):
//   - lvgl (8.3.x)          - ArduinoJson (7.x)
//   - Waveshare Display/Touch-Treiber (aus deren Demo/Wiki)
//   Board: "ESP32S3 Dev Module" (bzw. wie im Waveshare-Wiki angegeben),
//   PSRAM aktivieren (OPI), passende Flash-/Partition-Einstellung.
// ============================================================================

#include <WiFi.h>
#include <HTTPClient.h>
#include <ArduinoJson.h>
#include <lvgl.h>
#include "config.h"

// ---- Farben (passend zum Web-Dashboard) ----
static const uint32_t CTRL_COLORS[6] = {
  0xe74c3c, 0x3498db, 0x2ecc71, 0xf1c40f, 0xe67e22, 0x9b59b6  // C1..C6
};
static const uint32_t SECTOR_COLORS[3] = { 0xef5350, 0xffca28, 0x42a5f5 }; // S1,S2,S3

// ---- Zustand ----
static int g_controller = DEFAULT_CONTROLLER;   // 1..6
static uint32_t g_lastPoll = 0;
static uint32_t g_lastCtrlPoll = 0;

// ---- UI-Objekte ----
static lv_obj_t *accentBar;      // obere Farbleiste in Controller-Farbe
static lv_obj_t *lblBadge;       // "C1"-Badge in Controller-Farbe
static lv_obj_t *lblDriver;      // Fahrername + Auto
static lv_obj_t *lblPos;         // "P2"
static lv_obj_t *lblStatus;      // Status-Punkt/Text
static lv_obj_t *lblLast;        // grosse letzte Rundenzeit
static lv_obj_t *lblBest;        // Bestzeit
static lv_obj_t *lblDelta;       // Delta letzte vs. beste
static lv_obj_t *lblSec[3];      // S1/S2/S3
static lv_obj_t *lapList;        // Container fuer die letzten Runden
static lv_obj_t *btnCtrl[6];     // C1..C6 Picker (einzelne Buttons)

// Farbe des aktuell gewaehlten Controllers
static inline lv_color_t accent_color() {
  return lv_color_hex(CTRL_COLORS[(g_controller - 1) % 6]);
}

// ============================================================================
// Board-Init (LVGL + Display + Touch) -> aus Waveshare-Demo uebernehmen
// ============================================================================
static void board_init() {
  // ESP32-S3-Touch-LCD-4: ST7701 (RGB-Panel, 480x480) + GT911 (Touch, I2C)
  // + IO-Expander. Basis: Waveshare-Arduino-Beispiel "02_LVGL_Porting".
  // Benoetigte Libs: ESP32_Display_Panel, ESP32_IO_Expander,
  //                  GFX_Library_for_Arduino, lvgl 8.4.
  //
  // Typisches Muster aus 02_LVGL_Porting (an deine Lib-Version anpassen;
  // lvgl_port_v8.h/.cpp aus dem Beispiel mit ins Projekt kopieren):
  //
  //   #include <ESP_Panel_Library.h>
  //   #include "lvgl_port_v8.h"
  //   ESP_Panel *panel = new ESP_Panel();
  //   panel->init();
  //   panel->begin();
  //   lvgl_port_init(panel->getLcd(), panel->getTouch());
  //
  // Danach ist ein aktiver LVGL-Screen vorhanden (lv_scr_act()) und
  // lv_timer_handler() rendert Display + verarbeitet Touch.
}

// ============================================================================
// UI aufbauen
// ============================================================================
static void style_time_label(lv_obj_t *l, const lv_font_t *font, lv_color_t col) {
  lv_obj_set_style_text_font(l, font, 0);
  lv_obj_set_style_text_color(l, col, 0);
}

// Akzentfarbe (= Farbe des gewaehlten Controllers) an markanten Stellen anwenden
static void apply_accent() {
  lv_color_t c = accent_color();
  lv_obj_set_style_bg_color(accentBar, c, 0);
  lv_obj_set_style_bg_color(lblBadge, c, 0);
  lv_label_set_text_fmt(lblBadge, "C%d", g_controller);
  lv_obj_set_style_text_color(lblDriver, c, 0);
  lv_obj_set_style_text_color(lblPos, c, 0);
}

// Picker-Buttons: gewaehlten hervorheben (voll, weisser Rand), Rest abdunkeln
static void style_picker() {
  for (int i = 0; i < 6; i++) {
    bool sel = (i == g_controller - 1);
    lv_obj_set_style_bg_opa(btnCtrl[i], sel ? LV_OPA_COVER : LV_OPA_40, 0);
    lv_obj_set_style_border_width(btnCtrl[i], sel ? 3 : 0, 0);
    lv_obj_set_style_border_color(btnCtrl[i], lv_color_hex(0xffffff), 0);
    lv_obj_set_style_transform_zoom(btnCtrl[i], sel ? 270 : 256, 0); // leicht groesser
  }
}

static void select_controller(int n) {
  if (n < 1 || n > 6) return;
  g_controller = n;
  apply_accent();
  style_picker();
  g_lastPoll = 0;   // sofort neu laden
}

static void picker_event_cb(lv_event_t *e) {
  int idx = (int)(intptr_t)lv_event_get_user_data(e);
  select_controller(idx + 1);
}

static void build_ui() {
  lv_obj_t *scr = lv_scr_act();
  lv_obj_set_style_bg_color(scr, lv_color_hex(0x0d0d10), 0);
  lv_obj_clear_flag(scr, LV_OBJ_FLAG_SCROLLABLE);

  // --- Obere Farbleiste (Controller-Farbe) ---
  accentBar = lv_obj_create(scr);
  lv_obj_set_size(accentBar, LV_PCT(100), 6);
  lv_obj_align(accentBar, LV_ALIGN_TOP_MID, 0, 0);
  lv_obj_set_style_border_width(accentBar, 0, 0);
  lv_obj_set_style_radius(accentBar, 0, 0);

  // --- "C#"-Badge in Controller-Farbe ---
  lblBadge = lv_label_create(scr);
  lv_label_set_text(lblBadge, "C1");
  lv_obj_set_style_text_font(lblBadge, &lv_font_montserrat_18, 0);
  lv_obj_set_style_text_color(lblBadge, lv_color_hex(0xffffff), 0);
  lv_obj_set_style_bg_opa(lblBadge, LV_OPA_COVER, 0);
  lv_obj_set_style_radius(lblBadge, 5, 0);
  lv_obj_set_style_pad_hor(lblBadge, 8, 0);
  lv_obj_set_style_pad_ver(lblBadge, 3, 0);
  lv_obj_align(lblBadge, LV_ALIGN_TOP_LEFT, 12, 14);

  // --- Kopf: Fahrer + Position + Status ---
  lblDriver = lv_label_create(scr);
  lv_label_set_text(lblDriver, "Warte auf Daten...");
  lv_obj_set_style_text_font(lblDriver, &lv_font_montserrat_20, 0);
  lv_obj_align(lblDriver, LV_ALIGN_TOP_LEFT, 58, 16);

  lblPos = lv_label_create(scr);
  lv_label_set_text(lblPos, "P-");
  lv_obj_set_style_text_font(lblPos, &lv_font_montserrat_20, 0);
  lv_obj_align(lblPos, LV_ALIGN_TOP_RIGHT, -14, 16);

  lblStatus = lv_label_create(scr);
  lv_label_set_text(lblStatus, "");
  lv_obj_set_style_text_font(lblStatus, &lv_font_montserrat_14, 0);
  lv_obj_set_style_text_color(lblStatus, lv_color_hex(0x9aa0a6), 0);
  lv_obj_align(lblStatus, LV_ALIGN_TOP_LEFT, 12, 44);

  // --- Grosse letzte Rundenzeit ---
  lblLast = lv_label_create(scr);
  lv_label_set_text(lblLast, "--");
  style_time_label(lblLast, &lv_font_montserrat_48, lv_color_hex(0xffffff));
  lv_obj_align(lblLast, LV_ALIGN_TOP_MID, 0, 66);

  // --- Bestzeit + Delta ---
  lblBest = lv_label_create(scr);
  lv_label_set_text(lblBest, "Best --");
  style_time_label(lblBest, &lv_font_montserrat_20, lv_color_hex(0xc084fc));
  lv_obj_align(lblBest, LV_ALIGN_TOP_MID, -70, 124);

  lblDelta = lv_label_create(scr);
  lv_label_set_text(lblDelta, "");
  style_time_label(lblDelta, &lv_font_montserrat_20, lv_color_hex(0x9aa0a6));
  lv_obj_align(lblDelta, LV_ALIGN_TOP_MID, 70, 124);

  // --- Sektoren S1/S2/S3 ---
  for (int i = 0; i < 3; i++) {
    lblSec[i] = lv_label_create(scr);
    lv_label_set_text_fmt(lblSec[i], "S%d --", i + 1);
    lv_obj_set_style_text_font(lblSec[i], &lv_font_montserrat_16, 0);
    lv_obj_set_style_text_color(lblSec[i], lv_color_hex(SECTOR_COLORS[i]), 0);
    lv_obj_align(lblSec[i], LV_ALIGN_TOP_LEFT, 14 + i * 155, 156);
  }

  // --- Liste der letzten Runden (scrollbar) ---
  lapList = lv_obj_create(scr);
  lv_obj_set_size(lapList, LV_PCT(96), 190);
  lv_obj_align(lapList, LV_ALIGN_TOP_MID, 0, 184);
  lv_obj_set_style_bg_color(lapList, lv_color_hex(0x16161b), 0);
  lv_obj_set_style_border_width(lapList, 0, 0);
  lv_obj_set_style_pad_all(lapList, 4, 0);
  lv_obj_set_flex_flow(lapList, LV_FLEX_FLOW_COLUMN);

  // --- Picker C1..C6 (fest, je Button in Controller-Farbe) ---
  lv_obj_t *pick = lv_obj_create(scr);
  lv_obj_set_size(pick, LV_PCT(96), 64);
  lv_obj_align(pick, LV_ALIGN_BOTTOM_MID, 0, -8);
  lv_obj_set_style_bg_opa(pick, LV_OPA_0, 0);
  lv_obj_set_style_border_width(pick, 0, 0);
  lv_obj_set_style_pad_all(pick, 0, 0);
  lv_obj_set_flex_flow(pick, LV_FLEX_FLOW_ROW);
  lv_obj_set_flex_align(pick, LV_FLEX_ALIGN_SPACE_BETWEEN,
                        LV_FLEX_ALIGN_CENTER, LV_FLEX_ALIGN_CENTER);
  lv_obj_clear_flag(pick, LV_OBJ_FLAG_SCROLLABLE);

  for (int i = 0; i < 6; i++) {
    btnCtrl[i] = lv_btn_create(pick);
    lv_obj_set_size(btnCtrl[i], 66, 54);
    lv_obj_set_style_bg_color(btnCtrl[i], lv_color_hex(CTRL_COLORS[i]), 0);
    lv_obj_set_style_radius(btnCtrl[i], 8, 0);
    lv_obj_add_event_cb(btnCtrl[i], picker_event_cb, LV_EVENT_CLICKED,
                        (void *)(intptr_t)i);
    lv_obj_t *bl = lv_label_create(btnCtrl[i]);
    lv_label_set_text_fmt(bl, "C%d", i + 1);
    lv_obj_set_style_text_font(bl, &lv_font_montserrat_20, 0);
    lv_obj_center(bl);
  }

  // Startzustand: Akzentfarbe + ausgewaehlten Button setzen
  apply_accent();
  style_picker();
}

// Hilfszeile in der Runden-Liste erzeugen
static void add_lap_row(int lap, const char *t, const char *s1,
                        const char *s2, const char *s3) {
  lv_obj_t *row = lv_obj_create(lapList);
  lv_obj_set_size(row, LV_PCT(100), 30);
  lv_obj_set_style_bg_opa(row, LV_OPA_0, 0);
  lv_obj_set_style_border_width(row, 0, 0);
  lv_obj_set_style_pad_all(row, 2, 0);
  lv_obj_clear_flag(row, LV_OBJ_FLAG_SCROLLABLE);

  lv_obj_t *ln = lv_label_create(row);
  lv_label_set_text_fmt(ln, "R%d", lap);
  lv_obj_set_style_text_color(ln, lv_color_hex(0x9aa0a6), 0);
  lv_obj_align(ln, LV_ALIGN_LEFT_MID, 0, 0);

  lv_obj_t *lt = lv_label_create(row);
  lv_label_set_text(lt, t);
  lv_obj_set_style_text_font(lt, &lv_font_montserrat_18, 0);
  lv_obj_set_style_text_color(lt, lv_color_hex(0xffffff), 0);
  lv_obj_align(lt, LV_ALIGN_LEFT_MID, 60, 0);

  lv_obj_t *ls = lv_label_create(row);
  lv_label_set_text_fmt(ls, "%s  %s  %s", s1, s2, s3);
  lv_obj_set_style_text_color(ls, lv_color_hex(0x8a8f98), 0);
  lv_obj_align(ls, LV_ALIGN_RIGHT_MID, 0, 0);
}

// ============================================================================
// Netzwerk
// ============================================================================
static bool http_get_json(const String &url, JsonDocument &doc) {
  if (WiFi.status() != WL_CONNECTED) return false;
  HTTPClient http;
  http.setConnectTimeout(2000);
  http.begin(url);
  int code = http.GET();
  bool ok = false;
  if (code == 200) {
    DeserializationError err = deserializeJson(doc, http.getStream());
    ok = !err;
  }
  http.end();
  return ok;
}

static void fetch_laps() {
  JsonDocument doc;
  String url = String(SERVER_BASE) + "/api/device/laps?controller=" + g_controller;
  if (!http_get_json(url, doc)) {
    lv_label_set_text(lblStatus, "keine Verbindung");
    return;
  }

  const char *driver = doc["driver"] | "";
  const char *car = doc["car"] | "";
  const char *best = doc["best"] | "--";
  const char *last = doc["last"] | "--";
  long best_ms = doc["best_ms"] | 0;
  long last_ms = doc["last_ms"] | 0;
  int pos = doc["position"] | 0;
  int lapCount = doc["lap_count"] | 0;
  const char *status = doc["status"] | "";

  if (strlen(driver) == 0) {
    lv_label_set_text(lblDriver, "C" ); // Fallback
    lv_label_set_text_fmt(lblDriver, "C%d - keine Daten", g_controller);
  } else {
    lv_label_set_text_fmt(lblDriver, "%s  (%s)", driver, car);
  }
  lv_label_set_text_fmt(lblPos, pos > 0 ? "P%d" : "P-", pos);
  lv_label_set_text_fmt(lblStatus, "%s  -  %d Runden", status, lapCount);
  lv_label_set_text(lblLast, last);
  lv_label_set_text_fmt(lblBest, "Best %s", best);

  // Delta letzte vs. beste
  if (last_ms > 0 && best_ms > 0) {
    long d = last_ms - best_ms;
    if (d <= 0) {
      lv_label_set_text(lblDelta, "PB");
      lv_obj_set_style_text_color(lblDelta, lv_color_hex(0x2ecc71), 0);
    } else {
      lv_label_set_text_fmt(lblDelta, "+%.3f", d / 1000.0);
      uint32_t c = d < 500 ? 0x2ecc71 : (d < 1500 ? 0xf1c40f : 0xe74c3c);
      lv_obj_set_style_text_color(lblDelta, lv_color_hex(c), 0);
    }
  } else {
    lv_label_set_text(lblDelta, "");
  }

  // Sektoren der neuesten Runde
  JsonArray laps = doc["laps"].as<JsonArray>();
  if (laps.size() > 0) {
    JsonObject cur = laps[0];
    lv_label_set_text_fmt(lblSec[0], "S1 %s", (const char *)(cur["s1"] | "--"));
    lv_label_set_text_fmt(lblSec[1], "S2 %s", (const char *)(cur["s2"] | "--"));
    lv_label_set_text_fmt(lblSec[2], "S3 %s", (const char *)(cur["s3"] | "--"));
  }

  // Runden-Liste neu aufbauen
  lv_obj_clean(lapList);
  int shown = 0;
  for (JsonObject l : laps) {
    if (shown++ >= LAP_LIST_COUNT) break;
    add_lap_row(l["lap"] | 0, l["t"] | "--", l["s1"] | "--",
                l["s2"] | "--", l["s3"] | "--");
  }
}

// Aktive Controller markieren: Buttons ohne Daten leicht abdunkeln
// (der ausgewaehlte Button bleibt durch style_picker() immer voll sichtbar).
static void fetch_controllers() {
  JsonDocument doc;
  String url = String(SERVER_BASE) + "/api/device/controllers";
  if (!http_get_json(url, doc)) return;
  JsonArray arr = doc["controllers"].as<JsonArray>();
  int i = 0;
  for (JsonObject c : arr) {
    bool active = c["active"] | false;
    if (i < 6 && i != g_controller - 1) {
      lv_obj_set_style_bg_opa(btnCtrl[i], active ? LV_OPA_40 : LV_OPA_20, 0);
    }
    i++;
  }
}

// ============================================================================
// WLAN
// ============================================================================
static void wifi_connect() {
  WiFi.mode(WIFI_STA);
  WiFi.begin(WIFI_SSID, WIFI_PASSWORD);
  uint32_t t0 = millis();
  while (WiFi.status() != WL_CONNECTED && millis() - t0 < 15000) {
    delay(250);
  }
}

// ============================================================================
// Arduino Setup / Loop
// ============================================================================
void setup() {
  Serial.begin(115200);
  board_init();     // <- Waveshare Display/Touch/LVGL-Init
  build_ui();
  wifi_connect();
  fetch_controllers();
  fetch_laps();
}

void loop() {
  lv_timer_handler();   // LVGL rendern + Touch verarbeiten
  delay(5);

  uint32_t now = millis();
  if (now - g_lastPoll >= POLL_INTERVAL_MS) {
    g_lastPoll = now;
    if (WiFi.status() != WL_CONNECTED) wifi_connect();
    fetch_laps();
  }
  if (now - g_lastCtrlPoll >= CTRL_POLL_INTERVAL_MS) {
    g_lastCtrlPoll = now;
    fetch_controllers();
  }
}
