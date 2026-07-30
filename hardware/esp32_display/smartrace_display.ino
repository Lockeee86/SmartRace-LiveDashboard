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
#include <Wire.h>
#include <esp_display_panel.hpp>   // ESP32_Display_Panel (Anti-Tear via Doppel-Framebuffer)
#include "lvgl_v8_port.h"          // LVGL-Port aus dem ESP32_Display_Panel-Beispiel
#include "WS_CH32_IO.h"            // IO-Expander (Display-Reset/Backlight) — vor board.init()
#include "config.h"

using namespace esp_panel::drivers;
using namespace esp_panel::board;

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
static lv_obj_t *btnAll;         // 7. Button: "Letzte Runden" (alle Fahrer)

// g_controller == 0  -> Modus "Letzte Runden" (alle Fahrer gemischt)
// g_controller 1..6  -> einzelner Controller
static inline bool recent_mode() { return g_controller == 0; }

// Akzentfarbe: Controllerfarbe, im "Alle"-Modus neutral
static inline lv_color_t accent_color() {
  if (g_controller < 1 || g_controller > 6) return lv_color_hex(0x8a8f98);
  return lv_color_hex(CTRL_COLORS[g_controller - 1]);
}

// ---- Vorwaertsdeklarationen (werden vor ihrer Definition benutzt) ----
static void set_accent(lv_color_t c);
static void apply_accent();
static void apply_layout();
static void style_picker();
static void fetch_laps();
static void fetch_recent();
static void poll_data();

// ============================================================================
// Board-Init: Display + Touch via ESP32_Display_Panel (Anti-Tear/Doppelpuffer).
// Alle Hardware-Details (Pins, ST7701-Init, GT911, Timings, PCLK) stehen in
// esp_panel_board_custom_conf.h. Display-Reset/Backlight macht der CH32-IO-Expander.
// LVGL laeuft danach in einem eigenen Task -> alle lv_*-Aufrufe muessen mit
// lvgl_port_lock()/lvgl_port_unlock() geklammert werden.
// ============================================================================
static Board *g_board = nullptr;

static void board_init() {
  // IO-Expander (CH32V003) ZUERST: gibt Display-Reset frei + schaltet Backlight ein.
  // Nutzt Arduino-Wire auf demselben I2C-Bus (SDA15/SCL7) wie der GT911-Touch.
  if (!WS_CH32_IO::begin(Wire, WS_CH32_IO::DEFAULT_I2C_SDA, WS_CH32_IO::DEFAULT_I2C_SCL,
                         WS_CH32_IO::DEFAULT_I2C_FREQ, &Serial)) {
    Serial.println("CH32 IO-Expander init fehlgeschlagen");
  }
  // I2C-Peripherie wieder freigeben, damit ESP32_Display_Panel den Bus fuer den
  // GT911 selbst (IDF-Treiber) initialisieren kann. Die CH32-Ausgaenge (Reset/
  // Backlight) bleiben gesetzt, auch ohne weitere Kommunikation.
  Wire.end();

  g_board = new Board();
  g_board->init();

#if LVGL_PORT_AVOID_TEARING_MODE
  // Anti-Tear: dem RGB-Bus die noetige Framebuffer-Anzahl + Bounce-Buffer geben.
  auto lcd = g_board->getLCD();
  lcd->configFrameBufferNumber(LVGL_PORT_DISP_BUFFER_NUM);
  auto lcd_bus = lcd->getBus();
  if (lcd_bus->getBasicAttributes().type == ESP_PANEL_BUS_TYPE_RGB) {
    static_cast<BusRGB *>(lcd_bus)->configRGB_BounceBufferSize(lcd->getFrameWidth() * 10);
  }
#endif

  g_board->begin();

  // LVGL starten (eigener Task). Ab hier: lv_* nur zwischen lock()/unlock().
  lvgl_port_init(g_board->getLCD(), g_board->getTouch());
}

// ============================================================================
// UI aufbauen
// ============================================================================
static void style_time_label(lv_obj_t *l, const lv_font_t *font, lv_color_t col) {
  lv_obj_set_style_text_font(l, font, 0);
  lv_obj_set_style_text_color(l, col, 0);
}

// Akzentfarbe an markanten Stellen (Farbleiste, Badge, Fahrername, Position)
static void set_accent(lv_color_t c) {
  lv_obj_set_style_bg_color(accentBar, c, 0);
  lv_obj_set_style_bg_color(lblBadge, c, 0);
  lv_obj_set_style_text_color(lblDriver, c, 0);
  lv_obj_set_style_text_color(lblPos, c, 0);
}

// Akzent (= Farbe des gewaehlten Controllers) + Badge-Text setzen
static void apply_accent() {
  set_accent(accent_color());
  if (recent_mode()) lv_label_set_text(lblBadge, LV_SYMBOL_LIST);
  else               lv_label_set_text_fmt(lblBadge, "C%d", g_controller);
}

// Ein Picker-Button hervorheben/abdunkeln
static void style_btn(lv_obj_t *b, bool sel) {
  lv_obj_set_style_bg_opa(b, sel ? LV_OPA_COVER : LV_OPA_40, 0);
  lv_obj_set_style_border_width(b, sel ? 3 : 0, 0);
  lv_obj_set_style_border_color(b, lv_color_hex(0xffffff), 0);
  lv_obj_set_style_transform_zoom(b, sel ? 270 : 256, 0);
}

// Picker: gewaehlten (C1..C6 oder "Alle") hervorheben, Rest abdunkeln
static void style_picker() {
  for (int i = 0; i < 6; i++) style_btn(btnCtrl[i], i == g_controller - 1);
  style_btn(btnAll, recent_mode());
}

static void select_controller(int n) {
  if (n < 0 || n > 6) return;
  g_controller = n;   // 0 = "Letzte Runden" (alle), 1..6 = Controller
  apply_accent();
  apply_layout();
  style_picker();
  g_lastPoll = 0;   // sofort neu laden
}

static void picker_event_cb(lv_event_t *e) {
  int idx = (int)(intptr_t)lv_event_get_user_data(e);   // 0..5 = C1..C6
  select_controller(idx + 1);
}

static void all_event_cb(lv_event_t *e) {
  select_controller(0);   // "Letzte Runden"-Modus
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
    lv_obj_set_size(btnCtrl[i], 56, 54);
    lv_obj_set_style_bg_color(btnCtrl[i], lv_color_hex(CTRL_COLORS[i]), 0);
    lv_obj_set_style_radius(btnCtrl[i], 8, 0);
    lv_obj_add_event_cb(btnCtrl[i], picker_event_cb, LV_EVENT_CLICKED,
                        (void *)(intptr_t)i);
    lv_obj_t *bl = lv_label_create(btnCtrl[i]);
    lv_label_set_text_fmt(bl, "C%d", i + 1);
    lv_obj_set_style_text_font(bl, &lv_font_montserrat_20, 0);
    lv_obj_center(bl);
  }

  // 7. Button: "Letzte Runden" (alle Fahrer gemischt)
  btnAll = lv_btn_create(pick);
  lv_obj_set_size(btnAll, 56, 54);
  lv_obj_set_style_bg_color(btnAll, lv_color_hex(0x4b5563), 0);  // neutrales Grau
  lv_obj_set_style_radius(btnAll, 8, 0);
  lv_obj_add_event_cb(btnAll, all_event_cb, LV_EVENT_CLICKED, NULL);
  lv_obj_t *ba = lv_label_create(btnAll);
  lv_label_set_text(ba, LV_SYMBOL_LIST);   // Listen-Symbol
  lv_obj_set_style_text_font(ba, &lv_font_montserrat_20, 0);
  lv_obj_center(ba);

  // Startzustand: Akzentfarbe + Layout + ausgewaehlten Button setzen
  apply_accent();
  apply_layout();
  style_picker();
}

// Zeile in der Runden-Liste. driver != NULL -> farbiger Fahrername (Alle-Modus).
static void add_lap_row(const char *driver, uint32_t col, int lap, const char *t,
                        const char *s1, const char *s2, const char *s3) {
  lv_obj_t *row = lv_obj_create(lapList);
  lv_obj_set_size(row, LV_PCT(100), 30);
  lv_obj_set_style_bg_opa(row, LV_OPA_0, 0);
  lv_obj_set_style_border_width(row, 0, 0);
  lv_obj_set_style_pad_all(row, 2, 0);
  lv_obj_clear_flag(row, LV_OBJ_FLAG_SCROLLABLE);

  int xTime = 60;
  if (driver && driver[0]) {
    lv_obj_t *dn = lv_label_create(row);           // Fahrername (farbig)
    lv_label_set_text(dn, driver);
    lv_obj_set_style_text_font(dn, &lv_font_montserrat_16, 0);
    lv_obj_set_style_text_color(dn, lv_color_hex(col), 0);
    lv_obj_align(dn, LV_ALIGN_LEFT_MID, 4, 0);

    lv_obj_t *rn = lv_label_create(row);           // "R{n}"
    lv_label_set_text_fmt(rn, "R%d", lap);
    lv_obj_set_style_text_color(rn, lv_color_hex(0x9aa0a6), 0);
    lv_obj_align(rn, LV_ALIGN_LEFT_MID, 128, 0);
    xTime = 172;
  } else {
    lv_obj_t *ln = lv_label_create(row);
    lv_label_set_text_fmt(ln, "R%d", lap);
    lv_obj_set_style_text_color(ln, lv_color_hex(0x9aa0a6), 0);
    lv_obj_align(ln, LV_ALIGN_LEFT_MID, 0, 0);
  }

  lv_obj_t *lt = lv_label_create(row);             // Rundenzeit
  lv_label_set_text(lt, t);
  lv_obj_set_style_text_font(lt, &lv_font_montserrat_18, 0);
  lv_obj_set_style_text_color(lt, lv_color_hex(0xffffff), 0);
  lv_obj_align(lt, LV_ALIGN_LEFT_MID, xTime, 0);

  lv_obj_t *ls = lv_label_create(row);             // Sektoren
  lv_label_set_text_fmt(ls, "%s  %s  %s", s1, s2, s3);
  lv_obj_set_style_text_color(ls, lv_color_hex(0x8a8f98), 0);
  lv_obj_align(ls, LV_ALIGN_RIGHT_MID, 0, 0);
}

// Layout je nach Modus. In BEIDEN Modi: oben die neueste Runde gross mit
// Sektoren, darunter die Liste. Nur Best/Delta gibt's im "Alle"-Modus nicht
// (ueber alle Fahrer gemischt nicht sinnvoll).
static void apply_layout() {
  bool r = recent_mode();
  lv_obj_t *onlySingle[] = { lblBest, lblDelta };
  for (int i = 0; i < 2; i++) {
    if (r) lv_obj_add_flag(onlySingle[i], LV_OBJ_FLAG_HIDDEN);
    else   lv_obj_clear_flag(onlySingle[i], LV_OBJ_FLAG_HIDDEN);
  }
  // Grosse Zeit + Sektoren + Liste sind in beiden Modi sichtbar und gleich platziert
  lv_obj_clear_flag(lblLast, LV_OBJ_FLAG_HIDDEN);
  for (int i = 0; i < 3; i++) lv_obj_clear_flag(lblSec[i], LV_OBJ_FLAG_HIDDEN);
  lv_obj_align(lapList, LV_ALIGN_TOP_MID, 0, 184);
  lv_obj_set_height(lapList, 190);
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

  // Runden-Liste neu aufbauen (Einzel-Modus: ohne Fahrername)
  lv_obj_clean(lapList);
  int shown = 0;
  for (JsonObject l : laps) {
    if (shown++ >= LAP_LIST_COUNT) break;
    add_lap_row(NULL, 0, l["lap"] | 0, l["t"] | "--", l["s1"] | "--",
                l["s2"] | "--", l["s3"] | "--");
  }
}

// "Letzte Runden" (alle Fahrer gemischt) — wie das Web-Widget:
// oben die neueste Runde gross mit Sektoren (in Controllerfarbe),
// darunter die aelteren Runden in der Liste.
static void fetch_recent() {
  JsonDocument doc;
  String url = String(SERVER_BASE) + "/api/device/recent?limit=" + LAP_LIST_COUNT;
  if (!http_get_json(url, doc)) {
    lv_label_set_text(lblStatus, "keine Verbindung");
    return;
  }
  lv_label_set_text_fmt(lblStatus, "%s", (const char *)(doc["status"] | ""));

  JsonArray laps = doc["laps"].as<JsonArray>();
  lv_obj_clean(lapList);

  bool first = true;
  int shown = 0;
  for (JsonObject l : laps) {
    int ctrl = atoi((const char *)(l["controller"] | "1"));
    if (ctrl < 1 || ctrl > 6) ctrl = 1;
    const char *driver = (const char *)(l["driver"] | "");
    const char *t  = (const char *)(l["t"]  | "--");
    const char *s1 = (const char *)(l["s1"] | "--");
    const char *s2 = (const char *)(l["s2"] | "--");
    const char *s3 = (const char *)(l["s3"] | "--");

    if (first) {                          // neueste Runde gross oben
      first = false;
      set_accent(lv_color_hex(CTRL_COLORS[ctrl - 1]));   // Kopf in Fahrerfarbe
      lv_label_set_text(lblDriver, (driver[0] ? driver : "Letzte Runden"));
      lv_label_set_text_fmt(lblPos, "R%d", (int)(l["lap"] | 0));
      lv_label_set_text(lblLast, t);
      lv_label_set_text_fmt(lblSec[0], "S1 %s", s1);
      lv_label_set_text_fmt(lblSec[1], "S2 %s", s2);
      lv_label_set_text_fmt(lblSec[2], "S3 %s", s3);
      continue;                           // neueste nicht auch in die Liste
    }

    if (shown++ >= LAP_LIST_COUNT) break;
    add_lap_row(driver, CTRL_COLORS[ctrl - 1], l["lap"] | 0, t, s1, s2, s3);
  }

  if (first) {                            // keine Daten
    set_accent(accent_color());
    lv_label_set_text(lblDriver, "Letzte Runden");
    lv_label_set_text(lblPos, "");
    lv_label_set_text(lblLast, "--");
    for (int i = 0; i < 3; i++) lv_label_set_text_fmt(lblSec[i], "S%d --", i + 1);
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
  WiFi.setSleep(false);   // Modem-Sleep aus -> kein periodisches Flackern durch WLAN-Bursts
  WiFi.begin(WIFI_SSID, WIFI_PASSWORD);
  uint32_t t0 = millis();
  while (WiFi.status() != WL_CONNECTED && millis() - t0 < 15000) {
    delay(250);
  }
}

// ============================================================================
// Arduino Setup / Loop
// ============================================================================
// Je nach Modus die richtigen Daten holen
static void poll_data() {
  if (recent_mode()) fetch_recent();
  else               fetch_laps();
}

void setup() {
  Serial.begin(115200);
  board_init();     // Display/Touch/LVGL (ESP32_Display_Panel + lvgl_port)

  // UI aufbauen — lv_* nur unter dem LVGL-Lock.
  lvgl_port_lock(-1);
  build_ui();
  lvgl_port_unlock();

  wifi_connect();

  lvgl_port_lock(-1);
  fetch_controllers();
  poll_data();
  lvgl_port_unlock();
}

void loop() {
  // Kein lv_timer_handler() mehr — das erledigt der LVGL-Port in seinem Task.
  uint32_t now = millis();
  if (now - g_lastPoll >= POLL_INTERVAL_MS) {
    g_lastPoll = now;
    if (WiFi.status() != WL_CONNECTED) wifi_connect();
    lvgl_port_lock(-1);
    poll_data();
    lvgl_port_unlock();
  }
  if (now - g_lastCtrlPoll >= CTRL_POLL_INTERVAL_MS) {
    g_lastCtrlPoll = now;
    lvgl_port_lock(-1);
    fetch_controllers();
    lvgl_port_unlock();
  }
  delay(5);
}
