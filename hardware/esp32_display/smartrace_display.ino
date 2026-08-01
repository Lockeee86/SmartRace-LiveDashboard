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
#include "esp_rom_sys.h"           // esp_rom_printf() — Marker auch auf HW-UART0
#include <ArduinoJson.h>
#include <lvgl.h>
#include <Wire.h>
#include <esp_display_panel.hpp>   // ESP32_Display_Panel (Anti-Tear via Doppel-Framebuffer)
#include "lvgl_v8_port.h"          // LVGL-Port aus dem ESP32_Display_Panel-Beispiel
#include "TouchDrvGT911.hpp"       // SensorLib: GT911-Touch (ueber Arduino-Wire)
#include "WS_CH32_IO.h"            // IO-Expander (Display-Reset/Backlight) — vor board.init()
#include "config.h"

using namespace esp_panel::drivers;
using namespace esp_panel::board;

// ---- Diagnose / Optionen ----
#define SR_ENABLE_TOUCH  1   // Touch an (I2C-Konflikt via Core 3.1.x behoben)
// Sichtbare Marker: geht auf USB-CDC (braucht "USB CDC On Boot: Enabled") UND HW-UART0.
static inline void sr_mark(const char *s) { Serial.println(s); Serial.flush(); esp_rom_printf("%s\n", s); }

// ---- Farben (passend zum Web-Dashboard) ----
static const uint32_t SECTOR_COLORS[3] = { 0xef5350, 0xffca28, 0x42a5f5 }; // S1,S2,S3

// Laufzeit-Farben je Controller: mit den Defaults geseedet, dann DYNAMISCH aus der
// API ueberschrieben. SmartRace sendet die Controller-/Auto-Farben mit; das Backend
// liefert sie als "#rrggbb" in /api/device/controllers und /api/device/laps.
static uint32_t g_ctrlColors[6] = {
  0xe74c3c, 0x3498db, 0x2ecc71, 0xf1c40f, 0xe67e22, 0x9b59b6
};

// "#rrggbb" -> 0xRRGGBB; bei ungueltigem/leerem Wert den Fallback behalten.
static uint32_t parse_hex_color(const char *s, uint32_t fallback) {
  if (!s || s[0] != '#' || strlen(s) < 7) return fallback;
  return (uint32_t)strtol(s + 1, nullptr, 16) & 0xFFFFFF;
}

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
  return lv_color_hex(g_ctrlColors[g_controller - 1]);
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
// Board-Init: Display (RGB + ST7701) wird DIREKT im Sketch aufgebaut — OHNE die
// Board-Config-Dateien und OHNE die Board-Klasse von ESP32_Display_Panel. Grund:
// deren Auto-Config (esp_panel_board_default_config.cpp) findet Sketch-Configs im
// Arduino nicht zuverlaessig -> fiel auf einen Default MIT Touch zurueck und
// installierte einen zweiten I2C-Treiber -> Kollision mit Arduino-Wire (CH32/GT911).
// Manuell konstruiert nutzt ESP_PANEL fuer's Display KEIN I2C. Touch/Reset/Backlight
// laufen ueber Arduino-Wire (CH32 + GT911/SensorLib), also nur EIN I2C-Treiber.
// LVGL laeuft danach im eigenen Task -> lv_* nur zwischen lock()/unlock().
// ============================================================================
static esp_panel::drivers::LCD *g_lcd = nullptr;

// Panelgroesse (fuer Touch-Mapping).
static const int16_t PANEL_W = 480, PANEL_H = 480;

// ST7701-Init unseres Panels (1:1 aus Arduino_GFX st7701_type1_init_operations).
static const esp_panel_lcd_vendor_init_cmd_t st7701_init_cmd[] = {
  {0xFF, (uint8_t []){0x77, 0x01, 0x00, 0x00, 0x10}, 5, 0},
  {0xC0, (uint8_t []){0x3B, 0x00}, 2, 0},
  {0xC1, (uint8_t []){0x0D, 0x02}, 2, 0},
  {0xC2, (uint8_t []){0x31, 0x05}, 2, 0},
  {0xCD, (uint8_t []){0x08}, 1, 0},
  {0xB0, (uint8_t []){0x00, 0x11, 0x18, 0x0E, 0x11, 0x06, 0x07, 0x08, 0x07, 0x22, 0x04, 0x12, 0x0F, 0xAA, 0x31, 0x18}, 16, 0},
  {0xB1, (uint8_t []){0x00, 0x11, 0x19, 0x0E, 0x12, 0x07, 0x08, 0x08, 0x08, 0x22, 0x04, 0x11, 0x11, 0xA9, 0x32, 0x18}, 16, 0},
  {0xFF, (uint8_t []){0x77, 0x01, 0x00, 0x00, 0x11}, 5, 0},
  {0xB0, (uint8_t []){0x60}, 1, 0},
  {0xB1, (uint8_t []){0x32}, 1, 0},
  {0xB2, (uint8_t []){0x07}, 1, 0},
  {0xB3, (uint8_t []){0x80}, 1, 0},
  {0xB5, (uint8_t []){0x49}, 1, 0},
  {0xB7, (uint8_t []){0x85}, 1, 0},
  {0xB8, (uint8_t []){0x21}, 1, 0},
  {0xC1, (uint8_t []){0x78}, 1, 0},
  {0xC2, (uint8_t []){0x78}, 1, 0},
  {0xE0, (uint8_t []){0x00, 0x1B, 0x02}, 3, 0},
  {0xE1, (uint8_t []){0x08, 0xA0, 0x00, 0x00, 0x07, 0xA0, 0x00, 0x00, 0x00, 0x44, 0x44}, 11, 0},
  {0xE2, (uint8_t []){0x11, 0x11, 0x44, 0x44, 0xED, 0xA0, 0x00, 0x00, 0xEC, 0xA0, 0x00, 0x00}, 12, 0},
  {0xE3, (uint8_t []){0x00, 0x00, 0x11, 0x11}, 4, 0},
  {0xE4, (uint8_t []){0x44, 0x44}, 2, 0},
  {0xE5, (uint8_t []){0x0A, 0xE9, 0xD8, 0xA0, 0x0C, 0xEB, 0xD8, 0xA0, 0x0E, 0xED, 0xD8, 0xA0, 0x10, 0xEF, 0xD8, 0xA0}, 16, 0},
  {0xE6, (uint8_t []){0x00, 0x00, 0x11, 0x11}, 4, 0},
  {0xE7, (uint8_t []){0x44, 0x44}, 2, 0},
  {0xE8, (uint8_t []){0x09, 0xE8, 0xD8, 0xA0, 0x0B, 0xEA, 0xD8, 0xA0, 0x0D, 0xEC, 0xD8, 0xA0, 0x0F, 0xEE, 0xD8, 0xA0}, 16, 0},
  {0xEB, (uint8_t []){0x02, 0x00, 0xE4, 0xE4, 0x88, 0x00, 0x40}, 7, 0},
  {0xEC, (uint8_t []){0x3C, 0x00}, 2, 0},
  {0xED, (uint8_t []){0xAB, 0x89, 0x76, 0x54, 0x02, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x20, 0x45, 0x67, 0x98, 0xBA}, 16, 0},
  {0xFF, (uint8_t []){0x77, 0x01, 0x00, 0x00, 0x13}, 5, 0},
  {0xE5, (uint8_t []){0xE4}, 1, 0},
  {0xFF, (uint8_t []){0x77, 0x01, 0x00, 0x00, 0x00}, 5, 0},
  {0x21, (uint8_t []){0x00}, 0, 0},
  {0x3A, (uint8_t []){0x60}, 1, 0},
  {0x11, (uint8_t []){0x00}, 0, 120},
  {0x29, (uint8_t []){0x00}, 0, 0},
};

// --- GT911-Touch (SensorLib, ueber Arduino-Wire — selber Bus wie der CH32) ---
static TouchDrvGT911 GT911;
static int16_t ts_x[5], ts_y[5];
static uint8_t gt911_addr = 0;
static bool gt911_available = false;

// GT911-Adresse per I2C-Scan finden und initialisieren
static bool init_gt911(int sda, int scl) {
  Wire.begin(sda, scl);
  delay(100);
  for (byte a = 1; a < 127; a++) {
    Wire.beginTransmission(a);
    if (Wire.endTransmission() == 0 &&
        (a == GT911_SLAVE_ADDRESS_L || a == GT911_SLAVE_ADDRESS_H)) {
      gt911_addr = a;
    }
  }
  if (!gt911_addr) { Serial.println("GT911 nicht gefunden"); return false; }
  GT911.setPins(-1, -1);
  if (GT911.begin(Wire, gt911_addr, sda, scl)) {
    GT911.setMaxTouchPoint(1);
    return true;
  }
  Serial.println("GT911 begin fehlgeschlagen");
  return false;
}

// LVGL-Touch-Read -> Punkt vom GT911, um 180 Grad gedreht (passend zu MIRROR_X/Y).
static void touch_read(lv_indev_drv_t *drv, lv_indev_data_t *data) {
  if (!gt911_available) { data->state = LV_INDEV_STATE_REL; return; }
  uint8_t touched = GT911.getPoint(ts_x, ts_y, GT911.getSupportTouchPoint());
  if (touched > 0) {
    data->state   = LV_INDEV_STATE_PR;
    data->point.x = PANEL_W - 1 - ts_x[0];
    data->point.y = PANEL_H - 1 - ts_y[0];
  } else {
    data->state = LV_INDEV_STATE_REL;
  }
}

static void board_init() {
  sr_mark("[SR] board_init: starte CH32 (Wire)...");
  // IO-Expander (CH32V003) ZUERST: gibt Display-Reset + Touch-Reset frei + Backlight an.
  // Arduino-Wire auf SDA15/SCL7.
  if (!WS_CH32_IO::begin(Wire, WS_CH32_IO::DEFAULT_I2C_SDA, WS_CH32_IO::DEFAULT_I2C_SCL,
                         WS_CH32_IO::DEFAULT_I2C_FREQ, &Serial)) {
    sr_mark("[SR] CH32 IO-Expander init FEHLGESCHLAGEN");
  }
  sr_mark("[SR] CH32 fertig, baue RGB-Bus + ST7701...");

  // RGB-Bus (3-wire-SPI-Init + 16-bit RGB). Pins/Timings 1:1 aus 09_LVGL_Widgets:
  //   CS42/SCK2/SDA1 ; D0..D15 = B0-4,G0-5,R0-4 ; HSYNC38 VSYNC39 PCLK41 DE40 ;
  //   14 MHz ; 480x480 ; HPW8 HBP50 HFP10 VPW8 VBP20 VFP10.
  BusRGB *bus = new BusRGB(
    42, 2, 1,
    5, 45, 48, 47, 21, 14, 13, 12, 11, 10, 9, 46, 3, 8, 18, 17,
    38, 39, 41, 40, -1,
    14 * 1000 * 1000, 480, 480, 8, 50, 10, 8, 20, 10);

  // ST7701-LCD (RST=-1, den macht der CH32). RGB565.
  LCD_ST7701 *lcd = new LCD_ST7701(bus, 480, 480, ESP_PANEL_LCD_COLOR_BITS_RGB565, -1);
  lcd->configVendorCommands(st7701_init_cmd, sizeof(st7701_init_cmd) / sizeof(st7701_init_cmd[0]));
  lcd->configColorRGB_Order(false);   // 0 = RGB
#if LVGL_PORT_AVOID_TEARING_MODE
  // Anti-Tear: mehrere Framebuffer + Bounce-Buffer gegen PSRAM-Underrun.
  lcd->configFrameBufferNumber(LVGL_PORT_DISP_BUFFER_NUM);
  bus->configRGB_BounceBufferSize(PANEL_W * 10);
#endif
  lcd->configMirrorByCommand(true);   // 180 Grad per LCD-Kommando (statt Software)
  sr_mark("[SR] LCD begin() (ST7701-Init ueber SPI)...");
  if (!lcd->begin()) sr_mark("[SR] LCD begin FEHLGESCHLAGEN");
  lcd->mirrorX(true);
  lcd->mirrorY(true);
  g_lcd = lcd;
  sr_mark("[SR] LCD ok, starte LVGL-Port...");

  // LVGL starten (eigener Task, ohne Touch). Ab hier: lv_* nur zwischen lock()/unlock().
  lvgl_port_init(g_lcd, nullptr);
  sr_mark("[SR] LVGL laeuft.");

#if SR_ENABLE_TOUCH
  // Touch selbst initialisieren und als LVGL-Eingabegeraet registrieren.
  sr_mark("[SR] init GT911-Touch...");
  gt911_available = init_gt911(WS_CH32_IO::DEFAULT_I2C_SDA, WS_CH32_IO::DEFAULT_I2C_SCL);
  if (gt911_available) {
    lvgl_port_lock(-1);
    static lv_indev_drv_t indev_drv;
    lv_indev_drv_init(&indev_drv);
    indev_drv.type    = LV_INDEV_TYPE_POINTER;
    indev_drv.read_cb = touch_read;
    lv_indev_drv_register(&indev_drv);
    lvgl_port_unlock();
  }
  sr_mark("[SR] Touch fertig.");
#else
  sr_mark("[SR] Touch DEAKTIVIERT (SR_ENABLE_TOUCH=0).");
#endif
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
  // montserrat_48 ist die groesste fertige Schrift. Zoom/Transform rendert auf
  // diesem Panel (Direct-Mode/Anti-Tear) nicht -> daher ohne Zoom.
  style_time_label(lblLast, &lv_font_montserrat_48, lv_color_hex(0xffffff));
  lv_obj_align(lblLast, LV_ALIGN_TOP_MID, 0, 64);

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
    lv_obj_set_style_text_font(lblSec[i], &lv_font_montserrat_20, 0);
    lv_obj_set_style_text_color(lblSec[i], lv_color_hex(SECTOR_COLORS[i]), 0);
    // Zentriert als Dreiergruppe (Mitte bei i==1), etwas tiefer wegen groesserer Zeit.
    lv_obj_align(lblSec[i], LV_ALIGN_TOP_MID, (i - 1) * 160, 160);
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
    lv_obj_set_style_bg_color(btnCtrl[i], lv_color_hex(g_ctrlColors[i]), 0);
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
  lv_obj_align(ls, LV_ALIGN_RIGHT_MID, -16, 0);    // -16: freihalten vom Scrollbalken
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

  // Dynamische Controllerfarbe des gewaehlten Controllers uebernehmen
  if (g_controller >= 1 && g_controller <= 6) {
    g_ctrlColors[g_controller - 1] =
        parse_hex_color(doc["color"] | "", g_ctrlColors[g_controller - 1]);
    lv_obj_set_style_bg_color(btnCtrl[g_controller - 1],
                              lv_color_hex(g_ctrlColors[g_controller - 1]), 0);
    apply_accent();   // Akzent (Leiste/Badge/Name/Position) in aktueller Farbe
  }

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
      set_accent(lv_color_hex(g_ctrlColors[ctrl - 1]));  // Kopf in Fahrerfarbe
      lv_label_set_text(lblDriver, (driver[0] ? driver : "Letzte Runden"));
      lv_label_set_text_fmt(lblPos, "R%d", (int)(l["lap"] | 0));
      lv_label_set_text(lblLast, t);
      lv_label_set_text_fmt(lblSec[0], "S1 %s", s1);
      lv_label_set_text_fmt(lblSec[1], "S2 %s", s2);
      lv_label_set_text_fmt(lblSec[2], "S3 %s", s3);
      continue;                           // neueste nicht auch in die Liste
    }

    if (shown++ >= LAP_LIST_COUNT) break;
    add_lap_row(driver, g_ctrlColors[ctrl - 1], l["lap"] | 0, t, s1, s2, s3);
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
    if (i < 6) {
      // dynamische Controllerfarbe uebernehmen + Button einfaerben
      g_ctrlColors[i] = parse_hex_color(c["color"] | "", g_ctrlColors[i]);
      lv_obj_set_style_bg_color(btnCtrl[i], lv_color_hex(g_ctrlColors[i]), 0);
      if (i != g_controller - 1)
        lv_obj_set_style_bg_opa(btnCtrl[i], active ? LV_OPA_40 : LV_OPA_20, 0);
    }
    i++;
  }
  // Falls sich die Farbe des gewaehlten Controllers geaendert hat -> Akzent auffrischen
  if (!recent_mode()) apply_accent();
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
  delay(300);
  sr_mark("\n=== SMARTRACE BUILD touch-off-v4 === setup() gestartet ===");
  board_init();     // Display (Touch per SR_ENABLE_TOUCH schaltbar)

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
