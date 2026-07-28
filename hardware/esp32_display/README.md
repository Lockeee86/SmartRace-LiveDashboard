# SmartRace ESP32 Display

Ein physisches Renn-Display für das SmartRace LiveDashboard:
**ESP32-S3-Touch-LCD-4** (Waveshare) zeigt die Live-Rundenzeiten eines
Controllers, die letzten 10 Runden mit Sektoren, Bestzeit + Delta und Position.
Unten ein fester Touch-Picker **C1–C6**.

**Board (laut Waveshare-Wiki):** ESP32-S3-N16R8 (16 MB Flash, 8 MB PSRAM),
4″ IPS **480×480**, Display-Treiber **ST7701** (RGB), Touch **GT911** (I²C) +
IO-Expander. Frameworks: **Arduino IDE** und ESP-IDF; LVGL-Demo nutzt **LVGL 8.4**.

Das Display holt seine Daten per WLAN vom Dashboard-Server — es enthält **keine
Logik**, nur Anzeige.

```
SmartRace-App ─► Flask-Dashboard (z.B. 192.168.1.90:5000) ─WLAN─► ESP32 Display
```

## Verwendete Server-Endpunkte (schon im Dashboard vorhanden)

- `GET /api/device/laps?controller=N` — Fahrer, Best-/Letzte-Zeit, Position,
  Status, letzte 10 Runden + Sektoren (alles kompakt formatiert).
- `GET /api/device/controllers` — C1–C6 mit Fahrername/aktiv (für die Buttons).

Test vom PC aus:
```bash
curl "http://192.168.1.90:5000/api/device/laps?controller=1"
curl "http://192.168.1.90:5000/api/device/controllers"
```

## Voraussetzungen (Arduino IDE)

1. **Arduino IDE** (2.x) + **ESP32-Boardpaket** (Boards-Manager-URL von Espressif).
2. Board: *ESP32S3 Dev Module*, **PSRAM aktivieren** (OPI, wie im Wiki),
   16 MB Flash, passende Partition. Genaue Werte stehen im Waveshare-Wiki.
3. Bibliotheken (Bibliotheksverwalter / Waveshare-Demo):
   - **lvgl** 8.4.x  (Beispiel nutzt genau diese Version)
   - **ArduinoJson** 7.x
   - **ESP32_Display_Panel** (ST7701 + GT911, v0.1.8),
     **ESP32_IO_Expander**, **GFX_Library_for_Arduino**
   - `lv_conf.h` aus dem Waveshare-Beispiel übernehmen (Fonts wie
     `montserrat_48` müssen aktiviert sein)

## Einrichtung in 3 Schritten

### 1) Waveshare-Demo zum Laufen bringen
Lade das Beispiel **`02_LVGL_Porting`** (RGB-Touch mit LVGL) aus dem Waveshare-
Wiki/Demo und flashe es. Wenn dort die LVGL-Demo sauber angezeigt wird und der
Touch reagiert, ist die Board-Basis fertig.

Übernimm aus diesem Beispiel **`lv_conf.h`** sowie **`lvgl_port_v8.h/.cpp`**
in dein Projekt.

### 2) Diesen Sketch einbinden
- Ordner `smartrace_display/` mit `smartrace_display.ino` + `config.h` öffnen.
- In **`config.h`** eintragen:
  - `WIFI_SSID`, `WIFI_PASSWORD`
  - `SERVER_BASE` = Adresse deines Dashboards, z.B. `http://192.168.1.90:5000`
- In **`smartrace_display.ino`** die Funktion **`board_init()`** ausfüllen:
  den Display-/Touch-/LVGL-Init aus dem Waveshare-Demo (aus deren `setup()`)
  dorthin übernehmen bzw. deren Init-Funktion aufrufen. Danach muss ein aktiver
  LVGL-Screen vorhanden sein und `lv_timer_handler()` laufen.

### 3) Flashen
Kompilieren und hochladen. Auf dem Serial-Monitor (115200) siehst du den
WLAN-Status. Das Display verbindet sich, holt alle 0,5 s die Daten und zeigt
den gewählten Controller. Unten C1–C6 antippen zum Wechseln.

## Was der Sketch macht (Überblick)

| Bereich | Funktion |
|---|---|
| WLAN | `wifi_connect()` — verbindet, reconnectet bei Aussetzern |
| Daten | `fetch_laps()` — HTTP GET + ArduinoJson, aktualisiert die Labels |
| Buttons | `fetch_controllers()` — deaktiviert Buttons ohne Daten (optional) |
| UI | `build_ui()` — Labels, Sektoren, Runden-Liste, C1–C6-Buttonmatrix |
| Loop | `lv_timer_handler()` + Polling im Intervall |

Farben sind auf das Web-Dashboard abgestimmt (C1 rot … C6 lila; S1 rot, S2 gelb,
S3 blau).

**Auf einen Blick:** die Farbe des gewählten Controllers wird als Akzent an
mehreren Stellen gezeigt — obere Farbleiste, „C#"-Badge, Fahrername und Position.
Im Picker ist der aktive Button voll deckend + weißer Rand, die anderen sind
abgedunkelt (Buttons ohne Daten noch etwas mehr).

## Anpassen

- **Intervall**: `POLL_INTERVAL_MS` in `config.h` (Default 500 ms).
- **Anzahl Runden in der Liste**: `LAP_LIST_COUNT` (max 10, so viele liefert die API).
- **Schriftgrößen/Layout**: in `build_ui()` (LVGL-Fonts müssen in `lv_conf.h`
  aktiviert sein — z.B. `montserrat_48` für die große Zeit).

## Hinweise

- Diese Firmware nutzt **REST-Polling** (robust auf dem ESP32) statt der
  Web-WebSockets. 0,5 s Intervall ist für ein Display flüssig genug und schont
  das WLAN.
- LVGL-**Version**: der Sketch ist für **LVGL 8.x** geschrieben und passt damit
  zum Waveshare-Beispiel (**LVGL 8.4**) — keine API-Portierung nötig.
- Getestet werden konnte hier nur der **Server-Teil** (die Endpunkte). Den
  Firmware-Teil bitte auf der echten Hardware verifizieren — melde dich, wenn
  beim Kompilieren/Anzeigen etwas hakt, dann passen wir es an.
