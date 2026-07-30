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

## Layout (Vorschau)

Maßstabsgetreues Mockup (480×480), Beispiel: Controller **C1** ausgewählt — die
Kopfzeile (Farbleiste, C#-Badge, Fahrername, Position) färbt sich in der
Controllerfarbe.

![ESP32-Display Layout 480x480](layout_mockup.png)

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
3. **Bibliotheken** (siehe Tabelle unten). Wichtig: **nicht** alle liegen im
   Bibliotheksverwalter — die board-spezifischen kommen aus dem Waveshare-Repo.
4. ESP32-Core: **stabile** Version (z.B. 2.0.14 / 3.0.x) — **kein** `*-alpha`.

### Benötigte Bibliotheken

Alle board-spezifischen Libs liegen gebündelt im Waveshare-Repo unter
[`examples/arduino/libraries`](https://github.com/waveshareteam/ESP32-S3-Touch-LCD-4/tree/main/examples/arduino/libraries).

| Bibliothek | Version | Woher | Zweck |
|---|---|---|---|
| **lvgl** | **8.4.0** (nicht 9.x!) | Bibliotheksverwalter **oder** [Waveshare-Repo](https://github.com/waveshareteam/ESP32-S3-Touch-LCD-4/tree/main/examples/arduino/libraries/lvgl) · [Upstream](https://github.com/lvgl/lvgl) | GUI-Framework |
| **lv_conf.h** | zu 8.4.0 passend | [Waveshare-Repo](https://github.com/waveshareteam/ESP32-S3-Touch-LCD-4/blob/main/examples/arduino/libraries/lv_conf.h) | LVGL-Konfig (Farbtiefe 16, alle Montserrat-Fonts an) — **direkt in `libraries/` legen**, neben den `lvgl`-Ordner |
| **GFX_Library_for_Arduino** | Waveshare-Stand | [Waveshare-Repo](https://github.com/waveshareteam/ESP32-S3-Touch-LCD-4/tree/main/examples/arduino/libraries/GFX_Library_for_Arduino) · [Upstream](https://github.com/moononournation/Arduino_GFX) | Display (ST7701 RGB) |
| **SensorLib** | Waveshare-Stand | [Waveshare-Repo](https://github.com/waveshareteam/ESP32-S3-Touch-LCD-4/tree/main/examples/arduino/libraries/SensorLib) · [Upstream](https://github.com/lewisxhe/SensorLib) | Touch (GT911) |
| **WS_CH32_IO** | Waveshare-only | **nur** [Waveshare-Repo](https://github.com/waveshareteam/ESP32-S3-Touch-LCD-4/tree/main/examples/arduino/libraries/WS_CH32_IO) | IO-Expander (Display-Reset/Backlight) |
| **ArduinoJson** | **7.x** | [Bibliotheksverwalter](https://arduinojson.org/) | JSON-Parsing der API |

> ⚠️ **`WS_CH32_IO` gibt es NICHT im Bibliotheksverwalter** — die Suche findet
> sie nicht. Sie muss aus dem Waveshare-Repo kommen (siehe Installation unten).
> Ohne sie bleibt das Display dunkel (kein Reset/Backlight).

### Bibliotheken installieren

**Variante A — als ZIP über die IDE (empfohlen für die Waveshare-Libs):**
1. Repo-Ordner der Lib als ZIP holen (z.B. per „Code → Download ZIP" vom Repo,
   oder den einzelnen Lib-Ordner zippen).
2. Arduino IDE: **Sketch → Bibliothek einbinden → .ZIP-Bibliothek hinzufügen…**
   *(EN: Sketch → Include Library → Add .ZIP Library…)* und die ZIP wählen.

**Variante B — manuell kopieren:**
Lib-Ordner nach `Documents/Arduino/libraries/` kopieren, **genau eine Ebene tief**:
```
Documents/Arduino/libraries/
├── lvgl/
├── lv_conf.h                     ← neben (nicht in) den lvgl-Ordner!
├── GFX_Library_for_Arduino/
├── SensorLib/
│   ├── library.properties
│   └── src/TouchDrvGT911.hpp
└── WS_CH32_IO/
    ├── library.properties
    └── src/WS_CH32_IO.h
```
Häufige Fehler: Ordner eine Ebene zu tief (`libraries/SensorLib/SensorLib/…`) →
Lib wird nicht gefunden. Und: **nach dem Kopieren die IDE neu starten**, sonst
liest sie die neuen Libs nicht ein.

> 💡 Am einfachsten zuerst das Waveshare-Beispiel **`09_LVGL_Widgets`** flashen
> (Schritt 1 unten). Läuft das, sitzen `lvgl`, `lv_conf.h`, `GFX_Library_for_Arduino`,
> `SensorLib` und `WS_CH32_IO` alle korrekt — und dieser Sketch kompiliert auch.

## Einrichtung in 3 Schritten

### 1) Waveshare-Demo zum Laufen bringen
Lade das Beispiel **`09_LVGL_Widgets`** (RGB-Touch mit LVGL) aus dem Waveshare-
Repo (`examples/arduino/09_LVGL_Widgets`) und flashe es. Wenn dort die LVGL-Demo
sauber angezeigt wird und der Touch reagiert, ist die Board-Basis fertig — und
alle Bibliotheken/`lv_conf.h` sitzen richtig.

### 2) Diesen Sketch einbinden
- Ordner `smartrace_display/` mit `smartrace_display.ino` + `config.h` öffnen.
- In **`config.h`** eintragen:
  - `WIFI_SSID`, `WIFI_PASSWORD`
  - `SERVER_BASE` = Adresse deines Dashboards, z.B. `http://192.168.1.90:5000`
- **`board_init()` ist bereits ausgefüllt** — der Display-/Touch-/LVGL-Init aus
  `09_LVGL_Widgets` ist übernommen (Pins, ST7701, GT911, CH32-IO, LVGL-Tick).
  Es muss nur dasselbe Board + dieselben Libs wie in Schritt 1 aktiv sein.

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
