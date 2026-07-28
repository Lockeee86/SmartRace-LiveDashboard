# SmartRace ESP32 Display

Ein physisches Renn-Display für das SmartRace LiveDashboard:
**ESP32-S3-Touch-LCD-4** (Waveshare, 4″ Touch, 480×480) zeigt die Live-
Rundenzeiten eines Controllers, die letzten 10 Runden mit Sektoren, Bestzeit +
Delta und Position. Unten ein fester Touch-Picker **C1–C6**.

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
2. Board wählen wie im Waveshare-Wiki angegeben (i.d.R. *ESP32S3 Dev Module*),
   **PSRAM: OPI aktivieren**, passende Flash-/Partition-Einstellungen.
3. Bibliotheken (Bibliotheksverwalter):
   - **lvgl** 8.3.x
   - **ArduinoJson** 7.x
   - Waveshare **Display-/Touch-Treiber** (aus deren Demo/Wiki)

## Einrichtung in 3 Schritten

### 1) Waveshare-Demo zum Laufen bringen
Lade zuerst das **Arduino-LVGL-Demo für das ESP32-S3-Touch-LCD-4** aus dem
Waveshare-Wiki und flashe es. Wenn dort ein LVGL-Beispiel (Buttons/Slider)
sauber angezeigt wird und der Touch funktioniert, ist die Board-Basis fertig.

Kopiere die passende **`lv_conf.h`** aus dem Demo in dein Projekt.

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
- LVGL-**Version**: der Sketch ist für **LVGL 8.3** geschrieben. Nutzt dein
  Waveshare-Demo LVGL 9, unterscheiden sich einige API-Aufrufe (`lv_btnmatrix_*`,
  `lv_obj_align`, Event-API) — dann diese an die 9er-API anpassen.
- Getestet werden konnte hier nur der **Server-Teil** (die Endpunkte). Den
  Firmware-Teil bitte auf der echten Hardware verifizieren — melde dich, wenn
  beim Kompilieren/Anzeigen etwas hakt, dann passen wir es an.
