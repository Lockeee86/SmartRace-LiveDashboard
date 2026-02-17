/**
 * TrackPositionMap - Zeigt Fahrerposition auf dem Strecken-SVG
 *
 * Berechnet die geschaetzte Position jedes Fahrers anhand:
 * - Durchschnittlicher Sektorzeiten (sector_1, sector_2, sector_3)
 * - Verstrichener Zeit seit letzter Rundenzeit
 * - SVG-Pfad getPointAtLength() fuer exakte Koordinaten
 */
class TrackPositionMap {
    /**
     * @param {string} containerId - ID des Containers mit dem Track-SVG
     * @param {Function} getDataFn - Gibt aktuelle Live-Daten zurueck (gleiche Struktur wie /api/live-data)
     */
    constructor(containerId, getDataFn) {
        this.containerId = containerId;
        this.getData = getDataFn;
        this.svg = null;
        this.sectorPaths = [];
        this.sectorLengths = [];
        this.dotsGroup = null;
        this.intervalId = null;
    }

    init() {
        const container = document.getElementById(this.containerId);
        if (!container) return false;

        this.svg = container.querySelector('svg');
        if (!this.svg) return false;

        // Sektor-Pfade finden (id="sector_1", "sector_2", "sector_3")
        this.sectorPaths = [];
        this.sectorLengths = [];
        for (let i = 1; i <= 3; i++) {
            const path = this.svg.getElementById('sector_' + i);
            if (path) {
                this.sectorPaths.push(path);
                this.sectorLengths.push(path.getTotalLength());
            }
        }

        if (this.sectorPaths.length === 0) return false;

        // Overlay-Gruppe fuer Punkte erstellen
        this.dotsGroup = document.createElementNS('http://www.w3.org/2000/svg', 'g');
        this.dotsGroup.setAttribute('id', 'driver-dots');
        this.svg.appendChild(this.dotsGroup);

        // Update-Intervall starten (jede Sekunde)
        this.update();
        this.intervalId = setInterval(() => this.update(), 1000);

        return true;
    }

    destroy() {
        if (this.intervalId) {
            clearInterval(this.intervalId);
            this.intervalId = null;
        }
        if (this.dotsGroup) {
            this.dotsGroup.remove();
            this.dotsGroup = null;
        }
    }

    /**
     * Sektor-Zeitstring parsen (z.B. "15.234" oder "1:05.234")
     */
    static parseSectorTime(val) {
        if (!val) return 0;
        const s = String(val).trim();
        // m:ss.xxx Format
        const match = s.match(/^(\d+):(\d+\.?\d*)$/);
        if (match) return parseFloat(match[1]) * 60 + parseFloat(match[2]);
        // Reiner Zahlenwert (Sekunden)
        const num = parseFloat(s);
        return (!isNaN(num) && num > 0) ? num : 0;
    }

    update() {
        const data = this.getData();
        if (!data || !this.dotsGroup) return;

        this.dotsGroup.innerHTML = '';
        const now = Date.now();

        // Viewbox fuer Skalierung lesen
        const vb = this.svg.viewBox.baseVal;
        const scale = Math.max(vb.width, vb.height) || 3000;
        const dotR = scale * 0.022;
        const fontSize = scale * 0.02;
        const strokeW = scale * 0.005;
        const labelOffset = dotR + fontSize * 0.6;

        // Fahrerindex fuer Label-Offset (verhindert Ueberlappung)
        let driverIdx = 0;

        Object.entries(data).forEach(([cid, cd]) => {
            if (cd.retired || cd.disqualified) return;
            if (!cd.laps || cd.laps.length === 0) return;

            const color = cd.color || (typeof getControllerColor === 'function' ? getControllerColor(cid) : '#888');
            const name = cd.name || 'C' + cid;

            // Runden mit gueltigen Sektordaten filtern
            const sectorLaps = cd.laps.filter(l => {
                const s1 = TrackPositionMap.parseSectorTime(l.sector_1);
                const s2 = TrackPositionMap.parseSectorTime(l.sector_2);
                const s3 = TrackPositionMap.parseSectorTime(l.sector_3);
                return s1 > 0 && s2 > 0 && s3 > 0;
            });

            if (sectorLaps.length === 0) return;

            // Durchschnittliche Sektorzeiten berechnen
            const avgS1 = sectorLaps.reduce((s, l) => s + TrackPositionMap.parseSectorTime(l.sector_1), 0) / sectorLaps.length;
            const avgS2 = sectorLaps.reduce((s, l) => s + TrackPositionMap.parseSectorTime(l.sector_2), 0) / sectorLaps.length;
            const avgS3 = sectorLaps.reduce((s, l) => s + TrackPositionMap.parseSectorTime(l.sector_3), 0) / sectorLaps.length;
            const totalAvg = avgS1 + avgS2 + avgS3;

            if (totalAvg <= 0) return;

            // Sektorzeiten-Verhaeltnis fuer Pfadlaenge
            const sectorFractions = [avgS1 / totalAvg, avgS2 / totalAvg, avgS3 / totalAvg];

            // Letzter Rundenzeit-Timestamp
            const sorted = [...cd.laps]
                .filter(l => l.timestamp)
                .sort((a, b) => (b.timestamp || '').localeCompare(a.timestamp || ''));
            const lastLap = sorted[0];
            if (!lastLap || !lastLap.timestamp) return;

            // Verstrichene Zeit seit letzter Runde (in Sekunden)
            const elapsed = (now - new Date(lastLap.timestamp).getTime()) / 1000;
            // Position innerhalb einer Runde (zyklisch)
            const posInLap = ((elapsed % totalAvg) + totalAvg) % totalAvg;

            // Sektor und Fortschritt bestimmen
            let sectorIdx, progress;
            if (posInLap < avgS1) {
                sectorIdx = 0;
                progress = posInLap / avgS1;
            } else if (posInLap < avgS1 + avgS2) {
                sectorIdx = 1;
                progress = (posInLap - avgS1) / avgS2;
            } else {
                sectorIdx = 2;
                progress = (posInLap - avgS1 - avgS2) / avgS3;
            }

            progress = Math.max(0, Math.min(0.999, progress));

            if (sectorIdx >= this.sectorPaths.length) return;

            // Punkt auf SVG-Pfad berechnen
            const path = this.sectorPaths[sectorIdx];
            const pathLen = this.sectorLengths[sectorIdx];
            const point = path.getPointAtLength(progress * pathLen);

            // Farbiger Punkt
            const circle = document.createElementNS('http://www.w3.org/2000/svg', 'circle');
            circle.setAttribute('cx', point.x);
            circle.setAttribute('cy', point.y);
            circle.setAttribute('r', dotR);
            circle.setAttribute('fill', color);
            circle.setAttribute('stroke', '#ffffff');
            circle.setAttribute('stroke-width', strokeW);
            circle.setAttribute('opacity', '0.95');
            this.dotsGroup.appendChild(circle);

            // Fahrername-Label (versetzt um Ueberlappung zu vermeiden)
            const offsetDir = (driverIdx % 2 === 0) ? -1 : 1;
            const text = document.createElementNS('http://www.w3.org/2000/svg', 'text');
            text.setAttribute('x', point.x);
            text.setAttribute('y', point.y + (labelOffset * offsetDir));
            text.setAttribute('text-anchor', 'middle');
            text.setAttribute('dominant-baseline', offsetDir === -1 ? 'auto' : 'hanging');
            text.setAttribute('fill', '#ffffff');
            text.setAttribute('font-size', fontSize);
            text.setAttribute('font-weight', 'bold');
            text.setAttribute('paint-order', 'stroke');
            text.setAttribute('stroke', '#000000');
            text.setAttribute('stroke-width', strokeW * 0.8);
            // Kurzname (Vorname oder Controller-ID)
            const shortName = name.includes(' ') ? name.split(' ')[0] : name;
            text.textContent = shortName;
            this.dotsGroup.appendChild(text);

            driverIdx++;
        });
    }
}
