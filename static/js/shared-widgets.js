/**
 * Gemeinsame Widget-Helfer fuer Dashboard & Leaderboard.
 * Benoetigt globale Helfer aus base.html: formatTime(), getControllerColor().
 */

/**
 * Bündelt mehrere Aufrufe innerhalb eines Frames zu EINEM Aufruf (via
 * requestAnimationFrame). Bei Event-Bursts (viele Runden schnell
 * hintereinander) wird so nur einmal pro Frame gerendert statt pro Event.
 * @param {Function} fn  Render-Funktion ohne Argumente (nutzt globalen State)
 * @returns {Function}   Geplante Variante von fn
 */
function coalesceFrame(fn) {
    let scheduled = false;
    return function () {
        if (scheduled) return;
        scheduled = true;
        requestAnimationFrame(() => { scheduled = false; fn(); });
    };
}

// =============================================================================
// Per-Widget-Zoom (eigener Prozent-Regler je Widget, ueberschreibt globalen Zoom)
// =============================================================================
const WIDGET_ZOOM_STEPS = [50, 60, 70, 80, 90, 100, 115, 130, 150, 175, 200, 250, 300, 400];

function getWidgetZoomMap(key) {
    try { return JSON.parse(localStorage.getItem(key) || '{}'); } catch (e) { return {}; }
}

function applyWidgetZoomOverride(cardEl, wid, key) {
    const map = getWidgetZoomMap(key);
    const body = cardEl.querySelector('.card-body');
    const lbl = cardEl.querySelector('.widget-zoom-lbl');
    if (map[wid]) {
        if (body) body.style.zoom = (map[wid] / 100).toString();
        if (lbl) lbl.textContent = map[wid] + '%';
    } else {
        if (body) body.style.removeProperty('zoom'); // faellt auf globalen Zoom zurueck
        if (lbl) lbl.textContent = 'Auto';
    }
}

/**
 * Fuegt einer Widget-Ecke (.widget-size-ctrl) einen Zoom-Regler (- % +) hinzu.
 * @param {Element} ctrl   das .widget-size-ctrl-Element
 * @param {Element} cardEl die Widget-Karte (enthaelt .card-body)
 * @param {string} wid     Widget-ID
 * @param {string} key     localStorage-Key der Zoom-Map (pro Seite)
 */
function addWidgetZoomControls(ctrl, cardEl, wid, key) {
    if (ctrl.querySelector('.widget-zoom-btn')) return;
    const mk = (txt, cls) => { const b = document.createElement('button'); b.type = 'button'; b.className = cls; b.textContent = txt; return b; };
    const sep = document.createElement('span'); sep.className = 'wsc-sep';
    const down = mk('−', 'widget-zoom-btn'); down.title = 'Kleiner';
    const lbl = mk('Auto', 'widget-zoom-lbl'); lbl.title = 'Auf globalen Zoom zuruecksetzen';
    const up = mk('+', 'widget-zoom-btn'); up.title = 'Groesser';

    const globalPct = () => parseInt(localStorage.getItem('sr-widget-zoom')) || 100;
    const curPct = () => getWidgetZoomMap(key)[wid] || globalPct();
    const setZoom = (pct) => {
        const map = getWidgetZoomMap(key);
        if (pct == null) delete map[wid]; else map[wid] = pct;
        localStorage.setItem(key, JSON.stringify(map));
        applyWidgetZoomOverride(cardEl, wid, key);
    };
    down.addEventListener('click', (e) => {
        e.stopPropagation();
        const c = curPct();
        const prev = [...WIDGET_ZOOM_STEPS].reverse().find(s => s < c);
        if (prev) setZoom(prev);
    });
    up.addEventListener('click', (e) => {
        e.stopPropagation();
        const c = curPct();
        const next = WIDGET_ZOOM_STEPS.find(s => s > c);
        if (next) setZoom(next);
    });
    lbl.addEventListener('click', (e) => { e.stopPropagation(); setZoom(null); });

    ctrl.appendChild(sep);
    ctrl.appendChild(down);
    ctrl.appendChild(lbl);
    ctrl.appendChild(up);
    applyWidgetZoomOverride(cardEl, wid, key);
}

// Sektor-Zeitstring ("15.234" oder "1:05.234", auch mit Komma) -> Millisekunden
function parseSectorMs(val) {
    if (!val) return null;
    const s = String(val).trim();
    if (s.includes(':')) {
        const [minPart, secPart] = s.split(':');
        const mins = parseInt(minPart, 10) || 0;
        const secs = parseFloat(secPart.replace(',', '.'));
        if (isNaN(secs)) return null;
        return (mins * 60 + secs) * 1000;
    }
    const num = parseFloat(s.replace(',', '.'));
    return isNaN(num) || num === 0 ? null : num * 1000;
}

/**
 * "Positionen" rendern: kompakte Rangliste (P1, P2, ...) mit Fahrer,
 * Controller, Runden, Bestzeit und Abstand zum Fuehrenden.
 * @param {Object} opts
 * @param {string} opts.containerId
 * @param {Object} opts.data
 * @param {Function} [opts.getLaps]
 * @param {Function} [opts.includeController]
 * @param {string} [opts.mode]  'race' (nach Runden) oder 'training' (nach Bestzeit)
 */
function renderStandings(opts) {
    const container = document.getElementById(opts.containerId);
    if (!container) return;
    const getLaps = opts.getLaps || (cd => cd.laps || []);
    const mode = opts.mode || 'race';

    const drivers = [];
    Object.entries(opts.data).forEach(([cid, cd]) => {
        if (opts.includeController && !opts.includeController(cid)) return;
        const laps = getLaps(cd);
        if (laps.length === 0) return;
        const lapNums = laps.map(l => l.lap || 0).filter(n => n > 0);
        const maxLap = lapNums.length ? Math.max(...lapNums) : 0;
        const times = laps.map(l => l.laptime_raw).filter(t => t > 0);
        const best = cd.best_time_raw || (times.length ? Math.min(...times) : null);
        const color = (cd.color && cd.color !== '#333333') ? cd.color : getControllerColor(cid);
        drivers.push({
            cid, name: cd.name || 'Fahrer ' + cid, color, laps: maxLap, best,
            retired: !!cd.retired, disqualified: !!cd.disqualified,
        });
    });

    if (drivers.length === 0) {
        container.innerHTML = '<span class="text-muted small">Warte auf Daten...</span>';
        return;
    }

    drivers.sort((a, b) => {
        if (a.disqualified !== b.disqualified) return a.disqualified ? 1 : -1;
        if (a.retired !== b.retired) return a.retired ? 1 : -1;
        if (mode === 'race' && a.laps !== b.laps) return b.laps - a.laps;
        if (a.best && b.best) return a.best - b.best;
        if (a.best) return -1;
        if (b.best) return 1;
        return 0;
    });

    const leader = drivers.find(d => !d.retired && !d.disqualified) || drivers[0];

    let html = '<div class="st-list">';
    drivers.forEach((d, i) => {
        const pos = i + 1;
        const posCls = (!d.retired && !d.disqualified)
            ? (pos === 1 ? 'gold' : pos === 2 ? 'silver' : pos === 3 ? 'bronze' : '') : '';
        let gap;
        if (d.disqualified) gap = '<span class="text-danger">DQ</span>';
        else if (d.retired) gap = '<span class="text-warning">DNF</span>';
        else if (d === leader) gap = '<span class="text-warning">' + (mode === 'training' ? 'P1' : 'Leader') + '</span>';
        else if (mode === 'race' && d.laps < leader.laps) gap = '+' + (leader.laps - d.laps) + ' Rd';
        else if (d.best && leader.best) gap = '+' + formatTime(d.best - leader.best);
        else gap = '';
        html += `<div class="st-row">
            <span class="st-pos ${posCls}">${pos}</span>
            <span class="controller-badge st-cbadge" style="background:${d.color}">C${d.cid}</span>
            <span class="st-name" style="color:${d.color}">${d.name}</span>
            <span class="st-laps">${d.laps}</span>
            <span class="st-best font-mono">${d.best ? formatTime(d.best) : '--'}</span>
            <span class="st-gap font-mono">${gap}</span>
        </div>`;
    });
    html += '</div>';
    container.innerHTML = html;
}

/**
 * "Letzte Runden" rendern: oben die aktuellste Runde prominent inkl.
 * farbiger Sektoren, darunter die davor gefahrenen Runden als Liste.
 * @param {Object} opts
 * @param {string} opts.containerId
 * @param {Object} opts.data                  Live-Daten (controller -> {laps, color, name})
 * @param {Function} [opts.getLaps]           (cd) => laps[]
 * @param {Function} [opts.includeController] (cid) => bool
 * @param {number} [opts.limit]               max. Anzahl Zeilen inkl. aktueller (Default 8)
 */
function renderRecentLaps(opts) {
    const container = document.getElementById(opts.containerId);
    if (!container) return;
    const getLaps = opts.getLaps || (cd => cd.laps || []);
    const limit = opts.limit || 8;
    const SC = (typeof SECTOR_COLORS !== 'undefined') ? SECTOR_COLORS : ['#ef5350', '#ffca28', '#42a5f5'];

    // Alle Runden aller (sichtbaren) Controller einsammeln
    const all = [];
    let seq = 0; // Einsammel-Reihenfolge als Tiebreaker (falls Zeitstempel gleich/fehlen)
    Object.entries(opts.data).forEach(([cid, cd]) => {
        if (opts.includeController && !opts.includeController(cid)) return;
        const color = (cd.color && cd.color !== '#333333') ? cd.color : getControllerColor(cid);
        const name = cd.name || 'C' + cid;
        getLaps(cd).forEach(l => {
            if (!l.laptime_raw || l.laptime_raw <= 0) return;
            all.push({
                name, color,
                lap: l.lap, tf: l.laptime_formatted || formatTime(l.laptime_raw),
                s1: l.sector_1, s2: l.sector_2, s3: l.sector_3,
                ts: Date.parse(l.timestamp) || 0, seq: seq++, pb: l.is_pb,
            });
        });
    });

    if (all.length === 0) {
        container.innerHTML = '<span class="text-muted small">Warte auf Rundendaten...</span>';
        return;
    }

    // Neueste zuerst: nach Zeitstempel, bei Gleichstand nach Einsammel-Reihenfolge
    all.sort((a, b) => (b.ts - a.ts) || (b.seq - a.seq));
    const current = all[0];
    const rest = all.slice(1, limit);

    const chip = (label, val, i) =>
        `<span class="rl-sector" style="--sc:${SC[i]}"><b>${label}</b> ${val || '--'}</span>`;

    let html = `<div class="rl-current" style="border-left:3px solid ${current.color}">
        <div class="rl-current-head">
            <span class="rl-dot" style="background:${current.color}"></span>
            <span class="rl-name" style="color:${current.color}">${current.name}</span>
            <span class="rl-lapnum">Rd ${current.lap || '-'}</span>
            <span class="rl-time font-mono">${current.tf}</span>
            ${current.pb ? '<i class="fas fa-trophy text-warning ms-1"></i>' : ''}
        </div>
        <div class="rl-sectors">${chip('S1', formatSector(current.s1), 0)}${chip('S2', formatSector(current.s2), 1)}${chip('S3', formatSector(current.s3), 2)}</div>
    </div>`;

    if (rest.length) {
        html += '<div class="rl-list">';
        rest.forEach(l => {
            html += `<div class="rl-row">
                <span class="rl-dot" style="background:${l.color}"></span>
                <span class="rl-name-sm" style="color:${l.color}">${l.name}</span>
                <span class="rl-lapnum-sm">Rd ${l.lap || '-'}</span>
                <span class="rl-time-sm font-mono">${l.tf}</span>
                <span class="rl-sectors-sm font-mono">${formatSector(l.s1)} · ${formatSector(l.s2)} · ${formatSector(l.s3)}</span>
            </div>`;
        });
        html += '</div>';
    }

    container.innerHTML = html;
}

/**
 * Rundenzeit-Heatmap rendern (fit-to-view: nur die aktuellsten Runden,
 * die in die Breite passen).
 * @param {Object} opts
 * @param {string} opts.containerId              Ziel-Container
 * @param {Object} opts.data                     Live-Daten (controller -> {laps, color, name})
 * @param {Function} opts.getLaps                (cd) => laps[]  (Session-Filter)
 * @param {Function} [opts.includeController]    (cid) => bool   (Controller-Filter)
 * @param {number} [opts.fallbackWidth]          Breite falls Container (noch) 0
 */
function renderLapHeatmap(opts) {
    const container = document.getElementById(opts.containerId);
    if (!container) return;
    const getLaps = opts.getLaps;
    const includeController = opts.includeController;
    const fallbackWidth = opts.fallbackWidth || 700;

    const driverEntries = [];
    Object.entries(opts.data).forEach(([cid, cd]) => {
        if (includeController && !includeController(cid)) return;
        const laps = getLaps(cd);
        if (!laps || laps.length === 0) return;
        const color = (cd.color && cd.color !== '#333333') ? cd.color : getControllerColor(cid);
        const sorted = [...laps].filter(l => l.lap > 0 && l.laptime_raw > 0)
            .sort((a, b) => (a.lap || 0) - (b.lap || 0));
        if (sorted.length === 0) return;
        driverEntries.push({ cid, name: cd.name || 'C' + cid, color, laps: sorted });
    });

    if (driverEntries.length === 0) {
        container.innerHTML = '<span class="text-muted small">Warte auf Rundendaten...</span>';
        return;
    }

    const allTimes = driverEntries.flatMap(d => d.laps.map(l => l.laptime_raw));
    const minTime = Math.min(...allTimes);
    const maxTime = Math.max(...allTimes);
    const median = [...allTimes].sort((a, b) => a - b)[Math.floor(allTimes.length / 2)];
    const cap = median * 1.5;

    function heatColor(ms) {
        const clamped = Math.min(ms, cap);
        const ratio = Math.min((clamped - minTime) / (cap - minTime || 1), 1);
        if (ratio <= 0.5) {
            const t = ratio * 2;
            const r = Math.round(40 + t * 215);
            const g = Math.round(200 - t * 100);
            const b = Math.round(80 - t * 50);
            return `rgb(${r},${g},${b})`;
        } else {
            const t = (ratio - 0.5) * 2;
            const r = Math.round(255 - t * 50);
            const g = Math.round(100 - t * 70);
            const b = Math.round(30);
            return `rgb(${r},${g},${b})`;
        }
    }

    const maxLapNum = Math.max(...driverEntries.flatMap(d => d.laps.map(l => l.lap)));
    // Nur so viele (aktuellste) Runden zeigen, wie in die Breite passen
    const CELL_W = 20;   // 18px Zelle + 2px Abstand
    const LABEL_W = 130; // Fahrer-Spalte
    const availWidth = (container.clientWidth || fallbackWidth) - LABEL_W;
    const fitCols = Math.max(5, Math.floor(availWidth / CELL_W));
    const startLap = Math.max(1, maxLapNum - fitCols + 1);
    const lapCols = [];
    for (let i = startLap; i <= maxLapNum; i++) lapCols.push(i);

    let html = '<div class="heatmap-scroll"><table class="heatmap-table"><thead><tr>';
    html += '<th class="heatmap-driver-col"></th>';
    lapCols.forEach(n => { html += `<th class="heatmap-lap-header">${n}</th>`; });
    html += '</tr></thead><tbody>';

    driverEntries.forEach(d => {
        html += `<tr><td class="heatmap-driver-label" style="color:${d.color}"><span class="heatmap-driver-dot" style="background:${d.color}"></span>${d.name}</td>`;
        const lapMap = {};
        d.laps.forEach(l => { lapMap[l.lap] = l; });
        lapCols.forEach(n => {
            const lap = lapMap[n];
            if (lap) {
                const bg = heatColor(lap.laptime_raw);
                html += `<td class="heatmap-cell" style="background:${bg}" title="R${n}: ${formatTime(lap.laptime_raw)}"></td>`;
            } else {
                html += '<td class="heatmap-cell heatmap-empty"></td>';
            }
        });
        html += '</tr>';
    });
    html += '</tbody></table></div>';

    const rangeNote = startLap > 1
        ? `<span class="small text-muted ms-3">Runden ${startLap}–${maxLapNum} (${maxLapNum} gesamt)</span>`
        : '';
    const legendHtml = `<div class="heatmap-legend">
        <span class="small text-muted me-2">Schnell</span>
        <div class="heatmap-legend-bar"></div>
        <span class="small text-muted ms-2">Langsam</span>
        <span class="small text-muted ms-3 font-mono">${formatTime(minTime)} - ${formatTime(Math.min(maxTime, cap))}</span>
        ${rangeNote}
    </div>`;

    container.innerHTML = legendHtml + html;
}
