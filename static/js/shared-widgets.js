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
                ts: l.timestamp || '', pb: l.is_pb,
            });
        });
    });

    if (all.length === 0) {
        container.innerHTML = '<span class="text-muted small">Warte auf Rundendaten...</span>';
        return;
    }

    // Neueste zuerst
    all.sort((a, b) => (b.ts || '').localeCompare(a.ts || ''));
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
        <div class="rl-sectors">${chip('S1', current.s1, 0)}${chip('S2', current.s2, 1)}${chip('S3', current.s3, 2)}</div>
    </div>`;

    if (rest.length) {
        html += '<div class="rl-list">';
        rest.forEach(l => {
            html += `<div class="rl-row">
                <span class="rl-dot" style="background:${l.color}"></span>
                <span class="rl-name-sm" style="color:${l.color}">${l.name}</span>
                <span class="rl-lapnum-sm">Rd ${l.lap || '-'}</span>
                <span class="rl-time-sm font-mono">${l.tf}</span>
                <span class="rl-sectors-sm font-mono">${l.s1 || '--'} · ${l.s2 || '--'} · ${l.s3 || '--'}</span>
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
