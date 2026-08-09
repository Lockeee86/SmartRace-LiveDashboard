"""SVG-Streckenlayout server-seitig in eine Punkteliste sampeln.

Fuer das ESP32-Display: der Browser nutzt getPointAtLength(), das gibt es dort
nicht. Hier flatten wir den/die SVG-Pfad(e) (Sektoren id="sector_1/2/3") in eine
gleichmaessig ueber die Bogenlaenge verteilte Punkteliste. Reiner Python-Code,
keine externe Abhaengigkeit. Ergebnis wird pro SVG gecacht.
"""

import re
import math
import hashlib

_NUM = re.compile(r'[-+]?(?:\d*\.\d+|\d+\.?)(?:[eE][-+]?\d+)?')
_CMD = re.compile(r'([MmLlHhVvCcSsQqTtAaZz])([^MmLlHhVvCcSsQqTtAaZz]*)')
_PATH_TAG = re.compile(r'<path\b[^>]*>', re.IGNORECASE | re.DOTALL)
_STEPS = 18  # Segmente pro Bezierkurve beim Flatten

_cache = {}


def parse_sector_seconds(val):
    """Sektor-Zeitstring -> Sekunden ('15.234' oder '1:05.234', auch Komma)."""
    if not val:
        return 0.0
    s = str(val).strip().replace(',', '.')
    if ':' in s:
        mp, sp = s.split(':', 1)
        try:
            return int(mp) * 60 + float(sp)
        except ValueError:
            return 0.0
    try:
        n = float(s)
        return n if n > 0 else 0.0
    except ValueError:
        return 0.0


def _flatten(d):
    """SVG-Pfad 'd' -> dichte Punktliste [(x,y), ...] (approximiert Kurven)."""
    pts = []
    cx = cy = sx = sy = 0.0
    pcx = pcy = 0.0  # letzter Kontrollpunkt (fuer S/T)
    last_cmd = ''

    def cubic(x1, y1, x2, y2, x, y):
        for k in range(1, _STEPS + 1):
            t = k / _STEPS
            mt = 1 - t
            bx = mt*mt*mt*cx + 3*mt*mt*t*x1 + 3*mt*t*t*x2 + t*t*t*x
            by = mt*mt*mt*cy + 3*mt*mt*t*y1 + 3*mt*t*t*y2 + t*t*t*y
            pts.append((bx, by))

    def quad(x1, y1, x, y):
        for k in range(1, _STEPS + 1):
            t = k / _STEPS
            mt = 1 - t
            bx = mt*mt*cx + 2*mt*t*x1 + t*t*x
            by = mt*mt*cy + 2*mt*t*y1 + t*t*y
            pts.append((bx, by))

    for letter, argstr in _CMD.findall(d):
        nums = [float(x) for x in _NUM.findall(argstr)]
        rel = letter.islower()
        L = letter.upper()
        i = 0

        if L == 'M':
            if len(nums) >= 2:
                cx = (cx + nums[0]) if rel else nums[0]
                cy = (cy + nums[1]) if rel else nums[1]
                sx, sy = cx, cy
                pts.append((cx, cy))
                i = 2
                # weitere Paare = implizite L
                while i + 1 < len(nums):
                    cx = (cx + nums[i]) if rel else nums[i]
                    cy = (cy + nums[i+1]) if rel else nums[i+1]
                    pts.append((cx, cy))
                    i += 2
        elif L == 'L':
            while i + 1 < len(nums):
                cx = (cx + nums[i]) if rel else nums[i]
                cy = (cy + nums[i+1]) if rel else nums[i+1]
                pts.append((cx, cy))
                i += 2
        elif L == 'H':
            for n in nums:
                cx = (cx + n) if rel else n
                pts.append((cx, cy))
        elif L == 'V':
            for n in nums:
                cy = (cy + n) if rel else n
                pts.append((cx, cy))
        elif L == 'C':
            while i + 5 < len(nums):
                x1 = (cx + nums[i]) if rel else nums[i]
                y1 = (cy + nums[i+1]) if rel else nums[i+1]
                x2 = (cx + nums[i+2]) if rel else nums[i+2]
                y2 = (cy + nums[i+3]) if rel else nums[i+3]
                x = (cx + nums[i+4]) if rel else nums[i+4]
                y = (cy + nums[i+5]) if rel else nums[i+5]
                cubic(x1, y1, x2, y2, x, y)
                pcx, pcy = x2, y2
                cx, cy = x, y
                i += 6
        elif L == 'S':
            while i + 3 < len(nums):
                x1 = 2*cx - pcx if last_cmd in 'CcSs' else cx
                y1 = 2*cy - pcy if last_cmd in 'CcSs' else cy
                x2 = (cx + nums[i]) if rel else nums[i]
                y2 = (cy + nums[i+1]) if rel else nums[i+1]
                x = (cx + nums[i+2]) if rel else nums[i+2]
                y = (cy + nums[i+3]) if rel else nums[i+3]
                cubic(x1, y1, x2, y2, x, y)
                pcx, pcy = x2, y2
                cx, cy = x, y
                i += 4
        elif L == 'Q':
            while i + 3 < len(nums):
                x1 = (cx + nums[i]) if rel else nums[i]
                y1 = (cy + nums[i+1]) if rel else nums[i+1]
                x = (cx + nums[i+2]) if rel else nums[i+2]
                y = (cy + nums[i+3]) if rel else nums[i+3]
                quad(x1, y1, x, y)
                pcx, pcy = x1, y1
                cx, cy = x, y
                i += 4
        elif L == 'T':
            while i + 1 < len(nums):
                x1 = 2*cx - pcx if last_cmd in 'QqTt' else cx
                y1 = 2*cy - pcy if last_cmd in 'QqTt' else cy
                x = (cx + nums[i]) if rel else nums[i]
                y = (cy + nums[i+1]) if rel else nums[i+1]
                quad(x1, y1, x, y)
                pcx, pcy = x1, y1
                cx, cy = x, y
                i += 2
        elif L == 'A':
            # Bogen: vereinfacht als Linie zum Endpunkt (in Track-Layouts selten)
            while i + 6 < len(nums):
                x = (cx + nums[i+5]) if rel else nums[i+5]
                y = (cy + nums[i+6]) if rel else nums[i+6]
                pts.append((x, y))
                cx, cy = x, y
                i += 7
        elif L == 'Z':
            pts.append((sx, sy))
            cx, cy = sx, sy

        last_cmd = letter
    return pts


def _length(pts):
    tot = 0.0
    for a, b in zip(pts, pts[1:]):
        tot += math.hypot(b[0]-a[0], b[1]-a[1])
    return tot


def _resample(pts, n):
    """Dichte Punktliste -> n gleichmaessig ueber die Laenge verteilte Punkte."""
    if len(pts) < 2 or n < 2:
        return pts[:1] * n if pts else []
    # kumulierte Laengen
    cum = [0.0]
    for a, b in zip(pts, pts[1:]):
        cum.append(cum[-1] + math.hypot(b[0]-a[0], b[1]-a[1]))
    total = cum[-1] or 1.0
    out = []
    j = 0
    for k in range(n):
        target = total * k / (n - 1)
        while j < len(cum) - 2 and cum[j+1] < target:
            j += 1
        seg = cum[j+1] - cum[j]
        f = 0.0 if seg <= 0 else (target - cum[j]) / seg
        ax, ay = pts[j]
        bx, by = pts[j+1]
        out.append((ax + (bx-ax)*f, ay + (by-ay)*f))
    return out


def _viewbox(svg):
    m = re.search(r'viewBox\s*=\s*["\']([-\d.\seE+]+)["\']', svg)
    if m:
        p = [float(x) for x in m.group(1).split()]
        if len(p) == 4 and p[2] > 0 and p[3] > 0:
            return (p[0], p[1], p[2], p[3])
    return None


def sample_track(svg_text, total_points=240):
    """SVG -> {'vb', 'points'(0..1 norm.), 'sector_lengths', 'sector_start_idx'}.

    Bevorzugt Pfade id="sector_1/2/3" (fuer Auto-Positionen); sonst alle <path>.
    Ergebnis wird pro SVG gecacht.
    """
    if not svg_text:
        return None
    key = hashlib.md5(svg_text.encode('utf-8')).hexdigest()
    if key in _cache:
        return _cache[key]

    # Sektorpfade (nach id) sammeln
    sector_ds = []
    other_ds = []
    for tag in _PATH_TAG.findall(svg_text):
        dm = re.search(r'\bd\s*=\s*["\']([^"\']+)["\']', tag)
        if not dm:
            continue
        idm = re.search(r'\bid\s*=\s*["\']([^"\']+)["\']', tag)
        pid = idm.group(1) if idm else ''
        if pid.startswith('sector_'):
            sector_ds.append((pid, dm.group(1)))
        else:
            other_ds.append(dm.group(1))

    if sector_ds:
        sector_ds.sort(key=lambda x: x[0])
        segs = [d for _, d in sector_ds]
    elif other_ds:
        segs = other_ds
    else:
        return None

    # jeden Sektor flatten + Laenge
    dense = [_flatten(d) for d in segs]
    dense = [p for p in dense if len(p) >= 2]
    if not dense:
        return None
    lens = [_length(p) for p in dense]
    total_len = sum(lens) or 1.0

    # Punkte proportional zur Laenge verteilen, je Sektor resampeln
    points = []
    sector_start_idx = []
    sector_lengths = []
    for p, L in zip(dense, lens):
        ni = max(2, int(round(total_points * L / total_len)))
        rs = _resample(p, ni)
        if points:
            sector_start_idx.append(len(points))
        points.extend(rs)
        sector_lengths.append(L)

    # viewBox bestimmen (sonst Bounding-Box)
    vb = _viewbox(svg_text)
    if not vb:
        xs = [x for x, _ in points]
        ys = [y for _, y in points]
        vx, vy = min(xs), min(ys)
        vw = (max(xs) - vx) or 1.0
        vh = (max(ys) - vy) or 1.0
        vb = (vx, vy, vw, vh)
    vx, vy, vw, vh = vb

    norm = [((x - vx) / vw, (y - vy) / vh) for x, y in points]

    result = {
        'vb': vb,
        'points': norm,
        'sector_lengths': sector_lengths,
        'sector_start_idx': sector_start_idx,
    }
    _cache[key] = result
    return result


def t_for_sector(sample, sector_idx, progress):
    """(Sektor-Index, Fortschritt 0..1 im Sektor) -> t 0..1 auf der Gesamtstrecke."""
    sl = sample['sector_lengths']
    if not sl:
        return 0.0
    sector_idx = max(0, min(sector_idx, len(sl) - 1))
    total = sum(sl) or 1.0
    before = sum(sl[:sector_idx])
    return (before + max(0.0, min(1.0, progress)) * sl[sector_idx]) / total
