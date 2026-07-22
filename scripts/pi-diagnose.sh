#!/bin/bash
# SmartRace Pi-Diagnose — sammelt alle relevanten Crash-Infos
# Ausfuehren: bash scripts/pi-diagnose.sh

echo "========================================"
echo " SmartRace Pi-Diagnose"
echo " $(date)"
echo "========================================"

echo ""
echo "--- 1. SYSTEM-INFO ---"
cat /etc/os-release 2>/dev/null | grep PRETTY_NAME
uname -a
cat /proc/device-tree/model 2>/dev/null && echo ""

echo ""
echo "--- 2. SPANNUNGS- & THROTTLE-STATUS ---"
if command -v vcgencmd &>/dev/null; then
    THROTTLED=$(vcgencmd get_throttled)
    echo "$THROTTLED"
    VAL=$(echo "$THROTTLED" | cut -d= -f2)
    if [ "$VAL" = "0x0" ]; then
        echo "  -> OK, keine Probleme erkannt"
    else
        [ $((VAL & 0x1)) -ne 0 ]     && echo "  -> AKTUELL: Unterspannung!"
        [ $((VAL & 0x2)) -ne 0 ]     && echo "  -> AKTUELL: ARM-Frequenz gedrosselt!"
        [ $((VAL & 0x4)) -ne 0 ]     && echo "  -> AKTUELL: Gedrosselt!"
        [ $((VAL & 0x8)) -ne 0 ]     && echo "  -> AKTUELL: Temperatur-Limit!"
        [ $((VAL & 0x10000)) -ne 0 ] && echo "  -> HISTORIE: Unterspannung war aktiv"
        [ $((VAL & 0x20000)) -ne 0 ] && echo "  -> HISTORIE: ARM-Frequenz war gedrosselt"
        [ $((VAL & 0x40000)) -ne 0 ] && echo "  -> HISTORIE: War gedrosselt"
        [ $((VAL & 0x80000)) -ne 0 ] && echo "  -> HISTORIE: Temperatur-Limit war aktiv"
    fi
else
    echo "vcgencmd nicht verfuegbar"
fi

echo ""
echo "--- 3. TEMPERATUR ---"
if command -v vcgencmd &>/dev/null; then
    vcgencmd measure_temp
else
    cat /sys/class/thermal/thermal_zone0/temp 2>/dev/null | awk '{printf "temp=%.1f°C\n", $1/1000}'
fi

echo ""
echo "--- 4. RAM & SWAP ---"
free -h
echo ""
echo "Swap-Nutzung:"
swapon --show 2>/dev/null || echo "Kein Swap konfiguriert"

echo ""
echo "--- 5. SPEICHER (SSD/SD) ---"
df -h / /var/lib/docker 2>/dev/null | sort -u
echo ""
echo "I/O-Fehler:"
dmesg 2>/dev/null | grep -i -E "i/o error|ext4-fs error|ata.*error|nvme.*error|usb.*reset" | tail -10
if [ -z "$(dmesg 2>/dev/null | grep -i -E 'i/o error|ext4-fs error')" ]; then
    echo "  Keine I/O-Fehler gefunden"
fi

echo ""
echo "--- 6. DOCKER CONTAINER ---"
if command -v docker &>/dev/null; then
    echo "Container-Status:"
    docker ps -a --format "table {{.Names}}\t{{.Status}}\t{{.Size}}" 2>/dev/null
    echo ""
    echo "Ressourcen-Verbrauch:"
    docker stats --no-stream --format "table {{.Name}}\t{{.CPUPerc}}\t{{.MemUsage}}\t{{.MemPerc}}" 2>/dev/null
    echo ""
    echo "Container-Neustarts:"
    docker inspect --format='{{.Name}}: Restarts={{.RestartCount}}, LastExit={{.State.ExitCode}}' $(docker ps -aq) 2>/dev/null
    echo ""
    echo "Letzte Docker-Logs (web):"
    docker logs --tail 30 $(docker ps -aqf "name=web" 2>/dev/null) 2>&1 | tail -15
    echo ""
    echo "Letzte Docker-Logs (db):"
    docker logs --tail 30 $(docker ps -aqf "name=db" 2>/dev/null) 2>&1 | tail -15
else
    echo "Docker nicht installiert"
fi

echo ""
echo "--- 7. OOM-KILLER (Speicher-Kills) ---"
journalctl -k -b 0 --no-pager 2>/dev/null | grep -i "oom\|killed process\|out of memory" | tail -10
if [ -z "$(journalctl -k -b 0 --no-pager 2>/dev/null | grep -i 'oom\|killed process')" ]; then
    echo "  Kein OOM-Kill im aktuellen Boot"
fi
echo ""
echo "Vorheriger Boot (vor dem Crash):"
journalctl -k -b -1 --no-pager 2>/dev/null | grep -i "oom\|killed process\|out of memory" | tail -10
if [ -z "$(journalctl -k -b -1 --no-pager 2>/dev/null | grep -i 'oom\|killed process')" ]; then
    echo "  Kein OOM-Kill im vorherigen Boot (oder Logs nicht verfuegbar)"
fi

echo ""
echo "--- 8. KERNEL-FEHLER (vorheriger Boot) ---"
journalctl -b -1 -p err --no-pager 2>/dev/null | tail -20
if [ -z "$(journalctl -b -1 -p err --no-pager 2>/dev/null)" ]; then
    echo "  Keine Fehler-Logs vom vorherigen Boot (evtl. persistent logging deaktiviert)"
    echo "  Tipp: sudo mkdir -p /var/log/journal && sudo systemctl restart systemd-journald"
fi

echo ""
echo "--- 9. USB/SSD-POWER ---"
lsusb 2>/dev/null | head -10
echo ""
echo "Angeschlossene Blockgeraete:"
lsblk -o NAME,SIZE,TYPE,MOUNTPOINT,MODEL 2>/dev/null

echo ""
echo "--- 10. NETZTEIL-EMPFEHLUNG ---"
echo "Pi 5 benoetigt: Offizielles 27W USB-C Netzteil (5.1V/5A)"
echo "Mit SSD: Mindestens 5A empfohlen, USB-PD faehig"
echo ""
echo "========================================"
echo " Diagnose abgeschlossen"
echo " Schick mir die Ausgabe und ich analysiere sie"
echo "========================================"
