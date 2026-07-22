#!/bin/bash
# SmartRace Kiosk-Modus Setup fuer Raspberry Pi 5
# Ausfuehren: bash scripts/pi-kiosk-setup.sh

DASHBOARD_URL="http://localhost:5000"
WAYFIRE_CFG="$HOME/.config/wayfire.ini"

echo "========================================"
echo " SmartRace Kiosk-Modus Setup"
echo "========================================"

# wayfire.ini erstellen falls nicht vorhanden
if [ ! -f "$WAYFIRE_CFG" ]; then
    mkdir -p "$HOME/.config"
    touch "$WAYFIRE_CFG"
    echo "wayfire.ini erstellt"
fi

# Pruefen ob [autostart] Block schon existiert
if grep -q "\[autostart\]" "$WAYFIRE_CFG" 2>/dev/null; then
    # Bestehenden Block um dashboard ergaenzen (falls nicht schon drin)
    if grep -q "dashboard" "$WAYFIRE_CFG"; then
        echo "Kiosk-Eintrag existiert bereits, wird aktualisiert..."
        sed -i "s|^dashboard = .*|dashboard = chromium-browser --kiosk --noerrdialogs --disable-infobars --disable-session-crashed-bubble --start-fullscreen $DASHBOARD_URL|" "$WAYFIRE_CFG"
    else
        echo "Fuege Kiosk-Eintrag zu bestehendem [autostart] hinzu..."
        sed -i "/\[autostart\]/a dashboard = chromium-browser --kiosk --noerrdialogs --disable-infobars --disable-session-crashed-bubble --start-fullscreen $DASHBOARD_URL" "$WAYFIRE_CFG"
    fi
else
    echo "Fuege [autostart] Block hinzu..."
    cat >> "$WAYFIRE_CFG" << EOF

[autostart]
dashboard = chromium-browser --kiosk --noerrdialogs --disable-infobars --disable-session-crashed-bubble --start-fullscreen $DASHBOARD_URL
EOF
fi

# Bildschirmschoner/Standby deaktivieren
if ! grep -q "dpms_timeout" "$WAYFIRE_CFG" 2>/dev/null; then
    if grep -q "\[idle\]" "$WAYFIRE_CFG"; then
        sed -i "/\[idle\]/a dpms_timeout = -1" "$WAYFIRE_CFG"
    else
        cat >> "$WAYFIRE_CFG" << EOF

[idle]
dpms_timeout = -1
EOF
    fi
    echo "Bildschirmschoner deaktiviert"
fi

echo ""
echo "Done! wayfire.ini:"
echo "----------------------------------------"
cat "$WAYFIRE_CFG"
echo "----------------------------------------"
echo ""
echo "Neustart mit: sudo reboot"
echo "Dashboard oeffnet sich automatisch auf: $DASHBOARD_URL"
echo "SmartRace-App Datenschnittstelle auf: http://192.168.2.173:5000/api/smartrace"
