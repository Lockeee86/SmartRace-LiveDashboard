#!/bin/bash
# SmartRace Dashboard bauen mit Git-Versionsinformationen
# Ausfuehren: bash scripts/build.sh

export GIT_HASH=$(git rev-parse --short HEAD 2>/dev/null || echo "unknown")
export GIT_BRANCH=$(git rev-parse --abbrev-ref HEAD 2>/dev/null || echo "unknown")
export BUILD_DATE=$(date -Iseconds)

echo "Building SmartRace Dashboard..."
echo "  Version: $(cat VERSION 2>/dev/null || echo '?')"
echo "  Commit:  $GIT_HASH ($GIT_BRANCH)"
echo "  Date:    $BUILD_DATE"
echo ""

docker compose build --build-arg GIT_HASH="$GIT_HASH" --build-arg GIT_BRANCH="$GIT_BRANCH" --build-arg BUILD_DATE="$BUILD_DATE"
docker compose up -d

echo ""
echo "Done! Dashboard: http://$(hostname -I | awk '{print $1}'):5000"
