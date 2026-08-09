FROM python:3.12-slim

# Kein apt-get/curl noetig: der Healthcheck laeuft ueber Python (siehe
# docker-compose.yml) -> Build braucht keine Debian-Paketquellen.

WORKDIR /app

COPY requirements.txt .
RUN pip install --no-cache-dir -r requirements.txt

COPY . .

RUN date -Iseconds > /app/build-date.txt

EXPOSE 5000

# eventlet Worker fuer WebSocket-Support, 1 Worker (eventlet handled Concurrency)
CMD ["gunicorn", "--bind", "0.0.0.0:5000", "--worker-class", "eventlet", "--workers", "1", "--timeout", "120", "app:app"]
