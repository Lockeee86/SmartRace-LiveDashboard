FROM python:3.11-slim

WORKDIR /app

# Installiere Dependencies
RUN pip install flask mysql-connector-python

# Kopiere nur requirements falls vorhanden, sonst skip
# COPY requirements.txt . 
# RUN pip install -r requirements.txt

# Kopiere den Code (wird durch Volume überschrieben, aber für ersten Build nötig)
COPY . .

# Expose Port
EXPOSE 5000

# Starte die App
CMD ["python", "app.py"]
