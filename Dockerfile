FROM python:3.11-slim

WORKDIR /app

COPY app.py .
COPY templates/ templates/

RUN pip install flask mysql-connector-python requests

CMD ["python", "app.py"]
