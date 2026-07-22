FROM python:3.12-slim

ARG GIT_HASH=unknown
ARG GIT_BRANCH=unknown
ARG BUILD_DATE=unknown

RUN apt-get update && apt-get install -y --no-install-recommends curl \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY requirements.txt .
RUN pip install --no-cache-dir -r requirements.txt

COPY . .

RUN echo "{\"git_hash\": \"${GIT_HASH}\", \"git_branch\": \"${GIT_BRANCH}\", \"build_date\": \"${BUILD_DATE}\"}" > /app/build-info.json

EXPOSE 5000

# eventlet Worker fuer WebSocket-Support, 1 Worker (eventlet handled Concurrency)
CMD ["gunicorn", "--bind", "0.0.0.0:5000", "--worker-class", "eventlet", "--workers", "1", "--timeout", "120", "app:app"]
