# Dockerfile per COBOL to Java Migration Tool
FROM python:3.11-slim

# Metadata
LABEL maintainer="cobol-migration-tool"
LABEL description="COBOL to Java Migration Tool"
LABEL version="1.0.0"

# Installa dipendenze di sistema
RUN apt-get update && apt-get install -y \
    gcc \
    g++ \
    make \
    git \
    && rm -rf /var/lib/apt/lists/*

# Crea directory di lavoro
WORKDIR /app

# Copia requirements prima per sfruttare la cache Docker
COPY requirements.txt .

# Installa dipendenze Python
RUN pip install --no-cache-dir --upgrade pip && \
    pip install --no-cache-dir -r requirements.txt

# Copia il codice sorgente
COPY src/ ./src/
COPY config/ ./config/

# Crea directory per input/output
RUN mkdir -p /app/input /app/output /app/logs

# Imposta PYTHONPATH
ENV PYTHONPATH=/app/src

# Crea script di entry point
COPY docker-entrypoint.sh /usr/local/bin/
RUN chmod +x /usr/local/bin/docker-entrypoint.sh

# Imposta permessi corretti
RUN chmod -R 755 /app

# Volume per i file di input/output
VOLUME ["/app/input", "/app/output", "/app/logs"]

# Entry point
ENTRYPOINT ["docker-entrypoint.sh"]

# Default command (mostra help)
CMD ["--help"]