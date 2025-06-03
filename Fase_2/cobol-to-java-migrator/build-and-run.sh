#!/bin/bash

# build-and-run.sh
# Script per buildare e far girare il migration tool con Docker

set -e

echo "🐳 COBOL to Java Migration Tool - Docker Setup"
echo "=============================================="

# Colori per output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Funzione per stampare messaggi colorati
print_status() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

print_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Verifica che Docker sia installato
if ! command -v docker &> /dev/null; then
    print_error "Docker non è installato o non è nel PATH"
    exit 1
fi

if ! command -v docker-compose &> /dev/null; then
    print_error "Docker Compose non è installato o non è nel PATH"
    exit 1
fi

print_status "Docker e Docker Compose trovati"

# Crea le directory necessarie
print_status "Creazione directory..."
mkdir -p test-files output logs config

# Verifica che esistano i file di test
if [ ! -f "test-files/bank_system.cbl" ] || [ ! -f "test-files/bank_schema.sql" ]; then
    print_warning "File di test non trovati. Creazione dei file di esempio..."
    
    # Crea i file di test di base
    cat > test-files/bank_system.cbl << 'EOF'
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GESTIONE-CONTI.
       AUTHOR. MIGRATION-TOOL.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-MESSAGGIO PIC X(50) VALUE 'Hello from COBOL!'.
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY WS-MESSAGGIO
           STOP RUN.
EOF

    cat > test-files/bank_schema.sql << 'EOF'
CREATE TABLE CLIENTI (
    codice_cliente CHAR(8) PRIMARY KEY,
    nome VARCHAR(50) NOT NULL,
    cognome VARCHAR(50) NOT NULL
);

CREATE TABLE CONTI (
    numero_conto CHAR(12) PRIMARY KEY,
    codice_cliente CHAR(8) NOT NULL,
    saldo DECIMAL(15,2) DEFAULT 0.00,
    FOREIGN KEY (codice_cliente) REFERENCES CLIENTI(codice_cliente)
);
EOF

    print_success "File di test creati"
fi

# Verifica il file di configurazione
if [ ! -f "config/migration-config.yaml" ]; then
    print_warning "File di configurazione non trovato. Creazione configurazione di default..."
    
    cat > config/migration-config.yaml << 'EOF'
migration:
  source_encoding: "UTF-8"
  target_java_version: "11"

java:
  package_base: "com.bank.legacy"
  use_lombok: true

spring:
  version: "2.7.0"

database:
  type: "postgresql"

docker:
  base_image: "openjdk:11-jre-slim"
  expose_port: 8080

logging:
  level: "INFO"
  file: "migration.log"
EOF

    print_success "File di configurazione creato"
fi

# Mostra le opzioni disponibili
echo ""
print_status "Opzioni disponibili:"
echo "1) Build dell'immagine Docker"
echo "2) Run con Docker Compose (raccomandato)"
echo "3) Run con Docker diretto"
echo "4) Sviluppo interattivo"
echo "5) Pulizia (rimuovi container e immagini)"

read -p "Scegli un'opzione (1-5): " choice

case $choice in
    1)
        print_status "Building Docker image..."
        docker build -t cobol-migration-tool .
        print_success "Immagine Docker creata con successo!"
        ;;
    2)
        print_status "Running con Docker Compose..."
        docker-compose up --build migrator
        ;;
    3)
        print_status "Building e running con Docker diretto..."
        docker build -t cobol-migration-tool .
        
        print_status "Running migration..."
        docker run --rm \
            -v "$(pwd)/test-files:/app/input:ro" \
            -v "$(pwd)/output:/app/output" \
            -v "$(pwd)/logs:/app/logs" \
            -v "$(pwd)/config:/app/config:ro" \
            cobol-migration-tool \
            --input /app/input/bank_system.cbl \
            --schema /app/input/bank_schema.sql \
            --output /app/output \
            --config /app/config/migration-config.yaml \
            --debug
        ;;
    4)
        print_status "Avvio ambiente di sviluppo..."
        docker-compose --profile dev up -d migrator-dev
        print_success "Container di sviluppo avviato!"
        echo ""
        echo "Per entrare nel container:"
        echo "  docker exec -it cobol-migration-dev bash"
        echo ""
        echo "Per fermare:"
        echo "  docker-compose --profile dev down"
        ;;
    5)
        print_status "Pulizia Docker..."
        docker-compose down --remove-orphans
        docker rmi cobol-migration-tool 2>/dev/null || true
        docker system prune -f
        print_success "Pulizia completata!"
        ;;
    *)
        print_error "Opzione non valida"
        exit 1
        ;;
esac

# Mostra i risultati se la migrazione è stata eseguita
if [ "$choice" == "2" ] || [ "$choice" == "3" ]; then
    echo ""
    print_status "Migrazione completata!"
    
    if [ -d "output" ] && [ "$(ls -A output)" ]; then
        print_success "File generati in output/:"
        ls -la output/
        
        echo ""
        print_status "Per visualizzare i file generati via web:"
        echo "  docker-compose --profile serve up file-server"
        echo "  Apri http://localhost:8080"
    else
        print_warning "Nessun output generato. Controlla i logs per errori."
    fi
fi

echo ""
print_success "Script completato!"