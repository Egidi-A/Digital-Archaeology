#!/bin/bash

# docker-entrypoint.sh
# Entry point script per il container Docker

set -e

echo "🐳 COBOL to Java Migration Tool - Docker Edition"
echo "================================================"

# Se non ci sono argomenti, mostra l'help
if [ $# -eq 0 ]; then
    echo "Uso: docker run -v \$(pwd)/input:/app/input -v \$(pwd)/output:/app/output migration-tool [opzioni]"
    echo ""
    echo "Esempi:"
    echo "  # Migrazione base"
    echo "  docker run -v \$(pwd)/test-files:/app/input -v \$(pwd)/output:/app/output migration-tool \\"
    echo "    --input /app/input/bank_system.cbl \\"
    echo "    --schema /app/input/bank_schema.sql \\"
    echo "    --output /app/output"
    echo ""
    echo "  # Con debug abilitato"
    echo "  docker run -v \$(pwd)/test-files:/app/input -v \$(pwd)/output:/app/output migration-tool \\"
    echo "    --input /app/input/bank_system.cbl \\"
    echo "    --schema /app/input/bank_schema.sql \\"
    echo "    --output /app/output \\"
    echo "    --debug"
    echo ""
    echo "Opzioni disponibili:"
    python /app/src/migrator.py --help
    exit 0
fi

# Mostra informazioni sull'ambiente
echo "📁 Directory di lavoro: $(pwd)"
echo "📂 Input directory: /app/input"
echo "📂 Output directory: /app/output"
echo "📂 Logs directory: /app/logs"
echo ""

# Lista file nella directory input se esiste
if [ -d "/app/input" ] && [ "$(ls -A /app/input)" ]; then
    echo "📄 File disponibili in /app/input:"
    ls -la /app/input
    echo ""
fi

# Esegui il migration tool con gli argomenti passati
echo "🚀 Avvio del migration tool..."
echo "Comando: python /app/src/migrator.py $*"
echo ""

exec python /app/src/migrator.py "$@"