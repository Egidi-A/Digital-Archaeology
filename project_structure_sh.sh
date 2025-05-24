#!/bin/bash
# setup-project.sh - Crea la struttura del progetto

echo "Creating project structure..."

# Crea le directory necessarie
mkdir -p cobol-docker-env/{src,bin,sql,scripts,reports}

# Directory principale del progetto
cd cobol-docker-env

# Copia i file nella struttura corretta
echo "Setting up project files..."

# Messaggio per l'utente
cat > README.md << 'EOF'
# COBOL Docker Environment

Ambiente Docker completo per eseguire applicazioni COBOL con PostgreSQL.

## Struttura del progetto

```
cobol-docker-env/
├── Dockerfile              # Definizione immagine Docker COBOL
├── docker-compose.yml      # Orchestrazione servizi
├── src/                    # Sorgenti COBOL
│   ├── gestione-conti.cbl
│   ├── gestione-paghe.cbl
│   └── gestione-magazzino.cbl
├── sql/                    # Script SQL
│   ├── init-databases.sql
│   ├── bank_schema.sql
│   ├── payroll_schema.sql
│   └── warehouse_schema.sql
├── scripts/                # Script di utilità
│   ├── wait-for-postgres.sh
│   ├── compile.sh
│   └── init.sh
├── bin/                    # Programmi compilati
└── reports/                # Report generati
```

## Come utilizzare

1. Costruisci e avvia i container:
   ```bash
   docker-compose up -d --build
   ```

2. Accedi al container COBOL:
   ```bash
   docker exec -it cobol_runtime bash
   ```

3. Esegui i programmi:
   ```bash
   ./bin/gestione-conti.sh
   ./bin/gestione-paghe.sh
   ./bin/gestione-magazzino.sh
   ```

4. Accedi a pgAdmin (opzionale):
   - URL: http://localhost:8080
   - Email: admin@cobol.local
   - Password: admin

5. Ferma i container:
   ```bash
   docker-compose down
   ```

## Note

- I database vengono inizializzati automaticamente al primo avvio
- I report vengono salvati nella directory `reports/`
- I log di compilazione sono in `/tmp/` nel container
EOF

# Script di inizializzazione vuoto (se necessario in futuro)
cat > scripts/init.sh << 'EOF'
#!/bin/bash
# init.sh - Script di inizializzazione aggiuntivo
echo "Initializing COBOL environment..."
EOF

chmod +x scripts/*.sh

echo "Project structure created successfully!"
echo ""
echo "Next steps:"
echo "1. Copy your COBOL source files to: src/"
echo "2. Copy the SQL schema files to: sql/"
echo "3. Copy the Docker files to the root directory"
echo "4. Run: docker-compose up -d --build"