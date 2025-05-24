#!/bin/bash
# compile.sh - Script per compilare i programmi COBOL

echo "Starting COBOL compilation..."

# Directory
SRC_DIR="/cobol/src"
BIN_DIR="/cobol/bin"

# Assicurati che la directory bin esista
mkdir -p $BIN_DIR

# Funzione per compilare un programma COBOL
compile_program() {
    local source_file=$1
    local program_name=$2
    local database=$3
    
    echo "Compiling $program_name..."
    
    # Crea un file di configurazione temporaneo per la connessione al database
    cat > /tmp/${program_name}_precompile.cbl << EOF
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ${program_name}.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.
       
       PROCEDURE DIVISION.
       MAIN.
           EXEC SQL
               CONNECT TO '${database}'
               USER 'postgres'
           END-EXEC.
           
           IF SQLCODE = 0
               DISPLAY "Database connection successful"
           ELSE
               DISPLAY "Database connection failed: " SQLCODE
           END-IF.
           
           EXEC SQL
               DISCONNECT ALL
           END-EXEC.
           
           STOP RUN.
EOF
    
    # Compila con supporto PostgreSQL
    cobc -x \
        -o $BIN_DIR/$program_name \
        -L/usr/lib/x86_64-linux-gnu \
        -lpq \
        -I/usr/include/postgresql \
        $source_file \
        2>&1 | tee /tmp/${program_name}_compile.log
    
    if [ $? -eq 0 ]; then
        echo "✓ $program_name compiled successfully"
        chmod +x $BIN_DIR/$program_name
        
        # Crea wrapper script per impostare la connessione al database
        cat > $BIN_DIR/${program_name}.sh << EOF
#!/bin/bash
export PGHOST=postgres
export PGPORT=5432
export PGUSER=postgres
export PGPASSWORD=password
export PGDATABASE=$database

# Esegui il programma
$BIN_DIR/$program_name
EOF
        chmod +x $BIN_DIR/${program_name}.sh
        
    else
        echo "✗ Failed to compile $program_name"
        echo "Check /tmp/${program_name}_compile.log for details"
        return 1
    fi
}

# Modifica i file COBOL per usare la sintassi corretta di connessione
fix_connection_string() {
    local file=$1
    local database=$2
    
    # Crea una copia di backup
    cp $file ${file}.bak
    
    # Sostituisci la stringa di connessione
    sed -i "s|CONNECT TO 'postgresql://localhost/[^']*'|CONNECT TO '$database'|g" $file
    sed -i "s|USER 'postgres'.*|USER 'postgres'|g" $file
    sed -i "/USING 'password'/d" $file
}

# Prepara i file sorgenti
echo "Preparing source files..."
fix_connection_string $SRC_DIR/gestione-conti.cbl "banca"
fix_connection_string $SRC_DIR/gestione-paghe.cbl "paghe"
fix_connection_string $SRC_DIR/gestione-magazzino.cbl "magazzino"

# Compila i programmi
compile_program $SRC_DIR/gestione-conti.cbl "gestione-conti" "banca"
compile_program $SRC_DIR/gestione-paghe.cbl "gestione-paghe" "paghe"
compile_program $SRC_DIR/gestione-magazzino.cbl "gestione-magazzino" "magazzino"

echo ""
echo "Compilation complete!"
echo ""
echo "To run the programs, use:"
echo "  ./bin/gestione-conti.sh     - For bank account management"
echo "  ./bin/gestione-paghe.sh     - For payroll management"
echo "  ./bin/gestione-magazzino.sh - For warehouse management"