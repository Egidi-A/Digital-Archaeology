# Traduttore Diretto da COBOL a Java con Supporto JDBC

Questo progetto modernizza applicativi COBOL legacy traducendoli direttamente in codice Java moderno e compilabile, con un focus sull'integrazione di operazioni di database tramite JDBC. Il processo utilizza un singolo script Python che sfrutta l'API di Google Gemini per un'analisi e una traduzione end-to-end.

## Funzionalità Chiave

- **Traduzione Diretta**: Converte un file sorgente COBOL in un singolo file `.java` senza passaggi intermedi.
- **Integrazione Database**: Analizza sia il codice COBOL che uno schema SQL fornito per generare query JDBC appropriate.
- **Modernizzazione**: Produce codice Java leggibile, orientato agli oggetti e che utilizza pratiche moderne per la gestione delle eccezioni e delle risorse.

## Prerequisiti

1.  **Python 3.x**: Con la libreria `google-generativeai` installata.
    ```bash
    pip install google-generativeai
    ```
2.  **Database PostgreSQL**: Installato, in esecuzione e accessibile.
3.  **Driver JDBC PostgreSQL**: Necessario per compilare ed eseguire il codice Java generato.
4.  **Java (JDK)**: Necessario per compilare il file `.java` di output.
5.  **API Key di Google Gemini**: Configurare la propria API key all'interno dello script `traduttoreDirettoWithSQL_generator.py`.

## Flusso di Lavoro

### Passo 1: Configurazione del Database (Esempio)

Prima di eseguire la traduzione, assicurati che il database sia pronto.

```bash
# 1. Crea il database (es. 'banca')
sudo -u postgres psql -c "CREATE DATABASE banca;"

# 2. Carica lo schema SQL nel database creato
sudo -u postgres psql -d banca -f path/to/your/bank_schema.sql

# 3. Imposta una password per l'utente postgres (se necessario)
sudo -u postgres psql -c "ALTER USER postgres PASSWORD 'password';"

# 4. Assicurati che pg_hba.conf permetta connessioni md5
# Esempio di riga in /etc/postgresql/*/main/pg_hba.conf:
# local   all   postgres   
# md5
```
### Passo 2: Esecuzione dello Script di Traduzione
Esegui lo script Python specificando il file COBOL di input, il file dello schema SQL e il nome del file Java di output.

```bash
python traduttoreDirettoWithSQL_generator.py \
    --cobol "src/main/resources/cobol/File_COBOL.cbl" \
    --sql "src/main/resources/sql/File_SQL.sql" \
    --output "output/GestioneConti.java"
```
Lo script invierà il contenuto di entrambi i file sorgente all'API di Gemini e salverà il codice Java risultante nel percorso di output specificato.

### Passo 3: Compilazione ed Esecuzione del Codice Java
Una volta generato il file .java, puoi compilarlo ed eseguirlo.

```bash
# 1. Scarica il driver JDBC di PostgreSQL, se non lo hai già
wget [https://jdbc.postgresql.org/download/postgresql-42.7.3.jar](https://jdbc.postgresql.org/download/postgresql-42.7.3.jar)

# 2. Compila il file Java generato
# Assicurati di essere nella directory radice del progetto
javac -cp .:postgresql-42.7.3.jar output/GestioneConti.java

# 3. Esegui la classe Java compilata
# NOTA: Il classpath (-cp) deve includere la directory corrente (.) e il JAR del driver
java -cp .:postgresql-42.7.3.jar GestioneConti
```
A questo punto, l'applicazione Java tradotta sarà in esecuzione e interagirà con il database PostgreSQL come definito nella logica del programma COBOL originale.

Questa riorganizzazione rende il progetto molto più snello e focalizzato, sfruttando appieno le capacità dei moderni modelli linguistici per compiti complessi di traduzione e modernizzazione del codice.