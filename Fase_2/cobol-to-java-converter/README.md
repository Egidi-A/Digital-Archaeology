# COBOL to Java Converter

Un sistema automatizzato per convertire programmi COBOL con interazioni database SQL in applicazioni Java moderne utilizzando l'intelligenza artificiale di Google Gemini.

## 🎯 Panoramica

Questo sistema traduce automaticamente codice COBOL legacy in Java moderno, gestendo:
- Conversione della logica COBOL in strutture Java idiomatiche
- Traduzione di statement SQL embedded in JDBC
- Creazione di progetti Maven completi
- Generazione di JAR eseguibili con tutte le dipendenze

## 🏗️ Architettura

Il sistema è composto da tre moduli principali:

1. **`Translator_GenAI.py`** - Traduttore COBOL → Java usando Gemini AI
2. **`java_to_jar.py`** - Generatore di progetti Maven e JAR
3. **`run_conversion.py`** - Orchestratore del processo completo

## 📋 Prerequisiti

- **Python 3.7+**
- **Java 11+** 
- **Maven 3.6+**
- **API Key di Google Gemini** (già configurata nel codice)
- Libreria Python: `google-generativeai`

### Installazione dipendenze Python

```bash
pip install google-generativeai
```

## 📁 Struttura Directory

```
cobol-to-java-converter/
├── input/                  # Directory per i file di input
│   ├── file.cbl           # File COBOL sorgente
│   └── schema.sql         # Schema database SQL
├── output/                 # Directory per file intermedi
├── Translator_GenAI.py     # Traduttore AI
├── java_to_jar.py         # Generatore Maven/JAR
└── run_conversion.py      # Script principale
```

## 🚀 Come Usare il Sistema

### 1. Preparazione dei File

Inserisci nella cartella `input/`:
- **Un file COBOL** (estensione `.cbl` o `.cob`)
- **Un file SQL** con lo schema del database (estensione `.sql`)

⚠️ **Importante**: Deve esserci esattamente UN file per tipo nella cartella `input/`.

### 2. Esecuzione della Conversione

Esegui il comando principale:

```bash
python run_conversion.py
```

Opzionalmente, puoi specificare un nome personalizzato per il progetto:

```bash
python run_conversion.py --project-name MioProgetto
```

### 3. Output Generato

Il sistema creerà:
```
nome-progetto/
├── src/main/java/com/      # Codice Java tradotto
├── input/                  # Copia dei file originali
├── pom.xml                 # Configurazione Maven
├── NomeClasse-1.0.0.jar    # JAR semplice
└── NomeClasse-1.0.0-jar-with-dependencies.jar  # JAR eseguibile
```

## 🎮 Esecuzione del Programma Java

Dopo la conversione:

```bash
cd nome-progetto
java -jar NomeClasse-1.0.0-jar-with-dependencies.jar
```

## 🔧 Funzionalità Chiave

### Traduzione COBOL → Java
- Conversione di `WORKING-STORAGE` in variabili di istanza Java
- Traduzione di `PARAGRAPH` in metodi Java
- Mappatura dei tipi di dato COBOL ai tipi Java appropriati
- Gestione di `BigDecimal` per valori monetari

### Gestione SQL
- Conversione di `EXEC SQL` in JDBC `PreparedStatement`
- Gestione automatica di transazioni con commit/rollback
- Supporto per cursori SQL
- Gestione corretta dei valori NULL

### Configurazione Database
Il sistema configura automaticamente la connessione PostgreSQL:
- URL: `jdbc:postgresql://localhost:5432/[nome_database]`
- User: `postgres`
- Password: `password`

## 📝 Esempio di Conversione

**COBOL Input:**
```cobol
EXEC SQL
    SELECT NOME, SALDO 
    INTO :WS-NOME, :WS-SALDO
    FROM CLIENTI
    WHERE ID = :WS-ID
END-EXEC.
```

**Java Output:**
```java
try (PreparedStatement stmt = connection.prepareStatement(
    "SELECT NOME, SALDO FROM CLIENTI WHERE ID = ?")) {
    stmt.setString(1, wsId);
    ResultSet rs = stmt.executeQuery();
    if (rs.next()) {
        wsNome = rs.getString("NOME");
        wsSaldo = rs.getBigDecimal("SALDO");
    }
}
```

## 🛠️ Personalizzazione

### API Key Gemini
L'API key è già configurata nei file. Per usare la tua chiave, modifica la variabile `API_KEY` in:
- `Translator_GenAI.py`
- `java_to_jar.py`

### Configurazione Database
Per modificare i parametri di connessione, edita la sezione di configurazione in `Translator_GenAI.py`.

## ⚠️ Limitazioni

- Supporta solo PostgreSQL (estendibile ad altri DB)
- Richiede che il codice COBOL sia ben strutturato
- La qualità della traduzione dipende dalla complessità del codice sorgente
- Non gestisce automaticamente stored procedures o trigger

## 🐛 Troubleshooting

**Errore: "Più file COBOL/SQL trovati"**
- Assicurati di avere solo un file `.cbl` e un file `.sql` nella cartella `input/`

**Errore di compilazione Maven**
- Verifica che Java e Maven siano installati correttamente
- Controlla che il codice Java generato non abbia errori di sintassi

**Errore di connessione database**
- Verifica che PostgreSQL sia in esecuzione
- Controlla le credenziali di accesso nel codice generato

## 📄 Note

Il sistema utilizza Google Gemini AI per l'analisi e la traduzione del codice. La qualità della traduzione può variare in base alla complessità del codice COBOL originale. Si consiglia sempre di rivedere e testare il codice Java generato prima dell'uso in produzione.

- [ ] -- api key non esplicita
- [ ] -- readme
- [ ] -- gestione di quando non funziona
- [ ] -- doppia cartella input
- [ ] -- cartella creata automaticamente non va in output
- [ ] -- con un modello minore, funziona?
