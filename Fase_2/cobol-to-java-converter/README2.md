# COBOL to Java Converter

Un sistema completo per la conversione automatizzata di programmi COBOL legacy con interazioni database SQL in applicazioni Java moderne, utilizzando l'intelligenza artificiale di Google Gemini.

## 📋 Indice
- [Panoramica](#-panoramica)
- [Architettura del Sistema](#-architettura-del-sistema)
- [Prerequisiti](#-prerequisiti)
- [Struttura Directory](#-struttura-directory)
- [Guida all'Utilizzo](#-guida-allutilizzo)
- [Processo di Conversione Dettagliato](#-processo-di-conversione-dettagliato)
- [Caratteristiche della Conversione](#-caratteristiche-della-conversione)
- [Progetti Convertiti](#-progetti-convertiti)
- [Risoluzione Problemi](#-risoluzione-problemi)
- [Note Tecniche](#-note-tecniche)

## 🎯 Panoramica

Questo sistema traduce automaticamente codice COBOL legacy in Java moderno, consentendo la migrazione di applicazioni mission-critical verso tecnologie contemporanee. Il sistema gestisce:

- Conversione della logica COBOL in strutture Java idiomatiche
- Traduzione di statement SQL embedded in JDBC
- Creazione di progetti Maven completi con tutte le dipendenze necessarie
- Generazione di JAR eseguibili pronti per il deployment

L'approccio basato su AI permette di ottenere un codice Java di alta qualità che preserva la logica di business originale, migliorando al contempo la manutenibilità e l'estensibilità.

## 🏗️ Architettura del Sistema

Il sistema è composto da tre moduli principali che operano in sequenza:

1. **`translator_GenAI.py`**
   - Traduttore COBOL → Java usando Google Gemini AI
   - Analizza e interpreta il codice COBOL e lo schema SQL
   - Genera classi Java semanticamente equivalenti al codice originale

2. **`java_to_jar.py`**
   - Generatore di progetti Maven e JAR
   - Crea la struttura standard di un progetto Maven
   - Configura il pom.xml con le dipendenze necessarie
   - Compila il codice e genera i file JAR

3. **`run_conversion.py`**
   - Orchestratore dell'intero processo
   - Gestisce input e output
   - Coordina l'esecuzione dei moduli precedenti
   - Offre un'interfaccia a riga di comando unificata

## 📋 Prerequisiti

Per utilizzare il sistema di conversione sono necessari:

- **Python 3.7 o superiore**
- **Java 11 o superiore** 
- **Maven 3.6 o superiore**
- **API Key di Google Gemini** (già configurata nel codice)
- **Connessione Internet** (per l'interazione con l'API Gemini)

### Dipendenze Python
La libreria richiesta può essere installata con pip:

```bash
pip install google-generativeai
```

### Dipendenze Java
Il sistema genera progetti Java che utilizzano le seguenti dipendenze:
- PostgreSQL JDBC Driver (per l'interazione con database)
- Java Runtime Environment (JRE) per l'esecuzione dei JAR generati

## 📁 Struttura Directory

```
cobol-to-java-converter/
├── input/                   # Directory per i file di input
│   ├── file.cbl             # File COBOL sorgente
│   └── schema.sql           # Schema database SQL
├── output/                  # Directory per file intermedi
├── archived/                # Progetti convertiti archiviati
│   ├── bank_system_cobol/   # Esempio di progetto convertito (banca)
│   ├── payroll_system_cobol/# Esempio di progetto convertito (paghe)
│   └── warehouse_system_cobol/ # Esempio di progetto convertito (magazzino)
├── translator_GenAI.py      # Traduttore AI
├── java_to_jar.py           # Generatore Maven/JAR
└── run_conversion.py        # Script principale
```

## 🚀 Guida all'Utilizzo

### 1. Preparazione dei File di Input

Inserisci nella cartella `input/`:
- **Un file COBOL** (estensione `.cbl` o `.cob`)
- **Un file SQL** con lo schema del database (estensione `.sql`)

⚠️ **Importante**: Deve esserci esattamente UN file per tipo nella cartella `input/`.

### 2. Esecuzione della Conversione

Esegui il comando principale:

```bash
python run_conversion.py
```

Per specificare un nome personalizzato per il progetto:

```bash
python run_conversion.py --project-name MioProgetto
```

### 3. Output Generato

Il sistema creerà una directory con il nome del progetto contenente:

```
nome-progetto/
├── src/main/java/com/      # Codice Java tradotto
├── src/main/resources/     # Risorse del progetto
├── src/test/java/          # Directory per test unitari
├── input/                  # Copia dei file originali
├── pom.xml                 # Configurazione Maven
├── NomeClasse-1.0.0.jar    # JAR semplice
└── NomeClasse-1.0.0-jar-with-dependencies.jar  # JAR eseguibile con dipendenze
```

### 4. Esecuzione del Programma Java

Per eseguire l'applicazione Java convertita:

```bash
cd nome-progetto
java -jar NomeClasse-1.0.0-jar-with-dependencies.jar
```

## 🔄 Processo di Conversione Dettagliato

1. **Preparazione**
   - Lo script `run_conversion.py` verifica la presenza dei file di input necessari
   - Crea la directory di output se non esiste

2. **Traduzione COBOL → Java**
   - `translator_GenAI.py` analizza il codice COBOL e lo schema SQL
   - Utilizza l'API Gemini per generare il codice Java equivalente
   - Applica trasformazioni intelligenti preservando la logica di business

3. **Generazione Progetto Maven**
   - `java_to_jar.py` crea la struttura standard del progetto Maven
   - Genera un file `pom.xml` configurato con le dipendenze necessarie
   - Copia i file di input nella cartella del progetto per riferimento

4. **Compilazione e Packaging**
   - Invoca Maven per compilare il codice Java
   - Genera due tipi di JAR:
     - JAR standard (`.jar`)
     - JAR eseguibile con tutte le dipendenze incluse (`-jar-with-dependencies.jar`)

## 🔧 Caratteristiche della Conversione

### Traduzione COBOL → Java
- Conversione di `WORKING-STORAGE SECTION` in variabili di istanza Java
- Traduzione di `PROCEDURE DIVISION` in metodi Java
- Conversione di `PARAGRAPH` in metodi Java distinti
- Mappatura dei tipi di dato COBOL ai tipi Java appropriati
- Utilizzo di `BigDecimal` per valori monetari e calcoli precisi
- Gestione delle strutture di controllo (`IF`, `PERFORM`, `UNTIL`, ecc.)

### Gestione SQL
- Conversione di statement SQL embedded in chiamate JDBC
- Configurazione automatica delle connessioni al database
- Gestione delle transazioni
- Mapping dei cursori COBOL in `ResultSet` Java

### Struttura Progetto
- Organizzazione del codice secondo le convenzioni Java standard
- Generazione di JavaDoc dai commenti COBOL
- Configurazione Maven completa con tutte le dipendenze

## 📚 Progetti Convertiti

Nella directory `archived/` sono disponibili esempi di progetti già convertiti:

1. **bank_system_cobol** - Sistema di gestione conti bancari
   - Gestione clienti e conti
   - Operazioni di deposito/prelievo
   - Generazione estratti conto

2. **payroll_system_cobol** - Sistema di gestione paghe
   - Calcolo stipendi e contributi
   - Gestione dipendenti
   - Generazione buste paga

3. **warehouse_system_cobol** - Sistema di gestione magazzino
   - Inventario prodotti
   - Gestione ordini e fornitori
   - Monitoraggio giacenze

Questi progetti possono essere usati come riferimento per comprendere il processo di conversione e il risultato finale.

## ⚠️ Risoluzione Problemi

### Errori Comuni e Soluzioni

1. **File di input non trovati**
   - Verifica che nella cartella `input/` ci sia esattamente un file COBOL e un file SQL
   - Controlla che le estensioni siano corrette (`.cbl`/`.cob` e `.sql`)

2. **Errori di connessione API**
   - Verifica la connessione Internet
   - Controlla che l'API key di Gemini sia valida

3. **Errori di compilazione Java**
   - Esamina i log di Maven per identificare problemi specifici
   - Assicurati che sia stato inserito il package statement nel codice Java generato
   - Assicuratiche il codice Java generato non abbia commenti non validi lasciati da Gemini

4. **Errori in fase di esecuzione**
   - Controlla la disponibilità del database PostgreSQL
   - Verifica le credenziali di connessione al database

## 📝 Note Tecniche

### Limiti della Conversione
- Codice COBOL altamente specializzato potrebbe richiedere aggiustamenti manuali
- Alcune funzionalità specifiche dell'ambiente mainframe potrebbero non avere un equivalente diretto in Java
- La conversione è ottimizzata per COBOL strutturato; codice legacy con molti GOTO potrebbe richiedere revisione

### Sicurezza
- L'API key di Gemini è inclusa nel codice per semplicità ma in ambiente di produzione dovrebbe essere gestita in modo più sicuro (es. variabili d'ambiente)
- I file generati ereditano le stesse caratteristiche di sicurezza del codice originale

### Estensibilità
- Il sistema può essere esteso per supportare altre destinazioni oltre a Java
- È possibile aggiungere supporto per altri dialetti COBOL modificando il prompt AI
- Il processo di generazione Maven può essere personalizzato per includere dipendenze aggiuntive

---

**Data ultima revisione**: 25 giugno 2025
