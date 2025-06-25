# COBOL to Java Converter

Un sistema completo per la conversione automatizzata di programmi COBOL legacy con interazioni database SQL in applicazioni Java moderne, utilizzando l'intelligenza artificiale di Google Gemini.

## Indice
- [Panoramica](#panoramica)
- [Architettura del Sistema](#architettura-del-sistema)
  - [Translator_GenAI.py](#translator_genaipython)
  - [java_to_jar.py](#java_to_jarpython)
  - [run_conversion.py](#run_conversionpython)
- [Prerequisiti](#prerequisiti)
  - [Dipendenze Python](#dipendenze-python)
  - [Dipendenze Java](#dipendenze-java)
- [Struttura Directory](#struttura-directory)
- [Guida all'Utilizzo](#guida-allutilizzo)
  - [Preparazione dei File di Input](#1-preparazione-dei-file-di-input)
  - [Esecuzione della Conversione](#2-esecuzione-della-conversione)
  - [Output Generato](#3-output-generato)
  - [Esecuzione del Programma Java](#4-esecuzione-del-programma-java)
- [Processo di Conversione Dettagliato](#processo-di-conversione-dettagliato)
  - [Preparazione](#1-preparazione)
  - [Traduzione COBOL → Java](#2-traduzione-cobol--java)
  - [Generazione Progetto Maven](#3-generazione-progetto-maven)
  - [Compilazione e Packaging](#4-compilazione-e-packaging)
- [Progetti Convertiti](#progetti-convertiti)
  - [Sistema Bancario](#bank_system_cobol---sistema-di-gestione-conti-bancari)
  - [Sistema Paghe](#payroll_system_cobol---sistema-di-gestione-paghe)
  - [Sistema Magazzino](#warehouse_system_cobol---sistema-di-gestione-magazzino)
- [Risoluzione Problemi](#risoluzione-problemi)
  - [File di Input](#errori-comuni-e-soluzioni)
  - [Errori API](#errori-comuni-e-soluzioni)
  - [Errori Compilazione](#errori-comuni-e-soluzioni)
  - [Errori Runtime](#errori-comuni-e-soluzioni)
- [Note Tecniche](#note-tecniche)
  - [Limiti della Conversione](#limiti-della-conversione)
    - [Complessità del Codice COBOL](#complessità-del-codice-cobol)
    - [Limitazioni nella Traduzione](#limitazioni-nella-traduzione)
  - [Sicurezza e Credenziali](#sicurezza-e-gestione-delle-credenziali)
    - [API Key e Credenziali](#api-key-e-credenziali)
    - [Gestione Connessioni](#gestione-delle-connessioni)
  - [Estensibilità e Personalizzazione](#estensibilità-e-personalizzazione)
    - [Estensioni Possibili](#estensioni-possibili)
    - [Personalizzazione Maven](#personalizzazione-maven)
  - [Best Practices Implementate](#best-practices-implementate)
    - [Gestione del Codice](#gestione-del-codice)
    - [Performance e Scalabilità](#performance-e-scalabilità)
  - [Requisiti di Sistema](#requisiti-di-sistema)
    - [Runtime](#runtime)
    - [Storage e Memoria](#storage-e-memoria)

## Panoramica

Questo sistema traduce automaticamente codice COBOL legacy in Java moderno, consentendo la migrazione di applicazioni mission-critical verso tecnologie contemporanee. Il sistema gestisce:

- Conversione della logica COBOL in strutture Java idiomatiche
- Traduzione di statement SQL embedded in JDBC
- Creazione di progetti Maven completi con tutte le dipendenze necessarie
- Generazione di JAR eseguibili pronti per il deployment

L'approccio basato su AI permette di ottenere un codice Java di alta qualità che preserva la logica di business originale, migliorando al contempo la manutenibilità e l'estensibilità.

## Architettura del Sistema

Il sistema è composto dai seguenti moduli principali che operano in sequenza:

### Translator_GenAI.py

   - Traduttore COBOL → Java usando Google Gemini AI
   - Analizza e interpreta il codice COBOL e lo schema SQL
   - Genera classi Java semanticamente equivalenti al codice originale

### java_to_jar.py

   - Generatore di progetti Maven e JAR
   - Crea la struttura standard di un progetto Maven
   - Configura il pom.xml con le dipendenze necessarie
   - Compila il codice e genera i file JAR

### run_conversion.py

   - Orchestratore dell'intero processo
   - Gestisce input e output
   - Coordina l'esecuzione dei moduli precedenti
   - Offre un'interfaccia a riga di comando unificata

## Prerequisiti

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

## Struttura Directory

```
cobol-to-java-converter/
├── input/                         # Directory per i file di input
│   ├── file.cbl                   # File COBOL sorgente
│   └── schema.sql                 # Schema database SQL
├── output/                        # Directory per file intermedi
├── archived/                      # Progetti convertiti archiviati
│   ├── bank_system_cobol/         # Esempio di progetto convertito (banca)
│   ├── payroll_system_cobol/      # Esempio di progetto convertito (paghe)
│   ├── payroll_system_cobol_F/    # Esempio di progetto convertito (paghe) Fallito
│   ├── warehouse_system_cobol/    # Esempio di progetto convertito (magazzino)
│   ├── warehouse_system_cobol_F1/ # Esempio di progetto convertito (magazzino) Fallito
│   └── warehouse_system_cobol_F2/ # Esempio di progetto convertito (magazzino) Fallito
├── translator_GenAI.py            # Traduttore AI
├── java_to_jar.py                 # Generatore Maven/JAR
└── run_conversion.py              # Script principale
```

## Guida all'Utilizzo

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
├── src/main/java/com/                         # Codice Java tradotto
├── src/main/resources/                        # Risorse del progetto
├── src/test/java/                             # Directory per test unitari
├── input/                                     # Copia dei file originali
├── pom.xml                                    # Configurazione Maven
├── NomeClasse-1.0.0.jar                       # JAR semplice
└── NomeClasse-1.0.0-jar-with-dependencies.jar # JAR eseguibile con dipendenze
```

### 4. Esecuzione del Programma Java

Per eseguire l'applicazione Java convertita:

```bash
cd nome-progetto
java -jar NomeClasse-1.0.0-jar-with-dependencies.jar
```

## Processo di Conversione Dettagliato

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

## Progetti Convertiti

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

## Risoluzione Problemi

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

## Note Tecniche

### Limiti della Conversione
- **Complessità del Codice COBOL**
  - Codice COBOL altamente specializzato potrebbe richiedere aggiustamenti manuali
  - La conversione è ottimizzata per COBOL strutturato; codice con molti GOTO richiede revisione
  - Statement COBOL non standard o specifici del vendor potrebbero non essere riconosciuti
  - Codice che utilizza feature mainframe-specifiche richiede riscrittura manuale

- **Limitazioni nella Traduzione**
  - La gestione delle eccezioni COBOL (ON SIZE ERROR, etc.) viene mappata su try-catch Java
  - Le variabili di gruppo COBOL vengono convertite in classi Java separate
  - I REDEFINES vengono gestiti tramite metodi di conversione espliciti
  - Le prestazioni potrebbero differire tra COBOL e Java per operazioni numeriche di precisione

### Sicurezza e Gestione delle Credenziali
- **API Key e Credenziali**
  - L'API key di Gemini è hard-coded per semplicità (da modificare in produzione)
  - Credenziali database (user="postgres", password="password") sono predefinite
  - In produzione, utilizzare:
    ```java
    DB_URL = System.getenv("DB_URL");
    DB_USER = System.getenv("DB_USER");
    DB_PASSWORD = System.getenv("DB_PASSWORD");
    ```

- **Gestione delle Connessioni**
  - Utilizzo obbligatorio di PreparedStatement per prevenire SQL injection
  - Gestione automatica della chiusura delle risorse con try-with-resources
  - Implementazione di connection pooling consigliata per deployment

### Estensibilità e Personalizzazione
- **Estensioni Possibili**
  - Supporto per altri target language (Python, C#)
  - Integrazione con altri framework Java (Spring, JPA)
  - Aggiunta di generazione test unitari automatici
  - Supporto per altri dialetti COBOL modificando il prompt

- **Personalizzazione Maven**
  - Configurazione dependencies in pom.xml
  - Modifica delle fasi di build
  - Aggiunta plugin per qualità codice
  - Customizzazione target Java version

### Best Practices Implementate
- **Gestione del Codice**
  - Ogni paragrafo COBOL diventa un metodo Java privato
  - Nomi variabili convertiti in camelCase
  - JavaDoc generato dai commenti COBOL
  - Gestione appropriata delle eccezioni SQL

- **Performance e Scalabilità**
  - Uso di StringBuilder per manipolazione stringhe
  - PreparedStatement cachati quando possibile
  - Transazioni ottimizzate per batch operation
  - Connection pooling ready

### Requisiti di Sistema
- **Runtime**
  - JRE 11+ per supporto BigDecimal avanzato
  - PostgreSQL 12+ per feature SQL moderne
  - Python 3.7+ per script di conversione
  - Maven 3.6+ per build system

- **Storage e Memoria**
  - 2GB RAM minimo consigliato per Gemini API
  - Spazio disco proporzionale al codice sorgente
  - Cache Maven locale per dipendenze

---

**Data ultima revisione**: 25 giugno 2025
