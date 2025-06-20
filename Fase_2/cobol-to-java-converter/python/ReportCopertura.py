import google.generativeai as genai
import argparse
import sys
import os
import tempfile
import subprocess
import shutil

API_KEY = "YOUR_API_KEY"
genai.configure(api_key=API_KEY)

def leggi_file(percorso_file):
    """Legge il contenuto di un file e lo restituisce come stringa."""
    try:
        with open(percorso_file, 'r', encoding='utf-8', errors='ignore') as f:
            return f.read()
    except FileNotFoundError:
        print(f"ERRORE: Il file '{percorso_file}' non è stato trovato.")
        exit(1)
    except Exception as e:
        print(f"ERRORE: Impossibile leggere il file '{percorso_file}'. Dettagli: {e}")
        exit(1)

def verifica_ambiente_java():
    """Verifica se l'ambiente Java è disponibile."""
    try:
        result = subprocess.run(['java', '-version'], capture_output=True, text=True)
        return result.returncode == 0
    except:
        return False

def crea_prompt_per_report(contenuto_cobol, contenuto_java, nome_file_cobol, nome_file_java):
    """Crea il prompt dettagliato per il modello di AI generativa con istruzioni per test reali."""

    return f"""
Sei un analista esperto di qualità del software e ingegnere DevOps. Il tuo compito è analizzare il codice Java fornito attraverso TESTING REALE ed ESECUZIONE EFFETTIVA, non attraverso speculazioni teoriche.

**ISTRUZIONI CRUCIALI - LEGGI ATTENTAMENTE:**

1. **NON INVENTARE DATI**: Non fornire percentuali di copertura inventate o simulate
2. **ESEGUI TEST REALI**: Devi analizzare il codice Java fornito attraverso:
   - Compilazione effettiva del codice (se possibile)
   - Analisi statica reale della complessità ciclomatica
   - Identificazione di percorsi di esecuzione basata su analisi del codice effettivo
3. **VERIFICA I RISULTATI**: Ogni metrica deve essere giustificata con evidenze concrete dal codice
4. **AMMETTI LIMITAZIONI**: Se non puoi eseguire test reali, dichiaralo esplicitamente

**METODOLOGIA RICHIESTA:**

Prima di scrivere il report, esegui questi passi:

STEP 1: ANALISI STATICA DEL CODICE JAVA
- Conta effettivamente le linee di codice eseguibili
- Identifica tutti i branch (if, switch, try-catch, loop)
- Calcola la complessità ciclomatica reale per ogni metodo
- Elenca tutti i metodi pubblici e privati

STEP 2: IDENTIFICAZIONE DEI PATH DI ESECUZIONE
- Mappa tutti i possibili percorsi di esecuzione
- Identifica condizioni che potrebbero essere difficili da testare
- Analizza le dipendenze esterne (file I/O, database, etc.)

STEP 3: ANALISI DELLA TESTABILITÀ
- Identifica metodi con alta complessità ciclomatica (>10)
- Trova codice con dipendenze hard-coded
- Rileva pattern anti-testabili (singleton, static abuse, etc.)

Il report deve essere in **italiano** e seguire questa struttura, MA SOLO con dati verificati:

---
# Report di Analisi di Copertura: Migrazione COBOL-to-Java

**File Analizzati:**
* **COBOL:** `{nome_file_cobol}`
* **Java:** `{nome_file_java}`

## 1. Riepilogo Esecutivo

**IMPORTANTE**: Inizia questa sezione dichiarando esplicitamente:
- Se hai potuto compilare/eseguire il codice Java
- Quali strumenti di analisi hai potuto utilizzare
- Quali limitazioni hai riscontrato nell'analisi

### Metriche Verificate:
* **Linee di Codice Totali:** [NUMERO REALE CONTATO]
* **Metodi Totali:** [NUMERO REALE CONTATO]
* **Complessità Ciclomatica Media:** [CALCOLATA REALMENTE]

### Aree Critiche Identificate:
[SOLO quelle effettivamente trovate nell'analisi del codice]

## 2. Analisi Dettagliata della Complessità

### Tabella Complessità per Metodo (REALE):
| Metodo | Linee di Codice | Complessità Ciclomatica | Branch Points | Note |
|--------|----------------|------------------------|---------------|------|
[INSERIRE SOLO DATI REALI CALCOLATI DAL CODICE]

### Metodi ad Alta Complessità:
[ELENCARE SOLO metodi che effettivamente hanno alta complessità nel codice fornito]

## 3. Analisi dei Percorsi di Esecuzione

### Path Coverage Analysis:
[BASATA SULL'ANALISI REALE del codice, non su supposizioni]

### Condizioni Difficili da Testare:
[ELENCARE SOLO quelle effettivamente presenti nel codice]

## 4. Raccomandazioni Basate su Evidenze

### Refactoring Necessari:
[SOLO per metodi che effettivamente presentano problemi nel codice analizzato]

### Strategie di Testing:
[SPECIFICHE per il codice analizzato, non generiche]

---

**RICORDA**: 
- Se non puoi eseguire analisi reali, DICHIARALO ESPLICITAMENTE
- Non inventare percentuali o metriche
- Basa tutto sull'analisi effettiva del codice fornito
- Se il codice non compila, analizza comunque la struttura e riporta i problemi di compilazione

**Contenuto del File COBOL da Analizzare:**
```cobol
{contenuto_cobol}
```

**Contenuto del File JAVA da Analizzare:**
```java
{contenuto_java}
```

**INIZIA L'ANALISI REALE ORA:**
"""

def genera_report(prompt):
    """Chiama l'API di Gemini per generare il contenuto del report."""
    print("Richiamo il modello di AI generativa (Gemini)... Questo potrebbe richiedere alcuni istanti.")
    generation_config = {
        "temperature": 0.1,
        "top_p": 0.9,
        "top_k": 30,
        "max_output_tokens": 25000,
        "response_mime_type": "text/plain",
    }
    model = genai.GenerativeModel(
        model_name="gemini-2.5-pro-preview-06-05",
        generation_config=generation_config,
    )

    try:
        response = model.generate_content(prompt)
        # A volte l'API può restituire blocchi di codice markdown, li puliamo
        clean_text = response.text.replace("```markdown", "").replace("```", "")
        return clean_text.strip()
    except Exception as e:
        print(f"ERRORE durante la chiamata all'API di Gemini: {e}")
        exit(1)

def salva_report(contenuto_report, nome_file_output):
    """Salva il contenuto generato in un file Markdown."""
    try:
        with open(nome_file_output, 'w', encoding='utf-8') as f:
            f.write(contenuto_report)
        print(f"Report salvato con successo nel file: '{os.path.abspath(nome_file_output)}'")
    except Exception as e:
        print(f"ERRORE: Impossibile salvare il file di report. Dettagli: {e}")
        exit(1)

def main():
    """Funzione principale per orchestrare l'esecuzione dello script."""
    parser = argparse.ArgumentParser(
    description="Genera un report di analisi di copertura REALE usando GenAI, "
                "partendo da un file COBOL e un file Java migrato. "
                "L'AI è istruita a NON inventare dati ma a eseguire analisi reali.",
    formatter_class=argparse.RawTextHelpFormatter
    )
    parser.add_argument('--cobol', type=str, default="resources/cobol/File_COBOL.cbl",
                        help='Path del file COBOL di input')
    parser.add_argument('--java', type=str, default="output/File_Java.sql",
                        help='Path del file java')
    parser.add_argument('-o', '--output', default="output/report_copertura_reale.md",
                        help="Nome del file di output per il report Markdown (default: report_copertura_reale.md)")
    args = parser.parse_args()

    print("--- Avvio Generatore di Report di Copertura REALE ---")
    print("IMPORTANTE: Il sistema è configurato per eseguire analisi reali, non simulate.")

    # Verifica ambiente
    java_disponibile = verifica_ambiente_java()
    print(f"Ambiente Java disponibile: {'Sì' if java_disponibile else 'No'}")

    print(f"1. Lettura file COBOL: '{args.cobol}'")
    codice_cobol = leggi_file(args.cobol)

    print(f"2. Lettura file Java: '{args.java}'")
    codice_java = leggi_file(args.java)

    print("3. Creazione del prompt per analisi REALE...")
    prompt_dettagliato = crea_prompt_per_report(
        codice_cobol,
        codice_java,
        os.path.basename(args.cobol),
        os.path.basename(args.java)
    )

    print("4. Esecuzione analisi reale tramite AI...")
    report_generato = genera_report(prompt_dettagliato)

    print("5. Salvataggio del report verificato...")
    salva_report(report_generato, args.output)
    print("--- Operazione completata. ---")
    print("\nATTENZIONE: Anche se l'AI è stata istruita per analisi reali,")
    print("si raccomanda sempre di verificare i risultati con strumenti dedicati come:")
    print("- JaCoCo per coverage reale")
    print("- SonarQube per analisi statica")
    print("- PMD/Checkstyle per metriche di qualità")

if __name__ == "__main__":
    # Prima di eseguire, installa le librerie necessarie con il comando:
    # pip install google-generativeai argparse
    main()


