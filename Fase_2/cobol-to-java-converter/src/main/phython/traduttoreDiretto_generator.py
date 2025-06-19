import google.generativeai as genai
import os
import sys
import argparse

API_KEY = "YOUR_API_KEY"
genai.configure(api_key=API_KEY)

def read_source_file(filepath):
    """Legge il contenuto di un file sorgente."""
    try:
        with open(filepath, 'r', encoding='utf-8') as file:
            return file.read()
    except FileNotFoundError:
        print(f"Errore: File non trovato a '{filepath}'")
        sys.exit(1)
    except Exception as e:
        print(f"Errore durante la lettura del file '{filepath}': {e}")
        sys.exit(1)

def save_java_file(java_code, output_path):
    """Salva il codice Java generato in un file .java."""
    try:
        # Pulisce il codice da eventuali blocchi di markdown che il modello potrebbe aggiungere
        if java_code.strip().startswith("```java"):
            java_code = java_code.strip()[7:]
        if java_code.strip().endswith("```"):
            java_code = java_code.strip()[:-3]
        
        # Crea la directory di output se non esiste
        output_dir = os.path.dirname(output_path)
        if output_dir and not os.path.exists(output_dir):
            os.makedirs(output_dir)
            
        with open(output_path, 'w', encoding='utf-8') as file:
            file.write(java_code.strip())
        print(f"Codice Java salvato con successo in: {output_path}")
    except Exception as e:
        print(f"Errore durante il salvataggio del file: {e}")

def translate_cobol_to_java(cobol_code):
    """
    Usa Gemini per tradurre codice sorgente COBOL in codice sorgente Java.
    """
    generation_config = {
        "temperature": 0.1,
        "top_p": 0.9,
        "top_k": 30,
        "max_output_tokens": 20000,
        "response_mime_type": "text/plain",
    }
    
    model = genai.GenerativeModel(
        model_name="gemini-2.5-pro-preview-06-05",
        generation_config=generation_config,
    )

    prompt = f"""
Sei un compilatore avanzato e un traduttore di codice sorgente da COBOL a Java. Il tuo compito è analizzare il seguente codice sorgente COBOL e tradurlo in un singolo file Java moderno, completo, leggibile e compilabile. Il codice Java generato deve essere una simulazione funzionale del programma COBOL originale.

**Codice Sorgente COBOL da Tradurre:**
```cobol
{cobol_code}
```

**Istruzioni di Traduzione Dettagliate e Obbligatorie:**

1.  **Analisi Strutturale:**
    * **IDENTIFICATION DIVISION:** Usa il `PROGRAM-ID` per definire il nome della classe Java (es. `GESTIONE-CONTI` -> `GestioneConti`).
    * **ENVIRONMENT DIVISION:** Analizza la `FILE-CONTROL` per identificare le interazioni con i file.
    * **DATA DIVISION:** Analizza la `WORKING-STORAGE SECTION` per dichiarare i campi privati della classe Java.
    * **PROCEDURE DIVISION:** Analizza i `PARAGRAPH` e traducili in metodi privati Java. Il flusso principale del programma deve essere incapsulato in un metodo `mainLogic()`.

2.  **Mappatura Tipi di Dato (dalla `DATA DIVISION`):**
    * **Nomenclatura:** Converti le variabili COBOL (es. `WS-NOME-CLIENTE`) in camelCase Java (es. `wsNomeCliente`).
    * `PIC X(n)`: Deve diventare `String`.
    * `PIC 9(n)`: Deve diventare `String` se è un ID o un input non matematico. Altrimenti, usa `int` o `long`.
    * `PIC S9(n)V99 [COMP-3]`: **OBBLIGATORIO** tradurlo in `java.math.BigDecimal` per preservare la precisione finanziaria. Inizializza a `BigDecimal.ZERO`.
    * Crea le variabili ausiliarie necessarie in Java, come `private final Scanner scanner = new Scanner(System.in);` e `private int sqlcode;`.

3.  **Traduzione della Logica (dalla `PROCEDURE DIVISION`):**
    * `PERFORM paragraph-name`: Chiama il metodo Java corrispondente (`methodName();`).
    * `PERFORM ... UNTIL ...`: Usa un ciclo `do-while` per i menu o `while` per altre condizioni.
    * `DISPLAY '...'`: Traduci con `System.out.println(...)`. Per i prompt, usa `System.out.print(...)` (derivato da `WITH NO ADVANCING`).
    * `ACCEPT var`: Traduci con `var = scanner.nextLine();`. Per i `BigDecimal`, usa `var = new BigDecimal(scanner.nextLine());`.
    * `MOVE A TO B`: Usa l'assegnazione Java (`B = A;`).
    * `IF/ELSE/END-IF`: Usa la struttura `if/else` di Java.
    * `EVALUATE/WHEN/END-EVALUATE`: Usa la struttura `switch` di Java.
    * `COMPUTE C = A + B`: Per i numeri, usa `+`. Per `BigDecimal`, usa i metodi: `c = a.add(b);` per l'addizione, `a.subtract(b);` per la sottrazione, etc.
    * `STRING ... INTO ...`: Usa `StringBuilder` o concatenazione di stringhe in Java.

4.  **Simulazione di I/O su File:**
    * Identifica le operazioni sui file come `OPEN`, `WRITE`, `CLOSE`.
    * Traduci queste operazioni usando `java.io.FileWriter` e `java.io.PrintWriter`, preferibilmente in un blocco `try-with-resources` per la gestione automatica della chiusura.

5.  **Simulazione di SQL (`EXEC SQL ... END-EXEC`):**
    * **NON generare codice JDBC.** Il programma Java deve simulare le chiamate al DB.
    * Per ogni blocco `EXEC SQL` nel codice COBOL:
        1.  Estrai la query SQL e inseriscila in un commento Java.
        2.  Simula il risultato impostando `sqlcode` (`0` per successo, `100` per non trovato, `-1` per errore generico).
        3.  Per le `SELECT`, assegna valori fittizi e realistici alle variabili di destinazione (host variables).
        4.  Per `INSERT`/`UPDATE`/`DELETE`, stampa un messaggio di log e imposta `sqlcode`.
        5.  Per i **cursori SQL**, simula un ciclo `FETCH`. Dopo l' `OPEN`, esegui un ciclo `for` o `while` per un numero limitato di volte (es. 3-4 iterazioni), popolando le variabili con dati fittizi a ogni giro. Alla fine del ciclo, imposta `sqlcode = 100` per simulare la fine dei dati.

6.  **Output Finale:**
    * Produci **esclusivamente codice Java valido**.
    * Il codice deve essere contenuto in un unico blocco, senza testo o spiegazioni esterne (tutte le note devono essere commenti Java).
    * Aggiungi JavaDoc chiari per la classe e per i metodi principali.
    * Il file `.java` generato deve essere compilabile e eseguibile immediatamente per riprodurre la logica del programma COBOL.

Procedi con la traduzione.
"""
    try:
        print("Invio della richiesta all'API di Gemini 2.5 Pro Preview per la traduzione.\nL'operazione potrebbe richiedere alcuni secondi/minuti...")
        response = model.generate_content(prompt)
        return response.text
    except Exception as e:
        print(f"Errore durante la chiamata all'API Gemini: {e}")
        return None

def main():
    """
    Funzione principale per eseguire il processo di generazione.
    Accetta file di input e output come argomenti da riga di comando.
    """

    input_cobol_path = "src/main/resources/cobol/File_COBOL.cbl"
    output_java_path = "output/CodiceDirettoGenerato.java"
    
    print(f"1. Lettura del codice sorgente COBOL da: '{input_cobol_path}'...")
    cobol_code_content = read_source_file(input_cobol_path)
    
    print("2. Inizio della traduzione da COBOL a Java...")
    generated_java_code = translate_cobol_to_java(cobol_code_content)
    
    if generated_java_code:
        print("3. Traduzione completata. Salvataggio del file Java...")
        save_java_file(generated_java_code, output_java_path)
    else:
        print("Traduzione fallita. Controlla i messaggi di errore.")

if __name__ == "__main__":
    main()
