import google.generativeai as genai
import os
import sys
import argparse

# --- CONFIGURAZIONE ---
# Assicurati di aver impostato la tua API key come variabile d'ambiente
# con il nome GOOGLE_API_KEY, oppure inseriscila direttamente qui.
# Esempio: API_KEY = "AIzaSy..."
# Configura la tua API key
API_KEY = "YOUR_API_KEY"  # Sostituisci con la tua API key
genai.configure(api_key=API_KEY)


def read_xml_file(filepath):
    """Legge il contenuto di un file di testo (XML)."""
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

def generate_java_from_cobol_asg(cobol_asg_content):
    """
    Usa Gemini per generare codice Java a partire da un ASG COBOL.
    """
    generation_config = {
        "temperature": 0.1,  # Molto bassa per coerenza e precisione nella traduzione
        "top_p": 0.9,
        "top_k": 30,
        "max_output_tokens": 16384, # Aumentato per codice complesso
        "response_mime_type": "text/plain",
    }
    
    # Si consiglia un modello avanzato per compiti di traduzione complessi
    model = genai.GenerativeModel(
        model_name="gemini-1.5-pro-latest",
        generation_config=generation_config,
    )

    prompt = f"""
Sei un traduttore esperto da COBOL a Java. Il tuo compito è tradurre il seguente XML, che rappresenta un Abstract Semantic Graph (ASG) di un programma COBOL, in un singolo file sorgente Java completo e compilabile.

**ASG COBOL da Tradurre:**
```xml
{cobol_asg_content}
```

**Istruzioni di Traduzione Dettagliate:**

1.  **Struttura della Classe Java:**
    * Genera una singola classe pubblica. Il nome della classe deve derivare dal `PROGRAM-ID` del COBOL (es. `GESTIONE-CONTI` -> `GestioneConti`).
    * Tutte le variabili definite nella `WORKING-STORAGE SECTION` devono diventare campi (`field`) privati della classe Java.
    * Tutti i `PARAGRAPH` nella `PROCEDURE DIVISION` devono diventare metodi (`method`) privati della classe Java, convertendo i nomi da KEBAB-CASE a camelCase (es. `VISUALIZZA-MENU` -> `visualizzaMenu`).
    * Crea un metodo `mainLogic()` che contenga il flusso principale del programma (il paragrafo `MAIN-LOGIC`).
    * Aggiungi un metodo `public static void main(String[] args)` che istanzia la classe e chiama `mainLogic()`.

2.  **Traduzione della `DATA DIVISION`:**
    * **Nomenclatura:** Converti i nomi delle variabili COBOL in camelCase (es. `WS-NUMERO-CONTO` -> `wsNumeroConto`).
    * **Tipi di Dati (`PIC` clause):**
        * `PIC X(...)`: Traduci in `String`.
        * `PIC 9(...)`: Traduci in `String` se è un identificativo o un input utente non calcolato. Traduci in `int` o `long` se usato per calcoli semplici.
        * `PIC S9(...)V99`: **È obbligatorio tradurre in `java.math.BigDecimal`** per garantire la precisione decimale. Inizializza questi campi con `BigDecimal.ZERO`.
    * **Variabili Ausiliarie:** Aggiungi le variabili necessarie in Java, come `private final Scanner scanner = new Scanner(System.in);` per l'input e un `int sqlcode;` per la simulazione del DB.
    * **Non generare getter e setter.** La logica è interamente contenuta nella classe.

3.  **Traduzione della `PROCEDURE DIVISION`:**
    * `PERFORM paragraph-name.`: Deve diventare una chiamata a metodo (`methodName();`).
    * `PERFORM UNTIL condition ... END-PERFORM`: Traduci in un ciclo `do-while` o `while`. Un `do-while` è perfetto per i menu.
    * `DISPLAY "messaggio"`: Usa `System.out.println("messaggio");`.
    * `DISPLAY "prompt" WITH NO ADVANCING`: Usa `System.out.print("prompt");`.
    * `ACCEPT variabile`: Usa `variabile = scanner.nextLine();`. Se la destinazione è `BigDecimal`, usa `variabile = new BigDecimal(scanner.nextLine());`.
    * `MOVE valore TO variabile`: Diventa un'assegnazione (`variabile = valore;`).
    * `IF condition ... ELSE ... END-IF`: Usa la struttura standard `if (condition) {{ ... }} else {{ ... }}`.
    * `EVALUATE ... WHEN ... END-EVALUATE`: Usa una struttura `switch`.
    * `COMPUTE C = A + B`: Per `BigDecimal`, usa `c = a.add(b);`. Per la sottrazione, `a.subtract(b);`.

4.  **Gestione Generica di File I/O:**
    * Se l'ASG COBOL contiene una sezione `FILE-CONTROL`, il programma interagisce con dei file.
    * Traduci la definizione del file (`SELECT ... ASSIGN TO ...`) e i relativi comandi di I/O.
    * I comandi `OPEN`, `WRITE` e `CLOSE` devono essere tradotti usando le classi Java appropriate come `java.io.FileWriter` e `java.io.PrintWriter`, preferibilmente all'interno di un blocco `try-with-resources` per una gestione sicura delle risorse.

5.  **Gestione SQL (`EXEC SQL`):**
    * **È l'istruzione più importante.** L'ASG COBOL contiene le query SQL complete.
    * **NON generare codice JDBC reale.** Devi **simulare** l'interazione con il database.
    * Per ogni blocco `EXEC SQL`:
        1.  Aggiungi un commento che riporta l'intento originale e la query (es. `// ASG: execSqlStatement (SELECT su cliente)`).
        2.  Simula l'esito impostando il campo intero `sqlcode`. `sqlcode = 0;` indica successo, `sqlcode = 100;` indica 'record non trovato'.
        3.  Per le `SELECT ... INTO ...`, simula il recupero dei dati assegnando valori fittizi e plausibili alle variabili Java corrispondenti (es. `wsCliNome = "Mario"; wsCliCognome = "Rossi";`).
        4.  Per `INSERT`, `UPDATE`, `DELETE`, è sufficiente impostare `sqlcode` e stampare un messaggio di conferma o errore.
        5.  **Per i Cursori (`DECLARE CURSOR`, `OPEN`, `FETCH`, `CLOSE`):** Simula il ciclo. Dopo `OPEN`, imposta un contatore. Usa un ciclo `while` o `for` per simulare i `FETCH` (es. per 3-4 volte) e per ogni fetch assegna dati fittizi e poi imposta `sqlcode = 100;` per terminare il ciclo. Infine, simula il `CLOSE`.

6.  **Qualità del Codice:**
    * Il risultato deve essere un **singolo blocco di codice Java**. Non aggiungere spiegazioni al di fuori dei commenti.
    * Includi **JavaDoc completi** per la classe e i metodi più importanti, spiegando il loro scopo e da quale parte del COBOL derivano.
    * Il codice deve essere pulito, ben formattato e **compilabile con `javac` senza alcuna modifica manuale**.

Ora, traduci l'ASG COBOL fornito, prestando la massima attenzione a ogni dettaglio delle istruzioni.
"""
    try:
        print("Invio della richiesta all'API di Gemini. L'operazione potrebbe richiedere alcuni secondi...")
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
    # --- NUOVA SEZIONE: Gestione Argomenti da Riga di Comando ---
    input_xml = "output/ASG_COBOL.xml"  # o "ExampleASGJava.xml" se hai rinominato
    output_java = "output/GeneratedJavato.java"

    print(f"1. Lettura dell'ASG COBOL\n da: {input_xml}")
    cobol_asg_content = read_xml_file(input_xml)
    if not cobol_asg_content:
        print("Errore: Il contenuto dell'ASG COBOL è vuoto o non valido.")
        sys.exit(1)
    print("ASG COBOL letto con successo. Contenuto pronto per la generazione del codice Java.")
    # --- FINE NUOVA SEZIONE ---
    # --- NUOVA SEZIONE: Generazione del Codice Java ---
    output_java_path = output_java if output_java else "output/GeneratedJava.java"
    print(f"2. Inizio della generazione del codice Java...\nOutput previsto: {output_java_path}")
    # --- FINE NUOVA SEZIONE ---    
    generated_java_code = generate_java_from_cobol_asg(cobol_asg_content)
    # --- NUOVA SEZIONE: Salvataggio del Codice Java ---
    print("3. Generazione completata. Salvataggio del file Java...")
    # --- FINE NUOVA SEZIONE ---
    # Controlla se il codice generato è valido
    if not generated_java_code:
        print("Errore: Il codice Java generato è vuoto o non valido.")
        sys.exit(1)
    # Salva il codice Java generato
    print("Codice Java generato con successo. Procedo al salvataggio...")
    # --- NUOVA SEZIONE: Salvataggio del Codice Java ---
    save_java_file(generated_java_code, output_java_path)
    # --- FINE NUOVA SEZIONE ---
    print(f"Codice Java salvato con successo in: {output_java_path}")
    # --- NUOVA SEZIONE: Controllo del Codice Generato ---
    # Controlla se il codice generato è valido
    if not generated_java_code:
        print("Errore: Il codice Java generato è vuoto o non valido.")
        sys.exit(1)
    else:
        print("Generazione del codice fallita. Controlla i messaggi di errore.")

    # --- FINE NUOVA SEZIONE ---
        
if __name__ == "__main__":
    main()
