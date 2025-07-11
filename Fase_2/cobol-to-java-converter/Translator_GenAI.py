import google.generativeai as genai
import os
import sys
import argparse

API_KEY = "YOUR_API_KEY"
genai.configure(api_key=API_KEY)

# --- FUNZIONI HELPER ---
def read_source_file(filepath):
    """Legge il contenuto di un file sorgente."""
    try:
        with open(filepath, 'r', encoding='utf-8') as file:
            return file.read()
    except FileNotFoundError:
        print(f"    ❌ Errore: File non trovato '{filepath}'", file=sys.stderr)
        return None
    except Exception as e:
        print(f"    ❌ Errore durante la lettura del file '{filepath}': {e}", file=sys.stderr)
        return None

def save_java_file(java_code, output_path):
    """Salva il codice Java generato in un file .java."""
    try:
        # Pulisce il codice da eventuali blocchi di markdown
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
        print(f"    ✓ File Java salvato: {output_path}")
    except Exception as e:
        print(f"    ❌ Errore durante il salvataggio: {e}", file=sys.stderr)

def translate_cobol_to_java_with_jdbc(cobol_code, sql_schema=None):
    """
    Usa Gemini per tradurre codice sorgente COBOL in codice sorgente Java con supporto JDBC.
    """
    generation_config = {
        "temperature": 0.1,
        "top_p": 0.9,
        "top_k": 20,
        "max_output_tokens": 25000,
        "response_mime_type": "text/plain",
    }
    
    model = genai.GenerativeModel(
        model_name="gemini-2.5-pro-preview-06-05",
        generation_config=generation_config,
    )

    # Sezione schema SQL se disponibile
    schema_section = ""
    if sql_schema:
        schema_section = f"""
**Schema Database SQL fornito:**
```sql
{sql_schema}
```
**IMPORTANTE - Analizza attentamente lo schema SQL per:**
- Identificare quali campi hanno vincoli NOT NULL e quali possono accettare NULL
- Comprendere le relazioni di foreign key tra le tabelle
- Notare i tipi di dati (CHAR, VARCHAR, etc.) e le loro dimensioni
- Identificare le tabelle che hanno campi opzionali (che possono essere NULL)

"""

    prompt = f"""
Sei un compilatore avanzato e un traduttore di codice sorgente da COBOL a Java. Il tuo compito è analizzare il seguente codice sorgente COBOL e tradurlo in un singolo file Java moderno, completo, leggibile e compilabile. Il codice Java generato deve utilizzare JDBC per implementare le operazioni SQL presenti nel programma COBOL.

**Codice Sorgente COBOL da Tradurre:**
```cobol
{cobol_code}
```

{schema_section}

**Istruzioni di Traduzione Dettagliate e Obbligatorie:**

1. **Analisi Strutturale:**
   * **IDENTIFICATION DIVISION:** Usa il `PROGRAM-ID` per definire il nome della classe Java (es. `GESTIONE-CONTI` -> `GestioneConti`).
   * **ENVIRONMENT DIVISION:** Analizza la `FILE-CONTROL` per identificare le interazioni con i file.
   * **DATA DIVISION:** Analizza la `WORKING-STORAGE SECTION` per dichiarare i campi privati della classe Java.
   * **PROCEDURE DIVISION:** Analizza i `PARAGRAPH` e traducili in metodi privati Java. Il flusso principale del programma deve essere incapsulato in un metodo `mainLogic()`.

2. **Configurazione Database:**
   * Crea costanti per la configurazione del database PostgreSQL:
     - `DB_URL = "jdbc:postgresql://localhost:5432/banca"` - esattamente così ma al posto di banca metti il nome del file SQL
     - `DB_USER = "postgres"` - esattmente così
     - `DB_PASSWORD = "password"` - esattamente così
   * Dichiara un campo `private Connection connection;`
   * Implementa metodi `connectDatabase()` e `disconnectDatabase()` usando JDBC
   * Aggiungi sempre `import java.sql.Types;` per gestire i valori NULL


3. **Mappatura Tipi di Dato (dalla `DATA DIVISION`):**
   * **Nomenclatura:** Converti le variabili COBOL (es. `WS-NOME-CLIENTE`) in camelCase Java (es. `wsNomeCliente`).
   * `PIC X(n)`: Deve diventare `String`.
   * `PIC 9(n)`: Deve diventare `String` se è un ID o un input non matematico. Altrimenti, usa `int` o `long`.
   * `PIC S9(n)V99 [COMP-3]`: **OBBLIGATORIO** tradurlo in `java.math.BigDecimal` per preservare la precisione finanziaria.
   * Per i campi SQL host variables, usa i tipi Java appropriati per mappare i tipi di colonna del database.
   * Inizializza SEMPRE le variabili String a "" (stringa vuota) e non a null

4. **Traduzione SQL con JDBC:**
   * **EXEC SQL CONNECT**: Traduci in connessione JDBC usando `DriverManager.getConnection()`
   * **EXEC SQL SELECT**: Usa `PreparedStatement` con parametri posizionali `?`
   * **EXEC SQL INSERT/UPDATE/DELETE**: Usa `PreparedStatement.executeUpdate()`
   * **GESTIONE VALORI NULL**
     - Per OGNI PreparedStatement che inserisce dati in campi che possono essere NULL
     - Questo è CRITICO per campi che hanno foreign key ma possono essere NULL
     - Identifica nello schema SQL quali campi hanno foreign key ma NON sono NOT NULL
   * **Cursori SQL**: 
     - `DECLARE CURSOR`: Dichiara il PreparedStatement come campo della classe
     - `OPEN CURSOR`: Esegui `statement.executeQuery()` e salva il `ResultSet`
     - `FETCH`: Usa `resultSet.next()` e estrai i dati con `resultSet.getXXX()`
     - `CLOSE CURSOR`: Chiudi sia il `ResultSet` che il `PreparedStatement`
   * **Gestione SQLCODE**: Dopo ogni operazione SQL, cattura le eccezioni e imposta `sqlcode`:
     - `0` per successo
     - `100` per nessun dato trovato (SQLException con SQLState "02000")
     - Codice errore negativo per altri errori

5. **Traduzione della Logica (dalla `PROCEDURE DIVISION`):**
   * `PERFORM paragraph-name`: Chiama il metodo Java corrispondente (`methodName();`).
   * `PERFORM ... UNTIL ...`: Usa un ciclo `do-while` per i menu o `while` per altre condizioni.
   * `DISPLAY '...'`: Traduci con `System.out.println(...)`. Per i prompt, usa `System.out.print(...)`.
   * `ACCEPT var`: Traduci con `var = scanner.nextLine();`. Per i `BigDecimal`, usa `var = new BigDecimal(scanner.nextLine());`.
   * `MOVE A TO B`: Usa l'assegnazione Java (`B = A;`).
   * `IF/ELSE/END-IF`: Usa la struttura `if/else` di Java.
   * `EVALUATE/WHEN/END-EVALUATE`: Usa la struttura `switch` di Java.
   * `COMPUTE C = A + B`: Per `BigDecimal`, usa i metodi: `c = a.add(b);` per l'addizione, `a.subtract(b);` per la sottrazione, etc.

6. **Gestione Transazioni:**
   * Usa `connection.setAutoCommit(false)` dove necessario
   * Implementa `connection.commit()` per confermare le transazioni
   * Implementa `connection.rollback()` in caso di errori

7. **Import necessari:**
   * Aggiungi tutti gli import necessari per JDBC:
     ```java
     import java.sql.*;
     import java.sql.Types;
     import java.math.BigDecimal;
     import java.util.Scanner;
     import java.io.*;
     import java.time.LocalDate;
     import java.time.LocalDateTime;
     ```

8. **Gestione delle Eccezioni SQL - CRITICO:**
   * OGNI chiamata a `connection.commit()` DEVE essere dentro un blocco try-catch
   * OGNI chiamata a `connection.rollback()` DEVE essere dentro un blocco try-catch
   * Struttura corretta per transazioni:
     - Usa un blocco `try-catch(SQLException e)` per tutte le operazioni SQL.
     - Nel blocco `try`, l'ultima istruzione deve essere `connection.commit();`.
     - Nel blocco `catch(SQLException e)`, la prima istruzione deve essere il `rollback`.
     - Poiché anche il `rollback` può fallire, inseriscilo in un `try-catch` annidato
     - Infine, gestisci l'eccezione originale con un metodo `handleSqlException(e);`.

9. **Output Finale:**
   * Produci **esclusivamente codice Java valido**.
   * Il codice deve essere contenuto in un unico blocco, senza testo o spiegazioni esterne.
   * Aggiungi JavaDoc chiari per la classe e per i metodi principali.
   * Il file `.java` generato deve essere compilabile ed eseguibile con driver PostgreSQL JDBC nel classpath.
   * Gestisci correttamente le eccezioni SQL con try-catch appropriati.

10. **Verifica Finale OBBLIGATORIA:**
   * Prima di concludere, esegui un controllo completo del codice generato:
     - Verifica che non ci sia testo non commentato
     - Verifica che tutti i statement SQL utilizzino PreparedStatement con parametri posizionali
     - Controlla che ogni PreparedStatement venga chiuso correttamente (preferibilmente con try-with-resources)
     - Assicurati che tutte le variabili siano inizializzate prima dell'uso
     - Verifica che la gestione delle transazioni sia corretta (commit/rollback)
     - Controlla che tutti i tipi di dati SQL vengano gestiti correttamente, in particolare NULL e tipi numerici
     - Assicurati che il codice sia completo e pronto per essere compilato senza modifiche manuali
     - Verifica che il metodo main() sia presente e chiami correttamente mainLogic()

Procedi con la traduzione completa includendo l'implementazione JDBC reale per tutte le operazioni SQL.
"""
    
    try:
        print(f"    📤 Invio richiesta a Gemini API...")
        response = model.generate_content(prompt)
        print(f"    ✓ Risposta ricevuta da Gemini")
        return response.text
    except Exception as e:
        print(f"    ❌ Errore API Gemini: {e}", file=sys.stderr)
        return None

def main():
    """
    Funzione principale per eseguire il processo di generazione con supporto JDBC.
    """
    parser = argparse.ArgumentParser(description='Traduttore COBOL to Java con supporto JDBC')
    parser.add_argument('--cobol', type=str, default="resources/cobol/File_COBOL.cbl",
                        help='Path del file COBOL di input')
    parser.add_argument('--sql', type=str, default="resources/sql/File_SQL.sql",
                        help='Path del file SQL schema (opzionale)')
    parser.add_argument('--output', type=str, default="output/File_JavaJDBC.java",
                        help='Path del file Java di output')
    
    args = parser.parse_args()
    
    print(f"   📖 Lettura file COBOL: {os.path.basename(args.cobol)}")
    cobol_code_content = read_source_file(args.cobol)
    
    if not cobol_code_content:
        print(f"    ❌ Impossibile leggere il file COBOL")
        sys.exit(1)
    else:
        print(f"    ✓ File COBOL letto correttamente")
    
    # Leggi lo schema SQL se disponibile
    sql_schema_content = None
    if args.sql and os.path.exists(args.sql):
        print(f" 🗃️  Lettura schema SQL: {os.path.basename(args.sql)}")
        sql_schema_content = read_source_file(args.sql)

        if sql_schema_content:
            print(f"    ✓ Schema SQL caricato")
    else:
        print(f"    ⚠️ Nessuno schema SQL fornito")
    
    print(f" 🔄 Traduzione in corso...")
    generated_java_code = translate_cobol_to_java_with_jdbc(cobol_code_content, sql_schema_content)
    
    if generated_java_code:
        save_java_file(generated_java_code, args.output)
        print(f" ✅ Traduzione completata\n")
    else:
        print(f" ❌ Traduzione fallita\n", file=sys.stderr)
        sys.exit(1)

if __name__ == "__main__":
    main()