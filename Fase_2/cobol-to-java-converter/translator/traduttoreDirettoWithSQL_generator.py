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
        return None
    except Exception as e:
        print(f"Errore durante la lettura del file '{filepath}': {e}")
        return None

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

def translate_cobol_to_java_with_jdbc(cobol_code, sql_schema=None):
    """
    Usa Gemini per tradurre codice sorgente COBOL in codice sorgente Java con supporto JDBC.
    """
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

    # Sezione schema SQL se disponibile
    schema_section = ""
    if sql_schema:
        schema_section = f"""
**Schema Database SQL fornito:**
```sql
{sql_schema}
```
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
     - `DB_URL = "jdbc:postgresql://localhost:5432/banca"`
     - `DB_USER = "postgres"`
     - `DB_PASSWORD = "password"`
   * Dichiara un campo `private Connection connection;`
   * Implementa metodi `connectDatabase()` e `disconnectDatabase()` usando JDBC

3. **Mappatura Tipi di Dato (dalla `DATA DIVISION`):**
   * **Nomenclatura:** Converti le variabili COBOL (es. `WS-NOME-CLIENTE`) in camelCase Java (es. `wsNomeCliente`).
   * `PIC X(n)`: Deve diventare `String`.
   * `PIC 9(n)`: Deve diventare `String` se è un ID o un input non matematico. Altrimenti, usa `int` o `long`.
   * `PIC S9(n)V99 [COMP-3]`: **OBBLIGATORIO** tradurlo in `java.math.BigDecimal` per preservare la precisione finanziaria.
   * Per i campi SQL host variables, usa i tipi Java appropriati per mappare i tipi di colonna del database.

4. **Traduzione SQL con JDBC:**
   * **EXEC SQL CONNECT**: Traduci in connessione JDBC usando `DriverManager.getConnection()`
   * **EXEC SQL SELECT**: Usa `PreparedStatement` con parametri posizionali `?`
   * **EXEC SQL INSERT/UPDATE/DELETE**: Usa `PreparedStatement.executeUpdate()`
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
     import java.math.BigDecimal;
     import java.util.Scanner;
     import java.io.*;
     import java.time.LocalDate;
     import java.time.LocalDateTime;
     ```

8. **Gestione delle Eccezioni SQL - CRITICO:**
   * OGNI chiamata a `connection.commit()` DEVE essere dentro un blocco try-catch
   * OGNI chiamata a `connection.rollback()` DEVE essere dentro un blocco try-catch
   * Il metodo `registraMovimento()` NON deve dichiarare `throws SQLException` nella firma
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

Procedi con la traduzione completa includendo l'implementazione JDBC reale per tutte le operazioni SQL.
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
    
    print(f"1. Lettura del codice sorgente COBOL da: '{args.cobol}'...")
    cobol_code_content = read_source_file(args.cobol)
    
    if not cobol_code_content:
        print("Impossibile leggere il file COBOL. Uscita.")
        sys.exit(1)
    
    # Leggi lo schema SQL se disponibile
    sql_schema_content = None
    if args.sql and os.path.exists(args.sql):
        print(f"2. Lettura dello schema SQL da: '{args.sql}'...")
        sql_schema_content = read_source_file(args.sql)
        if sql_schema_content:
            print("   Schema SQL caricato con successo.")
    else:
        print("2. Nessuno schema SQL fornito, procedo senza.")
    
    print("3. Inizio della traduzione da COBOL a Java con supporto JDBC...")
    generated_java_code = translate_cobol_to_java_with_jdbc(cobol_code_content, sql_schema_content)
    
    if generated_java_code:
        print("4. Traduzione completata. Salvataggio del file Java...")
        save_java_file(generated_java_code, args.output)
        print("\nNOTA: Per eseguire il codice generato, assicurati di:")
        print("  1. Avere PostgreSQL installato e in esecuzione")
        print("  2. Aver creato il database 'banca' con lo schema fornito")
        print("  3. Includere il driver PostgreSQL JDBC nel classpath")
        print("  4. Verificare le credenziali di connessione nel codice generato")
    else:
        print("Traduzione fallita. Controlla i messaggi di errore.")

if __name__ == "__main__":
    main()