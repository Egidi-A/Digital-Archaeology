      ******************************************************************
      * SISTEMA DI GESTIONE CONTI CORRENTI BANCARI
      * Programma principale per la gestione di conti correnti
      * Include: apertura conti, depositi, prelievi, estratto conto
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GESTIONE-CONTI.
       AUTHOR. ANNALISA-EGIDI.
       DATE-WRITTEN. 2025-05-20.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT REPORT-FILE ASSIGN TO "ESTRATTO-CONTO.TXT"
                  ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD  REPORT-FILE.
       01  REPORT-RECORD PIC X(132).
       
       WORKING-STORAGE SECTION.
       01  WS-SCELTA                PIC 9 VALUE ZERO.
       01  WS-CONTINUA              PIC X VALUE 'S'.
       01  WS-MESSAGGIO             PIC X(80).
       01  WS-ESITO                 PIC X(2).
       
      * Variabili per database
       01  WS-NUMERO-CONTO          PIC X(12).
       01  WS-CODICE-CLIENTE        PIC X(8).
       01  WS-IMPORTO               PIC 9(13)V99.
       01  WS-SALDO                 PIC S9(13)V99.
       01  WS-SALDO-EDIT            PIC Z,ZZZ,ZZZ,ZZ9.99-.
       01  WS-TIPO-MOVIMENTO        PIC X.
       01  WS-CAUSALE               PIC X(100).
       01  WS-DATA-SISTEMA          PIC X(10).
       01  WS-ORA-SISTEMA           PIC X(8).
       
      * Strutture dati per cliente
       01  WS-CLIENTE.
           05  WS-CLI-CODICE        PIC X(8).
           05  WS-CLI-NOME          PIC X(50).
           05  WS-CLI-COGNOME       PIC X(50).
           05  WS-CLI-CF            PIC X(16).
           05  WS-CLI-DATA-NASCITA  PIC X(10).
           05  WS-CLI-INDIRIZZO     PIC X(100).
           05  WS-CLI-CITTA         PIC X(50).
           05  WS-CLI-CAP           PIC X(5).
           05  WS-CLI-TELEFONO      PIC X(15).
           05  WS-CLI-EMAIL         PIC X(100).
       
      * Strutture dati per conto
       01  WS-CONTO.
           05  WS-CON-NUMERO        PIC X(12).
           05  WS-CON-CLIENTE       PIC X(8).
           05  WS-CON-TIPO          PIC X.
           05  WS-CON-SALDO         PIC S9(13)V99.
           05  WS-CON-DATA-APERTURA PIC X(10).
           05  WS-CON-STATO         PIC X.
           05  WS-CON-FIDO          PIC 9(13)V99.
       
      * SQL area
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.
       
      * Dichiarazione cursore per movimenti
           EXEC SQL
               DECLARE CUR-MOVIMENTI CURSOR FOR
               SELECT data_movimento, tipo_movimento, 
                      importo, causale, saldo_dopo
               FROM MOVIMENTI
               WHERE numero_conto = :WS-NUMERO-CONTO
               ORDER BY data_movimento DESC
           END-EXEC.
       
       01  WS-MOVIMENTO.
           05  WS-MOV-DATA          PIC X(19).
           05  WS-MOV-TIPO          PIC X.
           05  WS-MOV-IMPORTO       PIC S9(13)V99.
           05  WS-MOV-CAUSALE       PIC X(100).
           05  WS-MOV-SALDO-DOPO    PIC S9(13)V99.
       
       01  WS-TITOLO-REPORT.
           05  FILLER               PIC X(50) VALUE SPACES.
           05  FILLER               PIC X(32) 
               VALUE "ESTRATTO CONTO BANCARIO".
           05  FILLER               PIC X(50) VALUE SPACES.
       
       01  WS-LINEA-SEPARATORE      PIC X(132) VALUE ALL "-".
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           PERFORM CONNETTI-DATABASE
           
           PERFORM UNTIL WS-CONTINUA = 'N' OR 'n'
               PERFORM VISUALIZZA-MENU
               PERFORM ELABORA-SCELTA
               
               DISPLAY " "
               DISPLAY "Continuare? (S/N): " WITH NO ADVANCING
               ACCEPT WS-CONTINUA
           END-PERFORM
           
           PERFORM DISCONNETTI-DATABASE
           STOP RUN.
       
      *DA SETTARE CORRETTAMENTE LE CREDENZIALI DEL DATABASE
       CONNETTI-DATABASE.
           EXEC SQL
               CONNECT TO 'postgresql://localhost/banca'
               USER 'postgres' 
               USING 'password'
           END-EXEC
           
           IF SQLCODE NOT = 0
               DISPLAY "Errore connessione database: " SQLCODE
               STOP RUN
           ELSE
               DISPLAY "Connessione al database stabilita"
           END-IF.
       
       DISCONNETTI-DATABASE.
           EXEC SQL
               DISCONNECT ALL
           END-EXEC
           DISPLAY "Disconnesso dal database".
       
       VISUALIZZA-MENU.
           DISPLAY " "
           DISPLAY "===== SISTEMA GESTIONE CONTI CORRENTI ====="
           DISPLAY "1. Apertura nuovo conto"
           DISPLAY "2. Deposito"
           DISPLAY "3. Prelievo"
           DISPLAY "4. Visualizza saldo"
           DISPLAY "5. Estratto conto"
           DISPLAY "6. Chiusura conto"
           DISPLAY "0. Esci"
           DISPLAY "==========================================="
           DISPLAY "Scelta: " WITH NO ADVANCING
           ACCEPT WS-SCELTA.
       
       ELABORA-SCELTA.
           EVALUATE WS-SCELTA
               WHEN 1
                   PERFORM APERTURA-CONTO
               WHEN 2
                   PERFORM DEPOSITO
               WHEN 3
                   PERFORM PRELIEVO
               WHEN 4
                   PERFORM VISUALIZZA-SALDO
               WHEN 5
                   PERFORM ESTRATTO-CONTO
               WHEN 6
                   PERFORM CHIUSURA-CONTO
               WHEN 0
                   MOVE 'N' TO WS-CONTINUA
               WHEN OTHER
                   DISPLAY "Scelta non valida!"
           END-EVALUATE.
       
       APERTURA-CONTO.
           DISPLAY " "
           DISPLAY "=== APERTURA NUOVO CONTO ==="
           
           DISPLAY "Codice cliente: " WITH NO ADVANCING
           ACCEPT WS-CON-CLIENTE
           
      * Verifica esistenza cliente
           EXEC SQL
               SELECT codice_cliente, nome, cognome
               INTO :WS-CLI-CODICE, :WS-CLI-NOME, :WS-CLI-COGNOME
               FROM CLIENTI
               WHERE codice_cliente = :WS-CON-CLIENTE
           END-EXEC
           
           IF SQLCODE = 100
               DISPLAY "Cliente non trovato!"
           END-IF
           
           IF SQLCODE NOT = 0
               DISPLAY "Errore database: " SQLCODE
           END-IF
           
           DISPLAY "Cliente: " WS-CLI-NOME " " WS-CLI-COGNOME
           
      * Genera nuovo numero conto
           PERFORM GENERA-NUMERO-CONTO
           
           DISPLAY "Tipo conto (C=Corrente, D=Deposito): " 
                   WITH NO ADVANCING
           ACCEPT WS-CON-TIPO
           
           DISPLAY "Importo iniziale: " WITH NO ADVANCING
           ACCEPT WS-CON-SALDO
           
           DISPLAY "Fido accordato: " WITH NO ADVANCING
           ACCEPT WS-CON-FIDO
           
           MOVE FUNCTION CURRENT-DATE(1:10) TO WS-CON-DATA-APERTURA
           MOVE 'A' TO WS-CON-STATO
           
      * Inserisci nuovo conto
           EXEC SQL
               INSERT INTO CONTI 
               (numero_conto, codice_cliente, tipo_conto, 
                saldo, data_apertura, stato, fido)
               VALUES 
               (:WS-CON-NUMERO, :WS-CON-CLIENTE, :WS-CON-TIPO,
                :WS-CON-SALDO, CURRENT_DATE, :WS-CON-STATO, 
                :WS-CON-FIDO)
           END-EXEC
           
           IF SQLCODE = 0
               DISPLAY "Conto " WS-CON-NUMERO " creato con successo!"
               
      * Registra movimento iniziale
               IF WS-CON-SALDO > 0
                   MOVE 'D' TO WS-TIPO-MOVIMENTO
                   MOVE "Deposito iniziale" TO WS-CAUSALE
                   MOVE WS-CON-SALDO TO WS-IMPORTO
                   MOVE WS-CON-NUMERO TO WS-NUMERO-CONTO
                   PERFORM REGISTRA-MOVIMENTO
               END-IF
           ELSE
               DISPLAY "Errore creazione conto: " SQLCODE
           END-IF.
       
       GENERA-NUMERO-CONTO.
           EXEC SQL
               SELECT 'IT' || LPAD(
                   CAST(COALESCE(MAX(CAST(SUBSTR(numero_conto, 3) 
                   AS INTEGER)), 0) + 1 AS VARCHAR), 10, '0')
               INTO :WS-CON-NUMERO
               FROM CONTI
               WHERE numero_conto LIKE 'IT%'
           END-EXEC.
       
       DEPOSITO.
           DISPLAY " "
           DISPLAY "=== DEPOSITO ==="
           
           DISPLAY "Numero conto: " WITH NO ADVANCING
           ACCEPT WS-NUMERO-CONTO
           
           PERFORM VERIFICA-CONTO
           IF WS-ESITO = "KO"
           END-IF
           
           DISPLAY "Importo deposito: " WITH NO ADVANCING
           ACCEPT WS-IMPORTO
           
           IF WS-IMPORTO <= 0
               DISPLAY "Importo non valido!"
           END-IF
           
           DISPLAY "Causale: " WITH NO ADVANCING
           ACCEPT WS-CAUSALE
           
      * Aggiorna saldo
           EXEC SQL
               UPDATE CONTI
               SET saldo = saldo + :WS-IMPORTO
               WHERE numero_conto = :WS-NUMERO-CONTO
               AND stato = 'A'
           END-EXEC
           
           IF SQLCODE = 0
               MOVE 'D' TO WS-TIPO-MOVIMENTO
               PERFORM REGISTRA-MOVIMENTO
               DISPLAY "Deposito effettuato con successo!"
           ELSE
               DISPLAY "Errore durante il deposito: " SQLCODE
           END-IF.
       
       PRELIEVO.
           DISPLAY " "
           DISPLAY "=== PRELIEVO ==="
           
           DISPLAY "Numero conto: " WITH NO ADVANCING
           ACCEPT WS-NUMERO-CONTO
           
           PERFORM VERIFICA-CONTO
           IF WS-ESITO = "KO"
           END-IF
           
           DISPLAY "Importo prelievo: " WITH NO ADVANCING
           ACCEPT WS-IMPORTO
           
           IF WS-IMPORTO <= 0
               DISPLAY "Importo non valido!"
           END-IF
           
      * Verifica disponibilità
           EXEC SQL
               SELECT saldo, fido
               INTO :WS-CON-SALDO, :WS-CON-FIDO
               FROM CONTI
               WHERE numero_conto = :WS-NUMERO-CONTO
               AND stato = 'A'
           END-EXEC
           
           IF WS-CON-SALDO - WS-IMPORTO < (0 - WS-CON-FIDO)
               DISPLAY "Fondi insufficienti!"
               DISPLAY "Saldo attuale: " WS-CON-SALDO
               DISPLAY "Fido disponibile: " WS-CON-FIDO
           END-IF
           
           DISPLAY "Causale: " WITH NO ADVANCING
           ACCEPT WS-CAUSALE
           
      * Aggiorna saldo
           EXEC SQL
               UPDATE CONTI
               SET saldo = saldo - :WS-IMPORTO
               WHERE numero_conto = :WS-NUMERO-CONTO
               AND stato = 'A'
           END-EXEC
           
           IF SQLCODE = 0
               MOVE 'P' TO WS-TIPO-MOVIMENTO
               PERFORM REGISTRA-MOVIMENTO
               DISPLAY "Prelievo effettuato con successo!"
           ELSE
               DISPLAY "Errore durante il prelievo: " SQLCODE
           END-IF.
       
       VISUALIZZA-SALDO.
           DISPLAY " "
           DISPLAY "=== VISUALIZZA SALDO ==="
           
           DISPLAY "Numero conto: " WITH NO ADVANCING
           ACCEPT WS-NUMERO-CONTO
           
           EXEC SQL
               SELECT c.saldo, c.fido, cl.nome, cl.cognome
               INTO :WS-CON-SALDO, :WS-CON-FIDO, 
                    :WS-CLI-NOME, :WS-CLI-COGNOME
               FROM CONTI c
               JOIN CLIENTI cl ON c.codice_cliente = cl.codice_cliente
               WHERE c.numero_conto = :WS-NUMERO-CONTO
               AND c.stato = 'A'
           END-EXEC
           
           IF SQLCODE = 0
               DISPLAY " "
               DISPLAY "Intestatario: " WS-CLI-NOME " " WS-CLI-COGNOME
               MOVE WS-CON-SALDO TO WS-SALDO-EDIT
               DISPLAY "Saldo attuale: EUR " WS-SALDO-EDIT
               MOVE WS-CON-FIDO TO WS-SALDO-EDIT
               DISPLAY "Fido accordato: EUR " WS-SALDO-EDIT
               COMPUTE WS-SALDO = WS-CON-SALDO + WS-CON-FIDO
               MOVE WS-SALDO TO WS-SALDO-EDIT
               DISPLAY "Disponibile: EUR " WS-SALDO-EDIT
           ELSE
               IF SQLCODE = 100
                   DISPLAY "Conto non trovato o non attivo!"
               ELSE
                   DISPLAY "Errore database: " SQLCODE
               END-IF
           END-IF.
       
       ESTRATTO-CONTO.
           DISPLAY " "
           DISPLAY "=== ESTRATTO CONTO ==="
           
           DISPLAY "Numero conto: " WITH NO ADVANCING
           ACCEPT WS-NUMERO-CONTO
           
           PERFORM VERIFICA-CONTO
           IF WS-ESITO = "KO"
           END-IF
           
           OPEN OUTPUT REPORT-FILE
           
      * Intestazione report
           WRITE REPORT-RECORD FROM WS-TITOLO-REPORT
           WRITE REPORT-RECORD FROM WS-LINEA-SEPARATORE
           
           STRING "Conto: " WS-NUMERO-CONTO 
                  "    Cliente: " WS-CLI-NOME " " WS-CLI-COGNOME
                  DELIMITED BY SIZE
                  INTO REPORT-RECORD
           WRITE REPORT-RECORD
           
           STRING "Data: " FUNCTION CURRENT-DATE(1:10)
                  "    Ora: " FUNCTION CURRENT-DATE(12:8)
                  DELIMITED BY SIZE
                  INTO REPORT-RECORD
           WRITE REPORT-RECORD
           WRITE REPORT-RECORD FROM WS-LINEA-SEPARATORE
           
      * Intestazione colonne
           STRING "DATA/ORA            TIPO  " 
                  "    IMPORTO     CAUSALE" 
                  "                    SALDO DOPO"
                  DELIMITED BY SIZE
                  INTO REPORT-RECORD
           WRITE REPORT-RECORD
           WRITE REPORT-RECORD FROM WS-LINEA-SEPARATORE
           
      * Apri cursore movimenti
           EXEC SQL
               OPEN CUR-MOVIMENTI
           END-EXEC
           
           PERFORM UNTIL SQLCODE NOT = 0
               EXEC SQL
                   FETCH CUR-MOVIMENTI
                   INTO :WS-MOV-DATA, :WS-MOV-TIPO,
                        :WS-MOV-IMPORTO, :WS-MOV-CAUSALE,
                        :WS-MOV-SALDO-DOPO
               END-EXEC
               
               IF SQLCODE = 0
                   PERFORM SCRIVI-MOVIMENTO-REPORT
               END-IF
           END-PERFORM
           
           EXEC SQL
               CLOSE CUR-MOVIMENTI
           END-EXEC
           
           WRITE REPORT-RECORD FROM WS-LINEA-SEPARATORE
           
      * Saldo finale
           MOVE WS-CON-SALDO TO WS-SALDO-EDIT
           STRING "SALDO FINALE: EUR " WS-SALDO-EDIT
                  DELIMITED BY SIZE
                  INTO REPORT-RECORD
           WRITE REPORT-RECORD
           
           CLOSE REPORT-FILE
           
           DISPLAY "Estratto conto salvato in ESTRATTO-CONTO.TXT".
       
       SCRIVI-MOVIMENTO-REPORT.
           MOVE SPACES TO REPORT-RECORD
           
           STRING WS-MOV-DATA(1:19) "  "
                  DELIMITED BY SIZE
                  INTO REPORT-RECORD
           
           EVALUATE WS-MOV-TIPO
               WHEN 'D'
                   STRING REPORT-RECORD(1:21) "DEP  " 
                          DELIMITED BY SIZE
                          INTO REPORT-RECORD
               WHEN 'P'
                   STRING REPORT-RECORD(1:21) "PRE  " 
                          DELIMITED BY SIZE
                          INTO REPORT-RECORD
               WHEN 'B'
                   STRING REPORT-RECORD(1:21) "BON  " 
                          DELIMITED BY SIZE
                          INTO REPORT-RECORD
               WHEN OTHER
                   STRING REPORT-RECORD(1:21) "???  " 
                          DELIMITED BY SIZE
                          INTO REPORT-RECORD
           END-EVALUATE
           
           MOVE WS-MOV-IMPORTO TO WS-SALDO-EDIT
           STRING REPORT-RECORD(1:26) WS-SALDO-EDIT " "
                  DELIMITED BY SIZE
                  INTO REPORT-RECORD
           
           STRING REPORT-RECORD(1:45) WS-MOV-CAUSALE(1:30) " "
                  DELIMITED BY SIZE
                  INTO REPORT-RECORD
           
           MOVE WS-MOV-SALDO-DOPO TO WS-SALDO-EDIT
           STRING REPORT-RECORD(1:76) WS-SALDO-EDIT
                  DELIMITED BY SIZE
                  INTO REPORT-RECORD
           
           WRITE REPORT-RECORD.
       
       CHIUSURA-CONTO.
           DISPLAY " "
           DISPLAY "=== CHIUSURA CONTO ==="
           
           DISPLAY "Numero conto da chiudere: " WITH NO ADVANCING
           ACCEPT WS-NUMERO-CONTO
           
           PERFORM VERIFICA-CONTO
           IF WS-ESITO = "KO"
           END-IF
           
      * Verifica saldo zero
           IF WS-CON-SALDO NOT = 0
               DISPLAY "Impossibile chiudere: saldo non zero!"
               MOVE WS-CON-SALDO TO WS-SALDO-EDIT
               DISPLAY "Saldo attuale: EUR " WS-SALDO-EDIT
           END-IF
           
           DISPLAY "Confermare chiusura conto (S/N): " 
                   WITH NO ADVANCING
           ACCEPT WS-CONTINUA
           
           IF WS-CONTINUA = 'S' OR 's'
               EXEC SQL
                   UPDATE CONTI
                   SET stato = 'C',
                       data_chiusura = CURRENT_DATE
                   WHERE numero_conto = :WS-NUMERO-CONTO
               END-EXEC
               
               IF SQLCODE = 0
                   DISPLAY "Conto chiuso con successo!"
               ELSE
                   DISPLAY "Errore chiusura conto: " SQLCODE
               END-IF
           ELSE
               DISPLAY "Chiusura annullata"
           END-IF.
       
       VERIFICA-CONTO.
           MOVE "OK" TO WS-ESITO
           
           EXEC SQL
               SELECT c.saldo, c.stato, cl.nome, cl.cognome
               INTO :WS-CON-SALDO, :WS-CON-STATO,
                    :WS-CLI-NOME, :WS-CLI-COGNOME
               FROM CONTI c
               JOIN CLIENTI cl ON c.codice_cliente = cl.codice_cliente
               WHERE c.numero_conto = :WS-NUMERO-CONTO
           END-EXEC
           
           IF SQLCODE = 100
               DISPLAY "Conto non trovato!"
               MOVE "KO" TO WS-ESITO
           ELSE IF SQLCODE NOT = 0
               DISPLAY "Errore database: " SQLCODE
               MOVE "KO" TO WS-ESITO
           ELSE IF WS-CON-STATO NOT = 'A'
               DISPLAY "Conto non attivo!"
               MOVE "KO" TO WS-ESITO
           END-IF.
       
       REGISTRA-MOVIMENTO.
           EXEC SQL
               SELECT saldo INTO :WS-SALDO
               FROM CONTI
               WHERE numero_conto = :WS-NUMERO-CONTO
           END-EXEC
           
           EXEC SQL
               INSERT INTO MOVIMENTI
               (numero_conto, tipo_movimento, importo, 
                causale, saldo_dopo, eseguito_da)
               VALUES
               (:WS-NUMERO-CONTO, :WS-TIPO-MOVIMENTO, :WS-IMPORTO,
                :WS-CAUSALE, :WS-SALDO, 'SISTEMA')
           END-EXEC.
          