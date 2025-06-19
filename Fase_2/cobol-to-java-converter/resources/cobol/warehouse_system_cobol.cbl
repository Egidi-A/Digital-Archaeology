      ******************************************************************
      * SISTEMA DI GESTIONE MAGAZZINO E INVENTARIO
      * Programma principale per gestione articoli, movimenti, ordini
      * Include: carico/scarico, valorizzazione FIFO/LIFO, inventari
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GESTIONE-MAGAZZINO.
       AUTHOR. ANNALISA-EGIDI.
       DATE-WRITTEN. 2025-05-20.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT REPORT-FILE ASSIGN TO "REPORT-MAGAZZINO.TXT"
                  ORGANIZATION IS LINE SEQUENTIAL.
           
           SELECT INVENTARIO-FILE ASSIGN TO "INVENTARIO.TXT"
                  ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD  REPORT-FILE.
       01  REPORT-RECORD PIC X(132).
       
       FD  INVENTARIO-FILE.
       01  INVENTARIO-RECORD PIC X(132).
       
       WORKING-STORAGE SECTION.
       01  WS-SCELTA               PIC 99 VALUE ZERO.
       01  WS-CONTINUA             PIC X VALUE 'S'.
       01  WS-RISPOSTA             PIC X.
       01  WS-ESITO                PIC X(2).
       01  WS-METODO-VALORIZZ      PIC X VALUE 'F'. 
      * F=FIFO, L=LIFO, M=Medio ponderato
       
      * Variabili per articoli
       01  WS-ARTICOLO.
           05  WS-ART-CODICE       PIC X(10).
           05  WS-ART-DESCRIZIONE  PIC X(200).
           05  WS-ART-CATEGORIA    PIC X(4).
           05  WS-ART-UM           PIC X(10).
           05  WS-ART-FORNITORE    PIC X(8).
           05  WS-ART-PREZZO-ACQ   PIC 9(8)V99.
           05  WS-ART-PREZZO-VEN   PIC 9(8)V99.
           05  WS-ART-SCORTA-MIN   PIC 9(8)V99.
           05  WS-ART-PUNTO-RIORD  PIC 9(8)V99.
           05  WS-ART-LOTTO-RIORD  PIC 9(8)V99.
           05  WS-ART-UBICAZIONE   PIC X(20).
           05  WS-ART-STATO        PIC X.
       
      * Variabili per giacenze
       01  WS-GIACENZA.
           05  WS-GIA-DISPONIBILE  PIC S9(8)V99.
           05  WS-GIA-IMPEGNATA    PIC S9(8)V99.
           05  WS-GIA-ORDINATA     PIC S9(8)V99.
           05  WS-GIA-VAL-MEDIO    PIC S9(8)V9999.
           05  WS-GIA-VAL-ULTIMO   PIC S9(8)V9999.
       
      * Variabili per movimenti
       01  WS-MOVIMENTO.
           05  WS-MOV-TIPO         PIC X(2).
           05  WS-MOV-NUMERO-DOC   PIC X(20).
           05  WS-MOV-DATA         PIC X(10).
           05  WS-MOV-ARTICOLO     PIC X(10).
           05  WS-MOV-QUANTITA     PIC S9(8)V99.
           05  WS-MOV-PREZZO       PIC S9(8)V9999.
           05  WS-MOV-VALORE       PIC S9(10)V99.
           05  WS-MOV-CAUSALE      PIC X(100).
           05  WS-MOV-FORNITORE    PIC X(8).
           05  WS-MOV-OPERATORE    PIC X(50).
       
      * Variabili per ordini
       01  WS-ORDINE.
           05  WS-ORD-NUMERO       PIC X(20).
           05  WS-ORD-DATA         PIC X(10).
           05  WS-ORD-FORNITORE    PIC X(8).
           05  WS-ORD-STATO        PIC X.
           05  WS-ORD-TOTALE       PIC 9(10)V99.
       
      * Variabili per lotti (FIFO/LIFO)
       01  WS-LOTTO.
           05  WS-LOT-ID           PIC 9(9).
           05  WS-LOT-NUMERO       PIC X(20).
           05  WS-LOT-DATA         PIC X(10).
           05  WS-LOT-QTA-INI      PIC 9(8)V99.
           05  WS-LOT-QTA-RES      PIC 9(8)V99.
           05  WS-LOT-PREZZO       PIC 9(8)V9999.
       
      * Variabili di calcolo
       01  WS-CALCOLI.
           05  WS-QTA-RICHIESTA    PIC 9(8)V99.
           05  WS-QTA-PRELEVATA    PIC 9(8)V99.
           05  WS-QTA-RESIDUA      PIC 9(8)V99.
           05  WS-VALORE-TOT       PIC 9(10)V99.
           05  WS-VALORE-MEDIO     PIC 9(8)V9999.
           05  WS-NUOVO-MEDIO      PIC 9(8)V9999.
       
      * Contatori e totali
       01  WS-CONTATORI.
           05  WS-CONTA-ARTICOLI   PIC 9(5) VALUE ZERO.
           05  WS-CONTA-MOVIMENTI  PIC 9(5) VALUE ZERO.
           05  WS-CONTA-SOTTOSCORTA PIC 9(5) VALUE ZERO.
           05  WS-VALORE-MAGAZZINO PIC 9(12)V99 VALUE ZERO.
       
      * Variabili per formattazione
       01  WS-IMPORTO-EDIT         PIC Z,ZZZ,ZZ9.99-.
       01  WS-QUANTITA-EDIT        PIC Z,ZZZ,ZZ9.99-.
       01  WS-NUMERO-EDIT          PIC ZZZ,ZZ9.
       
      * SQL area
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.
       
      * Cursori
           EXEC SQL
               DECLARE CUR-ARTICOLI CURSOR FOR
               SELECT a.codice_articolo, a.descrizione,
                      a.unita_misura, a.ubicazione,
                      g.quantita_disponibile, g.valore_medio,
                      a.scorta_minima, a.punto_riordino
               FROM ARTICOLI a
               JOIN GIACENZE g ON a.codice_articolo = g.codice_articolo
               WHERE a.stato = 'A'
               ORDER BY a.codice_articolo
           END-EXEC.
       
           EXEC SQL
               DECLARE CUR-LOTTI-FIFO CURSOR FOR
               SELECT id_lotto, numero_lotto, quantita_residua,
                      prezzo_acquisto, data_carico
               FROM LOTTI
               WHERE codice_articolo = :WS-ART-CODICE
               AND quantita_residua > 0
               AND stato = 'A'
               ORDER BY data_carico, id_lotto
           END-EXEC.
       
           EXEC SQL
               DECLARE CUR-LOTTI-LIFO CURSOR FOR
               SELECT id_lotto, numero_lotto, quantita_residua,
                      prezzo_acquisto, data_carico
               FROM LOTTI
               WHERE codice_articolo = :WS-ART-CODICE
               AND quantita_residua > 0
               AND stato = 'A'
               ORDER BY data_carico DESC, id_lotto DESC
           END-EXEC.
       
           EXEC SQL
               DECLARE CUR-MOVIMENTI CURSOR FOR
               SELECT tipo_movimento, numero_documento,
                      data_movimento, quantita, prezzo_unitario,
                      valore_totale, causale
               FROM MOVIMENTI_MAGAZZINO
               WHERE codice_articolo = :WS-ART-CODICE
               ORDER BY data_movimento DESC
               LIMIT 50
           END-EXEC.
       
           EXEC SQL
               DECLARE CUR-SOTTOSCORTA CURSOR FOR
               SELECT a.codice_articolo, a.descrizione,
                      g.quantita_disponibile, g.quantita_ordinata,
                      a.punto_riordino, a.lotto_riordino,
                      f.ragione_sociale
               FROM ARTICOLI a
               JOIN GIACENZE g ON a.codice_articolo = g.codice_articolo
               LEFT JOIN FORNITORI f ON a.codice_fornitore = f.codice_fornitore
               WHERE a.stato = 'A'
               AND g.quantita_disponibile <= a.punto_riordino
               AND a.punto_riordino > 0
               ORDER BY (a.punto_riordino - g.quantita_disponibile) DESC
           END-EXEC.
       
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
       
       CONNETTI-DATABASE.
           EXEC SQL
               CONNECT TO 'postgresql://localhost/magazzino'
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
           DISPLAY "===== SISTEMA GESTIONE MAGAZZINO ====="
           DISPLAY " 1. Carico merce"
           DISPLAY " 2. Scarico merce"
           DISPLAY " 3. Visualizza giacenza articolo"
           DISPLAY " 4. Lista articoli sottoscorta"
           DISPLAY " 5. Valorizzazione magazzino"
           DISPLAY " 6. Movimenti articolo"
           DISPLAY " 7. Rettifica inventario"
           DISPLAY " 8. Gestione ordini fornitori"
           DISPLAY " 9. Report inventario fisico"
           DISPLAY "10. Analisi ABC articoli"
           DISPLAY " 0. Esci"
           DISPLAY "======================================"
           DISPLAY "Scelta: " WITH NO ADVANCING
           ACCEPT WS-SCELTA.
       
       ELABORA-SCELTA.
           EVALUATE WS-SCELTA
               WHEN 1
                   PERFORM CARICO-MERCE
               WHEN 2
                   PERFORM SCARICO-MERCE
               WHEN 3
                   PERFORM VISUALIZZA-GIACENZA
               WHEN 4
                   PERFORM LISTA-SOTTOSCORTA
               WHEN 5
                   PERFORM VALORIZZAZIONE-MAGAZZINO
               WHEN 6
                   PERFORM MOVIMENTI-ARTICOLO
               WHEN 7
                   PERFORM RETTIFICA-INVENTARIO
               WHEN 8
                   PERFORM GESTIONE-ORDINI
               WHEN 9
                   PERFORM REPORT-INVENTARIO
               WHEN 10
                   PERFORM ANALISI-ABC
               WHEN 0
                   MOVE 'N' TO WS-CONTINUA
               WHEN OTHER
                   DISPLAY "Scelta non valida!"
           END-EVALUATE.
       
       CARICO-MERCE.
           DISPLAY " "
           DISPLAY "=== CARICO MERCE ==="
           
           DISPLAY "Codice articolo: " WITH NO ADVANCING
           ACCEPT WS-ART-CODICE
           
           PERFORM CARICA-ARTICOLO
           IF WS-ESITO NOT = "OK"
               EXIT PARAGRAPH
           END-IF
           
           DISPLAY "Articolo: " WS-ART-DESCRIZIONE(1:50)
           
           DISPLAY "Numero documento (DDT/Fattura): " WITH NO ADVANCING
           ACCEPT WS-MOV-NUMERO-DOC
           
           DISPLAY "Quantità da caricare: " WITH NO ADVANCING
           ACCEPT WS-MOV-QUANTITA
           
           IF WS-MOV-QUANTITA <= 0
               DISPLAY "Quantità non valida!"
               EXIT PARAGRAPH
           END-IF
           
           DISPLAY "Prezzo unitario: " WITH NO ADVANCING
           ACCEPT WS-MOV-PREZZO
           
           IF WS-MOV-PREZZO <= 0
               DISPLAY "Prezzo non valido!"
               EXIT PARAGRAPH
           END-IF
           
           DISPLAY "Codice fornitore: " WITH NO ADVANCING
           ACCEPT WS-MOV-FORNITORE
           
           DISPLAY "Causale: " WITH NO ADVANCING
           ACCEPT WS-MOV-CAUSALE
           
           DISPLAY "Numero lotto (opzionale): " WITH NO ADVANCING
           ACCEPT WS-LOT-NUMERO
           
      * Calcola valore totale
           COMPUTE WS-MOV-VALORE = WS-MOV-QUANTITA * WS-MOV-PREZZO
           
      * Inizia transazione
           EXEC SQL
               BEGIN TRANSACTION
           END-EXEC
           
      * Registra movimento
           MOVE 'CA' TO WS-MOV-TIPO
           PERFORM REGISTRA-MOVIMENTO
           
           IF WS-ESITO = "OK"
      * Aggiorna giacenza
               PERFORM AGGIORNA-GIACENZA-CARICO
               
               IF WS-ESITO = "OK"
      * Crea lotto se specificato
                   IF WS-LOT-NUMERO NOT = SPACES
                       PERFORM CREA-LOTTO
                   END-IF
               END-IF
           END-IF
           
           IF WS-ESITO = "OK"
               EXEC SQL
                   COMMIT
               END-EXEC
               DISPLAY "Carico registrato con successo!"
               MOVE WS-MOV-VALORE TO WS-IMPORTO-EDIT
               DISPLAY "Valore carico: EUR " WS-IMPORTO-EDIT
           ELSE
               EXEC SQL
                   ROLLBACK
               END-EXEC
               DISPLAY "Errore durante il carico!"
           END-IF.
       
       SCARICO-MERCE.
           DISPLAY " "
           DISPLAY "=== SCARICO MERCE ==="
           
           DISPLAY "Codice articolo: " WITH NO ADVANCING
           ACCEPT WS-ART-CODICE
           
           PERFORM CARICA-ARTICOLO
           IF WS-ESITO NOT = "OK"
               EXIT PARAGRAPH
           END-IF
           
           DISPLAY "Articolo: " WS-ART-DESCRIZIONE(1:50)
           MOVE WS-GIA-DISPONIBILE TO WS-QUANTITA-EDIT
           DISPLAY "Disponibile: " WS-QUANTITA-EDIT " " WS-ART-UM
           
           DISPLAY "Quantità da scaricare: " WITH NO ADVANCING
           ACCEPT WS-MOV-QUANTITA
           
           IF WS-MOV-QUANTITA <= 0
               DISPLAY "Quantità non valida!"
               EXIT PARAGRAPH
           END-IF
           
           IF WS-MOV-QUANTITA > WS-GIA-DISPONIBILE
               DISPLAY "Quantità non disponibile!"
               EXIT PARAGRAPH
           END-IF
           
           DISPLAY "Numero documento: " WITH NO ADVANCING
           ACCEPT WS-MOV-NUMERO-DOC
           
           DISPLAY "Causale: " WITH NO ADVANCING
           ACCEPT WS-MOV-CAUSALE
           
           DISPLAY "Metodo valorizzazione (F=FIFO, L=LIFO, M=Medio): "
                   WITH NO ADVANCING
           ACCEPT WS-METODO-VALORIZZ
           
      * Inizia transazione
           EXEC SQL
               BEGIN TRANSACTION
           END-EXEC
           
      * Calcola valore scarico
           PERFORM CALCOLA-VALORE-SCARICO
           
           IF WS-ESITO = "OK"
      * Registra movimento
               MOVE 'SC' TO WS-MOV-TIPO
               PERFORM REGISTRA-MOVIMENTO
               
               IF WS-ESITO = "OK"
      * Aggiorna giacenza
                   PERFORM AGGIORNA-GIACENZA-SCARICO
                   
                   IF WS-ESITO = "OK" AND WS-METODO-VALORIZZ NOT = 'M'
      * Aggiorna lotti se FIFO/LIFO
                       PERFORM AGGIORNA-LOTTI-SCARICO
                   END-IF
               END-IF
           END-IF
           
           IF WS-ESITO = "OK"
               EXEC SQL
                   COMMIT
               END-EXEC
               DISPLAY "Scarico registrato con successo!"
               MOVE WS-MOV-VALORE TO WS-IMPORTO-EDIT
               DISPLAY "Valore scarico: EUR " WS-IMPORTO-EDIT
           ELSE
               EXEC SQL
                   ROLLBACK
               END-EXEC
               DISPLAY "Errore durante lo scarico!"
           END-IF.
       
       CARICA-ARTICOLO.
           MOVE "OK" TO WS-ESITO
           
           EXEC SQL
               SELECT a.descrizione, a.codice_categoria,
                      a.unita_misura, a.codice_fornitore,
                      a.prezzo_acquisto, a.prezzo_vendita,
                      a.scorta_minima, a.punto_riordino,
                      a.lotto_riordino, a.ubicazione, a.stato,
                      g.quantita_disponibile, g.quantita_impegnata,
                      g.quantita_ordinata, g.valore_medio,
                      g.valore_ultimo
               INTO :WS-ART-DESCRIZIONE, :WS-ART-CATEGORIA,
                    :WS-ART-UM, :WS-ART-FORNITORE,
                    :WS-ART-PREZZO-ACQ, :WS-ART-PREZZO-VEN,
                    :WS-ART-SCORTA-MIN, :WS-ART-PUNTO-RIORD,
                    :WS-ART-LOTTO-RIORD, :WS-ART-UBICAZIONE,
                    :WS-ART-STATO, :WS-GIA-DISPONIBILE,
                    :WS-GIA-IMPEGNATA, :WS-GIA-ORDINATA,
                    :WS-GIA-VAL-MEDIO, :WS-GIA-VAL-ULTIMO
               FROM ARTICOLI a
               LEFT JOIN GIACENZE g 
                    ON a.codice_articolo = g.codice_articolo
               WHERE a.codice_articolo = :WS-ART-CODICE
           END-EXEC
           
           IF SQLCODE = 100
               DISPLAY "Articolo non trovato!"
               MOVE "KO" TO WS-ESITO
           ELSE IF SQLCODE NOT = 0
               DISPLAY "Errore database: " SQLCODE
               MOVE "KO" TO WS-ESITO
           ELSE IF WS-ART-STATO NOT = 'A'
               DISPLAY "Articolo non attivo!"
               MOVE "KO" TO WS-ESITO
           END-IF.
       
       REGISTRA-MOVIMENTO.
           MOVE FUNCTION CURRENT-DATE(1:50) TO WS-MOV-OPERATORE
           
           EXEC SQL
               INSERT INTO MOVIMENTI_MAGAZZINO
               (tipo_movimento, numero_documento, codice_articolo,
                quantita, prezzo_unitario, valore_totale,
                causale, codice_fornitore, operatore)
               VALUES
               (:WS-MOV-TIPO, :WS-MOV-NUMERO-DOC, :WS-ART-CODICE,
                :WS-MOV-QUANTITA, :WS-MOV-PREZZO, :WS-MOV-VALORE,
                :WS-MOV-CAUSALE, :WS-MOV-FORNITORE, :WS-MOV-OPERATORE)
           END-EXEC
           
           IF SQLCODE NOT = 0
               DISPLAY "Errore registrazione movimento: " SQLCODE
               MOVE "KO" TO WS-ESITO
           END-IF.
       
       AGGIORNA-GIACENZA-CARICO.
      * Calcola nuovo valore medio ponderato
           COMPUTE WS-VALORE-TOT = 
               (WS-GIA-DISPONIBILE * WS-GIA-VAL-MEDIO) +
               (WS-MOV-QUANTITA * WS-MOV-PREZZO)
           
           COMPUTE WS-NUOVO-MEDIO = 
               WS-VALORE-TOT / (WS-GIA-DISPONIBILE + WS-MOV-QUANTITA)
           
           EXEC SQL
               UPDATE GIACENZE
               SET quantita_disponibile = quantita_disponibile + :WS-MOV-QUANTITA,
                   valore_medio = :WS-NUOVO-MEDIO,
                   valore_ultimo = :WS-MOV-PREZZO,
                   data_ultimo_carico = CURRENT_DATE
               WHERE codice_articolo = :WS-ART-CODICE
           END-EXEC
           
           IF SQLCODE = -100
      * Giacenza non esiste, creala
               EXEC SQL
                   INSERT INTO GIACENZE
                   (codice_articolo, quantita_disponibile,
                    valore_medio, valore_ultimo, data_ultimo_carico)
                   VALUES
                   (:WS-ART-CODICE, :WS-MOV-QUANTITA,
                    :WS-MOV-PREZZO, :WS-MOV-PREZZO, CURRENT_DATE)
               END-EXEC
           END-IF
           
           IF SQLCODE NOT = 0 AND SQLCODE NOT = -100
               DISPLAY "Errore aggiornamento giacenza: " SQLCODE
               MOVE "KO" TO WS-ESITO
           END-IF.
       
       AGGIORNA-GIACENZA-SCARICO.
           EXEC SQL
               UPDATE GIACENZE
               SET quantita_disponibile = quantita_disponibile - :WS-MOV-QUANTITA,
                   data_ultimo_scarico = CURRENT_DATE
               WHERE codice_articolo = :WS-ART-CODICE
           END-EXEC
           
           IF SQLCODE NOT = 0
               DISPLAY "Errore aggiornamento giacenza: " SQLCODE
               MOVE "KO" TO WS-ESITO
           END-IF.
       
       CREA-LOTTO.
           EXEC SQL
               INSERT INTO LOTTI
               (codice_articolo, numero_lotto, data_carico,
                quantita_iniziale, quantita_residua, prezzo_acquisto)
               VALUES
               (:WS-ART-CODICE, :WS-LOT-NUMERO, CURRENT_DATE,
                :WS-MOV-QUANTITA, :WS-MOV-QUANTITA, :WS-MOV-PREZZO)
           END-EXEC
           
           IF SQLCODE = -803
               DISPLAY "Lotto già esistente!"
               MOVE "KO" TO WS-ESITO
           ELSE IF SQLCODE NOT = 0
               DISPLAY "Errore creazione lotto: " SQLCODE
               MOVE "KO" TO WS-ESITO
           END-IF.
       
       CALCOLA-VALORE-SCARICO.
           EVALUATE WS-METODO-VALORIZZ
               WHEN 'F'
                   PERFORM CALCOLA-VALORE-FIFO
               WHEN 'L'
                   PERFORM CALCOLA-VALORE-LIFO
               WHEN 'M'
                   MOVE WS-GIA-VAL-MEDIO TO WS-MOV-PREZZO
                   COMPUTE WS-MOV-VALORE = 
                       WS-MOV-QUANTITA * WS-MOV-PREZZO
               WHEN OTHER
                   DISPLAY "Metodo valorizzazione non valido!"
                   MOVE "KO" TO WS-ESITO
           END-EVALUATE.
       
       CALCOLA-VALORE-FIFO.
           MOVE WS-MOV-QUANTITA TO WS-QTA-RICHIESTA
           MOVE ZERO TO WS-MOV-VALORE
           MOVE ZERO TO WS-VALORE-MEDIO
           
           EXEC SQL
               OPEN CUR-LOTTI-FIFO
           END-EXEC
           
           PERFORM UNTIL WS-QTA-RICHIESTA = 0 OR SQLCODE NOT = 0
               EXEC SQL
                   FETCH CUR-LOTTI-FIFO
                   INTO :WS-LOT-ID, :WS-LOT-NUMERO,
                        :WS-LOT-QTA-RES, :WS-LOT-PREZZO,
                        :WS-LOT-DATA
               END-EXEC
               
               IF SQLCODE = 0
                   IF WS-LOT-QTA-RES >= WS-QTA-RICHIESTA
      * Lotto copre tutta la richiesta
                       COMPUTE WS-VALORE-TOT = 
                           WS-QTA-RICHIESTA * WS-LOT-PREZZO
                       ADD WS-VALORE-TOT TO WS-MOV-VALORE
                       MOVE ZERO TO WS-QTA-RICHIESTA
                   ELSE
      * Lotto copre parzialmente
                       COMPUTE WS-VALORE-TOT = 
                           WS-LOT-QTA-RES * WS-LOT-PREZZO
                       ADD WS-VALORE-TOT TO WS-MOV-VALORE
                       SUBTRACT WS-LOT-QTA-RES FROM WS-QTA-RICHIESTA
                   END-IF
               END-IF
           END-PERFORM
           
           EXEC SQL
               CLOSE CUR-LOTTI-FIFO
           END-EXEC
           
           IF WS-QTA-RICHIESTA > 0
               DISPLAY "Lotti insufficienti per FIFO!"
               MOVE "KO" TO WS-ESITO
           ELSE
               COMPUTE WS-MOV-PREZZO = 
                   WS-MOV-VALORE / WS-MOV-QUANTITA
           END-IF.
       
       CALCOLA-VALORE-LIFO.
           MOVE WS-MOV-QUANTITA TO WS-QTA-RICHIESTA
           MOVE ZERO TO WS-MOV-VALORE
           MOVE ZERO TO WS-VALORE-MEDIO
           
           EXEC SQL
               OPEN CUR-LOTTI-LIFO
           END-EXEC
           
           PERFORM UNTIL WS-QTA-RICHIESTA = 0 OR SQLCODE NOT = 0
               EXEC SQL
                   FETCH CUR-LOTTI-LIFO
                   INTO :WS-LOT-ID, :WS-LOT-NUMERO,
                        :WS-LOT-QTA-RES, :WS-LOT-PREZZO,
                        :WS-LOT-DATA
               END-EXEC
               
               IF SQLCODE = 0
                   IF WS-LOT-QTA-RES >= WS-QTA-RICHIESTA
                       COMPUTE WS-VALORE-TOT = 
                           WS-QTA-RICHIESTA * WS-LOT-PREZZO
                       ADD WS-VALORE-TOT TO WS-MOV-VALORE
                       MOVE ZERO TO WS-QTA-RICHIESTA
                   ELSE
                       COMPUTE WS-VALORE-TOT = 
                           WS-LOT-QTA-RES * WS-LOT-PREZZO
                       ADD WS-VALORE-TOT TO WS-MOV-VALORE
                       SUBTRACT WS-LOT-QTA-RES FROM WS-QTA-RICHIESTA
                   END-IF
               END-IF
           END-PERFORM
           
           EXEC SQL
               CLOSE CUR-LOTTI-LIFO
           END-EXEC
           
           IF WS-QTA-RICHIESTA > 0
               DISPLAY "Lotti insufficienti per LIFO!"
               MOVE "KO" TO WS-ESITO
           ELSE
               COMPUTE WS-MOV-PREZZO = 
                   WS-MOV-VALORE / WS-MOV-QUANTITA
           END-IF.
       
       AGGIORNA-LOTTI-SCARICO.
           MOVE WS-MOV-QUANTITA TO WS-QTA-RICHIESTA
           
           IF WS-METODO-VALORIZZ = 'F'
               EXEC SQL
                   OPEN CUR-LOTTI-FIFO
               END-EXEC
           ELSE
               EXEC SQL
                   OPEN CUR-LOTTI-LIFO
               END-EXEC
           END-IF
           
           PERFORM UNTIL WS-QTA-RICHIESTA = 0 OR SQLCODE NOT = 0
               IF WS-METODO-VALORIZZ = 'F'
                   EXEC SQL
                       FETCH CUR-LOTTI-FIFO
                       INTO :WS-LOT-ID, :WS-LOT-NUMERO,
                            :WS-LOT-QTA-RES, :WS-LOT-PREZZO,
                            :WS-LOT-DATA
                   END-EXEC
               ELSE
                   EXEC SQL
                       FETCH CUR-LOTTI-LIFO
                       INTO :WS-LOT-ID, :WS-LOT-NUMERO,
                            :WS-LOT-QTA-RES, :WS-LOT-PREZZO,
                            :WS-LOT-DATA
                   END-EXEC
               END-IF
               
               IF SQLCODE = 0
                   IF WS-LOT-QTA-RES >= WS-QTA-RICHIESTA
      * Aggiorna quantità residua
                       COMPUTE WS-QTA-RESIDUA = 
                           WS-LOT-QTA-RES - WS-QTA-RICHIESTA
                       EXEC SQL
                           UPDATE LOTTI
                           SET quantita_residua = :WS-QTA-RESIDUA
                           WHERE id_lotto = :WS-LOT-ID
                       END-EXEC
                       MOVE ZERO TO WS-QTA-RICHIESTA
                   ELSE
      * Esaurisci lotto
                       EXEC SQL
                           UPDATE LOTTI
                           SET quantita_residua = 0,
                               stato = 'E'
                           WHERE id_lotto = :WS-LOT-ID
                       END-EXEC
                       SUBTRACT WS-LOT-QTA-RES FROM WS-QTA-RICHIESTA
                   END-IF
               END-IF
           END-PERFORM
           
           IF WS-METODO-VALORIZZ = 'F'
               EXEC SQL
                   CLOSE CUR-LOTTI-FIFO
               END-EXEC
           ELSE
               EXEC SQL
                   CLOSE CUR-LOTTI-LIFO
               END-EXEC
           END-IF.
       
       VISUALIZZA-GIACENZA.
           DISPLAY " "
           DISPLAY "=== VISUALIZZA GIACENZA ==="
           
           DISPLAY "Codice articolo: " WITH NO ADVANCING
           ACCEPT WS-ART-CODICE
           
           PERFORM CARICA-ARTICOLO
           IF WS-ESITO NOT = "OK"
               EXIT PARAGRAPH
           END-IF
           
           DISPLAY " "
           DISPLAY "ARTICOLO: " WS-ART-CODICE
           DISPLAY "Descrizione: " WS-ART-DESCRIZIONE(1:60)
           DISPLAY "Categoria: " WS-ART-CATEGORIA 
                   "  UM: " WS-ART-UM
           DISPLAY "Ubicazione: " WS-ART-UBICAZIONE
           DISPLAY " "
           DISPLAY "QUANTITA':"
           MOVE WS-GIA-DISPONIBILE TO WS-QUANTITA-EDIT
           DISPLAY "  Disponibile:     " WS-QUANTITA-EDIT
           MOVE WS-GIA-IMPEGNATA TO WS-QUANTITA-EDIT
           DISPLAY "  Impegnata:       " WS-QUANTITA-EDIT
           MOVE WS-GIA-ORDINATA TO WS-QUANTITA-EDIT
           DISPLAY "  In ordine:       " WS-QUANTITA-EDIT
           COMPUTE WS-QTA-RESIDUA = WS-GIA-DISPONIBILE - 
                                    WS-GIA-IMPEGNATA
           MOVE WS-QTA-RESIDUA TO WS-QUANTITA-EDIT
           DISPLAY "  Netta:           " WS-QUANTITA-EDIT
           DISPLAY " "
           DISPLAY "SCORTE:"
           MOVE WS-ART-SCORTA-MIN TO WS-QUANTITA-EDIT
           DISPLAY "  Scorta minima:   " WS-QUANTITA-EDIT
           MOVE WS-ART-PUNTO-RIORD TO WS-QUANTITA-EDIT
           DISPLAY "  Punto riordino:  " WS-QUANTITA-EDIT
           MOVE WS-ART-LOTTO-RIORD TO WS-QUANTITA-EDIT
           DISPLAY "  Lotto riordino:  " WS-QUANTITA-EDIT
           DISPLAY " "
           DISPLAY "VALORI:"
           MOVE WS-GIA-VAL-MEDIO TO WS-IMPORTO-EDIT
           DISPLAY "  Valore medio:    EUR " WS-IMPORTO-EDIT
           MOVE WS-GIA-VAL-ULTIMO TO WS-IMPORTO-EDIT
           DISPLAY "  Valore ultimo:   EUR " WS-IMPORTO-EDIT
           COMPUTE WS-VALORE-TOT = 
               WS-GIA-DISPONIBILE * WS-GIA-VAL-MEDIO
           MOVE WS-VALORE-TOT TO WS-IMPORTO-EDIT
           DISPLAY "  Valore totale:   EUR " WS-IMPORTO-EDIT
           
      * Controlla stato scorte
           IF WS-GIA-DISPONIBILE <= WS-ART-SCORTA-MIN
               DISPLAY " "
               DISPLAY "*** ATTENZIONE: SCORTA MINIMA RAGGIUNTA ***"
           ELSE IF WS-GIA-DISPONIBILE <= WS-ART-PUNTO-RIORD
               DISPLAY " "
               DISPLAY "*** ATTENZIONE: PUNTO RIORDINO RAGGIUNTO ***"
           END-IF.
       
       LISTA-SOTTOSCORTA.
           DISPLAY " "
           DISPLAY "=== ARTICOLI SOTTOSCORTA ==="
           DISPLAY " "
           
           MOVE ZERO TO WS-CONTA-SOTTOSCORTA
           
           EXEC SQL
               OPEN CUR-SOTTOSCORTA
           END-EXEC
           
           PERFORM UNTIL SQLCODE NOT = 0
               EXEC SQL
                   FETCH CUR-SOTTOSCORTA
                   INTO :WS-ART-CODICE, :WS-ART-DESCRIZIONE,
                        :WS-GIA-DISPONIBILE, :WS-GIA-ORDINATA,
                        :WS-ART-PUNTO-RIORD, :WS-ART-LOTTO-RIORD,
                        :WS-MOV-CAUSALE
               END-EXEC
               
               IF SQLCODE = 0
                   ADD 1 TO WS-CONTA-SOTTOSCORTA
                   
                   DISPLAY WS-ART-CODICE " - " 
                           WS-ART-DESCRIZIONE(1:40)
                   
                   MOVE WS-GIA-DISPONIBILE TO WS-QUANTITA-EDIT
                   DISPLAY "  Disponibile: " WS-QUANTITA-EDIT
                   
                   MOVE WS-ART-PUNTO-RIORD TO WS-QUANTITA-EDIT
                   STRING "  Punto riordino: " WS-QUANTITA-EDIT
                          DELIMITED BY SIZE INTO WS-MOV-CAUSALE
                   
                   COMPUTE WS-QTA-RESIDUA = 
                       WS-ART-PUNTO-RIORD - WS-GIA-DISPONIBILE
                   MOVE WS-QTA-RESIDUA TO WS-QUANTITA-EDIT
                   STRING WS-MOV-CAUSALE(1:30) 
                          "  Mancanti: " WS-QUANTITA-EDIT
                          DELIMITED BY SIZE INTO WS-MOV-CAUSALE
                   DISPLAY WS-MOV-CAUSALE
                   
                   IF WS-GIA-ORDINATA > 0
                       MOVE WS-GIA-ORDINATA TO WS-QUANTITA-EDIT
                       DISPLAY "  In ordine: " WS-QUANTITA-EDIT
                   ELSE
                       MOVE WS-ART-LOTTO-RIORD TO WS-QUANTITA-EDIT
                       DISPLAY "  DA ORDINARE: " WS-QUANTITA-EDIT
                           " (Fornitore: " WS-MOV-CAUSALE(1:20) ")"
                   END-IF
                   
                   DISPLAY " "
               END-IF
           END-PERFORM
           
           EXEC SQL
               CLOSE CUR-SOTTOSCORTA
           END-EXEC
           
           MOVE WS-CONTA-SOTTOSCORTA TO WS-NUMERO-EDIT
           DISPLAY "Totale articoli sottoscorta: " WS-NUMERO-EDIT.
       
       VALORIZZAZIONE-MAGAZZINO.
           DISPLAY " "
           DISPLAY "=== VALORIZZAZIONE MAGAZZINO ==="
           DISPLAY " "
           
           OPEN OUTPUT REPORT-FILE
           
           WRITE REPORT-RECORD FROM 
               "REPORT VALORIZZAZIONE MAGAZZINO"
           MOVE ALL "=" TO REPORT-RECORD
           WRITE REPORT-RECORD
           
           STRING "Data: " FUNCTION CURRENT-DATE(1:10)
                  "    Ora: " FUNCTION CURRENT-DATE(12:8)
                  DELIMITED BY SIZE INTO REPORT-RECORD
           WRITE REPORT-RECORD
           WRITE REPORT-RECORD FROM SPACES
           
           MOVE ZERO TO WS-CONTA-ARTICOLI
           MOVE ZERO TO WS-VALORE-MAGAZZINO
           
           EXEC SQL
               OPEN CUR-ARTICOLI
           END-EXEC
           
           PERFORM UNTIL SQLCODE NOT = 0
               EXEC SQL
                   FETCH CUR-ARTICOLI
                   INTO :WS-ART-CODICE, :WS-ART-DESCRIZIONE,
                        :WS-ART-UM, :WS-ART-UBICAZIONE,
                        :WS-GIA-DISPONIBILE, :WS-GIA-VAL-MEDIO,
                        :WS-ART-SCORTA-MIN, :WS-ART-PUNTO-RIORD
               END-EXEC
               
               IF SQLCODE = 0 AND WS-GIA-DISPONIBILE > 0
                   ADD 1 TO WS-CONTA-ARTICOLI
                   
                   COMPUTE WS-VALORE-TOT = 
                       WS-GIA-DISPONIBILE * WS-GIA-VAL-MEDIO
                   ADD WS-VALORE-TOT TO WS-VALORE-MAGAZZINO
                   
      * Scrivi nel report
                   STRING WS-ART-CODICE " " 
                          WS-ART-DESCRIZIONE(1:40)
                          DELIMITED BY SIZE INTO REPORT-RECORD
                   WRITE REPORT-RECORD
                   
                   MOVE WS-GIA-DISPONIBILE TO WS-QUANTITA-EDIT
                   MOVE WS-GIA-VAL-MEDIO TO WS-IMPORTO-EDIT
                   STRING "  Qtà: " WS-QUANTITA-EDIT " " WS-ART-UM
                          "  Val.medio: " WS-IMPORTO-EDIT
                          DELIMITED BY SIZE INTO REPORT-RECORD
                   
                   MOVE WS-VALORE-TOT TO WS-IMPORTO-EDIT
                   STRING REPORT-RECORD(1:60) 
                          "  Totale: EUR " WS-IMPORTO-EDIT
                          DELIMITED BY SIZE INTO REPORT-RECORD
                   WRITE REPORT-RECORD
               END-IF
           END-PERFORM
           
           EXEC SQL
               CLOSE CUR-ARTICOLI
           END-EXEC
           
           WRITE REPORT-RECORD FROM SPACES
           MOVE ALL "-" TO REPORT-RECORD
           WRITE REPORT-RECORD
           
           MOVE WS-CONTA-ARTICOLI TO WS-NUMERO-EDIT
           STRING "Articoli valorizzati: " WS-NUMERO-EDIT
                  DELIMITED BY SIZE INTO REPORT-RECORD
           WRITE REPORT-RECORD
           
           MOVE WS-VALORE-MAGAZZINO TO WS-IMPORTO-EDIT
           STRING "VALORE TOTALE MAGAZZINO: EUR " WS-IMPORTO-EDIT
                  DELIMITED BY SIZE INTO REPORT-RECORD
           WRITE REPORT-RECORD
           
           CLOSE REPORT-FILE
           
           DISPLAY "Report salvato in REPORT-MAGAZZINO.TXT"
           DISPLAY " "
           DISPLAY "Articoli valorizzati: " WS-NUMERO-EDIT
           DISPLAY "Valore totale: EUR " WS-IMPORTO-EDIT.
       
       MOVIMENTI-ARTICOLO.
           DISPLAY " "
           DISPLAY "=== MOVIMENTI ARTICOLO ==="
           
           DISPLAY "Codice articolo: " WITH NO ADVANCING
           ACCEPT WS-ART-CODICE
           
           PERFORM CARICA-ARTICOLO
           IF WS-ESITO NOT = "OK"
               EXIT PARAGRAPH
           END-IF
           
           DISPLAY " "
           DISPLAY "Ultimi movimenti di: " WS-ART-DESCRIZIONE(1:50)
           DISPLAY " "
           DISPLAY "TIPO  DATA/ORA            DOCUMENTO         " 
                   "     QUANTITA    VALORE"
           DISPLAY ALL "-"
           
           EXEC SQL
               OPEN CUR-MOVIMENTI
           END-EXEC
           
           PERFORM UNTIL SQLCODE NOT = 0
               EXEC SQL
                   FETCH CUR-MOVIMENTI
                   INTO :WS-MOV-TIPO, :WS-MOV-NUMERO-DOC,
                        :WS-MOV-DATA, :WS-MOV-QUANTITA,
                        :WS-MOV-PREZZO, :WS-MOV-VALORE,
                        :WS-MOV-CAUSALE
               END-EXEC
               
               IF SQLCODE = 0
                   DISPLAY WS-MOV-TIPO "    " 
                           WS-MOV-DATA(1:19) " "
                           WS-MOV-NUMERO-DOC(1:15)
                   
                   MOVE WS-MOV-QUANTITA TO WS-QUANTITA-EDIT
                   MOVE WS-MOV-VALORE TO WS-IMPORTO-EDIT
                   
                   IF WS-MOV-TIPO = "SC" OR "RE"
                       DISPLAY "                                    -" 
                               WS-QUANTITA-EDIT "  EUR " 
                               WS-IMPORTO-EDIT
                   ELSE
                       DISPLAY "                                    +" 
                               WS-QUANTITA-EDIT "  EUR " 
                               WS-IMPORTO-EDIT
                   END-IF
                   
                   DISPLAY "      " WS-MOV-CAUSALE(1:50)
                   DISPLAY " "
               END-IF
           END-PERFORM
           
           EXEC SQL
               CLOSE CUR-MOVIMENTI
           END-EXEC.
       
       RETTIFICA-INVENTARIO.
           DISPLAY " "
           DISPLAY "=== RETTIFICA INVENTARIO ==="
           
           DISPLAY "Codice articolo: " WITH NO ADVANCING
           ACCEPT WS-ART-CODICE
           
           PERFORM CARICA-ARTICOLO
           IF WS-ESITO NOT = "OK"
               EXIT PARAGRAPH
           END-IF
           
           DISPLAY "Articolo: " WS-ART-DESCRIZIONE(1:50)
           MOVE WS-GIA-DISPONIBILE TO WS-QUANTITA-EDIT
           DISPLAY "Giacenza attuale: " WS-QUANTITA-EDIT " " WS-ART-UM
           
           DISPLAY "Giacenza rilevata: " WITH NO ADVANCING
           ACCEPT WS-QTA-RICHIESTA
           
           COMPUTE WS-MOV-QUANTITA = 
               WS-QTA-RICHIESTA - WS-GIA-DISPONIBILE
           
           IF WS-MOV-QUANTITA = 0
               DISPLAY "Nessuna differenza rilevata"
               EXIT PARAGRAPH
           END-IF
           
           MOVE WS-MOV-QUANTITA TO WS-QUANTITA-EDIT
           IF WS-MOV-QUANTITA > 0
               DISPLAY "Differenza: +" WS-QUANTITA-EDIT
               MOVE 'RI' TO WS-MOV-TIPO
               MOVE "Rettifica inventario positiva" TO WS-MOV-CAUSALE
           ELSE
               DISPLAY "Differenza: " WS-QUANTITA-EDIT
               MOVE 'RI' TO WS-MOV-TIPO
               MOVE "Rettifica inventario negativa" TO WS-MOV-CAUSALE
               MULTIPLY WS-MOV-QUANTITA BY -1 GIVING WS-MOV-QUANTITA
           END-IF
           
           DISPLAY "Note rettifica: " WITH NO ADVANCING
           ACCEPT WS-MOV-CAUSALE(31:70)
           
           DISPLAY "Confermare rettifica (S/N): " WITH NO ADVANCING
           ACCEPT WS-RISPOSTA
           
           IF WS-RISPOSTA = 'S' OR 's'
               MOVE "INV" TO WS-MOV-NUMERO-DOC
               STRING WS-MOV-NUMERO-DOC FUNCTION CURRENT-DATE(1:8)
                      DELIMITED BY SIZE INTO WS-MOV-NUMERO-DOC
               
               MOVE WS-GIA-VAL-MEDIO TO WS-MOV-PREZZO
               COMPUTE WS-MOV-VALORE = WS-MOV-QUANTITA * WS-MOV-PREZZO
               
               EXEC SQL
                   BEGIN TRANSACTION
               END-EXEC
               
               PERFORM REGISTRA-MOVIMENTO
               
               IF WS-ESITO = "OK"
                   EXEC SQL
                       UPDATE GIACENZE
                       SET quantita_disponibile = :WS-QTA-RICHIESTA,
                           data_ultimo_inventario = CURRENT_DATE
                       WHERE codice_articolo = :WS-ART-CODICE
                   END-EXEC
                   
                   IF SQLCODE = 0
                       EXEC SQL
                           COMMIT
                       END-EXEC
                       DISPLAY "Rettifica eseguita con successo!"
                   ELSE
                       EXEC SQL
                           ROLLBACK
                       END-EXEC
                       DISPLAY "Errore durante la rettifica!"
                   END-IF
               ELSE
                   EXEC SQL
                       ROLLBACK
                   END-EXEC
               END-IF
           ELSE
               DISPLAY "Rettifica annullata"
           END-IF.
       
       GESTIONE-ORDINI.
           DISPLAY " "
           DISPLAY "=== GESTIONE ORDINI FORNITORI ==="
           DISPLAY " "
           DISPLAY "1. Nuovo ordine"
           DISPLAY "2. Visualizza ordini aperti"
           DISPLAY "3. Ricevi merce da ordine"
           DISPLAY "4. Stato ordine"
           DISPLAY " "
           DISPLAY "Scelta: " WITH NO ADVANCING
           ACCEPT WS-SCELTA
           
           EVALUATE WS-SCELTA
               WHEN 1
                   PERFORM NUOVO-ORDINE
               WHEN 2
                   PERFORM VISUALIZZA-ORDINI-APERTI
               WHEN 3
                   PERFORM RICEVI-MERCE-ORDINE
               WHEN 4
                   PERFORM STATO-ORDINE
               WHEN OTHER
                   DISPLAY "Scelta non valida!"
           END-EVALUATE.
       
       NUOVO-ORDINE.
           DISPLAY " "
           DISPLAY "=== NUOVO ORDINE FORNITORE ==="
           
           DISPLAY "Codice fornitore: " WITH NO ADVANCING
           ACCEPT WS-ORD-FORNITORE
           
      * Verifica fornitore
           EXEC SQL
               SELECT ragione_sociale
               INTO :WS-MOV-CAUSALE
               FROM FORNITORI
               WHERE codice_fornitore = :WS-ORD-FORNITORE
               AND stato = 'A'
           END-EXEC
           
           IF SQLCODE = 100
               DISPLAY "Fornitore non trovato o non attivo!"
               EXIT PARAGRAPH
           END-IF
           
           DISPLAY "Fornitore: " WS-MOV-CAUSALE
           
      * Genera numero ordine
           STRING "ORD" FUNCTION CURRENT-DATE(1:4) "/"
                  FUNCTION CURRENT-DATE(6:2) 
                  FUNCTION CURRENT-DATE(9:2)
                  FUNCTION CURRENT-DATE(12:6)
                  DELIMITED BY SIZE INTO WS-ORD-NUMERO
           
           MOVE FUNCTION CURRENT-DATE(1:10) TO WS-ORD-DATA
           MOVE 'A' TO WS-ORD-STATO
           MOVE ZERO TO WS-ORD-TOTALE
           
           EXEC SQL
               INSERT INTO ORDINI
               (numero_ordine, data_ordine, codice_fornitore,
                stato_ordine, totale_ordine)
               VALUES
               (:WS-ORD-NUMERO, CURRENT_DATE, :WS-ORD-FORNITORE,
                :WS-ORD-STATO, :WS-ORD-TOTALE)
           END-EXEC
           
           IF SQLCODE = 0
               DISPLAY "Ordine " WS-ORD-NUMERO " creato!"
               DISPLAY " "
               PERFORM AGGIUNGI-RIGHE-ORDINE
           ELSE
               DISPLAY "Errore creazione ordine: " SQLCODE
           END-IF.
       
       AGGIUNGI-RIGHE-ORDINE.
           MOVE 'S' TO WS-CONTINUA
           
           PERFORM UNTIL WS-CONTINUA NOT = 'S' AND NOT = 's'
               DISPLAY "Codice articolo: " WITH NO ADVANCING
               ACCEPT WS-ART-CODICE
               
               PERFORM CARICA-ARTICOLO
               IF WS-ESITO = "OK"
                   DISPLAY "Articolo: " WS-ART-DESCRIZIONE(1:40)
                   
                   DISPLAY "Quantità da ordinare: " WITH NO ADVANCING
                   ACCEPT WS-MOV-QUANTITA
                   
                   IF WS-MOV-QUANTITA > 0
                       MOVE WS-ART-PREZZO-ACQ TO WS-MOV-PREZZO
                       COMPUTE WS-MOV-VALORE = 
                           WS-MOV-QUANTITA * WS-MOV-PREZZO
                       
                       EXEC SQL
                           INSERT INTO RIGHE_ORDINE
                           (numero_ordine, codice_articolo,
                            quantita_ordinata, prezzo_unitario,
                            importo_riga)
                           VALUES
                           (:WS-ORD-NUMERO, :WS-ART-CODICE,
                            :WS-MOV-QUANTITA, :WS-MOV-PREZZO,
                            :WS-MOV-VALORE)
                       END-EXEC
                       
                       IF SQLCODE = 0
                           ADD WS-MOV-VALORE TO WS-ORD-TOTALE
                           
      * Aggiorna quantità ordinata
                           EXEC SQL
                               UPDATE GIACENZE
                               SET quantita_ordinata = 
                                   quantita_ordinata + :WS-MOV-QUANTITA
                               WHERE codice_articolo = :WS-ART-CODICE
                           END-EXEC
                           
                           DISPLAY "Riga ordine aggiunta!"
                       ELSE
                           DISPLAY "Errore aggiunta riga: " SQLCODE
                       END-IF
                   END-IF
               END-IF
               
               DISPLAY "Aggiungere altro articolo (S/N): " 
                       WITH NO ADVANCING
               ACCEPT WS-CONTINUA
           END-PERFORM
           
      * Aggiorna totale ordine
           EXEC SQL
               UPDATE ORDINI
               SET totale_ordine = :WS-ORD-TOTALE
               WHERE numero_ordine = :WS-ORD-NUMERO
           END-EXEC
           
           MOVE WS-ORD-TOTALE TO WS-IMPORTO-EDIT
           DISPLAY " "
           DISPLAY "Ordine completato. Totale: EUR " WS-IMPORTO-EDIT.
       
       VISUALIZZA-ORDINI-APERTI.
           DISPLAY " "
           DISPLAY "=== ORDINI APERTI ==="
           DISPLAY " "
           
           EXEC SQL
               DECLARE CUR-ORDINI-APERTI CURSOR FOR
               SELECT o.numero_ordine, o.data_ordine,
                      f.ragione_sociale, o.totale_ordine,
                      o.stato_ordine
               FROM ORDINI o
               JOIN FORNITORI f ON o.codice_fornitore = f.codice_fornitore
               WHERE o.stato_ordine IN ('A', 'C', 'P')
               ORDER BY o.data_ordine DESC
           END-EXEC
           
           EXEC SQL
               OPEN CUR-ORDINI-APERTI
           END-EXEC
           
           MOVE ZERO TO WS-CONTA-ARTICOLI
           
           PERFORM UNTIL SQLCODE NOT = 0
               EXEC SQL
                   FETCH CUR-ORDINI-APERTI
                   INTO :WS-ORD-NUMERO, :WS-ORD-DATA,
                        :WS-MOV-CAUSALE, :WS-ORD-TOTALE,
                        :WS-ORD-STATO
               END-EXEC
               
               IF SQLCODE = 0
                   ADD 1 TO WS-CONTA-ARTICOLI
                   
                   DISPLAY WS-ORD-NUMERO " del " WS-ORD-DATA
                   DISPLAY "  Fornitore: " WS-MOV-CAUSALE(1:30)
                   MOVE WS-ORD-TOTALE TO WS-IMPORTO-EDIT
                   DISPLAY "  Totale: EUR " WS-IMPORTO-EDIT
                   
                   EVALUATE WS-ORD-STATO
                       WHEN 'A'
                           DISPLAY "  Stato: APERTO"
                       WHEN 'C'
                           DISPLAY "  Stato: CONFERMATO"
                       WHEN 'P'
                           DISPLAY "  Stato: PARZIALMENTE EVASO"
                   END-EVALUATE
                   
                   DISPLAY " "
               END-IF
           END-PERFORM
           
           EXEC SQL
               CLOSE CUR-ORDINI-APERTI
           END-EXEC
           
           IF WS-CONTA-ARTICOLI = 0
               DISPLAY "Nessun ordine aperto"
           END-IF.
       
       RICEVI-MERCE-ORDINE.
           DISPLAY " "
           DISPLAY "=== RICEVI MERCE DA ORDINE ==="
           
           DISPLAY "Numero ordine: " WITH NO ADVANCING
           ACCEPT WS-ORD-NUMERO
           
      * Verifica ordine
           EXEC SQL
               SELECT stato_ordine, codice_fornitore
               INTO :WS-ORD-STATO, :WS-ORD-FORNITORE
               FROM ORDINI
               WHERE numero_ordine = :WS-ORD-NUMERO
           END-EXEC
           
           IF SQLCODE = 100
               DISPLAY "Ordine non trovato!"
               EXIT PARAGRAPH
           ELSE IF WS-ORD-STATO = 'E'
               DISPLAY "Ordine già completamente evaso!"
               EXIT PARAGRAPH
           END-IF
           
           DISPLAY " "
           DISPLAY "Righe ordine da ricevere:"
           DISPLAY " "
           
      * Mostra righe non evase
           EXEC SQL
               DECLARE CUR-RIGHE-ORDINE CURSOR FOR
               SELECT r.codice_articolo, a.descrizione,
                      r.quantita_ordinata, r.quantita_ricevuta,
                      r.prezzo_unitario
               FROM RIGHE_ORDINE r
               JOIN ARTICOLI a ON r.codice_articolo = a.codice_articolo
               WHERE r.numero_ordine = :WS-ORD-NUMERO
               AND r.quantita_ricevuta < r.quantita_ordinata
               AND r.stato_riga <> 'C'
           END-EXEC
           
           EXEC SQL
               OPEN CUR-RIGHE-ORDINE
           END-EXEC
           
           PERFORM UNTIL SQLCODE NOT = 0
               EXEC SQL
                   FETCH CUR-RIGHE-ORDINE
                   INTO :WS-ART-CODICE, :WS-ART-DESCRIZIONE,
                        :WS-QTA-RICHIESTA, :WS-QTA-PRELEVATA,
                        :WS-MOV-PREZZO
               END-EXEC
               
               IF SQLCODE = 0
                   DISPLAY WS-ART-CODICE " - " 
                           WS-ART-DESCRIZIONE(1:35)
                   
                   COMPUTE WS-QTA-RESIDUA = 
                       WS-QTA-RICHIESTA - WS-QTA-PRELEVATA
                   MOVE WS-QTA-RESIDUA TO WS-QUANTITA-EDIT
                   DISPLAY "  Da ricevere: " WS-QUANTITA-EDIT
                   
                   DISPLAY "  Quantità ricevuta: " WITH NO ADVANCING
                   ACCEPT WS-MOV-QUANTITA
                   
                   IF WS-MOV-QUANTITA > 0
                       IF WS-MOV-QUANTITA > WS-QTA-RESIDUA
                           DISPLAY "  Quantità eccessiva!"
                           MOVE WS-QTA-RESIDUA TO WS-MOV-QUANTITA
                       END-IF
                       
      * Registra carico
                       MOVE WS-ORD-FORNITORE TO WS-MOV-FORNITORE
                       STRING "Ricevimento ordine " WS-ORD-NUMERO
                              DELIMITED BY SIZE INTO WS-MOV-CAUSALE
                       MOVE WS-ORD-NUMERO TO WS-MOV-NUMERO-DOC
                       
                       COMPUTE WS-MOV-VALORE = 
                           WS-MOV-QUANTITA * WS-MOV-PREZZO
                       
                       EXEC SQL
                           BEGIN TRANSACTION
                       END-EXEC
                       
                       MOVE 'CA' TO WS-MOV-TIPO
                       PERFORM REGISTRA-MOVIMENTO
                       
                       IF WS-ESITO = "OK"
                           PERFORM AGGIORNA-GIACENZA-CARICO
                           
                           IF WS-ESITO = "OK"
      * Aggiorna riga ordine
                               EXEC SQL
                                   UPDATE RIGHE_ORDINE
                                   SET quantita_ricevuta = 
                                       quantita_ricevuta + :WS-MOV-QUANTITA,
                                       stato_riga = CASE
                                           WHEN quantita_ricevuta + :WS-MOV-QUANTITA
                                                >= quantita_ordinata
                                           THEN 'E'
                                           ELSE 'P'
                                       END
                                   WHERE numero_ordine = :WS-ORD-NUMERO
                                   AND codice_articolo = :WS-ART-CODICE
                               END-EXEC
                               
      * Aggiorna quantità ordinata in giacenze
                               EXEC SQL
                                   UPDATE GIACENZE
                                   SET quantita_ordinata = GREATEST(0,
                                       quantita_ordinata - :WS-MOV-QUANTITA)
                                   WHERE codice_articolo = :WS-ART-CODICE
                               END-EXEC
                               
                               EXEC SQL
                                   COMMIT
                               END-EXEC
                               
                               DISPLAY "  Carico registrato!"
                           ELSE
                               EXEC SQL
                                   ROLLBACK
                               END-EXEC
                           END-IF
                       ELSE
                           EXEC SQL
                               ROLLBACK
                           END-EXEC
                       END-IF
                   END-IF
                   
                   DISPLAY " "
               END-IF
           END-PERFORM
           
           EXEC SQL
               CLOSE CUR-RIGHE-ORDINE
           END-EXEC
           
      * Aggiorna stato ordine
           PERFORM AGGIORNA-STATO-ORDINE.
       
       AGGIORNA-STATO-ORDINE.
           EXEC SQL
               UPDATE ORDINI
               SET stato_ordine = 
                   CASE
                       WHEN NOT EXISTS (
                           SELECT 1 FROM RIGHE_ORDINE
                           WHERE numero_ordine = :WS-ORD-NUMERO
                           AND quantita_ricevuta < quantita_ordinata
                           AND stato_riga <> 'C'
                       ) THEN 'E'
                       WHEN EXISTS (
                           SELECT 1 FROM RIGHE_ORDINE
                           WHERE numero_ordine = :WS-ORD-NUMERO
                           AND quantita_ricevuta > 0
                       ) THEN 'P'
                       ELSE stato_ordine
                   END
               WHERE numero_ordine = :WS-ORD-NUMERO
           END-EXEC.
       
       STATO-ORDINE.
           DISPLAY " "
           DISPLAY "=== STATO ORDINE ==="
           
           DISPLAY "Numero ordine: " WITH NO ADVANCING
           ACCEPT WS-ORD-NUMERO
           
           EXEC SQL
               SELECT o.data_ordine, o.stato_ordine,
                      f.ragione_sociale, o.totale_ordine
               INTO :WS-ORD-DATA, :WS-ORD-STATO,
                    :WS-MOV-CAUSALE, :WS-ORD-TOTALE
               FROM ORDINI o
               JOIN FORNITORI f ON o.codice_fornitore = f.codice_fornitore
               WHERE o.numero_ordine = :WS-ORD-NUMERO
           END-EXEC
           
           IF SQLCODE = 100
               DISPLAY "Ordine non trovato!"
               EXIT PARAGRAPH
           END-IF
           
           DISPLAY " "
           DISPLAY "Ordine: " WS-ORD-NUMERO
           DISPLAY "Data: " WS-ORD-DATA
           DISPLAY "Fornitore: " WS-MOV-CAUSALE
           MOVE WS-ORD-TOTALE TO WS-IMPORTO-EDIT
           DISPLAY "Totale: EUR " WS-IMPORTO-EDIT
           
           EVALUATE WS-ORD-STATO
               WHEN 'A'
                   DISPLAY "Stato: APERTO"
               WHEN 'C'
                   DISPLAY "Stato: CONFERMATO"
               WHEN 'P'
                   DISPLAY "Stato: PARZIALMENTE EVASO"
               WHEN 'E'
                   DISPLAY "Stato: EVASO"
           END-EVALUATE
           
           DISPLAY " "
           DISPLAY "Dettaglio righe:"
           DISPLAY " "
           
           EXEC SQL
               SELECT r.codice_articolo, a.descrizione,
                      r.quantita_ordinata, r.quantita_ricevuta,
                      r.importo_riga, r.stato_riga
               FROM RIGHE_ORDINE r
               JOIN ARTICOLI a ON r.codice_articolo = a.codice_articolo
               WHERE r.numero_ordine = :WS-ORD-NUMERO
               ORDER BY r.codice_articolo
           END-EXEC
           
           PERFORM UNTIL SQLCODE NOT = 0
               EXEC SQL
                   FETCH NEXT FROM RIGHE_ORDINE
                   INTO :WS-ART-CODICE, :WS-ART-DESCRIZIONE,
                        :WS-QTA-RICHIESTA, :WS-QTA-PRELEVATA,
                        :WS-MOV-VALORE, :WS-RISPOSTA
               END-EXEC
               
               IF SQLCODE = 0
                   DISPLAY WS-ART-CODICE " - " 
                           WS-ART-DESCRIZIONE(1:30)
                   
                   MOVE WS-QTA-RICHIESTA TO WS-QUANTITA-EDIT
                   DISPLAY "  Ordinata: " WS-QUANTITA-EDIT
                   
                   MOVE WS-QTA-PRELEVATA TO WS-QUANTITA-EDIT
                   STRING "  Ricevuta: " WS-QUANTITA-EDIT
                          DELIMITED BY SIZE INTO WS-MOV-CAUSALE
                   
                   EVALUATE WS-RISPOSTA
                       WHEN 'A'
                           STRING WS-MOV-CAUSALE(1:25) " (APERTA)"
                                  DELIMITED BY SIZE INTO WS-MOV-CAUSALE
                       WHEN 'P'
                           STRING WS-MOV-CAUSALE(1:25) " (PARZIALE)"
                                  DELIMITED BY SIZE INTO WS-MOV-CAUSALE
                       WHEN 'E'
                           STRING WS-MOV-CAUSALE(1:25) " (EVASA)"
                                  DELIMITED BY SIZE INTO WS-MOV-CAUSALE
                       WHEN 'C'
                           STRING WS-MOV-CAUSALE(1:25) " (CANCELLATA)"
                                  DELIMITED BY SIZE INTO WS-MOV-CAUSALE
                   END-EVALUATE
                   
                   DISPLAY WS-MOV-CAUSALE
                   DISPLAY " "
               END-IF
           END-PERFORM.
       
       REPORT-INVENTARIO.
           DISPLAY " "
           DISPLAY "=== REPORT INVENTARIO FISICO ==="
           
           OPEN OUTPUT INVENTARIO-FILE
           
           WRITE INVENTARIO-RECORD FROM 
               "LISTA INVENTARIO FISICO"
           MOVE ALL "=" TO INVENTARIO-RECORD
           WRITE INVENTARIO-RECORD
           
           STRING "Data: " FUNCTION CURRENT-DATE(1:10)
                  "    Ora: " FUNCTION CURRENT-DATE(12:8)
                  DELIMITED BY SIZE INTO INVENTARIO-RECORD
           WRITE INVENTARIO-RECORD
           WRITE INVENTARIO-RECORD FROM SPACES
           
           WRITE INVENTARIO-RECORD FROM
               "CODICE      DESCRIZIONE                     " &
               "UB.   QTA TEORICA  QTA RILEVATA  DIFFERENZA"
           MOVE ALL "-" TO INVENTARIO-RECORD
           WRITE INVENTARIO-RECORD
           
           EXEC SQL
               DECLARE CUR-INVENTARIO CURSOR FOR
               SELECT a.codice_articolo, a.descrizione,
                      a.ubicazione, g.quantita_disponibile,
                      a.unita_misura
               FROM ARTICOLI a
               JOIN GIACENZE g ON a.codice_articolo = g.codice_articolo
               WHERE a.stato = 'A'
               ORDER BY a.ubicazione, a.codice_articolo
           END-EXEC
           
           EXEC SQL
               OPEN CUR-INVENTARIO
           END-EXEC
           
           PERFORM UNTIL SQLCODE NOT = 0
               EXEC SQL
                   FETCH CUR-INVENTARIO
                   INTO :WS-ART-CODICE, :WS-ART-DESCRIZIONE,
                        :WS-ART-UBICAZIONE, :WS-GIA-DISPONIBILE,
                        :WS-ART-UM
               END-EXEC
               
               IF SQLCODE = 0
                   STRING WS-ART-CODICE " " 
                          WS-ART-DESCRIZIONE(1:30) " "
                          WS-ART-UBICAZIONE(1:5)
                          DELIMITED BY SIZE INTO INVENTARIO-RECORD
                   
                   MOVE WS-GIA-DISPONIBILE TO WS-QUANTITA-EDIT
                   STRING INVENTARIO-RECORD(1:50) 
                          WS-QUANTITA-EDIT
                          " ____________  ____________"
                          DELIMITED BY SIZE INTO INVENTARIO-RECORD
                   
                   WRITE INVENTARIO-RECORD
               END-IF
           END-PERFORM
           
           EXEC SQL
               CLOSE CUR-INVENTARIO
           END-EXEC
           
           WRITE INVENTARIO-RECORD FROM SPACES
           MOVE ALL "-" TO INVENTARIO-RECORD
           WRITE INVENTARIO-RECORD
           
           WRITE INVENTARIO-RECORD FROM 
               "Rilevato da: ________________  " &
               "Data: ________  Firma: ________________"
           
           CLOSE INVENTARIO-FILE
           
           DISPLAY "Report salvato in INVENTARIO.TXT".
       
       ANALISI-ABC.
           DISPLAY " "
           DISPLAY "=== ANALISI ABC ARTICOLI ==="
           DISPLAY " "
           
           EXEC SQL
               CREATE TEMP TABLE ANALISI_ABC AS
               SELECT a.codice_articolo, a.descrizione,
                      g.quantita_disponibile,
                      g.valore_medio,
                      (g.quantita_disponibile * g.valore_medio) as valore_tot,
                      0.0 as percentuale,
                      0.0 as perc_cumulata,
                      ' ' as classe
               FROM ARTICOLI a
               JOIN GIACENZE g ON a.codice_articolo = g.codice_articolo
               WHERE a.stato = 'A'
               AND g.quantita_disponibile > 0
               ORDER BY valore_tot DESC
           END-EXEC
           
      * Calcola totale e percentuali
           EXEC SQL
               SELECT SUM(valore_tot)
               INTO :WS-VALORE-MAGAZZINO
               FROM ANALISI_ABC
           END-EXEC
           
           EXEC SQL
               UPDATE ANALISI_ABC
               SET percentuale = (valore_tot / :WS-VALORE-MAGAZZINO) * 100
           END-EXEC
           
      * Calcola percentuali cumulate e classi ABC
           EXEC SQL
               UPDATE ANALISI_ABC a1
               SET perc_cumulata = (
                   SELECT SUM(a2.percentuale)
                   FROM ANALISI_ABC a2
                   WHERE a2.valore_tot >= a1.valore_tot
               ),
               classe = CASE
                   WHEN perc_cumulata <= 80 THEN 'A'
                   WHEN perc_cumulata <= 95 THEN 'B'
                   ELSE 'C'
               END
           END-EXEC
           
           DISPLAY "CLASSE A - Alto valore (80% del valore)"
           DISPLAY ALL "-"
           
           EXEC SQL
               DECLARE CUR-ABC-A CURSOR FOR
               SELECT codice_articolo, descrizione,
                      valore_tot, percentuale, perc_cumulata
               FROM ANALISI_ABC
               WHERE classe = 'A'
               ORDER BY valore_tot DESC
           END-EXEC
           
           EXEC SQL
               OPEN CUR-ABC-A
           END-EXEC
           
           MOVE ZERO TO WS-CONTA-ARTICOLI
           
           PERFORM UNTIL SQLCODE NOT = 0
               EXEC SQL
                   FETCH CUR-ABC-A
                   INTO :WS-ART-CODICE, :WS-ART-DESCRIZIONE,
                        :WS-VALORE-TOT, :WS-ALIQUOTA,
                        :WS-VALORE-MEDIO
               END-EXEC
               
               IF SQLCODE = 0
                   ADD 1 TO WS-CONTA-ARTICOLI
                   
                   DISPLAY WS-ART-CODICE " - " 
                           WS-ART-DESCRIZIONE(1:35)
                   
                   MOVE WS-VALORE-TOT TO WS-IMPORTO-EDIT
                   MOVE WS-ALIQUOTA TO WS-PERC-EDIT
                   DISPLAY "  Valore: EUR " WS-IMPORTO-EDIT
                           "  (" WS-PERC-EDIT "%)"
               END-IF
           END-PERFORM
           
           EXEC SQL
               CLOSE CUR-ABC-A
           END-EXEC
           
           MOVE WS-CONTA-ARTICOLI TO WS-NUMERO-EDIT
           DISPLAY " "
           DISPLAY "Articoli classe A: " WS-NUMERO-EDIT
           DISPLAY " "
           
           DISPLAY "CLASSE B - Medio valore (15% del valore)"
           DISPLAY ALL "-"
           
           EXEC SQL
               SELECT COUNT(*), SUM(valore_tot)
               INTO :WS-CONTA-ARTICOLI, :WS-VALORE-TOT
               FROM ANALISI_ABC
               WHERE classe = 'B'
           END-EXEC
           
           MOVE WS-CONTA-ARTICOLI TO WS-NUMERO-EDIT
           MOVE WS-VALORE-TOT TO WS-IMPORTO-EDIT
           DISPLAY "Articoli: " WS-NUMERO-EDIT
                   "  Valore totale: EUR " WS-IMPORTO-EDIT
           DISPLAY " "
           
           DISPLAY "CLASSE C - Basso valore (5% del valore)"
           DISPLAY ALL "-"
           
           EXEC SQL
               SELECT COUNT(*), SUM(valore_tot)
               INTO :WS-CONTA-ARTICOLI, :WS-VALORE-TOT
               FROM ANALISI_ABC
               WHERE classe = 'C'
           END-EXEC
           
           MOVE WS-CONTA-ARTICOLI TO WS-NUMERO-EDIT
           MOVE WS-VALORE-TOT TO WS-IMPORTO-EDIT
           DISPLAY "Articoli: " WS-NUMERO-EDIT
                   "  Valore totale: EUR " WS-IMPORTO-EDIT
           
           EXEC SQL
               DROP TABLE ANALISI_ABC
           END-EXEC.