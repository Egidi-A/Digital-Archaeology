      ******************************************************************
      * SISTEMA DI GESTIONE PAGHE E STIPENDI
      * Programma principale per elaborazione stipendi mensili
      * Include: calcolo stipendi, trattenute, cedolini
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GESTIONE-PAGHE.
       AUTHOR. ANNALISA-EGIDI.
       DATE-WRITTEN. 2025-05-20.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CEDOLINO-FILE ASSIGN TO "CEDOLINO.TXT"
                  ORGANIZATION IS LINE SEQUENTIAL.
           
           SELECT REPORT-FILE ASSIGN TO "REPORT-STIPENDI.TXT"
                  ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD  CEDOLINO-FILE.
       01  CEDOLINO-RECORD PIC X(132).
       
       FD  REPORT-FILE.
       01  REPORT-RECORD PIC X(132).
       
       WORKING-STORAGE SECTION.
       01  WS-SCELTA               PIC 9 VALUE ZERO.
       01  WS-CONTINUA             PIC X VALUE 'S'.
       01  WS-RISPOSTA             PIC X.
       01  WS-ESITO                PIC X(2).
       
      * Parametri elaborazione
       01  WS-ANNO                 PIC 9(4).
       01  WS-MESE                 PIC 99.
       01  WS-MATRICOLA            PIC X(6).
       01  WS-DATA-ELABORAZIONE    PIC X(10).
       
      * Contatori e totali
       01  WS-CONTA-DIPENDENTI     PIC 999 VALUE ZERO.
       01  WS-CONTA-ELABORATI      PIC 999 VALUE ZERO.
       01  WS-TOTALE-LORDO         PIC 9(9)V99 VALUE ZERO.
       01  WS-TOTALE-NETTO         PIC 9(9)V99 VALUE ZERO.
       01  WS-TOTALE-TRATTENUTE    PIC 9(9)V99 VALUE ZERO.
       
      * Struttura dipendente
       01  WS-DIPENDENTE.
           05  WS-DIP-MATRICOLA    PIC X(6).
           05  WS-DIP-NOME         PIC X(50).
           05  WS-DIP-COGNOME      PIC X(50).
           05  WS-DIP-CF           PIC X(16).
           05  WS-DIP-QUALIFICA    PIC X(50).
           05  WS-DIP-LIVELLO      PIC X(2).
           05  WS-DIP-REPARTO      PIC X(30).
           05  WS-DIP-STIPENDIO    PIC 9(8)V99.
           05  WS-DIP-STATO        PIC X.
       
      * Struttura stipendio
       01  WS-STIPENDIO.
           05  WS-STI-ID           PIC 9(9).
           05  WS-STI-GIORNI       PIC 99.
           05  WS-STI-ORE-ORD      PIC 999V99.
           05  WS-STI-ORE-STR      PIC 999V99.
           05  WS-STI-BASE         PIC 9(8)V99.
           05  WS-STI-STRAORD      PIC 9(8)V99.
           05  WS-STI-ALTRE        PIC 9(8)V99.
           05  WS-STI-LORDO        PIC 9(8)V99.
           05  WS-STI-TRATTENUTE   PIC 9(8)V99.
           05  WS-STI-NETTO        PIC 9(8)V99.
       
      * Struttura presenze
       01  WS-PRESENZE.
           05  WS-PRE-GIORNI-LAV   PIC 99 VALUE ZERO.
           05  WS-PRE-GIORNI-FER   PIC 99 VALUE ZERO.
           05  WS-PRE-GIORNI-MAL   PIC 99 VALUE ZERO.
           05  WS-PRE-GIORNI-PER   PIC 99 VALUE ZERO.
           05  WS-PRE-ORE-ORD      PIC 999V99 VALUE ZERO.
           05  WS-PRE-ORE-STR      PIC 999V99 VALUE ZERO.
       
      * Calcoli trattenute
       01  WS-CALCOLI.
           05  WS-IMPONIBILE       PIC 9(8)V99.
           05  WS-IRPEF            PIC 9(8)V99.
           05  WS-INPS             PIC 9(8)V99.
           05  WS-ADD-REG          PIC 9(8)V99.
           05  WS-ADD-COM          PIC 9(8)V99.
           05  WS-DETRAZIONI       PIC 9(8)V99.
           05  WS-ALIQUOTA         PIC 99V99.
           05  WS-SCAGLIONE-MIN    PIC 9(8)V99.
           05  WS-SCAGLIONE-MAX    PIC 9(8)V99.
       
      * Variabili per formattazione
       01  WS-IMPORTO-EDIT         PIC Z,ZZZ,ZZ9.99-.
       01  WS-NUMERO-EDIT          PIC ZZ9.
       01  WS-PERC-EDIT            PIC Z9.99.
       
      * SQL area
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.
       
      * Cursori
           EXEC SQL
               DECLARE CUR-DIPENDENTI CURSOR FOR
               SELECT matricola, nome, cognome, codice_fiscale,
                      qualifica, livello, reparto, stipendio_base
               FROM DIPENDENTI
               WHERE stato = 'A'
               ORDER BY matricola
           END-EXEC.
       
           EXEC SQL
               DECLARE CUR-PRESENZE CURSOR FOR
               SELECT tipo_giornata, 
                      SUM(ore_ordinarie), 
                      SUM(ore_straordinarie)
               FROM PRESENZE
               WHERE matricola = :WS-MATRICOLA
               AND EXTRACT(YEAR FROM data_presenza) = :WS-ANNO
               AND EXTRACT(MONTH FROM data_presenza) = :WS-MESE
               GROUP BY tipo_giornata
           END-EXEC.
       
           EXEC SQL
               DECLARE CUR-TRATTENUTE CURSOR FOR
               SELECT tipo_trattenuta, descrizione, 
                      aliquota, importo
               FROM TRATTENUTE
               WHERE id_stipendio = :WS-STI-ID
               ORDER BY tipo_trattenuta
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
               CONNECT TO 'postgresql://localhost/paghe'
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
           DISPLAY "===== SISTEMA GESTIONE PAGHE E STIPENDI ====="
           DISPLAY "1. Elaborazione stipendi mensili"
           DISPLAY "2. Calcolo singolo stipendio"
           DISPLAY "3. Stampa cedolino"
           DISPLAY "4. Report mensile stipendi"
           DISPLAY "5. Inserimento presenze"
           DISPLAY "6. Visualizza presenze dipendente"
           DISPLAY "0. Esci"
           DISPLAY "============================================="
           DISPLAY "Scelta: " WITH NO ADVANCING
           ACCEPT WS-SCELTA.
       
       ELABORA-SCELTA.
           EVALUATE WS-SCELTA
               WHEN 1
                   PERFORM ELABORAZIONE-MENSILE
               WHEN 2
                   PERFORM CALCOLO-SINGOLO
               WHEN 3
                   PERFORM STAMPA-CEDOLINO
               WHEN 4
                   PERFORM REPORT-MENSILE
               WHEN 5
                   PERFORM INSERIMENTO-PRESENZE
               WHEN 6
                   PERFORM VISUALIZZA-PRESENZE
               WHEN 0
                   MOVE 'N' TO WS-CONTINUA
               WHEN OTHER
                   DISPLAY "Scelta non valida!"
           END-EVALUATE.
       
       ELABORAZIONE-MENSILE.
           DISPLAY " "
           DISPLAY "=== ELABORAZIONE STIPENDI MENSILI ==="
           
           DISPLAY "Anno (AAAA): " WITH NO ADVANCING
           ACCEPT WS-ANNO
           
           DISPLAY "Mese (MM): " WITH NO ADVANCING
           ACCEPT WS-MESE
           
           IF WS-MESE < 1 OR WS-MESE > 12
               DISPLAY "Mese non valido!"
               EXIT PARAGRAPH
           END-IF
           
      * Verifica se già elaborato
           EXEC SQL
               SELECT COUNT(*)
               INTO :WS-CONTA-ELABORATI
               FROM STIPENDI
               WHERE anno = :WS-ANNO
               AND mese = :WS-MESE
               AND stato_pagamento <> 'A'
           END-EXEC
           
           IF WS-CONTA-ELABORATI > 0
               DISPLAY "Stipendi già elaborati per " 
                       WS-MESE "/" WS-ANNO
               DISPLAY "Vuoi rielaborare? (S/N): " WITH NO ADVANCING
               ACCEPT WS-RISPOSTA
               IF WS-RISPOSTA NOT = 'S' AND NOT = 's'
                   EXIT PARAGRAPH
               END-IF
           END-IF
           
           MOVE FUNCTION CURRENT-DATE(1:10) TO WS-DATA-ELABORAZIONE
           MOVE ZERO TO WS-CONTA-ELABORATI
           MOVE ZERO TO WS-TOTALE-LORDO
           MOVE ZERO TO WS-TOTALE-NETTO
           
           EXEC SQL
               OPEN CUR-DIPENDENTI
           END-EXEC
           
           PERFORM UNTIL SQLCODE NOT = 0
               EXEC SQL
                   FETCH CUR-DIPENDENTI
                   INTO :WS-DIP-MATRICOLA, :WS-DIP-NOME,
                        :WS-DIP-COGNOME, :WS-DIP-CF,
                        :WS-DIP-QUALIFICA, :WS-DIP-LIVELLO,
                        :WS-DIP-REPARTO, :WS-DIP-STIPENDIO
               END-EXEC
               
               IF SQLCODE = 0
                   MOVE WS-DIP-MATRICOLA TO WS-MATRICOLA
                   PERFORM CALCOLA-STIPENDIO
                   IF WS-ESITO = "OK"
                       ADD 1 TO WS-CONTA-ELABORATI
                       ADD WS-STI-LORDO TO WS-TOTALE-LORDO
                       ADD WS-STI-NETTO TO WS-TOTALE-NETTO
                   END-IF
               END-IF
           END-PERFORM
           
           EXEC SQL
               CLOSE CUR-DIPENDENTI
           END-EXEC
           
           DISPLAY " "
           DISPLAY "Elaborazione completata!"
           DISPLAY "Dipendenti elaborati: " WS-CONTA-ELABORATI
           MOVE WS-TOTALE-LORDO TO WS-IMPORTO-EDIT
           DISPLAY "Totale lordo: EUR " WS-IMPORTO-EDIT
           MOVE WS-TOTALE-NETTO TO WS-IMPORTO-EDIT
           DISPLAY "Totale netto: EUR " WS-IMPORTO-EDIT.
       
       CALCOLO-SINGOLO.
           DISPLAY " "
           DISPLAY "=== CALCOLO SINGOLO STIPENDIO ==="
           
           DISPLAY "Matricola dipendente: " WITH NO ADVANCING
           ACCEPT WS-MATRICOLA
           
           DISPLAY "Anno (AAAA): " WITH NO ADVANCING
           ACCEPT WS-ANNO
           
           DISPLAY "Mese (MM): " WITH NO ADVANCING
           ACCEPT WS-MESE
           
           PERFORM CARICA-DIPENDENTE
           IF WS-ESITO NOT = "OK"
               EXIT PARAGRAPH
           END-IF
           
           PERFORM CALCOLA-STIPENDIO
           
           IF WS-ESITO = "OK"
               DISPLAY " "
               DISPLAY "Stipendio calcolato per " 
                       WS-DIP-NOME " " WS-DIP-COGNOME
               PERFORM VISUALIZZA-DETTAGLIO-STIPENDIO
           END-IF.
       
       CARICA-DIPENDENTE.
           MOVE "OK" TO WS-ESITO
           
           EXEC SQL
               SELECT matricola, nome, cognome, codice_fiscale,
                      qualifica, livello, reparto, stipendio_base,
                      stato
               INTO :WS-DIP-MATRICOLA, :WS-DIP-NOME,
                    :WS-DIP-COGNOME, :WS-DIP-CF,
                    :WS-DIP-QUALIFICA, :WS-DIP-LIVELLO,
                    :WS-DIP-REPARTO, :WS-DIP-STIPENDIO,
                    :WS-DIP-STATO
               FROM DIPENDENTI
               WHERE matricola = :WS-MATRICOLA
           END-EXEC
           
           IF SQLCODE = 100
               DISPLAY "Dipendente non trovato!"
               MOVE "KO" TO WS-ESITO
           ELSE IF SQLCODE NOT = 0
               DISPLAY "Errore database: " SQLCODE
               MOVE "KO" TO WS-ESITO
           ELSE IF WS-DIP-STATO NOT = 'A'
               DISPLAY "Dipendente non attivo!"
               MOVE "KO" TO WS-ESITO
           END-IF.
       
       CALCOLA-STIPENDIO.
           MOVE "OK" TO WS-ESITO
           INITIALIZE WS-PRESENZE
           INITIALIZE WS-STIPENDIO
           INITIALIZE WS-CALCOLI
           
      * Carica presenze del mese
           PERFORM CARICA-PRESENZE
           
      * Calcola stipendio base proporzionale
           MOVE WS-DIP-STIPENDIO TO WS-STI-BASE
           
      * Calcola straordinari (25% sulla paga oraria)
           IF WS-PRE-ORE-STR > 0
               COMPUTE WS-STI-STRAORD = 
                   (WS-DIP-STIPENDIO / 168) * WS-PRE-ORE-STR * 1.25
           END-IF
           
      * Totale lordo
           COMPUTE WS-STI-LORDO = 
               WS-STI-BASE + WS-STI-STRAORD + WS-STI-ALTRE
           
      * Calcola trattenute
           PERFORM CALCOLA-TRATTENUTE
           
      * Netto
           COMPUTE WS-STI-NETTO = 
               WS-STI-LORDO - WS-STI-TRATTENUTE
           
      * Salva stipendio
           PERFORM SALVA-STIPENDIO.
       
       CARICA-PRESENZE.
           EXEC SQL
               OPEN CUR-PRESENZE
           END-EXEC
           
           PERFORM UNTIL SQLCODE NOT = 0
               EXEC SQL
                   FETCH CUR-PRESENZE
                   INTO :WS-RISPOSTA, :WS-PRE-ORE-ORD, 
                        :WS-PRE-ORE-STR
               END-EXEC
               
               IF SQLCODE = 0
                   EVALUATE WS-RISPOSTA
                       WHEN 'L'
                           ADD 1 TO WS-PRE-GIORNI-LAV
                       WHEN 'F'
                           ADD 1 TO WS-PRE-GIORNI-FER
                       WHEN 'M'
                           ADD 1 TO WS-PRE-GIORNI-MAL
                       WHEN 'P'
                           ADD 1 TO WS-PRE-GIORNI-PER
                   END-EVALUATE
               END-IF
           END-PERFORM
           
           EXEC SQL
               CLOSE CUR-PRESENZE
           END-EXEC
           
           MOVE WS-PRE-GIORNI-LAV TO WS-STI-GIORNI
           MOVE WS-PRE-ORE-ORD TO WS-STI-ORE-ORD
           MOVE WS-PRE-ORE-STR TO WS-STI-ORE-STR.
       
       CALCOLA-TRATTENUTE.
           MOVE WS-STI-LORDO TO WS-IMPONIBILE
           
      * Calcola INPS (9.19% a carico dipendente)
           COMPUTE WS-INPS = WS-IMPONIBILE * 0.0919
           
      * Calcola IRPEF con scaglioni
           PERFORM CALCOLA-IRPEF
           
      * Calcola addizionali
           COMPUTE WS-ADD-REG = WS-IMPONIBILE * 0.0173 / 12
           COMPUTE WS-ADD-COM = WS-IMPONIBILE * 0.0080 / 12
           
      * Calcola detrazioni lavoro dipendente
           PERFORM CALCOLA-DETRAZIONI
           
      * Totale trattenute
           COMPUTE WS-STI-TRATTENUTE = 
               WS-INPS + WS-IRPEF + WS-ADD-REG + WS-ADD-COM
               - WS-DETRAZIONI
           
           IF WS-STI-TRATTENUTE < 0
               MOVE 0 TO WS-STI-TRATTENUTE
           END-IF.
       
       CALCOLA-IRPEF.
           MOVE ZERO TO WS-IRPEF
           COMPUTE WS-IMPONIBILE = WS-STI-LORDO * 12
           
      * Primo scaglione (fino a 15.000)
           IF WS-IMPONIBILE > 0
               IF WS-IMPONIBILE <= 15000
                   COMPUTE WS-IRPEF = WS-IMPONIBILE * 0.23
               ELSE
                   COMPUTE WS-IRPEF = 15000 * 0.23
               END-IF
           END-IF
           
      * Secondo scaglione (15.000 - 28.000)
           IF WS-IMPONIBILE > 15000
               IF WS-IMPONIBILE <= 28000
                   COMPUTE WS-IRPEF = WS-IRPEF + 
                       (WS-IMPONIBILE - 15000) * 0.25
               ELSE
                   COMPUTE WS-IRPEF = WS-IRPEF + 
                       (28000 - 15000) * 0.25
               END-IF
           END-IF
           
      * Terzo scaglione (28.000 - 50.000)
           IF WS-IMPONIBILE > 28000
               IF WS-IMPONIBILE <= 50000
                   COMPUTE WS-IRPEF = WS-IRPEF + 
                       (WS-IMPONIBILE - 28000) * 0.35
               ELSE
                   COMPUTE WS-IRPEF = WS-IRPEF + 
                       (50000 - 28000) * 0.35
               END-IF
           END-IF
           
      * Quarto scaglione (oltre 50.000)
           IF WS-IMPONIBILE > 50000
               COMPUTE WS-IRPEF = WS-IRPEF + 
                   (WS-IMPONIBILE - 50000) * 0.43
           END-IF
           
      * Riporta a valore mensile
           COMPUTE WS-IRPEF = WS-IRPEF / 12.
       
       CALCOLA-DETRAZIONI.
           MOVE ZERO TO WS-DETRAZIONI
           COMPUTE WS-IMPONIBILE = WS-STI-LORDO * 12
           
           IF WS-IMPONIBILE <= 8000
               MOVE 1880 TO WS-DETRAZIONI
           ELSE IF WS-IMPONIBILE <= 28000
               MOVE 1910 TO WS-DETRAZIONI
           ELSE IF WS-IMPONIBILE <= 55000
               MOVE 1370 TO WS-DETRAZIONI
           ELSE
               MOVE 0 TO WS-DETRAZIONI
           END-IF
           
      * Riporta a valore mensile
           IF WS-DETRAZIONI > 0
               COMPUTE WS-DETRAZIONI = WS-DETRAZIONI / 12
           END-IF.
       
       SALVA-STIPENDIO.
      * Verifica se esiste già
           EXEC SQL
               SELECT id_stipendio, stato_pagamento
               INTO :WS-STI-ID, :WS-RISPOSTA
               FROM STIPENDI
               WHERE matricola = :WS-MATRICOLA
               AND anno = :WS-ANNO
               AND mese = :WS-MESE
           END-EXEC
           
           IF SQLCODE = 0
      * Aggiorna esistente
               IF WS-RISPOSTA = 'P'
                   DISPLAY "Stipendio già pagato, non modificabile!"
                   MOVE "KO" TO WS-ESITO
               ELSE
                   EXEC SQL
                       UPDATE STIPENDI
                       SET giorni_lavorati = :WS-STI-GIORNI,
                           ore_ordinarie = :WS-STI-ORE-ORD,
                           ore_straordinarie = :WS-STI-ORE-STR,
                           stipendio_base = :WS-STI-BASE,
                           importo_straord = :WS-STI-STRAORD,
                           altre_competenze = :WS-STI-ALTRE,
                           totale_lordo = :WS-STI-LORDO,
                           totale_trattenute = :WS-STI-TRATTENUTE,
                           netto_pagare = :WS-STI-NETTO,
                           data_elaborazione = CURRENT_TIMESTAMP
                       WHERE id_stipendio = :WS-STI-ID
                   END-EXEC
                   
                   IF SQLCODE = 0
                       PERFORM SALVA-DETTAGLIO-TRATTENUTE
                   ELSE
                       DISPLAY "Errore aggiornamento: " SQLCODE
                       MOVE "KO" TO WS-ESITO
                   END-IF
               END-IF
           ELSE
      * Inserisci nuovo
               EXEC SQL
                   INSERT INTO STIPENDI
                   (matricola, anno, mese, giorni_lavorati,
                    ore_ordinarie, ore_straordinarie,
                    stipendio_base, importo_straord,
                    altre_competenze, totale_lordo,
                    totale_trattenute, netto_pagare)
                   VALUES
                   (:WS-MATRICOLA, :WS-ANNO, :WS-MESE,
                    :WS-STI-GIORNI, :WS-STI-ORE-ORD,
                    :WS-STI-ORE-STR, :WS-STI-BASE,
                    :WS-STI-STRAORD, :WS-STI-ALTRE,
                    :WS-STI-LORDO, :WS-STI-TRATTENUTE,
                    :WS-STI-NETTO)
                   RETURNING id_stipendio INTO :WS-STI-ID
               END-EXEC
               
               IF SQLCODE = 0
                   PERFORM SALVA-DETTAGLIO-TRATTENUTE
               ELSE
                   DISPLAY "Errore inserimento: " SQLCODE
                   MOVE "KO" TO WS-ESITO
               END-IF
           END-IF.
       
       SALVA-DETTAGLIO-TRATTENUTE.
      * Elimina trattenute esistenti
           EXEC SQL
               DELETE FROM TRATTENUTE
               WHERE id_stipendio = :WS-STI-ID
           END-EXEC
           
      * Inserisci nuove trattenute
           EXEC SQL
               INSERT INTO TRATTENUTE
               (id_stipendio, tipo_trattenuta, descrizione, importo)
               VALUES
               (:WS-STI-ID, 'INPS', 
                'Contributi previdenziali', :WS-INPS)
           END-EXEC
           
           EXEC SQL
               INSERT INTO TRATTENUTE
               (id_stipendio, tipo_trattenuta, descrizione, importo)
               VALUES
               (:WS-STI-ID, 'IRPEF', 
                'Imposta sul reddito', :WS-IRPEF)
           END-EXEC
           
           EXEC SQL
               INSERT INTO TRATTENUTE
               (id_stipendio, tipo_trattenuta, descrizione, importo)
               VALUES
               (:WS-STI-ID, 'ADD_REG', 
                'Addizionale regionale', :WS-ADD-REG)
           END-EXEC
           
           EXEC SQL
               INSERT INTO TRATTENUTE
               (id_stipendio, tipo_trattenuta, descrizione, importo)
               VALUES
               (:WS-STI-ID, 'ADD_COM', 
                'Addizionale comunale', :WS-ADD-COM)
           END-EXEC
           
           IF WS-DETRAZIONI > 0
               COMPUTE WS-DETRAZIONI = 0 - WS-DETRAZIONI
               EXEC SQL
                   INSERT INTO TRATTENUTE
                   (id_stipendio, tipo_trattenuta, 
                    descrizione, importo)
                   VALUES
                   (:WS-STI-ID, 'DETRAZ', 
                    'Detrazioni lavoro dipendente', :WS-DETRAZIONI)
               END-EXEC
           END-IF.
       
       VISUALIZZA-DETTAGLIO-STIPENDIO.
           DISPLAY " "
           DISPLAY "=== DETTAGLIO STIPENDIO ==="
           DISPLAY "Periodo: " WS-MESE "/" WS-ANNO
           MOVE WS-STI-GIORNI TO WS-NUMERO-EDIT
           DISPLAY "Giorni lavorati: " WS-NUMERO-EDIT
           MOVE WS-STI-ORE-STR TO WS-IMPORTO-EDIT
           DISPLAY "Ore straordinario: " WS-IMPORTO-EDIT
           DISPLAY " "
           DISPLAY "COMPETENZE:"
           MOVE WS-STI-BASE TO WS-IMPORTO-EDIT
           DISPLAY "  Stipendio base:     EUR " WS-IMPORTO-EDIT
           MOVE WS-STI-STRAORD TO WS-IMPORTO-EDIT
           DISPLAY "  Straordinari:       EUR " WS-IMPORTO-EDIT
           MOVE WS-STI-LORDO TO WS-IMPORTO-EDIT
           DISPLAY "  TOTALE LORDO:       EUR " WS-IMPORTO-EDIT
           DISPLAY " "
           DISPLAY "TRATTENUTE:"
           MOVE WS-INPS TO WS-IMPORTO-EDIT
           DISPLAY "  INPS:               EUR " WS-IMPORTO-EDIT
           MOVE WS-IRPEF TO WS-IMPORTO-EDIT
           DISPLAY "  IRPEF:              EUR " WS-IMPORTO-EDIT
           MOVE WS-ADD-REG TO WS-IMPORTO-EDIT
           DISPLAY "  Add. Regionale:     EUR " WS-IMPORTO-EDIT
           MOVE WS-ADD-COM TO WS-IMPORTO-EDIT
           DISPLAY "  Add. Comunale:      EUR " WS-IMPORTO-EDIT
           IF WS-DETRAZIONI > 0
               MOVE WS-DETRAZIONI TO WS-IMPORTO-EDIT
               DISPLAY "  Detrazioni:        -EUR " WS-IMPORTO-EDIT
           END-IF
           MOVE WS-STI-TRATTENUTE TO WS-IMPORTO-EDIT
           DISPLAY "  TOTALE TRATTENUTE:  EUR " WS-IMPORTO-EDIT
           DISPLAY " "
           MOVE WS-STI-NETTO TO WS-IMPORTO-EDIT
           DISPLAY "  NETTO A PAGARE:     EUR " WS-IMPORTO-EDIT.
       
       STAMPA-CEDOLINO.
           DISPLAY " "
           DISPLAY "=== STAMPA CEDOLINO ==="
           
           DISPLAY "Matricola: " WITH NO ADVANCING
           ACCEPT WS-MATRICOLA
           
           DISPLAY "Anno: " WITH NO ADVANCING
           ACCEPT WS-ANNO
           
           DISPLAY "Mese: " WITH NO ADVANCING
           ACCEPT WS-MESE
           
      * Carica dati stipendio
           EXEC SQL
               SELECT s.id_stipendio, s.giorni_lavorati,
                      s.ore_ordinarie, s.ore_straordinarie,
                      s.stipendio_base, s.importo_straord,
                      s.altre_competenze, s.totale_lordo,
                      s.totale_trattenute, s.netto_pagare,
                      d.nome, d.cognome, d.codice_fiscale,
                      d.qualifica, d.livello
               INTO :WS-STI-ID, :WS-STI-GIORNI,
                    :WS-STI-ORE-ORD, :WS-STI-ORE-STR,
                    :WS-STI-BASE, :WS-STI-STRAORD,
                    :WS-STI-ALTRE, :WS-STI-LORDO,
                    :WS-STI-TRATTENUTE, :WS-STI-NETTO,
                    :WS-DIP-NOME, :WS-DIP-COGNOME,
                    :WS-DIP-CF, :WS-DIP-QUALIFICA,
                    :WS-DIP-LIVELLO
               FROM STIPENDI s
               JOIN DIPENDENTI d ON s.matricola = d.matricola
               WHERE s.matricola = :WS-MATRICOLA
               AND s.anno = :WS-ANNO
               AND s.mese = :WS-MESE
           END-EXEC
           
           IF SQLCODE = 100
               DISPLAY "Stipendio non trovato!"
               EXIT PARAGRAPH
           ELSE IF SQLCODE NOT = 0
               DISPLAY "Errore database: " SQLCODE
               EXIT PARAGRAPH
           END-IF
           
           OPEN OUTPUT CEDOLINO-FILE
           
           PERFORM GENERA-CEDOLINO
           
           CLOSE CEDOLINO-FILE
           
           DISPLAY "Cedolino salvato in CEDOLINO.TXT"
           
      * Registra emissione cedolino
           PERFORM REGISTRA-CEDOLINO.
       
       GENERA-CEDOLINO.
           MOVE SPACES TO CEDOLINO-RECORD
           STRING "                    CEDOLINO PAGA"
                  DELIMITED BY SIZE INTO CEDOLINO-RECORD
           WRITE CEDOLINO-RECORD
           
           MOVE ALL "=" TO CEDOLINO-RECORD
           WRITE CEDOLINO-RECORD
           
           STRING "AZIENDA: ESEMPIO SPA" 
                  "                    Periodo: " 
                  WS-MESE "/" WS-ANNO
                  DELIMITED BY SIZE INTO CEDOLINO-RECORD
           WRITE CEDOLINO-RECORD
           
           MOVE ALL "-" TO CEDOLINO-RECORD
           WRITE CEDOLINO-RECORD
           
           STRING "Dipendente: " WS-DIP-COGNOME " " WS-DIP-NOME
                  DELIMITED BY SIZE INTO CEDOLINO-RECORD
           WRITE CEDOLINO-RECORD
           
           STRING "Matricola: " WS-MATRICOLA 
                  "    C.F.: " WS-DIP-CF
                  DELIMITED BY SIZE INTO CEDOLINO-RECORD
           WRITE CEDOLINO-RECORD
           
           STRING "Qualifica: " WS-DIP-QUALIFICA 
                  "    Livello: " WS-DIP-LIVELLO
                  DELIMITED BY SIZE INTO CEDOLINO-RECORD
           WRITE CEDOLINO-RECORD
           
           MOVE ALL "-" TO CEDOLINO-RECORD
           WRITE CEDOLINO-RECORD
           
           WRITE CEDOLINO-RECORD FROM SPACES
           STRING "COMPETENZE:"
                  DELIMITED BY SIZE INTO CEDOLINO-RECORD
           WRITE CEDOLINO-RECORD
           
           MOVE WS-STI-BASE TO WS-IMPORTO-EDIT
           STRING "  Stipendio base.............. EUR " 
                  WS-IMPORTO-EDIT
                  DELIMITED BY SIZE INTO CEDOLINO-RECORD
           WRITE CEDOLINO-RECORD
           
           IF WS-STI-STRAORD > 0
               MOVE WS-STI-STRAORD TO WS-IMPORTO-EDIT
               STRING "  Straordinari................ EUR " 
                      WS-IMPORTO-EDIT
                      DELIMITED BY SIZE INTO CEDOLINO-RECORD
               WRITE CEDOLINO-RECORD
           END-IF
           
           WRITE CEDOLINO-RECORD FROM SPACES
           STRING "TRATTENUTE:"
                  DELIMITED BY SIZE INTO CEDOLINO-RECORD
           WRITE CEDOLINO-RECORD
           
      * Carica e stampa trattenute
           EXEC SQL
               OPEN CUR-TRATTENUTE
           END-EXEC
           
           PERFORM UNTIL SQLCODE NOT = 0
               EXEC SQL
                   FETCH CUR-TRATTENUTE
                   INTO :WS-RISPOSTA, :WS-CAUSALE,
                        :WS-ALIQUOTA, :WS-IMPORTO
               END-EXEC
               
               IF SQLCODE = 0
                   MOVE WS-IMPORTO TO WS-IMPORTO-EDIT
                   STRING "  " WS-CAUSALE(1:25) "... EUR " 
                          WS-IMPORTO-EDIT
                          DELIMITED BY SIZE INTO CEDOLINO-RECORD
                   WRITE CEDOLINO-RECORD
               END-IF
           END-PERFORM
           
           EXEC SQL
               CLOSE CUR-TRATTENUTE
           END-EXEC
           
           MOVE ALL "-" TO CEDOLINO-RECORD
           WRITE CEDOLINO-RECORD
           
           MOVE WS-STI-LORDO TO WS-IMPORTO-EDIT
           STRING "TOTALE LORDO................. EUR " 
                  WS-IMPORTO-EDIT
                  DELIMITED BY SIZE INTO CEDOLINO-RECORD
           WRITE CEDOLINO-RECORD
           
           MOVE WS-STI-TRATTENUTE TO WS-IMPORTO-EDIT
           STRING "TOTALE TRATTENUTE............ EUR " 
                  WS-IMPORTO-EDIT
                  DELIMITED BY SIZE INTO CEDOLINO-RECORD
           WRITE CEDOLINO-RECORD
           
           MOVE ALL "-" TO CEDOLINO-RECORD
           WRITE CEDOLINO-RECORD
           
           MOVE WS-STI-NETTO TO WS-IMPORTO-EDIT
           STRING "NETTO A PAGARE............... EUR " 
                  WS-IMPORTO-EDIT
                  DELIMITED BY SIZE INTO CEDOLINO-RECORD
           WRITE CEDOLINO-RECORD
           
           MOVE ALL "=" TO CEDOLINO-RECORD
           WRITE CEDOLINO-RECORD.
       
       REGISTRA-CEDOLINO.
           STRING "CED" WS-ANNO WS-MESE WS-MATRICOLA
                  DELIMITED BY SIZE INTO WS-CAUSALE
           
           EXEC SQL
               INSERT INTO CEDOLINI
               (id_stipendio, numero_cedolino, data_emissione)
               VALUES
               (:WS-STI-ID, :WS-CAUSALE, CURRENT_DATE)
           END-EXEC.
       
       REPORT-MENSILE.
           DISPLAY " "
           DISPLAY "=== REPORT MENSILE STIPENDI ==="
           
           DISPLAY "Anno: " WITH NO ADVANCING
           ACCEPT WS-ANNO
           
           DISPLAY "Mese: " WITH NO ADVANCING
           ACCEPT WS-MESE
           
           OPEN OUTPUT REPORT-FILE
           
           PERFORM GENERA-REPORT
           
           CLOSE REPORT-FILE
           
           DISPLAY "Report salvato in REPORT-STIPENDI.TXT".
       
       GENERA-REPORT.
           STRING "REPORT STIPENDI - " WS-MESE "/" WS-ANNO
                  DELIMITED BY SIZE INTO REPORT-RECORD
           WRITE REPORT-RECORD
           
           MOVE ALL "=" TO REPORT-RECORD
           WRITE REPORT-RECORD
           
           EXEC SQL
               SELECT COUNT(*), 
                      SUM(totale_lordo),
                      SUM(totale_trattenute),
                      SUM(netto_pagare)
               INTO :WS-CONTA-DIPENDENTI,
                    :WS-TOTALE-LORDO,
                    :WS-TOTALE-TRATTENUTE,
                    :WS-TOTALE-NETTO
               FROM STIPENDI
               WHERE anno = :WS-ANNO
               AND mese = :WS-MESE
               AND stato_pagamento <> 'A'
           END-EXEC
           
           MOVE WS-CONTA-DIPENDENTI TO WS-NUMERO-EDIT
           STRING "Dipendenti elaborati: " WS-NUMERO-EDIT
                  DELIMITED BY SIZE INTO REPORT-RECORD
           WRITE REPORT-RECORD
           
           MOVE WS-TOTALE-LORDO TO WS-IMPORTO-EDIT
           STRING "Totale lordo:        EUR " WS-IMPORTO-EDIT
                  DELIMITED BY SIZE INTO REPORT-RECORD
           WRITE REPORT-RECORD
           
           MOVE WS-TOTALE-TRATTENUTE TO WS-IMPORTO-EDIT
           STRING "Totale trattenute:   EUR " WS-IMPORTO-EDIT
                  DELIMITED BY SIZE INTO REPORT-RECORD
           WRITE REPORT-RECORD
           
           MOVE WS-TOTALE-NETTO TO WS-IMPORTO-EDIT
           STRING "Totale netto:        EUR " WS-IMPORTO-EDIT
                  DELIMITED BY SIZE INTO REPORT-RECORD
           WRITE REPORT-RECORD
           
           MOVE ALL "-" TO REPORT-RECORD
           WRITE REPORT-RECORD
           
           STRING "DETTAGLIO PER DIPENDENTE:"
                  DELIMITED BY SIZE INTO REPORT-RECORD
           WRITE REPORT-RECORD
           
           EXEC SQL
               DECLARE CUR-REPORT CURSOR FOR
               SELECT d.matricola, d.cognome, d.nome,
                      s.totale_lordo, s.totale_trattenute,
                      s.netto_pagare
               FROM STIPENDI s
               JOIN DIPENDENTI d ON s.matricola = d.matricola
               WHERE s.anno = :WS-ANNO
               AND s.mese = :WS-MESE
               AND s.stato_pagamento <> 'A'
               ORDER BY d.matricola
           END-EXEC
           
           EXEC SQL
               OPEN CUR-REPORT
           END-EXEC
           
           PERFORM UNTIL SQLCODE NOT = 0
               EXEC SQL
                   FETCH CUR-REPORT
                   INTO :WS-DIP-MATRICOLA, :WS-DIP-COGNOME,
                        :WS-DIP-NOME, :WS-STI-LORDO,
                        :WS-STI-TRATTENUTE, :WS-STI-NETTO
               END-EXEC
               
               IF SQLCODE = 0
                   STRING WS-DIP-MATRICOLA " " 
                          WS-DIP-COGNOME(1:15) " "
                          WS-DIP-NOME(1:15)
                          DELIMITED BY SIZE INTO REPORT-RECORD
                   WRITE REPORT-RECORD
                   
                   MOVE WS-STI-LORDO TO WS-IMPORTO-EDIT
                   STRING "  Lordo: " WS-IMPORTO-EDIT
                          DELIMITED BY SIZE INTO REPORT-RECORD
                   
                   MOVE WS-STI-NETTO TO WS-IMPORTO-EDIT
                   STRING REPORT-RECORD(1:30) 
                          "  Netto: " WS-IMPORTO-EDIT
                          DELIMITED BY SIZE INTO REPORT-RECORD
                   WRITE REPORT-RECORD
               END-IF
           END-PERFORM
           
           EXEC SQL
               CLOSE CUR-REPORT
           END-EXEC.
       
       INSERIMENTO-PRESENZE.
           DISPLAY " "
           DISPLAY "=== INSERIMENTO PRESENZE ==="
           
           DISPLAY "Matricola: " WITH NO ADVANCING
           ACCEPT WS-MATRICOLA
           
           PERFORM CARICA-DIPENDENTE
           IF WS-ESITO NOT = "OK"
               EXIT PARAGRAPH
           END-IF
           
           DISPLAY "Dipendente: " WS-DIP-NOME " " WS-DIP-COGNOME
           
           DISPLAY "Data presenza (AAAA-MM-GG): " WITH NO ADVANCING
           ACCEPT WS-DATA-ELABORAZIONE
           
           DISPLAY "Tipo giornata (L/F/M/P/A): " WITH NO ADVANCING
           ACCEPT WS-RISPOSTA
           
           IF WS-RISPOSTA = 'L'
               DISPLAY "Ore ordinarie: " WITH NO ADVANCING
               ACCEPT WS-PRE-ORE-ORD
               
               DISPLAY "Ore straordinarie: " WITH NO ADVANCING
               ACCEPT WS-PRE-ORE-STR
           ELSE
               MOVE ZERO TO WS-PRE-ORE-ORD
               MOVE ZERO TO WS-PRE-ORE-STR
           END-IF
           
           EXEC SQL
               INSERT INTO PRESENZE
               (matricola, data_presenza, tipo_giornata,
                ore_ordinarie, ore_straordinarie)
               VALUES
               (:WS-MATRICOLA, :WS-DATA-ELABORAZIONE,
                :WS-RISPOSTA, :WS-PRE-ORE-ORD, :WS-PRE-ORE-STR)
           END-EXEC
           
           IF SQLCODE = 0
               DISPLAY "Presenza registrata con successo!"
           ELSE IF SQLCODE = -803
               DISPLAY "Presenza già registrata per questa data!"
           ELSE
               DISPLAY "Errore inserimento: " SQLCODE
           END-IF.
       
       VISUALIZZA-PRESENZE.
           DISPLAY " "
           DISPLAY "=== VISUALIZZA PRESENZE ==="
           
           DISPLAY "Matricola: " WITH NO ADVANCING
           ACCEPT WS-MATRICOLA
           
           DISPLAY "Anno: " WITH NO ADVANCING
           ACCEPT WS-ANNO
           
           DISPLAY "Mese: " WITH NO ADVANCING
           ACCEPT WS-MESE
           
           PERFORM CARICA-DIPENDENTE
           IF WS-ESITO NOT = "OK"
               EXIT PARAGRAPH
           END-IF
           
           DISPLAY " "
           DISPLAY "Presenze di " WS-DIP-NOME " " WS-DIP-COGNOME
           DISPLAY "Periodo: " WS-MESE "/" WS-ANNO
           DISPLAY " "
           
           PERFORM CARICA-PRESENZE
           
           DISPLAY " "
           DISPLAY "RIEPILOGO:"
           MOVE WS-PRE-GIORNI-LAV TO WS-NUMERO-EDIT
           DISPLAY "Giorni lavorati: " WS-NUMERO-EDIT
           MOVE WS-PRE-GIORNI-FER TO WS-NUMERO-EDIT
           DISPLAY "Giorni ferie:    " WS-NUMERO-EDIT
           MOVE WS-PRE-GIORNI-MAL TO WS-NUMERO-EDIT
           DISPLAY "Giorni malattia: " WS-NUMERO-EDIT
           MOVE WS-PRE-GIORNI-PER TO WS-NUMERO-EDIT
           DISPLAY "Giorni permesso: " WS-NUMERO-EDIT
           MOVE WS-PRE-ORE-ORD TO WS-IMPORTO-EDIT
           DISPLAY "Ore ordinarie:   " WS-IMPORTO-EDIT
           MOVE WS-PRE-ORE-STR TO WS-IMPORTO-EDIT
           DISPLAY "Ore straord.:    " WS-IMPORTO-EDIT.