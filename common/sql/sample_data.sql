-- File: sample_data.sql
-- Descrizione: Dati di esempio aggiuntivi per i nuovi schemi

-- Inserimento dati per le categorie di spesa
INSERT INTO CATEGORIE_SPESA (NOME_CATEGORIA, DESCRIZIONE, ATTIVA) VALUES
    ('Personale', 'Costi relativi al personale', 'S'),
    ('Hardware', 'Acquisto e manutenzione hardware', 'S'),
    ('Software', 'Licenze e sviluppo software', 'S'),
    ('Servizi', 'Servizi esterni e consulenze', 'S'),
    ('Formazione', 'Corsi e formazione', 'S'),
    ('Trasferte', 'Viaggi e trasferte', 'S'),
    ('Marketing', 'Costi di marketing e promozione', 'S'),
    ('Ufficio', 'Materiali e gestione ufficio', 'S'),
    ('Utenze', 'Elettricità, acqua, connettività', 'S'),
    ('Altri costi', 'Costi generali non classificati', 'S');

-- Inserimento dati per i centri di costo
INSERT INTO CENTRI_COSTO (CODICE_CENTRO, NOME_CENTRO, ID_DIPARTIMENTO, DESCRIZIONE, ATTIVO, DATA_CREAZIONE) VALUES
    ('CC-IT', 'Centro Costo IT', (SELECT ID_DIPARTIMENTO FROM DIPARTIMENTI WHERE NOME_DIPARTIMENTO = 'Tecnologia'), 'Centro di costo per il dipartimento IT', 'S', CURRENT TIMESTAMP),
    ('CC-HR', 'Centro Costo HR', (SELECT ID_DIPARTIMENTO FROM DIPARTIMENTI WHERE NOME_DIPARTIMENTO = 'Risorse Umane'), 'Centro di costo per il dipartimento HR', 'S', CURRENT TIMESTAMP),
    ('CC-FIN', 'Centro Costo Finanza', (SELECT ID_DIPARTIMENTO FROM DIPARTIMENTI WHERE NOME_DIPARTIMENTO = 'Finanza'), 'Centro di costo per il dipartimento Finanza', 'S', CURRENT TIMESTAMP),
    ('CC-MKT', 'Centro Costo Marketing', (SELECT ID_DIPARTIMENTO FROM DIPARTIMENTI WHERE NOME_DIPARTIMENTO = 'Generale'), 'Centro di costo per il dipartimento Marketing', 'S', CURRENT TIMESTAMP),
    ('CC-ADM', 'Centro Costo Amministrativo', (SELECT ID_DIPARTIMENTO FROM DIPARTIMENTI WHERE NOME_DIPARTIMENTO = 'Amministrazione'), 'Centro di costo per il dipartimento Amministrativo', 'S', CURRENT TIMESTAMP);

-- Inserimento dati per i periodi di budget
INSERT INTO PERIODI_BUDGET (ANNO, NOME_PERIODO, DATA_INIZIO, DATA_FINE, STATO) VALUES
    (2025, 'Trimestre 1', '2025-01-01', '2025-03-31', 'ATTIVO'),
    (2025, 'Trimestre 2', '2025-04-01', '2025-06-30', 'PIANIFICAZIONE'),
    (2025, 'Trimestre 3', '2025-07-01', '2025-09-30', 'PIANIFICAZIONE'),
    (2025, 'Trimestre 4', '2025-10-01', '2025-12-31', 'PIANIFICAZIONE'),
    (2025, 'Anno 2025', '2025-01-01', '2025-12-31', 'ATTIVO');

-- Inserimento dati per le categorie di budget
INSERT INTO CATEGORIE_BUDGET (NOME_CATEGORIA, DESCRIZIONE, ATTIVA) VALUES
    ('Stipendi', 'Budget per stipendi del personale', 'S'),
    ('Investimenti IT', 'Budget per investimenti in tecnologia', 'S'),
    ('Formazione', 'Budget per formazione del personale', 'S'),
    ('Marketing', 'Budget per attività di marketing', 'S'),
    ('Consulenze', 'Budget per consulenze esterne', 'S'),
    ('Trasferte', 'Budget per viaggi e trasferte', 'S'),
    ('Materiali', 'Budget per materiali e forniture', 'S'),
    ('Software', 'Budget per licenze software', 'S'),
    ('Hardware', 'Budget per hardware', 'S'),
    ('Altri costi', 'Budget per costi non classificati', 'S');

-- Inserimento fornitori di esempio
INSERT INTO FORNITORI (RAGIONE_SOCIALE, PARTITA_IVA, INDIRIZZO, CITTA, CAP, PROVINCIA, NAZIONE, TELEFONO, EMAIL, ATTIVO, DATA_INSERIMENTO) VALUES
    ('InfoTech Srl', 'IT12345678901', 'Via Roma 123', 'Milano', '20100', 'MI', 'Italia', '+39 02 12345678', 'info@infotech.it', 'S', CURRENT TIMESTAMP),
    ('Hardware Plus Spa', 'IT98765432109', 'Via Verdi 45', 'Roma', '00100', 'RM', 'Italia', '+39 06 87654321', 'vendite@hardwareplus.it', 'S', CURRENT TIMESTAMP),
    ('Software Solutions Ltd', 'GB123456789', '10 Main Street', 'London', 'EC1A 1BB', '', 'Regno Unito', '+44 20 12345678', 'sales@softwaresolutions.co.uk', 'S', CURRENT TIMESTAMP),
    ('Office Supplies Srl', 'IT45678901234', 'Via Dante 67', 'Torino', '10100', 'TO', 'Italia', '+39 011 4567890', 'ordini@officesupplies.it', 'S', CURRENT TIMESTAMP),
    ('Training Experts Sas', 'IT56789012345', 'Via Manzoni 89', 'Firenze', '50100', 'FI', 'Italia', '+39 055 5678901', 'info@trainingexperts.it', 'S', CURRENT TIMESTAMP);

-- Inserimento categorie articoli
INSERT INTO CATEGORIE_ARTICOLI (NOME_CATEGORIA, DESCRIZIONE, ATTIVA) VALUES
    ('Hardware', 'Dispositivi hardware', 'S'),
    ('Software', 'Licenze software', 'S'),
    ('Materiale ufficio', 'Materiali per ufficio', 'S'),
    ('Arredi', 'Mobili e arredi', 'S'),
    ('Servizi', 'Servizi vari', 'S');

-- Inserimento articoli nel catalogo
INSERT INTO CATALOGO_ARTICOLI (CODICE_ARTICOLO, NOME_ARTICOLO, DESCRIZIONE, ID_CATEGORIA, UNITA_MISURA, PREZZO_LISTINO, ALIQUOTA_IVA, ATTIVO, DATA_INSERIMENTO) VALUES
    ('LAP001', 'Laptop business 15"', 'Laptop per uso aziendale con display da 15 pollici', (SELECT ID_CATEGORIA FROM CATEGORIE_ARTICOLI WHERE NOME_CATEGORIA = 'Hardware'), 'PZ', 1200.00, 22.00, 'S', CURRENT TIMESTAMP),
    ('MON002', 'Monitor 24" Full HD', 'Monitor Full HD da 24 pollici', (SELECT ID_CATEGORIA FROM CATEGORIE_ARTICOLI WHERE NOME_CATEGORIA = 'Hardware'), 'PZ', 200.00, 22.00, 'S', CURRENT TIMESTAMP),
    ('WIN001', 'Licenza Windows 11 Pro', 'Licenza sistema operativo Windows 11 Professional', (SELECT ID_CATEGORIA FROM CATEGORIE_ARTICOLI WHERE NOME_CATEGORIA = 'Software'), 'PZ', 180.00, 22.00, 'S', CURRENT TIMESTAMP),
    ('OFF001', 'Licenza Microsoft Office', 'Licenza Microsoft Office 365 Business', (SELECT ID_CATEGORIA FROM CATEGORIE_ARTICOLI WHERE NOME_CATEGORIA = 'Software'), 'PZ', 150.00, 22.00, 'S', CURRENT TIMESTAMP),
    ('CART001', 'Carta A4', 'Risma di carta A4 da 500 fogli', (SELECT ID_CATEGORIA FROM CATEGORIE_ARTICOLI WHERE NOME_CATEGORIA = 'Materiale ufficio'), 'PZ', 5.00, 22.00, 'S', CURRENT TIMESTAMP),
    ('PEN001', 'Penne blu', 'Confezione da 50 penne a sfera blu', (SELECT ID_CATEGORIA FROM CATEGORIE_ARTICOLI WHERE NOME_CATEGORIA = 'Materiale ufficio'), 'PZ', 12.50, 22.00, 'S', CURRENT TIMESTAMP),
    ('DESK001', 'Scrivania ufficio', 'Scrivania da ufficio 160x80 cm', (SELECT ID_CATEGORIA FROM CATEGORIE_ARTICOLI WHERE NOME_CATEGORIA = 'Arredi'), 'PZ', 250.00, 22.00, 'S', CURRENT TIMESTAMP),
    ('CHAIR001', 'Sedia ergonomica', 'Sedia da ufficio ergonomica', (SELECT ID_CATEGORIA FROM CATEGORIE_ARTICOLI WHERE NOME_CATEGORIA = 'Arredi'), 'PZ', 180.00, 22.00, 'S', CURRENT TIMESTAMP),
    ('CONS001', 'Consulenza IT', 'Consulenza tecnica IT (tariffa giornaliera)', (SELECT ID_CATEGORIA FROM CATEGORIE_ARTICOLI WHERE NOME_CATEGORIA = 'Servizi'), 'GG', 400.00, 22.00, 'S', CURRENT TIMESTAMP),
    ('TRAIN001', 'Formazione base', 'Corso di formazione base (prezzo per partecipante)', (SELECT ID_CATEGORIA FROM CATEGORIE_ARTICOLI WHERE NOME_CATEGORIA = 'Servizi'), 'PZ', 300.00, 22.00, 'S', CURRENT TIMESTAMP);

-- Inserimento dati per i periodi di paga
INSERT INTO PERIODI_PAGA (ANNO, MESE, DATA_INIZIO, DATA_FINE, STATO) VALUES
    (2025, 1, '2025-01-01', '2025-01-31', 'ELABORATO'),
    (2025, 2, '2025-02-01', '2025-02-28', 'ELABORATO'),
    (2025, 3, '2025-03-01', '2025-03-31', 'ELABORATO'),
    (2025, 4, '2025-04-01', '2025-04-30', 'CHIUSO'),
    (2025, 5, '2025-05-01', '2025-05-31', 'APERTO');

-- Inserimento dati per i giorni lavorativi
INSERT INTO GIORNI_LAVORATIVI (DATA, GIORNO_SETTIMANA, FESTIVITA, LAVORATIVO)
WITH date_range AS (
    SELECT DATE('2025-01-01') + (ROW_NUMBER() OVER() - 1) DAYS AS DATE_VALUE
    FROM SYSCAT.COLUMNS
    FETCH FIRST 365 ROWS ONLY
)
SELECT 
    DATE_VALUE, 
    DAYOFWEEK(DATE_VALUE),
    CASE 
        WHEN DATE_VALUE = DATE('2025-01-01') THEN 'Capodanno'
        WHEN DATE_VALUE = DATE('2025-01-06') THEN 'Epifania'
        WHEN DATE_VALUE = DATE('2025-04-21') THEN 'Pasquetta'
        WHEN DATE_VALUE = DATE('2025-04-25') THEN 'Festa della Liberazione'
        WHEN DATE_VALUE = DATE('2025-05-01') THEN 'Festa del Lavoro'
        WHEN DATE_VALUE = DATE('2025-06-02') THEN 'Festa della Repubblica'
        WHEN DATE_VALUE = DATE('2025-08-15') THEN 'Ferragosto'
        WHEN DATE_VALUE = DATE('2025-11-01') THEN 'Ognissanti'
        WHEN DATE_VALUE = DATE('2025-12-08') THEN 'Immacolata Concezione'
        WHEN DATE_VALUE = DATE('2025-12-25') THEN 'Natale'
        WHEN DATE_VALUE = DATE('2025-12-26') THEN 'Santo Stefano'
        ELSE NULL
    END,
    CASE 
        WHEN DAYOFWEEK(DATE_VALUE) IN (1, 7) THEN 'N'
        WHEN DATE_VALUE IN (DATE('2025-01-01'), DATE('2025-01-06'), DATE('2025-04-21'), 
                           DATE('2025-04-25'), DATE('2025-05-01'), DATE('2025-06-02'), 
                           DATE('2025-08-15'), DATE('2025-11-01'), DATE('2025-12-08'), 
                           DATE('2025-12-25'), DATE('2025-12-26')) THEN 'N'
        ELSE 'S'
    END
FROM date_range;

-- Inserimento dati per i cicli di valutazione
INSERT INTO CICLI_VALUTAZIONE (ANNO, NOME_CICLO, DATA_INIZIO, DATA_FINE, STATO, DESCRIZIONE) VALUES
    (2025, 'Performance Annuale', '2025-01-01', '2025-12-31', 'IN_CORSO', 'Ciclo di valutazione annuale 2025'),
    (2025, 'Mid-Year Review', '2025-06-01', '2025-06-30', 'PIANIFICATO', 'Revisione di metà anno'),
    (2024, 'Performance Annuale', '2024-01-01', '2024-12-31', 'COMPLETATO', 'Ciclo di valutazione annuale 2024');

-- Inserimento dati per le categorie di valutazione
INSERT INTO CATEGORIE_VALUTAZIONE (NOME_CATEGORIA, DESCRIZIONE, PESO_DEFAULT, ATTIVA) VALUES
    ('Competenze tecniche', 'Valutazione delle competenze tecniche', 0.30, 'S'),
    ('Leadership', 'Capacità di leadership e gestione del team', 0.20, 'S'),
    ('Orientamento al risultato', 'Capacità di raggiungere gli obiettivi', 0.25, 'S'),
    ('Comunicazione', 'Capacità di comunicazione interna ed esterna', 0.15, 'S'),
    ('Innovazione', 'Capacità di innovare e proporre miglioramenti', 0.10, 'S');

-- Inserimento dati per le locazioni magazzino
INSERT INTO LOCAZIONI_MAGAZZINO (CODICE_LOCAZIONE, DESCRIZIONE, AREA, SCAFFALE, RIPIANO, POSIZIONE, ATTIVA) VALUES
    ('A-01-01-01', 'Magazzino Principale - Area A', 'A', '01', '01', '01', 'S'),
    ('A-01-02-01', 'Magazzino Principale - Area A', 'A', '01', '02', '01', 'S'),
    ('A-02-01-01', 'Magazzino Principale - Area A', 'A', '02', '01', '01', 'S'),
    ('B-01-01-01', 'Magazzino Principale - Area B', 'B', '01', '01', '01', 'S'),
    ('B-01-02-01', 'Magazzino Principale - Area B', 'B', '01', '02', '01', 'S');

-- Inserimento livelli di approvazione
INSERT INTO LIVELLI_APPROVAZIONE (NOME_LIVELLO, SOGLIA_MIN, SOGLIA_MAX, ORDINE_APPROVAZIONE, DESCRIZIONE, ATTIVO) VALUES
    ('Responsabile diretto', 0.00, 1000.00, 1, 'Primo livello di approvazione', 'S'),
    ('Responsabile dipartimento', 1000.01, 5000.00, 2, 'Secondo livello di approvazione', 'S'),
    ('Direzione', 5000.01, 20000.00, 3, 'Terzo livello di approvazione', 'S'),
    ('Amministratore delegato', 20000.01, NULL, 4, 'Livello massimo di approvazione', 'S');

-- Inserimento tipi di obiettivi
INSERT INTO TIPI_OBIETTIVI (NOME_TIPO, DESCRIZIONE, ATTIVO) VALUES
    ('Obiettivo di business', 'Obiettivo legato ai risultati di business', 'S'),
    ('Obiettivo di sviluppo', 'Obiettivo di sviluppo personale/professionale', 'S'),
    ('Obiettivo di progetto', 'Obiettivo legato a progetti specifici', 'S'),
    ('Obiettivo di innovazione', 'Obiettivo legato all''innovazione', 'S'),
    ('Obiettivo di processo', 'Obiettivo legato al miglioramento dei processi', 'S');

-- Inserimento periodi degli obiettivi
INSERT INTO PERIODI_OBIETTIVI (ANNO, NOME_PERIODO, DATA_INIZIO, DATA_FINE, STATO) VALUES
    (2025, 'Annuale 2025', '2025-01-01', '2025-12-31', 'ATTIVO'),
    (2025, 'Q1 2025', '2025-01-01', '2025-03-31', 'ATTIVO'),
    (2025, 'Q2 2025', '2025-04-01', '2025-06-30', 'PIANIFICAZIONE'),
    (2025, 'Q3 2025', '2025-07-01', '2025-09-30', 'PIANIFICAZIONE'),
    (2025, 'Q4 2025', '2025-10-01', '2025-12-31', 'PIANIFICAZIONE');