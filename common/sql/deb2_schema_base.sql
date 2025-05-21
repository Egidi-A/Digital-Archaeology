-- File: db2_schema_base.sql
-- Descrizione: Schema DB2 di base per l'applicazione

-- Schema DB2 per gestione dipendenti
-- Tabella dei dipartimenti
CREATE TABLE DIPARTIMENTI (
    ID_DIPARTIMENTO INTEGER NOT NULL GENERATED ALWAYS AS IDENTITY,
    NOME_DIPARTIMENTO VARCHAR(15) NOT NULL,
    PRIMARY KEY (ID_DIPARTIMENTO)
);

-- Tabella delle posizioni lavorative
CREATE TABLE POSIZIONI (
    ID_POSIZIONE INTEGER NOT NULL GENERATED ALWAYS AS IDENTITY,
    NOME_POSIZIONE VARCHAR(20) NOT NULL,
    STIPENDIO_BASE DECIMAL(8,2) NOT NULL,
    ID_DIPARTIMENTO INTEGER NOT NULL,
    PRIMARY KEY (ID_POSIZIONE),
    FOREIGN KEY (ID_DIPARTIMENTO) REFERENCES DIPARTIMENTI(ID_DIPARTIMENTO)
);

-- Tabella dei dipendenti
CREATE TABLE DIPENDENTI (
    ID_DIPENDENTE INTEGER NOT NULL GENERATED ALWAYS AS IDENTITY,
    NOME VARCHAR(30) NOT NULL,
    ID_POSIZIONE INTEGER NOT NULL,
    DATA_ASSUNZIONE DATE, 
    PRIMARY KEY (ID_DIPENDENTE),
    FOREIGN KEY (ID_POSIZIONE) REFERENCES POSIZIONI(ID_POSIZIONE)
);

-- Indici per migliorare le performance delle query più comuni
CREATE INDEX IDX_DIPENDENTI_POSIZIONE ON DIPENDENTI(ID_POSIZIONE);
CREATE INDEX IDX_POSIZIONI_DIPARTIMENTO ON POSIZIONI(ID_DIPARTIMENTO);

-- Commento per funzionalità di audit e logging (da implementare in futuro)
-- Le tabelle di audit registreranno le modifiche ai dati principali
-- e saranno utilizzate per la tracciabilità delle operazioni
-- CREATE TABLE AUDIT_LOG (
--    ID_LOG INTEGER NOT NULL GENERATED ALWAYS AS IDENTITY,
--    TABELLA VARCHAR(30) NOT NULL,
--    OPERAZIONE CHAR(1) NOT NULL, -- 'I'=Insert, 'U'=Update, 'D'=Delete
--    ID_RECORD INTEGER NOT NULL,
--    DATA_OPERAZIONE TIMESTAMP NOT NULL DEFAULT CURRENT TIMESTAMP,
--    ID_UTENTE VARCHAR(30),
--    PRIMARY KEY (ID_LOG)
-- );

-- Aggiungi eventuali altri commenti o documentazione per il database
-- Lo schema di base contiene solo le tabelle principali dell'anagrafica.
-- Le tabelle aggiuntive per gli altri moduli (stipendi, presenze, etc.)
-- sono definite nei rispettivi file SQL specifici per ogni modulo.

-- Nota: Questo file non include i dati di esempio. I dati di test
-- sono definiti nel file sample_data.sql