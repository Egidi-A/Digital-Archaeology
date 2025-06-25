-- Schema database per Sistema Gestione Paghe e Stipendi
-- Compatibile con PostgreSQL/MySQL

-- Tabella DIPENDENTI
CREATE TABLE DIPENDENTI (
    matricola          CHAR(6) PRIMARY KEY,
    nome               VARCHAR(50) NOT NULL,
    cognome            VARCHAR(50) NOT NULL,
    codice_fiscale     CHAR(16) UNIQUE NOT NULL,
    data_nascita       DATE NOT NULL,
    data_assunzione    DATE NOT NULL,
    qualifica          VARCHAR(50) NOT NULL,
    livello            CHAR(2) NOT NULL,
    reparto            VARCHAR(30),
    stipendio_base     DECIMAL(10,2) NOT NULL,
    stato              CHAR(1) DEFAULT 'A' CHECK (stato IN ('A', 'S', 'C')), -- A=Attivo, S=Sospeso, C=Cessato
    data_cessazione    DATE,
    iban               VARCHAR(27),
    indirizzo          VARCHAR(100),
    citta              VARCHAR(50),
    cap                CHAR(5),
    telefono           VARCHAR(15),
    email              VARCHAR(100)
);

-- Tabella STIPENDI (storico mensile)
CREATE TABLE STIPENDI (
    id_stipendio       SERIAL PRIMARY KEY,
    matricola          CHAR(6) NOT NULL,
    anno               INTEGER NOT NULL,
    mese               INTEGER NOT NULL CHECK (mese BETWEEN 1 AND 12),
    giorni_lavorati    INTEGER DEFAULT 0,
    ore_ordinarie      DECIMAL(5,2) DEFAULT 0,
    ore_straordinarie  DECIMAL(5,2) DEFAULT 0,
    stipendio_base     DECIMAL(10,2) NOT NULL,
    importo_straord    DECIMAL(10,2) DEFAULT 0,
    altre_competenze   DECIMAL(10,2) DEFAULT 0,
    totale_lordo       DECIMAL(10,2) NOT NULL,
    totale_trattenute  DECIMAL(10,2) NOT NULL,
    netto_pagare       DECIMAL(10,2) NOT NULL,
    data_elaborazione  TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    stato_pagamento    CHAR(1) DEFAULT 'E' CHECK (stato_pagamento IN ('E', 'P', 'A')), -- E=Elaborato, P=Pagato, A=Annullato
    FOREIGN KEY (matricola) REFERENCES DIPENDENTI(matricola),
    UNIQUE(matricola, anno, mese)
);

-- Tabella TRATTENUTE (dettaglio trattenute per stipendio)
CREATE TABLE TRATTENUTE (
    id_trattenuta      SERIAL PRIMARY KEY,
    id_stipendio       INTEGER NOT NULL,
    tipo_trattenuta    VARCHAR(20) NOT NULL,
    descrizione        VARCHAR(100),
    aliquota           DECIMAL(5,2),
    importo            DECIMAL(10,2) NOT NULL,
    FOREIGN KEY (id_stipendio) REFERENCES STIPENDI(id_stipendio)
);

-- Tabella CEDOLINI (per archiviazione cedolini generati)
CREATE TABLE CEDOLINI (
    id_cedolino        SERIAL PRIMARY KEY,
    id_stipendio       INTEGER NOT NULL,
    numero_cedolino    VARCHAR(20) UNIQUE NOT NULL,
    data_emissione     DATE NOT NULL,
    contenuto_pdf      TEXT, -- In produzione sarebbe BYTEA per PDF reale
    FOREIGN KEY (id_stipendio) REFERENCES STIPENDI(id_stipendio)
);

-- Tabella PARAMETRI_FISCALI (aliquote e scaglioni)
CREATE TABLE PARAMETRI_FISCALI (
    id_parametro       SERIAL PRIMARY KEY,
    tipo_parametro     VARCHAR(30) NOT NULL,
    descrizione        VARCHAR(100),
    valore_min         DECIMAL(10,2),
    valore_max         DECIMAL(10,2),
    aliquota           DECIMAL(5,2),
    importo_fisso      DECIMAL(10,2),
    anno_validita      INTEGER NOT NULL,
    attivo             BOOLEAN DEFAULT TRUE
);

-- Tabella PRESENZE (per calcolo ore/giorni)
CREATE TABLE PRESENZE (
    id_presenza        SERIAL PRIMARY KEY,
    matricola          CHAR(6) NOT NULL,
    data_presenza      DATE NOT NULL,
    ora_entrata        TIME,
    ora_uscita         TIME,
    ore_ordinarie      DECIMAL(4,2) DEFAULT 8.00,
    ore_straordinarie  DECIMAL(4,2) DEFAULT 0.00,
    tipo_giornata      CHAR(1) DEFAULT 'L' CHECK (tipo_giornata IN ('L', 'F', 'M', 'P', 'A')), -- L=Lavorativo, F=Ferie, M=Malattia, P=Permesso, A=Assenza
    note               VARCHAR(200),
    FOREIGN KEY (matricola) REFERENCES DIPENDENTI(matricola),
    UNIQUE(matricola, data_presenza)
);

-- Indici per ottimizzare le query
CREATE INDEX idx_stipendi_matricola ON STIPENDI(matricola);
CREATE INDEX idx_stipendi_periodo ON STIPENDI(anno, mese);
CREATE INDEX idx_presenze_matricola ON PRESENZE(matricola);
CREATE INDEX idx_presenze_data ON PRESENZE(data_presenza);
CREATE INDEX idx_trattenute_stipendio ON TRATTENUTE(id_stipendio);

-- Dati di esempio
INSERT INTO DIPENDENTI (matricola, nome, cognome, codice_fiscale, data_nascita, data_assunzione, 
                       qualifica, livello, reparto, stipendio_base, iban, indirizzo, citta, cap, telefono, email)
VALUES 
    ('EMP001', 'Mario', 'Rossi', 'RSSMRA80A01H501Z', '1980-01-01', '2020-03-15', 
     'Impiegato', '3A', 'Amministrazione', 1800.00, 'IT60X0542811101000000123456', 
     'Via Roma 1', 'Milano', '20100', '0212345678', 'mario.rossi@azienda.it'),
    
    ('EMP002', 'Anna', 'Bianchi', 'BNCNNA85B41L219K', '1985-02-01', '2019-06-01', 
     'Responsabile', '2B', 'Vendite', 2500.00, 'IT60X0542811101000000123457', 
     'Via Verdi 15', 'Roma', '00100', '0687654321', 'anna.bianchi@azienda.it'),
    
    ('EMP003', 'Luigi', 'Verde', 'VRDLGU75C15F205X', '1975-03-15', '2015-09-01', 
     'Operaio Specializzato', '4A', 'Produzione', 1600.00, 'IT60X0542811101000000123458', 
     'Corso Italia 30', 'Napoli', '80100', '0811234567', 'luigi.verde@azienda.it'),
    
    ('EMP004', 'Giulia', 'Neri', 'NRIGLI90D55H501W', '1990-04-15', '2021-01-10', 
     'Programmatore', '3B', 'IT', 2200.00, 'IT60X0542811101000000123459', 
     'Via Dante 45', 'Torino', '10100', '0119876543', 'giulia.neri@azienda.it'),
    
    ('EMP005', 'Franco', 'Gialli', 'GLLFNC70M10F839X', '1970-08-10', '2010-05-01', 
     'Quadro', '1A', 'Direzione', 3500.00, 'IT60X0542811101000000123460', 
     'Viale Europa 100', 'Bologna', '40100', '0514567890', 'franco.gialli@azienda.it');

-- Parametri fiscali di esempio (semplificati)
INSERT INTO PARAMETRI_FISCALI (tipo_parametro, descrizione, valore_min, valore_max, aliquota, importo_fisso, anno_validita)
VALUES 
    -- IRPEF scaglioni 2025
    ('IRPEF', 'Primo scaglione', 0, 15000, 23.00, 0, 2025),
    ('IRPEF', 'Secondo scaglione', 15000.01, 28000, 25.00, 0, 2025),
    ('IRPEF', 'Terzo scaglione', 28000.01, 50000, 35.00, 0, 2025),
    ('IRPEF', 'Quarto scaglione', 50000.01, 999999, 43.00, 0, 2025),
    
    -- Contributi previdenziali
    ('INPS', 'Contributo IVS dipendente', 0, 999999, 9.19, 0, 2025),
    ('INPS', 'Contributo IVS azienda', 0, 999999, 23.81, 0, 2025),
    
    -- Addizionali
    ('ADD_REG', 'Addizionale regionale', 0, 999999, 1.73, 0, 2025),
    ('ADD_COM', 'Addizionale comunale', 0, 999999, 0.80, 0, 2025),
    
    -- Detrazioni (importi annui)
    ('DETRAZ_LAV', 'Detrazione lavoro dipendente', 0, 8000, 0, 1880, 2025),
    ('DETRAZ_LAV', 'Detrazione lavoro dipendente', 8000.01, 28000, 0, 1910, 2025),
    ('DETRAZ_LAV', 'Detrazione lavoro dipendente', 28000.01, 55000, 0, 1370, 2025),
    
    -- Straordinari
    ('STRAORD', 'Maggiorazione straordinario 25%', 0, 999999, 25.00, 0, 2025),
    ('STRAORD', 'Maggiorazione straordinario 50%', 0, 999999, 50.00, 0, 2025);

-- Presenze di esempio per il mese corrente
INSERT INTO PRESENZE (matricola, data_presenza, ora_entrata, ora_uscita, ore_ordinarie, ore_straordinarie, tipo_giornata)
VALUES 
    -- Presenze EMP001 (20 giorni lavorativi + 2 straordinari)
    ('EMP001', '2025-04-01', '09:00', '18:00', 8.00, 0.00, 'L'),
    ('EMP001', '2025-04-02', '09:00', '19:30', 8.00, 1.50, 'L'),
    ('EMP001', '2025-04-03', '09:00', '18:00', 8.00, 0.00, 'L'),
    ('EMP001', '2025-04-04', '09:00', '20:00', 8.00, 2.00, 'L'),
    ('EMP001', '2025-04-07', '09:00', '18:00', 8.00, 0.00, 'L'),
    
    -- Presenze EMP002 (18 giorni + 2 ferie)
    ('EMP002', '2025-04-01', '08:30', '17:30', 8.00, 0.00, 'L'),
    ('EMP002', '2025-04-02', '08:30', '17:30', 8.00, 0.00, 'L'),
    ('EMP002', '2025-04-03', NULL, NULL, 0.00, 0.00, 'F'),
    ('EMP002', '2025-04-04', NULL, NULL, 0.00, 0.00, 'F'),
    ('EMP002', '2025-04-07', '08:30', '17:30', 8.00, 0.00, 'L'),
    
    -- Presenze EMP003 (19 giorni + 1 malattia)
    ('EMP003', '2025-04-01', '08:00', '17:00', 8.00, 0.00, 'L'),
    ('EMP003', '2025-04-02', NULL, NULL, 0.00, 0.00, 'M'),
    ('EMP003', '2025-04-03', '08:00', '17:00', 8.00, 0.00, 'L'),
    ('EMP003', '2025-04-04', '08:00', '17:00', 8.00, 0.00, 'L'),
    ('EMP003', '2025-04-07', '08:00', '19:00', 8.00, 2.00, 'L');