-- Schema database per Sistema Gestione Magazzino e Inventario
-- Compatibile con PostgreSQL

-- Elimina le tabelle se esistono (in ordine inverso per rispettare le foreign key)
DROP TABLE IF EXISTS RIGHE_INVENTARIO CASCADE;
DROP TABLE IF EXISTS INVENTARI CASCADE;
DROP TABLE IF EXISTS RIGHE_ORDINE CASCADE;
DROP TABLE IF EXISTS ORDINI CASCADE;
DROP TABLE IF EXISTS LOTTI CASCADE;
DROP TABLE IF EXISTS MOVIMENTI_MAGAZZINO CASCADE;
DROP TABLE IF EXISTS GIACENZE CASCADE;
DROP TABLE IF EXISTS ARTICOLI CASCADE;
DROP TABLE IF EXISTS CATEGORIE CASCADE;
DROP TABLE IF EXISTS FORNITORI CASCADE;

-- Tabella FORNITORI
CREATE TABLE FORNITORI (
    codice_fornitore    CHAR(8) PRIMARY KEY,
    ragione_sociale     VARCHAR(100) NOT NULL,
    partita_iva         CHAR(11) UNIQUE NOT NULL,
    codice_fiscale      VARCHAR(16),
    indirizzo           VARCHAR(100),
    citta               VARCHAR(50),
    cap                 CHAR(5),
    provincia           CHAR(2),
    telefono            VARCHAR(15),
    email               VARCHAR(100),
    iban                VARCHAR(27),
    condizioni_pagamento VARCHAR(50),
    stato               CHAR(1) DEFAULT 'A' CHECK (stato IN ('A', 'S', 'C')) -- A=Attivo, S=Sospeso, C=Cessato
);

-- Tabella CATEGORIE
CREATE TABLE CATEGORIE (
    codice_categoria    CHAR(4) PRIMARY KEY,
    descrizione         VARCHAR(100) NOT NULL,
    reparto             VARCHAR(30),
    aliquota_iva        DECIMAL(5,2) DEFAULT 22.00
);

-- Tabella ARTICOLI
CREATE TABLE ARTICOLI (
    codice_articolo     CHAR(10) PRIMARY KEY,
    descrizione         VARCHAR(200) NOT NULL,
    codice_categoria    CHAR(4) NOT NULL,
    unita_misura        VARCHAR(10) NOT NULL,
    codice_fornitore    CHAR(8),
    prezzo_acquisto     DECIMAL(10,2) DEFAULT 0,
    prezzo_vendita      DECIMAL(10,2) DEFAULT 0,
    scorta_minima       DECIMAL(10,2) DEFAULT 0,
    scorta_massima      DECIMAL(10,2) DEFAULT 0,
    punto_riordino      DECIMAL(10,2) DEFAULT 0,
    lotto_riordino      DECIMAL(10,2) DEFAULT 0,
    lead_time_giorni    INTEGER DEFAULT 0,
    ubicazione          VARCHAR(20),
    stato               CHAR(1) DEFAULT 'A' CHECK (stato IN ('A', 'S', 'O')), -- A=Attivo, S=Sospeso, O=Obsoleto
    data_creazione      DATE DEFAULT CURRENT_DATE,
    FOREIGN KEY (codice_categoria) REFERENCES CATEGORIE(codice_categoria),
    FOREIGN KEY (codice_fornitore) REFERENCES FORNITORI(codice_fornitore)
);

-- Tabella GIACENZE (situazione attuale per articolo)
CREATE TABLE GIACENZE (
    codice_articolo     CHAR(10) PRIMARY KEY,
    quantita_disponibile DECIMAL(10,2) DEFAULT 0,
    quantita_impegnata  DECIMAL(10,2) DEFAULT 0,
    quantita_ordinata   DECIMAL(10,2) DEFAULT 0,
    valore_medio        DECIMAL(10,4) DEFAULT 0,
    valore_ultimo       DECIMAL(10,4) DEFAULT 0,
    data_ultimo_carico  DATE,
    data_ultimo_scarico DATE,
    data_ultimo_inventario DATE,
    FOREIGN KEY (codice_articolo) REFERENCES ARTICOLI(codice_articolo)
);

-- Tabella MOVIMENTI_MAGAZZINO
CREATE TABLE MOVIMENTI_MAGAZZINO (
    id_movimento        SERIAL PRIMARY KEY,
    tipo_movimento      CHAR(2) NOT NULL CHECK (tipo_movimento IN ('CA', 'SC', 'RE', 'RI', 'IV', 'TR')), 
    -- CA=Carico, SC=Scarico, RE=Reso, RI=Rettifica Inventario, IV=Inventario, TR=Trasferimento
    numero_documento    VARCHAR(20),
    data_movimento      TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    codice_articolo     CHAR(10) NOT NULL,
    quantita            DECIMAL(10,2) NOT NULL,
    prezzo_unitario     DECIMAL(10,4),
    valore_totale       DECIMAL(12,2),
    causale             VARCHAR(100),
    riferimento_ordine  VARCHAR(20),
    codice_fornitore    CHAR(8),
    operatore           VARCHAR(50),
    note                VARCHAR(200),
    FOREIGN KEY (codice_articolo) REFERENCES ARTICOLI(codice_articolo),
    FOREIGN KEY (codice_fornitore) REFERENCES FORNITORI(codice_fornitore)
);

-- Tabella ORDINI (ordini a fornitori)
CREATE TABLE ORDINI (
    numero_ordine       VARCHAR(20) PRIMARY KEY,
    data_ordine         DATE NOT NULL,
    codice_fornitore    CHAR(8) NOT NULL,
    stato_ordine        CHAR(1) DEFAULT 'A' CHECK (stato_ordine IN ('A', 'C', 'P', 'E')), 
    -- A=Aperto, C=Confermato, P=Parzialmente evaso, E=Evaso
    data_consegna_prevista DATE,
    totale_ordine       DECIMAL(12,2) DEFAULT 0,
    note                VARCHAR(500),
    FOREIGN KEY (codice_fornitore) REFERENCES FORNITORI(codice_fornitore)
);

-- Tabella RIGHE_ORDINE
CREATE TABLE RIGHE_ORDINE (
    id_riga             SERIAL PRIMARY KEY,
    numero_ordine       VARCHAR(20) NOT NULL,
    codice_articolo     CHAR(10) NOT NULL,
    quantita_ordinata   DECIMAL(10,2) NOT NULL,
    quantita_ricevuta   DECIMAL(10,2) DEFAULT 0,
    prezzo_unitario     DECIMAL(10,4) NOT NULL,
    sconto_percentuale  DECIMAL(5,2) DEFAULT 0,
    importo_riga        DECIMAL(12,2) NOT NULL,
    data_consegna_prevista DATE,
    stato_riga          CHAR(1) DEFAULT 'A' CHECK (stato_riga IN ('A', 'P', 'E', 'C')), 
    -- A=Aperta, P=Parzialmente evasa, E=Evasa, C=Cancellata
    FOREIGN KEY (numero_ordine) REFERENCES ORDINI(numero_ordine),
    FOREIGN KEY (codice_articolo) REFERENCES ARTICOLI(codice_articolo)
);

-- Tabella INVENTARI (testate inventari fisici)
CREATE TABLE INVENTARI (
    id_inventario       SERIAL PRIMARY KEY,
    data_inventario     DATE NOT NULL,
    tipo_inventario     CHAR(1) CHECK (tipo_inventario IN ('T', 'P', 'R')), -- T=Totale, P=Parziale, R=Rotativo
    stato               CHAR(1) DEFAULT 'A' CHECK (stato IN ('A', 'C', 'V')), -- A=Aperto, C=Chiuso, V=Validato
    operatore           VARCHAR(50),
    note                VARCHAR(500)
);

-- Tabella RIGHE_INVENTARIO
CREATE TABLE RIGHE_INVENTARIO (
    id_riga             SERIAL PRIMARY KEY,
    id_inventario       INTEGER NOT NULL,
    codice_articolo     CHAR(10) NOT NULL,
    quantita_teorica    DECIMAL(10,2) NOT NULL,
    quantita_rilevata   DECIMAL(10,2),
    differenza          DECIMAL(10,2),
    valore_differenza   DECIMAL(12,2),
    note                VARCHAR(200),
    FOREIGN KEY (id_inventario) REFERENCES INVENTARI(id_inventario),
    FOREIGN KEY (codice_articolo) REFERENCES ARTICOLI(codice_articolo)
);

-- Tabella LOTTI (per tracciabilità FIFO/LIFO)
CREATE TABLE LOTTI (
    id_lotto            SERIAL PRIMARY KEY,
    codice_articolo     CHAR(10) NOT NULL,
    numero_lotto        VARCHAR(20) NOT NULL,
    data_carico         DATE NOT NULL,
    quantita_iniziale   DECIMAL(10,2) NOT NULL,
    quantita_residua    DECIMAL(10,2) NOT NULL,
    prezzo_acquisto     DECIMAL(10,4) NOT NULL,
    data_scadenza       DATE,
    stato               CHAR(1) DEFAULT 'A' CHECK (stato IN ('A', 'E', 'S')), -- A=Attivo, E=Esaurito, S=Scaduto
    FOREIGN KEY (codice_articolo) REFERENCES ARTICOLI(codice_articolo),
    UNIQUE(codice_articolo, numero_lotto)
);

-- Indici per ottimizzare le performance
CREATE INDEX idx_movimenti_articolo ON MOVIMENTI_MAGAZZINO(codice_articolo);
CREATE INDEX idx_movimenti_data ON MOVIMENTI_MAGAZZINO(data_movimento);
CREATE INDEX idx_movimenti_tipo ON MOVIMENTI_MAGAZZINO(tipo_movimento);
CREATE INDEX idx_righe_ordine_articolo ON RIGHE_ORDINE(codice_articolo);
CREATE INDEX idx_righe_ordine_numero ON RIGHE_ORDINE(numero_ordine);
CREATE INDEX idx_lotti_articolo ON LOTTI(codice_articolo);
CREATE INDEX idx_lotti_data ON LOTTI(data_carico);

-- DATI DI ESEMPIO
-- Inserimento fornitori
INSERT INTO FORNITORI (codice_fornitore, ragione_sociale, partita_iva, indirizzo, citta, cap, provincia, telefono, email, condizioni_pagamento)
VALUES 
    ('FOR00001', 'Elettronica SpA', '12345678901', 'Via Industria 100', 'Milano', '20100', 'MI', '0212345678', 'info@elettronica.it', '30 gg FM'),
    ('FOR00002', 'Componenti Srl', '23456789012', 'Via Commercio 50', 'Torino', '10100', 'TO', '0119876543', 'ordini@componenti.it', '60 gg FM'),
    ('FOR00003', 'Tech Supplies', '34567890123', 'Zona Industriale 1', 'Bologna', '40100', 'BO', '0514567890', 'tech@supplies.it', '90 gg FM');

-- Inserimento categorie
INSERT INTO CATEGORIE (codice_categoria, descrizione, reparto, aliquota_iva)
VALUES 
    ('ELET', 'Componenti Elettronici', 'Elettronica', 22.00),
    ('MECC', 'Componenti Meccanici', 'Meccanica', 22.00),
    ('CONS', 'Materiale di Consumo', 'Generale', 22.00),
    ('TOOL', 'Attrezzature', 'Manutenzione', 22.00);

-- Inserimento articoli
INSERT INTO ARTICOLI (codice_articolo, descrizione, codice_categoria, unita_misura, codice_fornitore, 
                     prezzo_acquisto, prezzo_vendita, scorta_minima, punto_riordino, lotto_riordino, ubicazione)
VALUES 
    ('ART0000001', 'Resistenza 10K Ohm 1/4W', 'ELET', 'PZ', 'FOR00001', 0.02, 0.05, 1000, 2000, 5000, 'A01-01'),
    ('ART0000002', 'Condensatore 100uF 25V', 'ELET', 'PZ', 'FOR00001', 0.15, 0.35, 500, 1000, 2000, 'A01-02'),
    ('ART0000003', 'LED Rosso 5mm', 'ELET', 'PZ', 'FOR00001', 0.10, 0.25, 2000, 4000, 10000, 'A01-03'),
    ('ART0000004', 'Vite M3x10 Acciaio', 'MECC', 'PZ', 'FOR00002', 0.01, 0.03, 5000, 10000, 20000, 'B01-01'),
    ('ART0000005', 'Dado M3 Acciaio', 'MECC', 'PZ', 'FOR00002', 0.01, 0.02, 5000, 10000, 20000, 'B01-02'),
    ('ART0000006', 'Guanti Nitrile L', 'CONS', 'CF', 'FOR00003', 8.50, 12.00, 10, 20, 50, 'C01-01'),
    ('ART0000007', 'Cacciavite Phillips PH2', 'TOOL', 'PZ', 'FOR00003', 5.50, 9.90, 5, 10, 20, 'D01-01'),
    ('ART0000008', 'Multimetro Digitale', 'TOOL', 'PZ', 'FOR00001', 35.00, 59.90, 2, 5, 10, 'D02-01'),
    ('ART0000009', 'Stagno 60/40 1mm 100g', 'CONS', 'PZ', 'FOR00001', 12.00, 18.50, 10, 20, 30, 'C02-01'),
    ('ART0000010', 'Breadboard 830 punti', 'ELET', 'PZ', 'FOR00001', 4.50, 8.90, 20, 40, 100, 'A02-01');

-- Inizializza giacenze - CORREZIONE: uso sintassi PostgreSQL corretta
INSERT INTO GIACENZE (codice_articolo, quantita_disponibile, valore_medio, valore_ultimo, data_ultimo_carico)
SELECT codice_articolo, 
       CASE 
           WHEN codice_articolo IN ('ART0000001', 'ART0000003') THEN 5000
           WHEN codice_articolo IN ('ART0000002') THEN 1500
           WHEN codice_articolo IN ('ART0000004', 'ART0000005') THEN 15000
           WHEN codice_articolo IN ('ART0000006', 'ART0000009') THEN 25
           WHEN codice_articolo IN ('ART0000007') THEN 12
           WHEN codice_articolo IN ('ART0000008') THEN 3
           WHEN codice_articolo IN ('ART0000010') THEN 55
       END,
       prezzo_acquisto,
       prezzo_acquisto,
       (CURRENT_DATE - INTERVAL '30' DAY)::DATE
FROM ARTICOLI;

-- Movimenti di esempio
INSERT INTO MOVIMENTI_MAGAZZINO (tipo_movimento, numero_documento, codice_articolo, quantita, prezzo_unitario, valore_totale, causale, codice_fornitore, operatore)
VALUES 
    ('CA', 'DDT001/2025', 'ART0000001', 5000, 0.02, 100.00, 'Carico iniziale', 'FOR00001', 'SISTEMA'),
    ('CA', 'DDT001/2025', 'ART0000002', 1500, 0.15, 225.00, 'Carico iniziale', 'FOR00001', 'SISTEMA'),
    ('CA', 'DDT001/2025', 'ART0000003', 5000, 0.10, 500.00, 'Carico iniziale', 'FOR00001', 'SISTEMA'),
    ('CA', 'DDT002/2025', 'ART0000004', 15000, 0.01, 150.00, 'Carico iniziale', 'FOR00002', 'SISTEMA'),
    ('CA', 'DDT002/2025', 'ART0000005', 15000, 0.01, 150.00, 'Carico iniziale', 'FOR00002', 'SISTEMA'),
    ('SC', 'BDC001/2025', 'ART0000001', 250, 0.02, 5.00, 'Prelievo produzione', NULL, 'MAG01'),
    ('SC', 'BDC001/2025', 'ART0000003', 100, 0.10, 10.00, 'Prelievo produzione', NULL, 'MAG01');

-- Lotti di esempio per FIFO/LIFO
INSERT INTO LOTTI (codice_articolo, numero_lotto, data_carico, quantita_iniziale, quantita_residua, prezzo_acquisto)
VALUES 
    ('ART0000001', 'L202501001', '2025-01-15', 3000, 2750, 0.02),
    ('ART0000001', 'L202502001', '2025-02-20', 2000, 2000, 0.021),
    ('ART0000002', 'L202501002', '2025-01-15', 1000, 1000, 0.15),
    ('ART0000002', 'L202502002', '2025-02-20', 500, 500, 0.14),
    ('ART0000003', 'L202501003', '2025-01-15', 5000, 4900, 0.10);

-- Ordini di esempio
INSERT INTO ORDINI (numero_ordine, data_ordine, codice_fornitore, stato_ordine, data_consegna_prevista, totale_ordine)
VALUES 
    ('ORD2025/001', '2025-04-01', 'FOR00001', 'C', '2025-04-15', 1250.00),
    ('ORD2025/002', '2025-04-05', 'FOR00002', 'A', '2025-04-20', 450.00);

INSERT INTO RIGHE_ORDINE (numero_ordine, codice_articolo, quantita_ordinata, prezzo_unitario, importo_riga)
VALUES 
    ('ORD2025/001', 'ART0000001', 10000, 0.02, 200.00),
    ('ORD2025/001', 'ART0000002', 3000, 0.15, 450.00),
    ('ORD2025/001', 'ART0000003', 6000, 0.10, 600.00),
    ('ORD2025/002', 'ART0000004', 20000, 0.01, 200.00),
    ('ORD2025/002', 'ART0000005', 25000, 0.01, 250.00);
