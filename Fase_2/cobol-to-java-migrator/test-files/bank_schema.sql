-- Creazione database per Sistema Gestione Conti Correnti
-- Compatibile con PostgreSQL/MySQL

-- Tabella CLIENTI
CREATE TABLE CLIENTI (
    codice_cliente      CHAR(8) PRIMARY KEY,
    nome               VARCHAR(50) NOT NULL,
    cognome            VARCHAR(50) NOT NULL,
    codice_fiscale     CHAR(16) UNIQUE NOT NULL,
    data_nascita       DATE NOT NULL,
    indirizzo          VARCHAR(100),
    citta              VARCHAR(50),
    cap                CHAR(5),
    telefono           VARCHAR(15),
    email              VARCHAR(100),
    data_registrazione DATE DEFAULT CURRENT_DATE
);

-- Tabella CONTI
CREATE TABLE CONTI (
    numero_conto       CHAR(12) PRIMARY KEY,
    codice_cliente     CHAR(8) NOT NULL,
    tipo_conto         CHAR(1) NOT NULL CHECK (tipo_conto IN ('C', 'D')), -- C=Corrente, D=Deposito
    saldo              DECIMAL(15,2) DEFAULT 0.00,
    data_apertura      DATE NOT NULL,
    data_chiusura      DATE,
    stato              CHAR(1) DEFAULT 'A' CHECK (stato IN ('A', 'C', 'S')), -- A=Attivo, C=Chiuso, S=Sospeso
    fido               DECIMAL(15,2) DEFAULT 0.00,
    FOREIGN KEY (codice_cliente) REFERENCES CLIENTI(codice_cliente)
);

-- Tabella MOVIMENTI
CREATE TABLE MOVIMENTI (
    id_movimento       SERIAL PRIMARY KEY,
    numero_conto       CHAR(12) NOT NULL,
    data_movimento     TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    tipo_movimento     CHAR(1) NOT NULL CHECK (tipo_movimento IN ('D', 'P', 'B', 'G')), -- D=Deposito, P=Prelievo, B=Bonifico, G=Giroconto
    importo            DECIMAL(15,2) NOT NULL,
    causale            VARCHAR(100),
    saldo_dopo         DECIMAL(15,2) NOT NULL,
    conto_destinazione CHAR(12),
    eseguito_da        VARCHAR(50),
    FOREIGN KEY (numero_conto) REFERENCES CONTI(numero_conto)
);

-- Indici per migliorare le performance
CREATE INDEX idx_movimenti_conto ON MOVIMENTI(numero_conto);
CREATE INDEX idx_movimenti_data ON MOVIMENTI(data_movimento);
CREATE INDEX idx_conti_cliente ON CONTI(codice_cliente);

-- Dati di esempio per test
INSERT INTO CLIENTI (codice_cliente, nome, cognome, codice_fiscale, data_nascita, indirizzo, citta, cap, telefono, email)
VALUES 
    ('CLI00001', 'Mario', 'Rossi', 'RSSMRA80A01H501Z', '1980-01-01', 'Via Roma 1', 'Milano', '20100', '0212345678', 'mario.rossi@email.it'),
    ('CLI00002', 'Anna', 'Bianchi', 'BNCNNA85B41L219K', '1985-02-01', 'Via Verdi 15', 'Roma', '00100', '0687654321', 'anna.bianchi@email.it'),
    ('CLI00003', 'Luigi', 'Verde', 'VRDLGU75C15F205X', '1975-03-15', 'Corso Italia 30', 'Napoli', '80100', '0811234567', 'luigi.verde@email.it');

INSERT INTO CONTI (numero_conto, codice_cliente, tipo_conto, saldo, data_apertura, stato)
VALUES 
    ('IT0000000001', 'CLI00001', 'C', 5000.00, '2023-01-15', 'A'),
    ('IT0000000002', 'CLI00001', 'D', 10000.00, '2023-06-01', 'A'),
    ('IT0000000003', 'CLI00002', 'C', 2500.00, '2023-03-20', 'A'),
    ('IT0000000004', 'CLI00003', 'C', 7500.00, '2022-11-10', 'A');

-- Alcuni movimenti di esempio
INSERT INTO MOVIMENTI (numero_conto, data_movimento, tipo_movimento, importo, causale, saldo_dopo, eseguito_da)
VALUES 
    ('IT0000000001', '2023-01-15 10:00:00', 'D', 5000.00, 'Deposito iniziale', 5000.00, 'SPORTELLO01'),
    ('IT0000000001', '2023-02-01 14:30:00', 'P', 500.00, 'Prelievo bancomat', 4500.00, 'ATM001'),
    ('IT0000000001', '2023-02-15 09:15:00', 'D', 1000.00, 'Stipendio', 5500.00, 'BONIFICO'),
    ('IT0000000003', '2023-03-20 11:00:00', 'D', 2500.00, 'Deposito iniziale', 2500.00, 'SPORTELLO02');