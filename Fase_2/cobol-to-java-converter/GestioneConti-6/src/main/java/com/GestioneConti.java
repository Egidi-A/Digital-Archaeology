package com;

import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.sql.Connection;
import java.sql.Date;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.text.DecimalFormat;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.Scanner;

/**
 * Traduzione Java del programma COBOL GESTIONE-CONTI.
 * Questo programma gestisce le operazioni base di un conto corrente bancario,
 * interfacciandosi con un database PostgreSQL tramite JDBC.
 *
 * @author ANNALISA-EGIDI (Tradotto da Compilatore Avanzato)
 * @version 2025-05-20
 */
public class GestioneConti {

    // --- Configurazione Database ---
    private static final String DB_URL = "jdbc:postgresql://localhost:5432/banca";
    private static final String DB_USER = "postgres";
    private static final String DB_PASSWORD = "password";

    // --- Connessione e I/O ---
    private Connection connection;
    private final Scanner scanner = new Scanner(System.in);
    private static final DecimalFormat currencyFormatter = new DecimalFormat("###,##0.00");

    // --- Variabili di stato (simulano la WORKING-STORAGE) ---
    private int wsScelta = 0;
    private String wsContinua = "S";
    private String wsEsito;

    // Variabili per operazioni
    private String wsNumeroConto;
    private String wsCodiceCliente;
    private BigDecimal wsImporto;
    private BigDecimal wsSaldo;
    private String wsTipoMovimento;
    private String wsCausale;

    // Dati Cliente
    private String wsCliCodice;
    private String wsCliNome;
    private String wsCliCognome;

    // Dati Conto
    private String wsConNumero;
    private String wsConCliente;
    private String wsConTipo;
    private BigDecimal wsConSaldo;
    private String wsConDataApertura;
    private String wsConStato;
    private BigDecimal wsConFido;

    // Dati Movimento (per estratto conto)
    private Timestamp wsMovData;
    private String wsMovTipo;
    private BigDecimal wsMovImporto;
    private String wsMovCausale;
    private BigDecimal wsMovSaldoDopo;

    // Simulazione SQLCODE
    private int sqlcode = 0;

    /**
     * Metodo principale che avvia l'applicazione.
     *
     * @param args Argomenti della riga di comando (non usati).
     */
    public static void main(String[] args) {
        GestioneConti app = new GestioneConti();
        app.mainLogic();
    }

    /**
     * Contiene il flusso principale del programma, equivalente al paragrafo MAIN-LOGIC del COBOL.
     */
    public void mainLogic() {
        if (!connectDatabase()) {
            return; // Esce se la connessione fallisce
        }

        try {
            do {
                displayMenu();
                processChoice();

                if (wsScelta != 0) {
                    System.out.println(" ");
                    System.out.print("Continuare? (S/N): ");
                    wsContinua = scanner.nextLine().toUpperCase();
                } else {
                    wsContinua = "N"; // Esce dal ciclo se la scelta è 0
                }

            } while (wsContinua.equalsIgnoreCase("S"));

        } finally {
            disconnectDatabase();
            scanner.close();
            System.out.println("Programma terminato.");
        }
    }

    /**
     * Stabilisce la connessione al database PostgreSQL.
     * Imposta auto-commit a false per gestire le transazioni manualmente.
     *
     * @return true se la connessione ha successo, altrimenti false.
     */
    private boolean connectDatabase() {
        try {
            connection = DriverManager.getConnection(DB_URL, DB_USER, DB_PASSWORD);
            connection.setAutoCommit(false); // Fondamentale per la gestione delle transazioni
            System.out.println("Connessione al database stabilita");
            return true;
        } catch (SQLException e) {
            System.err.println("Errore connessione database: " + e.getMessage());
            handleSqlException(e);
            return false;
        }
    }

    /**
     * Chiude la connessione al database.
     */
    private void disconnectDatabase() {
        try {
            if (connection != null && !connection.isClosed()) {
                connection.close();
                System.out.println("Disconnesso dal database");
            }
        } catch (SQLException e) {
            System.err.println("Errore durante la disconnessione: " + e.getMessage());
        }
    }

    /**
     * Visualizza il menu principale delle operazioni.
     */
    private void displayMenu() {
        System.out.println("\n===== SISTEMA GESTIONE CONTI CORRENTI =====");
        System.out.println("1. Apertura nuovo conto");
        System.out.println("2. Deposito");
        System.out.println("3. Prelievo");
        System.out.println("4. Visualizza saldo");
        System.out.println("5. Estratto conto");
        System.out.println("6. Chiusura conto");
        System.out.println("0. Esci");
        System.out.println("===========================================");
        System.out.print("Scelta: ");
        try {
            wsScelta = Integer.parseInt(scanner.nextLine());
        } catch (NumberFormatException e) {
            wsScelta = -1; // Scelta non valida
        }
    }

    /**
     * Esegue l'azione corrispondente alla scelta dell'utente.
     * Equivalente al paragrafo ELABORA-SCELTA.
     */
    private void processChoice() {
        switch (wsScelta) {
            case 1:
                openAccount();
                break;
            case 2:
                makeDeposit();
                break;
            case 3:
                makeWithdrawal();
                break;
            case 4:
                viewBalance();
                break;
            case 5:
                generateAccountStatement();
                break;
            case 6:
                closeAccount();
                break;
            case 0:
                // L'uscita è gestita nel ciclo principale
                break;
            default:
                System.out.println("Scelta non valida!");
                break;
        }
    }

    /**
     * Gestisce l'apertura di un nuovo conto corrente.
     * Esegue tutte le operazioni in una singola transazione.
     */
    private void openAccount() {
        System.out.println("\n=== APERTURA NUOVO CONTO ===");
        try {
            System.out.print("Codice cliente: ");
            wsConCliente = scanner.nextLine();

            // Verifica esistenza cliente
            String sqlVerifyClient = "SELECT codice_cliente, nome, cognome FROM CLIENTI WHERE codice_cliente = ?";
            try (PreparedStatement ps = connection.prepareStatement(sqlVerifyClient)) {
                ps.setString(1, wsConCliente);
                try (ResultSet rs = ps.executeQuery()) {
                    if (rs.next()) {
                        sqlcode = 0;
                        wsCliCodice = rs.getString("codice_cliente");
                        wsCliNome = rs.getString("nome");
                        wsCliCognome = rs.getString("cognome");
                        System.out.println("Cliente: " + wsCliNome + " " + wsCliCognome);
                    } else {
                        sqlcode = 100;
                        System.out.println("Cliente non trovato!");
                        return; // Esce dalla procedura
                    }
                }
            }

            // Genera nuovo numero conto
            generateAccountNumber();
            if (sqlcode != 0) {
                System.out.println("Impossibile generare un nuovo numero di conto.");
                return;
            }
            wsConNumero = this.wsConNumero; // Assegna il numero generato

            System.out.print("Tipo conto (C=Corrente, D=Deposito): ");
            wsConTipo = scanner.nextLine().toUpperCase();
            System.out.print("Importo iniziale: ");
            wsConSaldo = new BigDecimal(scanner.nextLine()).setScale(2, RoundingMode.HALF_UP);
            System.out.print("Fido accordato: ");
            wsConFido = new BigDecimal(scanner.nextLine()).setScale(2, RoundingMode.HALF_UP);
            wsConStato = "A";

            // Inserisci nuovo conto
            String sqlInsertAccount = "INSERT INTO CONTI (numero_conto, codice_cliente, tipo_conto, saldo, data_apertura, stato, fido) VALUES (?, ?, ?, ?, CURRENT_DATE, ?, ?)";
            try (PreparedStatement ps = connection.prepareStatement(sqlInsertAccount)) {
                ps.setString(1, wsConNumero);
                ps.setString(2, wsConCliente);
                ps.setString(3, wsConTipo);
                ps.setBigDecimal(4, wsConSaldo);
                ps.setString(5, wsConStato);
                ps.setBigDecimal(6, wsConFido);
                ps.executeUpdate();
            }

            System.out.println("Conto " + wsConNumero + " creato con successo!");

            // Registra movimento iniziale se c'è un saldo
            if (wsConSaldo.compareTo(BigDecimal.ZERO) > 0) {
                this.wsNumeroConto = wsConNumero;
                this.wsImporto = wsConSaldo;
                this.wsTipoMovimento = "D";
                this.wsCausale = "Deposito iniziale";
                recordTransaction();
            }

            connection.commit(); // Conferma la transazione

        } catch (SQLException e) {
            System.err.println("Errore creazione conto: " + e.getMessage());
            handleSqlException(e);
            rollbackTransaction();
        } catch (NumberFormatException e) {
            System.err.println("Input numerico non valido.");
            rollbackTransaction();
        }
    }

    /**
     * Genera un nuovo numero di conto sequenziale.
     * Equivalente al paragrafo GENERA-NUMERO-CONTO.
     */
    private void generateAccountNumber() {
        String sql = "SELECT 'IT' || LPAD(CAST(COALESCE(MAX(CAST(SUBSTR(numero_conto, 3) AS INTEGER)), 0) + 1 AS VARCHAR), 10, '0') FROM CONTI WHERE numero_conto LIKE 'IT%'";
        try (PreparedStatement ps = connection.prepareStatement(sql);
             ResultSet rs = ps.executeQuery()) {
            if (rs.next()) {
                this.wsConNumero = rs.getString(1);
                sqlcode = 0;
            } else {
                // Caso improbabile, ma gestito per sicurezza
                sqlcode = 100;
                this.wsConNumero = "IT0000000001";
            }
        } catch (SQLException e) {
            System.err.println("Errore durante la generazione del numero conto.");
            handleSqlException(e);
        }
    }

    /**
     * Gestisce un'operazione di deposito.
     */
    private void makeDeposit() {
        System.out.println("\n=== DEPOSITO ===");
        try {
            System.out.print("Numero conto: ");
            wsNumeroConto = scanner.nextLine();

            if (!verifyAccount()) return;

            System.out.print("Importo deposito: ");
            wsImporto = new BigDecimal(scanner.nextLine()).setScale(2, RoundingMode.HALF_UP);

            if (wsImporto.compareTo(BigDecimal.ZERO) <= 0) {
                System.out.println("Importo non valido!");
                return;
            }

            System.out.print("Causale: ");
            wsCausale = scanner.nextLine();

            // Aggiorna saldo
            String sqlUpdate = "UPDATE CONTI SET saldo = saldo + ? WHERE numero_conto = ? AND stato = 'A'";
            try (PreparedStatement ps = connection.prepareStatement(sqlUpdate)) {
                ps.setBigDecimal(1, wsImporto);
                ps.setString(2, wsNumeroConto);
                int rowsAffected = ps.executeUpdate();
                if (rowsAffected > 0) {
                    sqlcode = 0;
                    wsTipoMovimento = "D";
                    recordTransaction();
                    connection.commit();
                    System.out.println("Deposito effettuato con successo!");
                } else {
                    sqlcode = 100;
                    System.out.println("Errore durante il deposito: conto non trovato o non attivo.");
                    rollbackTransaction();
                }
            }
        } catch (SQLException e) {
            System.err.println("Errore durante il deposito: " + e.getMessage());
            handleSqlException(e);
            rollbackTransaction();
        } catch (NumberFormatException e) {
            System.err.println("Input numerico non valido.");
            rollbackTransaction();
        }
    }

    /**
     * Gestisce un'operazione di prelievo.
     */
    private void makeWithdrawal() {
        System.out.println("\n=== PRELIEVO ===");
        try {
            System.out.print("Numero conto: ");
            wsNumeroConto = scanner.nextLine();

            if (!verifyAccount()) return;

            System.out.print("Importo prelievo: ");
            wsImporto = new BigDecimal(scanner.nextLine()).setScale(2, RoundingMode.HALF_UP);

            if (wsImporto.compareTo(BigDecimal.ZERO) <= 0) {
                System.out.println("Importo non valido!");
                return;
            }

            // Verifica disponibilità
            BigDecimal saldoCorrente;
            BigDecimal fido;
            String sqlCheck = "SELECT saldo, fido FROM CONTI WHERE numero_conto = ? AND stato = 'A'";
            try (PreparedStatement ps = connection.prepareStatement(sqlCheck)) {
                ps.setString(1, wsNumeroConto);
                try (ResultSet rs = ps.executeQuery()) {
                    if (rs.next()) {
                        saldoCorrente = rs.getBigDecimal("saldo");
                        fido = rs.getBigDecimal("fido");
                    } else {
                        System.out.println("Conto non trovato o non attivo.");
                        return;
                    }
                }
            }

            if (saldoCorrente.subtract(wsImporto).compareTo(fido.negate()) < 0) {
                System.out.println("Fondi insufficienti!");
                System.out.println("Saldo attuale: " + currencyFormatter.format(saldoCorrente));
                System.out.println("Fido disponibile: " + currencyFormatter.format(fido));
                return;
            }

            System.out.print("Causale: ");
            wsCausale = scanner.nextLine();

            // Aggiorna saldo
            String sqlUpdate = "UPDATE CONTI SET saldo = saldo - ? WHERE numero_conto = ? AND stato = 'A'";
            try (PreparedStatement ps = connection.prepareStatement(sqlUpdate)) {
                ps.setBigDecimal(1, wsImporto);
                ps.setString(2, wsNumeroConto);
                int rowsAffected = ps.executeUpdate();
                if (rowsAffected > 0) {
                    sqlcode = 0;
                    wsTipoMovimento = "P";
                    recordTransaction();
                    connection.commit();
                    System.out.println("Prelievo effettuato con successo!");
                } else {
                    sqlcode = 100;
                    System.out.println("Errore durante il prelievo.");
                    rollbackTransaction();
                }
            }
        } catch (SQLException e) {
            System.err.println("Errore durante il prelievo: " + e.getMessage());
            handleSqlException(e);
            rollbackTransaction();
        } catch (NumberFormatException e) {
            System.err.println("Input numerico non valido.");
            rollbackTransaction();
        }
    }

    /**
     * Visualizza il saldo e le informazioni principali del conto.
     */
    private void viewBalance() {
        System.out.println("\n=== VISUALIZZA SALDO ===");
        System.out.print("Numero conto: ");
        wsNumeroConto = scanner.nextLine();

        String sql = "SELECT c.saldo, c.fido, cl.nome, cl.cognome FROM CONTI c JOIN CLIENTI cl ON c.codice_cliente = cl.codice_cliente WHERE c.numero_conto = ? AND c.stato = 'A'";
        try (PreparedStatement ps = connection.prepareStatement(sql)) {
            ps.setString(1, wsNumeroConto);
            try (ResultSet rs = ps.executeQuery()) {
                if (rs.next()) {
                    sqlcode = 0;
                    wsConSaldo = rs.getBigDecimal("saldo");
                    wsConFido = rs.getBigDecimal("fido");
                    wsCliNome = rs.getString("nome");
                    wsCliCognome = rs.getString("cognome");

                    System.out.println("\nIntestatario: " + wsCliNome + " " + wsCliCognome);
                    System.out.println("Saldo attuale: EUR " + currencyFormatter.format(wsConSaldo));
                    System.out.println("Fido accordato: EUR " + currencyFormatter.format(wsConFido));
                    BigDecimal disponibile = wsConSaldo.add(wsConFido);
                    System.out.println("Disponibile: EUR " + currencyFormatter.format(disponibile));
                } else {
                    sqlcode = 100;
                    System.out.println("Conto non trovato o non attivo!");
                }
            }
        } catch (SQLException e) {
            System.err.println("Errore database: " + e.getMessage());
            handleSqlException(e);
        }
    }

    /**
     * Genera un file di testo con l'estratto conto.
     */
    private void generateAccountStatement() {
        System.out.println("\n=== ESTRATTO CONTO ===");
        System.out.print("Numero conto: ");
        wsNumeroConto = scanner.nextLine();

        if (!verifyAccount()) return;

        String fileName = "ESTRATTO-CONTO.TXT";
        try (PrintWriter writer = new PrintWriter(new FileWriter(fileName))) {
            // Intestazione report
            writer.println(String.format("%-50s%s", "", "ESTRATTO CONTO BANCARIO"));
            writer.println("-".repeat(132));
            writer.println("Conto: " + wsNumeroConto + "    Cliente: " + wsCliNome + " " + wsCliCognome);
            writer.println("Data: " + LocalDate.now().format(DateTimeFormatter.ISO_LOCAL_DATE) + "    Ora: " + java.time.LocalTime.now().format(DateTimeFormatter.ofPattern("HH:mm:ss")));
            writer.println("-".repeat(132));
            writer.println(String.format("%-20s %-5s %15s   %-30s %15s", "DATA/ORA", "TIPO", "IMPORTO", "CAUSALE", "SALDO DOPO"));
            writer.println("-".repeat(132));

            // Cursore movimenti
            String sqlCursor = "SELECT data_movimento, tipo_movimento, importo, causale, saldo_dopo FROM MOVIMENTI WHERE numero_conto = ? ORDER BY data_movimento DESC";
            try (PreparedStatement ps = connection.prepareStatement(sqlCursor)) {
                ps.setString(1, wsNumeroConto);
                try (ResultSet rs = ps.executeQuery()) {
                    while (rs.next()) {
                        wsMovData = rs.getTimestamp("data_movimento");
                        wsMovTipo = rs.getString("tipo_movimento");
                        wsMovImporto = rs.getBigDecimal("importo");
                        wsMovCausale = rs.getString("causale");
                        wsMovSaldoDopo = rs.getBigDecimal("saldo_dopo");
                        writeMovementToReport(writer);
                    }
                }
            }

            writer.println("-".repeat(132));
            writer.println("SALDO FINALE: EUR " + currencyFormatter.format(this.wsConSaldo)); // Usa il saldo verificato

            System.out.println("Estratto conto salvato in " + fileName);

        } catch (IOException e) {
            System.err.println("Errore durante la scrittura del file di report: " + e.getMessage());
        } catch (SQLException e) {
            System.err.println("Errore database durante la generazione dell'estratto conto: " + e.getMessage());
            handleSqlException(e);
        }
    }

    /**
     * Scrive una singola riga di movimento nel file di report.
     * @param writer L'oggetto PrintWriter per scrivere sul file.
     */
    private void writeMovementToReport(PrintWriter writer) {
        String tipoDesc;
        switch (wsMovTipo) {
            case "D": tipoDesc = "DEP"; break;
            case "P": tipoDesc = "PRE"; break;
            case "B": tipoDesc = "BON"; break;
            default: tipoDesc = "???"; break;
        }

        String dataFormatted = wsMovData.toLocalDateTime().format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"));
        String importoFormatted = currencyFormatter.format(wsMovImporto);
        String saldoDopoFormatted = currencyFormatter.format(wsMovSaldoDopo);
        String causaleTrunc = wsMovCausale.length() > 30 ? wsMovCausale.substring(0, 30) : wsMovCausale;

        writer.println(String.format("%-20s %-5s %15s   %-30s %15s",
                dataFormatted,
                tipoDesc,
                importoFormatted,
                causaleTrunc,
                saldoDopoFormatted));
    }

    /**
     * Gestisce la chiusura di un conto.
     */
    private void closeAccount() {
        System.out.println("\n=== CHIUSURA CONTO ===");
        try {
            System.out.print("Numero conto da chiudere: ");
            wsNumeroConto = scanner.nextLine();

            if (!verifyAccount()) return;

            if (wsConSaldo.compareTo(BigDecimal.ZERO) != 0) {
                System.out.println("Impossibile chiudere: saldo non a zero!");
                System.out.println("Saldo attuale: EUR " + currencyFormatter.format(wsConSaldo));
                return;
            }

            System.out.print("Confermare chiusura conto (S/N): ");
            String conferma = scanner.nextLine();

            if (conferma.equalsIgnoreCase("S")) {
                String sqlUpdate = "UPDATE CONTI SET stato = 'C', data_chiusura = CURRENT_DATE WHERE numero_conto = ?";
                try (PreparedStatement ps = connection.prepareStatement(sqlUpdate)) {
                    ps.setString(1, wsNumeroConto);
                    int rowsAffected = ps.executeUpdate();
                    if (rowsAffected > 0) {
                        sqlcode = 0;
                        connection.commit();
                        System.out.println("Conto chiuso con successo!");
                    } else {
                        sqlcode = 100;
                        System.out.println("Errore chiusura conto.");
                        rollbackTransaction();
                    }
                }
            } else {
                System.out.println("Chiusura annullata.");
            }
        } catch (SQLException e) {
            System.err.println("Errore chiusura conto: " + e.getMessage());
            handleSqlException(e);
            rollbackTransaction();
        }
    }

    /**
     * Verifica l'esistenza e lo stato di un conto.
     * Imposta le variabili di classe relative al conto se trovato.
     *
     * @return true se il conto è valido e attivo, altrimenti false.
     */
    private boolean verifyAccount() {
        wsEsito = "OK";
        String sql = "SELECT c.saldo, c.stato, cl.nome, cl.cognome FROM CONTI c JOIN CLIENTI cl ON c.codice_cliente = cl.codice_cliente WHERE c.numero_conto = ?";
        try (PreparedStatement ps = connection.prepareStatement(sql)) {
            ps.setString(1, this.wsNumeroConto);
            try (ResultSet rs = ps.executeQuery()) {
                if (rs.next()) {
                    sqlcode = 0;
                    this.wsConSaldo = rs.getBigDecimal("saldo");
                    this.wsConStato = rs.getString("stato");
                    this.wsCliNome = rs.getString("nome");
                    this.wsCliCognome = rs.getString("cognome");

                    if (!"A".equals(this.wsConStato)) {
                        System.out.println("Conto non attivo!");
                        wsEsito = "KO";
                    }
                } else {
                    sqlcode = 100;
                    System.out.println("Conto non trovato!");
                    wsEsito = "KO";
                }
            }
        } catch (SQLException e) {
            System.err.println("Errore database: " + e.getMessage());
            handleSqlException(e);
            wsEsito = "KO";
        }
        return "OK".equals(wsEsito);
    }

    /**
     * Registra un movimento nella tabella MOVIMENTI.
     * Questa operazione fa parte di una transazione più grande (deposito, prelievo, etc.).
     * Non gestisce commit o rollback.
     */
    private void recordTransaction() {
        String sqlSelectSaldo = "SELECT saldo FROM CONTI WHERE numero_conto = ?";
        String sqlInsertMov = "INSERT INTO MOVIMENTI (numero_conto, tipo_movimento, importo, causale, saldo_dopo, eseguito_da) VALUES (?, ?, ?, ?, ?, 'SISTEMA')";

        try (PreparedStatement psSelect = connection.prepareStatement(sqlSelectSaldo)) {
            psSelect.setString(1, this.wsNumeroConto);
            try (ResultSet rs = psSelect.executeQuery()) {
                if (rs.next()) {
                    this.wsSaldo = rs.getBigDecimal("saldo");
                } else {
                    // Se non trova il saldo, c'è un problema grave nella transazione
                    throw new SQLException("Impossibile recuperare il saldo aggiornato per il conto " + this.wsNumeroConto);
                }
            }
        } catch (SQLException e) {
            // Rilancia l'eccezione per far fallire la transazione principale
            throw new RuntimeException("Fallimento nel recupero del saldo per la registrazione del movimento.", e);
        }

        try (PreparedStatement psInsert = connection.prepareStatement(sqlInsertMov)) {
            psInsert.setString(1, this.wsNumeroConto);
            psInsert.setString(2, this.wsTipoMovimento);
            psInsert.setBigDecimal(3, this.wsImporto);
            psInsert.setString(4, this.wsCausale);
            psInsert.setBigDecimal(5, this.wsSaldo);
            psInsert.executeUpdate();
        } catch (SQLException e) {
            // Rilancia l'eccezione per far fallire la transazione principale
            throw new RuntimeException("Fallimento nella registrazione del movimento.", e);
        }
    }

    /**
     * Annulla la transazione corrente.
     */
    private void rollbackTransaction() {
        try {
            if (connection != null) {
                connection.rollback();
                System.out.println("Transazione annullata a causa di un errore.");
            }
        } catch (SQLException ex) {
            System.err.println("Errore critico durante il rollback della transazione: " + ex.getMessage());
        }
    }

    /**
     * Simula il comportamento di SQLCODE basato su SQLException.
     * @param e L'eccezione SQL catturata.
     */
    private void handleSqlException(SQLException e) {
        // "02000" è lo SQLState standard per "no data found"
        if ("02000".equals(e.getSQLState())) {
            this.sqlcode = 100;
        } else {
            this.sqlcode = e.getErrorCode(); // Codice di errore specifico del driver
        }
    }
}