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
import java.sql.Types; // Import richiesto per la gestione dei valori NULL
import java.text.DecimalFormat;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.Scanner;

/**
 * Traduzione Java del programma COBOL GESTIONE-CONTI.
 * Questo programma gestisce le operazioni di base di un conto corrente bancario,
 * interfacciandosi con un database PostgreSQL tramite JDBC.
 *
 * @author Annalisa Egidi (COBOL), Traduttore AI (Java)
 * @version 2025-05-20
 */
public class GestioneConti {

    // --- Configurazione Database ---
    // NOTA: Assicurarsi che il driver JDBC di PostgreSQL sia nel classpath.
    private static final String DB_URL = "jdbc:postgresql://localhost:5432/banca";
    private static final String DB_USER = "postgres";
    private static final String DB_PASSWORD = "password";

    private Connection connection;
    private final Scanner scanner;

    // --- Mappatura della WORKING-STORAGE SECTION ---
    private int wsScelta = 0;
    private String wsContinua = "S";
    private String wsMessaggio = "";
    private String wsEsito = "";

    // Variabili per database
    private String wsNumeroConto = "";
    private String wsCodiceCliente = "";
    private BigDecimal wsImporto = BigDecimal.ZERO;
    private BigDecimal wsSaldo = BigDecimal.ZERO;
    private String wsTipoMovimento = "";
    private String wsCausale = "";

    // Strutture dati per cliente
    private String wsCliCodice = "";
    private String wsCliNome = "";
    private String wsCliCognome = "";
    // Altri campi cliente non usati attivamente nel flusso principale
    // ma dichiarati per completezza.

    // Strutture dati per conto
    private String wsConNumero = "";
    private String wsConCliente = "";
    private String wsConTipo = "";
    private BigDecimal wsConSaldo = BigDecimal.ZERO;
    private String wsConDataApertura = "";
    private String wsConStato = "";
    private BigDecimal wsConFido = BigDecimal.ZERO;

    // Variabile per simulare SQLCODE
    private int sqlcode = 0;

    // Formattatore per importi monetari, simile a PIC Z,ZZZ,ZZZ,ZZ9.99-
    private final DecimalFormat currencyFormatter = new DecimalFormat("€ #,##0.00");

    /**
     * Costruttore della classe. Inizializza lo scanner per l'input utente.
     */
    public GestioneConti() {
        this.scanner = new Scanner(System.in);
    }

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
     * Logica principale del programma, equivalente al paragrafo MAIN-LOGIC del COBOL.
     */
    private void mainLogic() {
        if (!connectDatabase()) {
            return; // Interrompe l'esecuzione se la connessione fallisce
        }

        do {
            visualizzaMenu();
            elaboraScelta();

            if (wsScelta != 0) {
                System.out.print("\nContinuare? (S/N): ");
                wsContinua = scanner.nextLine().toUpperCase();
            } else {
                wsContinua = "N"; // Esce se la scelta è 0
            }

        } while (wsContinua.equals("S"));

        disconnectDatabase();
        scanner.close();
        System.out.println("Programma terminato.");
    }

    /**
     * Stabilisce la connessione al database.
     * Equivalente al paragrafo CONNETTI-DATABASE.
     * @return true se la connessione ha successo, altrimenti false.
     */
    private boolean connectDatabase() {
        try {
            connection = DriverManager.getConnection(DB_URL, DB_USER, DB_PASSWORD);
            // Impostiamo l'autocommit a false per gestire le transazioni manualmente
            connection.setAutoCommit(false);
            System.out.println("Connessione al database stabilita.");
            return true;
        } catch (SQLException e) {
            System.err.println("Errore connessione database: " + e.getMessage());
            sqlcode = e.getErrorCode();
            return false;
        }
    }

    /**
     * Chiude la connessione al database.
     * Equivalente al paragrafo DISCONNETTI-DATABASE.
     */
    private void disconnectDatabase() {
        if (connection != null) {
            try {
                connection.close();
                System.out.println("Disconnesso dal database.");
            } catch (SQLException e) {
                System.err.println("Errore durante la disconnessione: " + e.getMessage());
            }
        }
    }

    /**
     * Mostra il menu delle opzioni all'utente.
     * Equivalente al paragrafo VISUALIZZA-MENU.
     */
    private void visualizzaMenu() {
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
    private void elaboraScelta() {
        switch (wsScelta) {
            case 1:
                aperturaConto();
                break;
            case 2:
                deposito();
                break;
            case 3:
                prelievo();
                break;
            case 4:
                visualizzaSaldo();
                break;
            case 5:
                estrattoConto();
                break;
            case 6:
                chiusuraConto();
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
     * Equivalente al paragrafo APERTURA-CONTO.
     */
    private void aperturaConto() {
        System.out.println("\n=== APERTURA NUOVO CONTO ===");
        System.out.print("Codice cliente: ");
        wsConCliente = scanner.nextLine();

        String sqlCheckCliente = "SELECT codice_cliente, nome, cognome FROM CLIENTI WHERE codice_cliente = ?";
        try (PreparedStatement pstmt = connection.prepareStatement(sqlCheckCliente)) {
            pstmt.setString(1, wsConCliente);
            ResultSet rs = pstmt.executeQuery();

            if (!rs.next()) {
                System.out.println("Cliente non trovato!");
                return;
            }
            wsCliCodice = rs.getString("codice_cliente");
            wsCliNome = rs.getString("nome");
            wsCliCognome = rs.getString("cognome");
            System.out.println("Cliente: " + wsCliNome + " " + wsCliCognome);

        } catch (SQLException e) {
            handleSqlException(e, "Errore database durante la verifica del cliente.");
            return;
        }

        // Genera nuovo numero conto
        if (!generaNumeroConto()) {
            return; // Errore durante la generazione
        }

        System.out.print("Tipo conto (C=Corrente, D=Deposito): ");
        wsConTipo = scanner.nextLine().toUpperCase();
        System.out.print("Importo iniziale: ");
        wsConSaldo = new BigDecimal(scanner.nextLine());
        System.out.print("Fido accordato: ");
        wsConFido = new BigDecimal(scanner.nextLine());

        wsConStato = "A";

        // Inizio transazione
        String sqlInsertConto = "INSERT INTO CONTI (numero_conto, codice_cliente, tipo_conto, saldo, data_apertura, stato, fido) VALUES (?, ?, ?, ?, CURRENT_DATE, ?, ?)";
        try (PreparedStatement pstmtInsert = connection.prepareStatement(sqlInsertConto)) {
            pstmtInsert.setString(1, wsConNumero);
            pstmtInsert.setString(2, wsConCliente);
            pstmtInsert.setString(3, wsConTipo);
            pstmtInsert.setBigDecimal(4, wsConSaldo);
            pstmtInsert.setString(5, wsConStato);
            pstmtInsert.setBigDecimal(6, wsConFido);
            
            int rowsAffected = pstmtInsert.executeUpdate();

            if (rowsAffected > 0) {
                System.out.println("Conto " + wsConNumero + " creato con successo!");
                // Registra movimento iniziale se c'è un saldo
                if (wsConSaldo.compareTo(BigDecimal.ZERO) > 0) {
                    wsTipoMovimento = "D";
                    wsCausale = "Deposito iniziale";
                    wsImporto = wsConSaldo;
                    wsNumeroConto = wsConNumero;
                    registraMovimento(); // Questo metodo non committa/rollbacka
                }
                connection.commit(); // Conferma la transazione
            } else {
                throw new SQLException("Creazione conto fallita, nessuna riga modificata.");
            }

        } catch (SQLException e) {
            handleSqlException(e, "Errore creazione conto.");
            rollbackTransaction();
        }
    }

    /**
     * Genera un nuovo numero di conto sequenziale.
     * Equivalente al paragrafo GENERA-NUMERO-CONTO.
     * @return true se la generazione ha successo, altrimenti false.
     */
    private boolean generaNumeroConto() {
        String sql = "SELECT 'IT' || LPAD(CAST(COALESCE(MAX(CAST(SUBSTR(numero_conto, 3) AS INTEGER)), 0) + 1 AS VARCHAR), 10, '0') FROM CONTI WHERE numero_conto LIKE 'IT%'";
        try (PreparedStatement pstmt = connection.prepareStatement(sql)) {
            ResultSet rs = pstmt.executeQuery();
            if (rs.next()) {
                wsConNumero = rs.getString(1);
                return true;
            }
            return false;
        } catch (SQLException e) {
            handleSqlException(e, "Errore durante la generazione del numero conto.");
            return false;
        }
    }

    /**
     * Gestisce un'operazione di deposito.
     * Equivalente al paragrafo DEPOSITO.
     */
    private void deposito() {
        System.out.println("\n=== DEPOSITO ===");
        System.out.print("Numero conto: ");
        wsNumeroConto = scanner.nextLine();

        if (!verificaConto()) {
            return;
        }

        System.out.print("Importo deposito: ");
        try {
            wsImporto = new BigDecimal(scanner.nextLine());
        } catch (NumberFormatException e) {
            System.out.println("Importo non valido!");
            return;
        }

        if (wsImporto.compareTo(BigDecimal.ZERO) <= 0) {
            System.out.println("L'importo deve essere positivo!");
            return;
        }

        System.out.print("Causale: ");
        wsCausale = scanner.nextLine();

        String sqlUpdate = "UPDATE CONTI SET saldo = saldo + ? WHERE numero_conto = ? AND stato = 'A'";
        try (PreparedStatement pstmt = connection.prepareStatement(sqlUpdate)) {
            pstmt.setBigDecimal(1, wsImporto);
            pstmt.setString(2, wsNumeroConto);

            int rowsAffected = pstmt.executeUpdate();
            if (rowsAffected > 0) {
                wsTipoMovimento = "D";
                registraMovimento();
                connection.commit();
                System.out.println("Deposito effettuato con successo!");
            } else {
                throw new SQLException("Deposito fallito, conto non trovato o non attivo.");
            }
        } catch (SQLException e) {
            handleSqlException(e, "Errore durante il deposito.");
            rollbackTransaction();
        }
    }

    /**
     * Gestisce un'operazione di prelievo.
     * Equivalente al paragrafo PRELIEVO.
     */
    private void prelievo() {
        System.out.println("\n=== PRELIEVO ===");
        System.out.print("Numero conto: ");
        wsNumeroConto = scanner.nextLine();

        // Verifica preliminare per ottenere saldo e fido
        String sqlCheck = "SELECT saldo, fido, stato FROM CONTI WHERE numero_conto = ?";
        try (PreparedStatement pstmtCheck = connection.prepareStatement(sqlCheck)) {
            pstmtCheck.setString(1, wsNumeroConto);
            ResultSet rs = pstmtCheck.executeQuery();
            if (!rs.next()) {
                System.out.println("Conto non trovato!");
                return;
            }
            wsConSaldo = rs.getBigDecimal("saldo");
            wsConFido = rs.getBigDecimal("fido");
            if (!"A".equals(rs.getString("stato"))) {
                System.out.println("Conto non attivo!");
                return;
            }
        } catch (SQLException e) {
            handleSqlException(e, "Errore durante la verifica del conto.");
            return;
        }

        System.out.print("Importo prelievo: ");
        try {
            wsImporto = new BigDecimal(scanner.nextLine());
        } catch (NumberFormatException e) {
            System.out.println("Importo non valido!");
            return;
        }

        if (wsImporto.compareTo(BigDecimal.ZERO) <= 0) {
            System.out.println("L'importo deve essere positivo!");
            return;
        }

        // Verifica disponibilità: saldo - importo >= -fido
        if (wsConSaldo.subtract(wsImporto).compareTo(wsConFido.negate()) < 0) {
            System.out.println("Fondi insufficienti!");
            System.out.println("Saldo attuale: " + currencyFormatter.format(wsConSaldo));
            System.out.println("Fido disponibile: " + currencyFormatter.format(wsConFido));
            return;
        }

        System.out.print("Causale: ");
        wsCausale = scanner.nextLine();

        String sqlUpdate = "UPDATE CONTI SET saldo = saldo - ? WHERE numero_conto = ? AND stato = 'A'";
        try (PreparedStatement pstmtUpdate = connection.prepareStatement(sqlUpdate)) {
            pstmtUpdate.setBigDecimal(1, wsImporto);
            pstmtUpdate.setString(2, wsNumeroConto);

            int rowsAffected = pstmtUpdate.executeUpdate();
            if (rowsAffected > 0) {
                wsTipoMovimento = "P";
                registraMovimento();
                connection.commit();
                System.out.println("Prelievo effettuato con successo!");
            } else {
                throw new SQLException("Prelievo fallito, conto non trovato o non attivo.");
            }
        } catch (SQLException e) {
            handleSqlException(e, "Errore durante il prelievo.");
            rollbackTransaction();
        }
    }

    /**
     * Mostra il saldo e altre informazioni del conto.
     * Equivalente al paragrafo VISUALIZZA-SALDO.
     */
    private void visualizzaSaldo() {
        System.out.println("\n=== VISUALIZZA SALDO ===");
        System.out.print("Numero conto: ");
        wsNumeroConto = scanner.nextLine();

        String sql = "SELECT c.saldo, c.fido, cl.nome, cl.cognome FROM CONTI c JOIN CLIENTI cl ON c.codice_cliente = cl.codice_cliente WHERE c.numero_conto = ? AND c.stato = 'A'";
        try (PreparedStatement pstmt = connection.prepareStatement(sql)) {
            pstmt.setString(1, wsNumeroConto);
            ResultSet rs = pstmt.executeQuery();

            if (rs.next()) {
                wsConSaldo = rs.getBigDecimal("saldo");
                wsConFido = rs.getBigDecimal("fido");
                wsCliNome = rs.getString("nome");
                wsCliCognome = rs.getString("cognome");

                BigDecimal disponibile = wsConSaldo.add(wsConFido);

                System.out.println("\nIntestatario: " + wsCliNome + " " + wsCliCognome);
                System.out.println("Saldo attuale: " + currencyFormatter.format(wsConSaldo));
                System.out.println("Fido accordato: " + currencyFormatter.format(wsConFido));
                System.out.println("Disponibile: " + currencyFormatter.format(disponibile));
            } else {
                System.out.println("Conto non trovato o non attivo!");
            }
        } catch (SQLException e) {
            handleSqlException(e, "Errore database durante la visualizzazione del saldo.");
        }
    }

    /**
     * Genera un file di testo con l'estratto conto.
     * Equivalente al paragrafo ESTRATTO-CONTO.
     */
    private void estrattoConto() {
        System.out.println("\n=== ESTRATTO CONTO ===");
        System.out.print("Numero conto: ");
        wsNumeroConto = scanner.nextLine();

        if (!verificaConto()) {
            return;
        }

        String fileName = "ESTRATTO-CONTO.TXT";
        try (PrintWriter writer = new PrintWriter(new FileWriter(fileName))) {
            // Intestazione report
            writer.println(String.format("%-50s%s", "", "ESTRATTO CONTO BANCARIO"));
            writer.println("-".repeat(132));
            writer.println("Conto: " + wsNumeroConto + "    Cliente: " + wsCliNome + " " + wsCliCognome);
            writer.println("Data: " + LocalDate.now().format(DateTimeFormatter.ISO_LOCAL_DATE) + "    Ora: " + java.time.LocalTime.now().format(DateTimeFormatter.ofPattern("HH:mm:ss")));
            writer.println("-".repeat(132));
            writer.println(String.format("%-20s %-5s %15s   %-50s %15s", "DATA/ORA", "TIPO", "IMPORTO", "CAUSALE", "SALDO DOPO"));
            writer.println("-".repeat(132));

            // Cursore per i movimenti
            String sqlCursor = "SELECT data_movimento, tipo_movimento, importo, causale, saldo_dopo FROM MOVIMENTI WHERE numero_conto = ? ORDER BY data_movimento DESC";
            try (PreparedStatement pstmt = connection.prepareStatement(sqlCursor)) {
                pstmt.setString(1, wsNumeroConto);
                ResultSet rs = pstmt.executeQuery();

                while (rs.next()) {
                    scriviMovimentoReport(writer, rs);
                }
            }

            writer.println("-".repeat(132));
            writer.println("SALDO FINALE: " + currencyFormatter.format(wsConSaldo));

            System.out.println("Estratto conto salvato in " + fileName);

        } catch (IOException e) {
            System.err.println("Errore durante la scrittura del file di report: " + e.getMessage());
        } catch (SQLException e) {
            handleSqlException(e, "Errore durante il recupero dei movimenti.");
        }
    }

    /**
     * Scrive una singola riga di movimento nel file di report.
     * Equivalente al paragrafo SCRIVI-MOVIMENTO-REPORT.
     * @param writer Il PrintWriter per scrivere sul file.
     * @param rs Il ResultSet contenente i dati del movimento.
     * @throws SQLException Se c'è un errore leggendo dal ResultSet.
     */
    private void scriviMovimentoReport(PrintWriter writer, ResultSet rs) throws SQLException {
        Timestamp dataMov = rs.getTimestamp("data_movimento");
        String tipoMov = rs.getString("tipo_movimento");
        BigDecimal importoMov = rs.getBigDecimal("importo");
        String causaleMov = rs.getString("causale");
        BigDecimal saldoDopoMov = rs.getBigDecimal("saldo_dopo");

        String tipoDesc;
        switch (tipoMov) {
            case "D": tipoDesc = "DEP"; break;
            case "P": tipoDesc = "PRE"; break;
            case "B": tipoDesc = "BON"; break;
            default: tipoDesc = "???"; break;
        }

        String dataFormatted = dataMov.toLocalDateTime().format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"));
        String importoFormatted = currencyFormatter.format(importoMov);
        String saldoDopoFormatted = currencyFormatter.format(saldoDopoMov);
        
        // Tronca la causale se troppo lunga per il layout
        if (causaleMov.length() > 50) {
            causaleMov = causaleMov.substring(0, 47) + "...";
        }

        writer.println(String.format("%-20s %-5s %15s   %-50s %15s",
                dataFormatted, tipoDesc, importoFormatted, causaleMov, saldoDopoFormatted));
    }

    /**
     * Gestisce la chiusura di un conto.
     * Equivalente al paragrafo CHIUSURA-CONTO.
     */
    private void chiusuraConto() {
        System.out.println("\n=== CHIUSURA CONTO ===");
        System.out.print("Numero conto da chiudere: ");
        wsNumeroConto = scanner.nextLine();

        if (!verificaConto()) {
            return;
        }

        // Verifica saldo zero
        if (wsConSaldo.compareTo(BigDecimal.ZERO) != 0) {
            System.out.println("Impossibile chiudere: saldo non a zero!");
            System.out.println("Saldo attuale: " + currencyFormatter.format(wsConSaldo));
            return;
        }

        System.out.print("Confermare chiusura conto (S/N): ");
        String conferma = scanner.nextLine().toUpperCase();

        if ("S".equals(conferma)) {
            String sqlUpdate = "UPDATE CONTI SET stato = 'C', data_chiusura = CURRENT_DATE WHERE numero_conto = ?";
            try (PreparedStatement pstmt = connection.prepareStatement(sqlUpdate)) {
                pstmt.setString(1, wsNumeroConto);
                int rowsAffected = pstmt.executeUpdate();
                if (rowsAffected > 0) {
                    connection.commit();
                    System.out.println("Conto chiuso con successo!");
                } else {
                    throw new SQLException("Chiusura conto fallita.");
                }
            } catch (SQLException e) {
                handleSqlException(e, "Errore chiusura conto.");
                rollbackTransaction();
            }
        } else {
            System.out.println("Chiusura annullata.");
        }
    }

    /**
     * Verifica l'esistenza e lo stato di un conto.
     * Equivalente al paragrafo VERIFICA-CONTO.
     * @return true se il conto è valido e attivo, altrimenti false.
     */
    private boolean verificaConto() {
        String sql = "SELECT c.saldo, c.stato, cl.nome, cl.cognome FROM CONTI c JOIN CLIENTI cl ON c.codice_cliente = cl.codice_cliente WHERE c.numero_conto = ?";
        try (PreparedStatement pstmt = connection.prepareStatement(sql)) {
            pstmt.setString(1, wsNumeroConto);
            ResultSet rs = pstmt.executeQuery();

            if (!rs.next()) {
                System.out.println("Conto non trovato!");
                return false;
            }

            wsConSaldo = rs.getBigDecimal("saldo");
            wsConStato = rs.getString("stato");
            wsCliNome = rs.getString("nome");
            wsCliCognome = rs.getString("cognome");

            if (!"A".equals(wsConStato)) {
                System.out.println("Conto non attivo! (Stato: " + wsConStato + ")");
                return false;
            }
            return true;

        } catch (SQLException e) {
            handleSqlException(e, "Errore database durante la verifica del conto.");
            return false;
        }
    }

    /**
     * Registra un movimento nella tabella MOVIMENTI.
     * Equivalente al paragrafo REGISTRA-MOVIMENTO.
     * Questo metodo è chiamato all'interno di una transazione più grande,
     * quindi non gestisce commit o rollback.
     * @throws SQLException se l'inserimento fallisce.
     */
    private void registraMovimento() throws SQLException {
        // Recupera il saldo aggiornato
        String sqlSelectSaldo = "SELECT saldo FROM CONTI WHERE numero_conto = ?";
        try (PreparedStatement pstmtSelect = connection.prepareStatement(sqlSelectSaldo)) {
            pstmtSelect.setString(1, wsNumeroConto);
            ResultSet rs = pstmtSelect.executeQuery();
            if (rs.next()) {
                wsSaldo = rs.getBigDecimal("saldo");
            } else {
                throw new SQLException("Impossibile recuperare il saldo aggiornato per il conto " + wsNumeroConto);
            }
        }

        // Inserisce il movimento
        String sqlInsert = "INSERT INTO MOVIMENTI (numero_conto, tipo_movimento, importo, causale, saldo_dopo, eseguito_da) VALUES (?, ?, ?, ?, ?, 'SISTEMA')";
        try (PreparedStatement pstmtInsert = connection.prepareStatement(sqlInsert)) {
            pstmtInsert.setString(1, wsNumeroConto);
            pstmtInsert.setString(2, wsTipoMovimento);
            pstmtInsert.setBigDecimal(3, wsImporto);
            pstmtInsert.setString(4, wsCausale);
            pstmtInsert.setBigDecimal(5, wsSaldo);
            pstmtInsert.executeUpdate();
        }
    }

    /**
     * Gestisce le eccezioni SQL, simulando il comportamento di SQLCODE.
     * @param e L'eccezione SQL catturata.
     * @param customMessage Un messaggio descrittivo del contesto dell'errore.
     */
    private void handleSqlException(SQLException e, String customMessage) {
        // SQLState "02000" corrisponde a "no data found", equivalente a SQLCODE 100
        if ("02000".equals(e.getSQLState())) {
            this.sqlcode = 100;
            System.err.println(customMessage + " (Nessun dato trovato)");
        } else {
            this.sqlcode = e.getErrorCode();
            System.err.println(customMessage);
            System.err.println("SQL Error: " + e.getErrorCode() + ", SQLState: " + e.getSQLState());
            System.err.println("Message: " + e.getMessage());
        }
    }

    /**
     * Esegue il rollback della transazione corrente in caso di errore.
     */
    private void rollbackTransaction() {
        if (connection != null) {
            try {
                connection.rollback();
                System.err.println("Transazione annullata (rollback).");
            } catch (SQLException ex) {
                System.err.println("Errore critico durante il rollback della transazione: " + ex.getMessage());
            }
        }
    }
}