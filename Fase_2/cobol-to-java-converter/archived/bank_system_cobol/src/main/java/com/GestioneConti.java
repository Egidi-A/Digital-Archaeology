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
import java.sql.Types;
import java.text.DecimalFormat;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.Scanner;

/**
 * Traduzione Java del programma COBOL GESTIONE-CONTI.
 * Questo programma gestisce le operazioni base di un conto corrente bancario,
 * interfacciandosi con un database PostgreSQL tramite JDBC.
 *
 * @author ANNALISA-EGIDI (COBOL), Tradotto da Compilatore Avanzato (Java)
 * @version 2025-05-20
 */
public class GestioneConti {

    // --- Configurazione Database ---
    private static final String DB_URL = "jdbc:postgresql://localhost:5432/banca";
    private static final String DB_USER = "postgres";
    private static final String DB_PASSWORD = "password";

    // --- Connessione e stato SQL ---
    private Connection connection;
    private int sqlcode; // Simulazione di SQLCODE COBOL

    // --- Strumenti di I/O e formattazione ---
    private final Scanner scanner;
    private final DecimalFormat currencyFormatter;

    // --- Variabili di stato (da WORKING-STORAGE) ---
    private int wsScelta = 0;
    private String wsContinua = "S";
    private String wsEsito = "";

    // --- Variabili per operazioni (Host Variables) ---
    private String wsNumeroConto = "";
    private String wsCodiceCliente = "";
    private BigDecimal wsImporto = BigDecimal.ZERO;
    private BigDecimal wsSaldo = BigDecimal.ZERO;
    private String wsTipoMovimento = "";
    private String wsCausale = "";

    // --- Strutture dati per Cliente (da WS-CLIENTE) ---
    private String wsCliCodice = "";
    private String wsCliNome = "";
    private String wsCliCognome = "";
    // Altri campi cliente non usati attivamente nella logica ma presenti in COBOL
    // private String wsCliCf = "";
    // ...

    // --- Strutture dati per Conto (da WS-CONTO) ---
    private String wsConNumero = "";
    private String wsConCliente = "";
    private String wsConTipo = "";
    private BigDecimal wsConSaldo = BigDecimal.ZERO;
    private String wsConDataApertura = "";
    private String wsConStato = "";
    private BigDecimal wsConFido = BigDecimal.ZERO;

    /**
     * Costruttore della classe. Inizializza lo scanner e il formattatore di valuta.
     */
    public GestioneConti() {
        this.scanner = new Scanner(System.in);
        // Formato corrispondente a PIC Z,ZZZ,ZZZ,ZZ9.99-
        this.currencyFormatter = new DecimalFormat("€ #,##0.00;€ -#,##0.00");
    }

    /**
     * Metodo main, punto di ingresso dell'applicazione.
     * @param args Argomenti della riga di comando (non usati).
     */
    public static void main(String[] args) {
        GestioneConti applicazione = new GestioneConti();
        applicazione.mainLogic();
    }

    /**
     * Logica principale del programma, corrisponde al paragrafo MAIN-LOGIC.
     */
    public void mainLogic() {
        connectDatabase();
        if (connection == null) {
            return; // Interrompe l'esecuzione se la connessione fallisce
        }

        do {
            visualizzaMenu();
            try {
                wsScelta = Integer.parseInt(scanner.nextLine());
                elaboraScelta();
            } catch (NumberFormatException e) {
                System.out.println("Input non valido. Inserire un numero.");
                wsScelta = -1; // Valore non valido per evitare di uscire
            }

            if (wsScelta != 0) {
                System.out.println();
                System.out.print("Continuare? (S/N): ");
                wsContinua = scanner.nextLine();
            } else {
                wsContinua = "N"; // Uscita dal programma
            }

        } while (wsContinua.equalsIgnoreCase("S"));

        disconnectDatabase();
        scanner.close();
        System.out.println("Programma terminato.");
    }

    /**
     * Stabilisce la connessione al database. Corrisponde a CONNETTI-DATABASE.
     */
    private void connectDatabase() {
        try {
            connection = DriverManager.getConnection(DB_URL, DB_USER, DB_PASSWORD);
            sqlcode = 0;
            System.out.println("Connessione al database stabilita.");
        } catch (SQLException e) {
            handleSqlException(e, "Errore connessione database: ");
            connection = null; // Assicura che la connessione sia null in caso di fallimento
        }
    }

    /**
     * Chiude la connessione al database. Corrisponde a DISCONNETTI-DATABASE.
     */
    private void disconnectDatabase() {
        if (connection != null) {
            try {
                connection.close();
                System.out.println("Disconnesso dal database.");
            } catch (SQLException e) {
                handleSqlException(e, "Errore durante la disconnessione: ");
            }
        }
    }

    /**
     * Mostra il menu delle opzioni. Corrisponde a VISUALIZZA-MENU.
     */
    private void visualizzaMenu() {
        System.out.println();
        System.out.println("===== SISTEMA GESTIONE CONTI CORRENTI =====");
        System.out.println("1. Apertura nuovo conto");
        System.out.println("2. Deposito");
        System.out.println("3. Prelievo");
        System.out.println("4. Visualizza saldo");
        System.out.println("5. Estratto conto");
        System.out.println("6. Chiusura conto");
        System.out.println("0. Esci");
        System.out.println("===========================================");
        System.out.print("Scelta: ");
    }

    /**
     * Esegue l'azione scelta dall'utente. Corrisponde a ELABORA-SCELTA.
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
     * Gestisce l'apertura di un nuovo conto. Corrisponde a APERTURA-CONTO.
     * Questa operazione è transazionale.
     */
    private void aperturaConto() {
        System.out.println("\n=== APERTURA NUOVO CONTO ===");
        System.out.print("Codice cliente: ");
        wsConCliente = scanner.nextLine().trim();

        // Verifica esistenza cliente
        String sqlCheckCliente = "SELECT codice_cliente, nome, cognome FROM CLIENTI WHERE codice_cliente = ?";
        try (PreparedStatement ps = connection.prepareStatement(sqlCheckCliente)) {
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
                    return;
                }
            }
        } catch (SQLException e) {
            handleSqlException(e, "Errore database: ");
            return;
        }

        // Inizio transazione
        try {
            connection.setAutoCommit(false);

            // Genera nuovo numero conto
            generaNumeroConto();
            if (sqlcode != 0) throw new SQLException("Impossibile generare il numero di conto.");

            System.out.print("Tipo conto (C=Corrente, D=Deposito): ");
            wsConTipo = scanner.nextLine().trim().toUpperCase();
            System.out.print("Importo iniziale: ");
            wsConSaldo = readBigDecimalFromUser();
            System.out.print("Fido accordato: ");
            wsConFido = readBigDecimalFromUser();

            wsConDataApertura = LocalDate.now().toString();
            wsConStato = "A";

            // Inserisci nuovo conto
            String sqlInsertConto = "INSERT INTO CONTI (numero_conto, codice_cliente, tipo_conto, saldo, data_apertura, stato, fido) VALUES (?, ?, ?, ?, CURRENT_DATE, ?, ?)";
            try (PreparedStatement ps = connection.prepareStatement(sqlInsertConto)) {
                ps.setString(1, wsConNumero);
                ps.setString(2, wsConCliente);
                ps.setString(3, wsConTipo);
                ps.setBigDecimal(4, wsConSaldo);
                ps.setString(5, wsConStato);
                ps.setBigDecimal(6, wsConFido);
                int rowsAffected = ps.executeUpdate();
                if (rowsAffected > 0) {
                    sqlcode = 0;
                    System.out.println("Conto " + wsConNumero + " creato con successo!");

                    // Registra movimento iniziale se c'è un saldo
                    if (wsConSaldo.compareTo(BigDecimal.ZERO) > 0) {
                        wsTipoMovimento = "D";
                        wsCausale = "Deposito iniziale";
                        wsImporto = wsConSaldo;
                        wsNumeroConto = wsConNumero;
                        registraMovimento(); // Questo metodo fa parte della transazione
                        if (sqlcode != 0) throw new SQLException("Errore registrazione movimento iniziale.");
                    }
                } else {
                    throw new SQLException("Creazione conto fallita, nessuna riga inserita.");
                }
            }
            
            connection.commit(); // Conferma la transazione

        } catch (SQLException e) {
            handleSqlException(e, "Errore creazione conto: ");
            rollbackTransaction();
        } finally {
            resetAutoCommit();
        }
    }

    /**
     * Genera un nuovo numero di conto progressivo. Corrisponde a GENERA-NUMERO-CONTO.
     */
    private void generaNumeroConto() {
        String sql = "SELECT 'IT' || LPAD(CAST(COALESCE(MAX(CAST(SUBSTR(numero_conto, 3) AS INTEGER)), 0) + 1 AS VARCHAR), 10, '0') FROM CONTI WHERE numero_conto LIKE 'IT%'";
        try (PreparedStatement ps = connection.prepareStatement(sql);
             ResultSet rs = ps.executeQuery()) {
            if (rs.next()) {
                sqlcode = 0;
                wsConNumero = rs.getString(1);
            } else {
                // Questo caso non dovrebbe mai accadere con questa query
                throw new SQLException("Query per la generazione del numero conto non ha prodotto risultati.");
            }
        } catch (SQLException e) {
            handleSqlException(e, "Errore generazione numero conto: ");
        }
    }

    /**
     * Gestisce un'operazione di deposito. Corrisponde a DEPOSITO.
     * Operazione transazionale.
     */
    private void deposito() {
        System.out.println("\n=== DEPOSITO ===");
        System.out.print("Numero conto: ");
        wsNumeroConto = scanner.nextLine().trim();

        if (!verificaConto(wsNumeroConto)) {
            return;
        }

        System.out.print("Importo deposito: ");
        wsImporto = readBigDecimalFromUser();

        if (wsImporto.compareTo(BigDecimal.ZERO) <= 0) {
            System.out.println("Importo non valido!");
            return;
        }

        System.out.print("Causale: ");
        wsCausale = scanner.nextLine().trim();

        try {
            connection.setAutoCommit(false);

            // Aggiorna saldo
            String sqlUpdate = "UPDATE CONTI SET saldo = saldo + ? WHERE numero_conto = ? AND stato = 'A'";
            try (PreparedStatement ps = connection.prepareStatement(sqlUpdate)) {
                ps.setBigDecimal(1, wsImporto);
                ps.setString(2, wsNumeroConto);
                int rowsAffected = ps.executeUpdate();
                if (rowsAffected > 0) {
                    sqlcode = 0;
                    // Registra movimento
                    wsTipoMovimento = "D";
                    registraMovimento();
                    if (sqlcode != 0) throw new SQLException("Errore registrazione movimento.");
                    System.out.println("Deposito effettuato con successo!");
                } else {
                    throw new SQLException("Deposito fallito, conto non trovato o non attivo.");
                }
            }
            
            connection.commit();

        } catch (SQLException e) {
            handleSqlException(e, "Errore durante il deposito: ");
            rollbackTransaction();
        } finally {
            resetAutoCommit();
        }
    }

    /**
     * Gestisce un'operazione di prelievo. Corrisponde a PRELIEVO.
     * Operazione transazionale.
     */
    private void prelievo() {
        System.out.println("\n=== PRELIEVO ===");
        System.out.print("Numero conto: ");
        wsNumeroConto = scanner.nextLine().trim();

        if (!verificaConto(wsNumeroConto)) {
            return;
        }

        System.out.print("Importo prelievo: ");
        wsImporto = readBigDecimalFromUser();

        if (wsImporto.compareTo(BigDecimal.ZERO) <= 0) {
            System.out.println("Importo non valido!");
            return;
        }

        try {
            connection.setAutoCommit(false);

            // Verifica disponibilità (con blocco per aggiornamento)
            String sqlCheck = "SELECT saldo, fido FROM CONTI WHERE numero_conto = ? AND stato = 'A' FOR UPDATE";
            try (PreparedStatement psCheck = connection.prepareStatement(sqlCheck)) {
                psCheck.setString(1, wsNumeroConto);
                try (ResultSet rs = psCheck.executeQuery()) {
                    if (rs.next()) {
                        wsConSaldo = rs.getBigDecimal("saldo");
                        wsConFido = rs.getBigDecimal("fido");
                    } else {
                        throw new SQLException("Conto non trovato o non attivo per la verifica disponibilità.");
                    }
                }
            }

            // Calcolo disponibilità: saldo - importo >= -fido
            if (wsConSaldo.subtract(wsImporto).compareTo(wsConFido.negate()) < 0) {
                System.out.println("Fondi insufficienti!");
                System.out.println("Saldo attuale: " + formatCurrency(wsConSaldo));
                System.out.println("Fido disponibile: " + formatCurrency(wsConFido));
                connection.rollback(); // Rilascia il lock
                return;
            }

            System.out.print("Causale: ");
            wsCausale = scanner.nextLine().trim();

            // Aggiorna saldo
            String sqlUpdate = "UPDATE CONTI SET saldo = saldo - ? WHERE numero_conto = ? AND stato = 'A'";
            try (PreparedStatement psUpdate = connection.prepareStatement(sqlUpdate)) {
                psUpdate.setBigDecimal(1, wsImporto);
                psUpdate.setString(2, wsNumeroConto);
                int rowsAffected = psUpdate.executeUpdate();
                if (rowsAffected > 0) {
                    sqlcode = 0;
                    // Registra movimento
                    wsTipoMovimento = "P";
                    registraMovimento();
                    if (sqlcode != 0) throw new SQLException("Errore registrazione movimento.");
                    System.out.println("Prelievo effettuato con successo!");
                } else {
                    throw new SQLException("Prelievo fallito, il conto potrebbe essere stato modificato.");
                }
            }
            
            connection.commit();

        } catch (SQLException e) {
            handleSqlException(e, "Errore durante il prelievo: ");
            rollbackTransaction();
        } finally {
            resetAutoCommit();
        }
    }

    /**
     * Mostra il saldo e la disponibilità di un conto. Corrisponde a VISUALIZZA-SALDO.
     */
    private void visualizzaSaldo() {
        System.out.println("\n=== VISUALIZZA SALDO ===");
        System.out.print("Numero conto: ");
        wsNumeroConto = scanner.nextLine().trim();

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

                    System.out.println();
                    System.out.println("Intestatario: " + wsCliNome + " " + wsCliCognome);
                    System.out.println("Saldo attuale: " + formatCurrency(wsConSaldo));
                    System.out.println("Fido accordato: " + formatCurrency(wsConFido));
                    BigDecimal disponibile = wsConSaldo.add(wsConFido);
                    System.out.println("Disponibile: " + formatCurrency(disponibile));
                } else {
                    sqlcode = 100;
                    System.out.println("Conto non trovato o non attivo!");
                }
            }
        } catch (SQLException e) {
            handleSqlException(e, "Errore database: ");
        }
    }

    /**
     * Genera un file di testo con l'estratto conto. Corrisponde a ESTRATTO-CONTO.
     */
    private void estrattoConto() {
        System.out.println("\n=== ESTRATTO CONTO ===");
        System.out.print("Numero conto: ");
        wsNumeroConto = scanner.nextLine().trim();

        if (!verificaConto(wsNumeroConto)) {
            return;
        }

        String reportFileName = "ESTRATTO-CONTO.TXT";
        String sqlCursor = "SELECT data_movimento, tipo_movimento, importo, causale, saldo_dopo FROM MOVIMENTI WHERE numero_conto = ? ORDER BY data_movimento DESC";

        try (PrintWriter writer = new PrintWriter(new FileWriter(reportFileName));
             PreparedStatement ps = connection.prepareStatement(sqlCursor)) {

            // Intestazione report
            writer.println(" ".repeat(50) + "ESTRATTO CONTO BANCARIO");
            writer.println("-".repeat(132));
            writer.println("Conto: " + wsNumeroConto + "    Cliente: " + wsCliNome + " " + wsCliCognome);
            writer.println("Data: " + LocalDate.now().format(DateTimeFormatter.ISO_LOCAL_DATE) + "    Ora: " + java.time.LocalTime.now().format(DateTimeFormatter.ofPattern("HH:mm:ss")));
            writer.println("-".repeat(132));
            writer.println(String.format("%-20s %-5s %15s   %-50s %15s", "DATA/ORA", "TIPO", "IMPORTO", "CAUSALE", "SALDO DOPO"));
            writer.println("-".repeat(132));

            // Esecuzione cursore
            ps.setString(1, wsNumeroConto);
            try (ResultSet rs = ps.executeQuery()) {
                sqlcode = 100; // Assume no records found initially
                while (rs.next()) {
                    sqlcode = 0; // Record found
                    Timestamp movDataOra = rs.getTimestamp("data_movimento");
                    String movTipo = rs.getString("tipo_movimento");
                    BigDecimal movImporto = rs.getBigDecimal("importo");
                    String movCausale = rs.getString("causale");
                    BigDecimal movSaldoDopo = rs.getBigDecimal("saldo_dopo");
                    
                    scriviMovimentoReport(writer, movDataOra, movTipo, movImporto, movCausale, movSaldoDopo);
                }
            }

            writer.println("-".repeat(132));
            writer.println("SALDO FINALE: " + formatCurrency(wsConSaldo)); // wsConSaldo è stato popolato da verificaConto

            System.out.println("Estratto conto salvato in " + reportFileName);

        } catch (IOException e) {
            System.err.println("Errore durante la scrittura del file di report: " + e.getMessage());
        } catch (SQLException e) {
            handleSqlException(e, "Errore durante la lettura dei movimenti: ");
        }
    }
    
    /**
     * Scrive una singola riga di movimento nel file di report. Corrisponde a SCRIVI-MOVIMENTO-REPORT.
     */
    private void scriviMovimentoReport(PrintWriter writer, Timestamp data, String tipo, BigDecimal importo, String causale, BigDecimal saldoDopo) {
        String tipoDesc;
        switch (tipo) {
            case "D": tipoDesc = "DEP"; break;
            case "P": tipoDesc = "PRE"; break;
            case "B": tipoDesc = "BON"; break;
            default: tipoDesc = "???"; break;
        }

        String dataFormatted = data.toLocalDateTime().format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"));
        String importoFormatted = formatCurrency(importo);
        String saldoDopoFormatted = formatCurrency(saldoDopo);
        
        // Tronca la causale se troppo lunga per il layout
        String causaleFormatted = (causale != null && causale.length() > 50) ? causale.substring(0, 50) : (causale == null ? "" : causale);

        writer.println(String.format("%-20s %-5s %15s   %-50s %15s", 
            dataFormatted, tipoDesc, importoFormatted, causaleFormatted, saldoDopoFormatted));
    }


    /**
     * Gestisce la chiusura di un conto. Corrisponde a CHIUSURA-CONTO.
     */
    private void chiusuraConto() {
        System.out.println("\n=== CHIUSURA CONTO ===");
        System.out.print("Numero conto da chiudere: ");
        wsNumeroConto = scanner.nextLine().trim();

        if (!verificaConto(wsNumeroConto)) {
            return;
        }

        // Verifica saldo zero
        if (wsConSaldo.compareTo(BigDecimal.ZERO) != 0) {
            System.out.println("Impossibile chiudere: saldo non zero!");
            System.out.println("Saldo attuale: " + formatCurrency(wsConSaldo));
            return;
        }

        System.out.print("Confermare chiusura conto (S/N): ");
        String conferma = scanner.nextLine().trim();

        if (conferma.equalsIgnoreCase("S")) {
            String sql = "UPDATE CONTI SET stato = 'C', data_chiusura = CURRENT_DATE WHERE numero_conto = ?";
            try (PreparedStatement ps = connection.prepareStatement(sql)) {
                ps.setString(1, wsNumeroConto);
                int rowsAffected = ps.executeUpdate();
                if (rowsAffected > 0) {
                    sqlcode = 0;
                    System.out.println("Conto chiuso con successo!");
                } else {
                    sqlcode = 100; // Tecnicamente un errore se il conto non viene trovato qui
                    System.out.println("Errore: il conto non è stato trovato durante l'operazione di chiusura.");
                }
            } catch (SQLException e) {
                handleSqlException(e, "Errore chiusura conto: ");
            }
        } else {
            System.out.println("Chiusura annullata.");
        }
    }

    /**
     * Verifica l'esistenza e lo stato di un conto. Corrisponde a VERIFICA-CONTO.
     * @param numeroConto Il numero di conto da verificare.
     * @return true se il conto è valido e attivo, false altrimenti.
     */
    private boolean verificaConto(String numeroConto) {
        String sql = "SELECT c.saldo, c.stato, cl.nome, cl.cognome FROM CONTI c JOIN CLIENTI cl ON c.codice_cliente = cl.codice_cliente WHERE c.numero_conto = ?";
        try (PreparedStatement ps = connection.prepareStatement(sql)) {
            ps.setString(1, numeroConto);
            try (ResultSet rs = ps.executeQuery()) {
                if (rs.next()) {
                    sqlcode = 0;
                    wsConSaldo = rs.getBigDecimal("saldo");
                    wsConStato = rs.getString("stato");
                    wsCliNome = rs.getString("nome");
                    wsCliCognome = rs.getString("cognome");

                    if (!wsConStato.equals("A")) {
                        System.out.println("Conto non attivo! (Stato: " + wsConStato + ")");
                        wsEsito = "KO";
                        return false;
                    }
                    wsEsito = "OK";
                    return true;
                } else {
                    sqlcode = 100;
                    System.out.println("Conto non trovato!");
                    wsEsito = "KO";
                    return false;
                }
            }
        } catch (SQLException e) {
            handleSqlException(e, "Errore database: ");
            wsEsito = "KO";
            return false;
        }
    }

    /**
     * Inserisce un record nella tabella MOVIMENTI. Corrisponde a REGISTRA-MOVIMENTO.
     * Questo metodo deve essere chiamato all'interno di una transazione esistente.
     */
    private void registraMovimento() throws SQLException {
        String sqlSelectSaldo = "SELECT saldo FROM CONTI WHERE numero_conto = ?";
        String sqlInsertMovimento = "INSERT INTO MOVIMENTI (numero_conto, tipo_movimento, importo, causale, saldo_dopo, eseguito_da) VALUES (?, ?, ?, ?, ?, 'SISTEMA')";
        
        // Ottieni il saldo aggiornato
        try (PreparedStatement psSelect = connection.prepareStatement(sqlSelectSaldo)) {
            psSelect.setString(1, wsNumeroConto);
            try (ResultSet rs = psSelect.executeQuery()) {
                if (rs.next()) {
                    wsSaldo = rs.getBigDecimal("saldo");
                } else {
                    throw new SQLException("Conto non trovato per recuperare il saldo post-operazione.");
                }
            }
        }

        // Inserisci il movimento
        try (PreparedStatement psInsert = connection.prepareStatement(sqlInsertMovimento)) {
            psInsert.setString(1, wsNumeroConto);
            psInsert.setString(2, wsTipoMovimento);
            psInsert.setBigDecimal(3, wsImporto);
            psInsert.setString(4, wsCausale);
            psInsert.setBigDecimal(5, wsSaldo);
            int rowsAffected = psInsert.executeUpdate();
            if (rowsAffected > 0) {
                sqlcode = 0;
            } else {
                throw new SQLException("Inserimento movimento fallito.");
            }
        }
    }

    // --- Metodi Helper ---

    /**
     * Gestisce le eccezioni SQL, simulando SQLCODE.
     * @param e L'eccezione SQL catturata.
     * @param userMessage Il messaggio da mostrare all'utente.
     */
    private void handleSqlException(SQLException e, String userMessage) {
        // SQLState "02000" corrisponde a "no data found", simulando SQLCODE = 100
        if ("02000".equals(e.getSQLState())) {
            sqlcode = 100;
        } else {
            sqlcode = e.getErrorCode(); // Usa il codice di errore del driver
            System.err.println(userMessage + sqlcode);
            e.printStackTrace();
        }
    }

    /**
     * Esegue il rollback di una transazione in modo sicuro.
     */
    private void rollbackTransaction() {
        if (connection != null) {
            try {
                connection.rollback();
                System.out.println("Transazione annullata.");
            } catch (SQLException ex) {
                handleSqlException(ex, "Errore critico durante il rollback: ");
            }
        }
    }

    /**
     * Ripristina la modalità auto-commit a true in modo sicuro.
     */
    private void resetAutoCommit() {
        if (connection != null) {
            try {
                connection.setAutoCommit(true);
            } catch (SQLException ex) {
                handleSqlException(ex, "Errore durante il ripristino dell'auto-commit: ");
            }
        }
    }

    /**
     * Formatta un valore BigDecimal come stringa di valuta.
     * @param amount L'importo da formattare.
     * @return La stringa formattata.
     */
    private String formatCurrency(BigDecimal amount) {
        return currencyFormatter.format(amount);
    }

    /**
     * Legge un valore BigDecimal dall'input dell'utente, gestendo errori.
     * @return Il BigDecimal letto, o BigDecimal.ZERO in caso di errore.
     */
    private BigDecimal readBigDecimalFromUser() {
        try {
            String input = scanner.nextLine().trim().replace(',', '.');
            return new BigDecimal(input).setScale(2, RoundingMode.HALF_UP);
        } catch (NumberFormatException e) {
            System.out.println("Formato importo non valido. Usato 0.00 come default.");
            return BigDecimal.ZERO;
        }
    }
}