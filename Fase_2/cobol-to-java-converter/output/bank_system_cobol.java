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
import java.util.Scanner;

/**
 * Traduzione Java del programma COBOL GESTIONE-CONTI.
 * Questo programma gestisce le operazioni di base di un conto corrente bancario,
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

    // --- Connessione e utility ---
    private Connection connection;
    private final Scanner scanner;
    private final DecimalFormat currencyFormatter;

    // --- Variabili di stato (emulazione WORKING-STORAGE) ---
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

    // --- Strutture dati (emulazione strutture COBOL) ---
    private Cliente wsCliente = new Cliente();
    private Conto wsConto = new Conto();

    // --- Emulazione SQLCODE ---
    private int sqlcode = 0;

    // --- Record per dati strutturati (migliora leggibilità) ---
    private record Cliente(String codice, String nome, String cognome) {
        Cliente() { this("", "", ""); }
    }

    private record Conto(String numero, String cliente, String tipo, BigDecimal saldo,
                         LocalDate dataApertura, String stato, BigDecimal fido) {
        Conto() { this("", "", "", BigDecimal.ZERO, null, "", BigDecimal.ZERO); }
    }

    private record Movimento(Timestamp data, String tipo, BigDecimal importo, String causale, BigDecimal saldoDopo) {
        Movimento() { this(null, "", BigDecimal.ZERO, "", BigDecimal.ZERO); }
    }

    /**
     * Costruttore della classe. Inizializza lo scanner e il formattatore di valuta.
     */
    public GestioneConti() {
        this.scanner = new Scanner(System.in);
        this.currencyFormatter = new DecimalFormat("€ #,##0.00");
    }

    /**
     * Punto di ingresso del programma.
     * @param args Argomenti della riga di comando (non usati).
     */
    public static void main(String[] args) {
        GestioneConti gestore = new GestioneConti();
        gestore.mainLogic();
    }

    /**
     * Logica principale del programma, che emula il paragrafo MAIN-LOGIC del COBOL.
     */
    public void mainLogic() {
        if (!connectDatabase()) {
            return; // Interrompe l'esecuzione se la connessione fallisce
        }

        do {
            visualizzaMenu();
            elaboraScelta();

            if (wsScelta != 0) {
                System.out.print("\nContinuare? (S/N): ");
                wsContinua = scanner.nextLine();
            } else {
                wsContinua = "N"; // Esce dal ciclo se la scelta è 0
            }

        } while (wsContinua.equalsIgnoreCase("S"));

        disconnectDatabase();
        System.out.println("Programma terminato.");
    }

    /**
     * Stabilisce la connessione al database PostgreSQL.
     * Emula il paragrafo CONNETTI-DATABASE.
     * @return true se la connessione ha successo, false altrimenti.
     */
    private boolean connectDatabase() {
        try {
            connection = DriverManager.getConnection(DB_URL, DB_USER, DB_PASSWORD);
            // Imposta l'autocommit a false per gestire le transazioni manualmente
            connection.setAutoCommit(false);
            System.out.println("Connessione al database stabilita.");
            sqlcode = 0;
            return true;
        } catch (SQLException e) {
            System.err.println("Errore connessione database: " + e.getMessage());
            handleSqlException(e);
            return false;
        }
    }

    /**
     * Chiude la connessione al database.
     * Emula il paragrafo DISCONNETTI-DATABASE.
     */
    private void disconnectDatabase() {
        try {
            if (connection != null && !connection.isClosed()) {
                connection.close();
                System.out.println("Disconnesso dal database.");
            }
        } catch (SQLException e) {
            System.err.println("Errore durante la disconnessione: " + e.getMessage());
        } finally {
            scanner.close();
        }
    }

    /**
     * Mostra il menu principale delle operazioni.
     * Emula il paragrafo VISUALIZZA-MENU.
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
     * Emula il paragrafo ELABORA-SCELTA.
     */
    private void elaboraScelta() {
        switch (wsScelta) {
            case 1: aperturaConto(); break;
            case 2: deposito(); break;
            case 3: prelievo(); break;
            case 4: visualizzaSaldo(); break;
            case 5: estrattoConto(); break;
            case 6: chiusuraConto(); break;
            case 0: break; // Gestito nel ciclo principale
            default: System.out.println("Scelta non valida!"); break;
        }
    }

    /**
     * Gestisce l'apertura di un nuovo conto corrente.
     * Emula il paragrafo APERTURA-CONTO.
     */
    private void aperturaConto() {
        System.out.println("\n=== APERTURA NUOVO CONTO ===");
        System.out.print("Codice cliente: ");
        String codiceClienteInput = scanner.nextLine().trim();

        String sqlCliente = "SELECT codice_cliente, nome, cognome FROM CLIENTI WHERE codice_cliente = ?";
        try (PreparedStatement ps = connection.prepareStatement(sqlCliente)) {
            ps.setString(1, codiceClienteInput);
            try (ResultSet rs = ps.executeQuery()) {
                if (!rs.next()) {
                    sqlcode = 100;
                    System.out.println("Cliente non trovato!");
                    return;
                }
                wsCliente = new Cliente(rs.getString("codice_cliente"), rs.getString("nome"), rs.getString("cognome"));
                System.out.println("Cliente: " + wsCliente.nome() + " " + wsCliente.cognome());
            }

            // Genera nuovo numero conto
            wsNumeroConto = generaNumeroConto();
            if (wsNumeroConto == null) {
                // Errore già gestito in generaNumeroConto
                connection.rollback();
                return;
            }

            System.out.print("Tipo conto (C=Corrente, D=Deposito): ");
            String tipoConto = scanner.nextLine().trim().toUpperCase();
            System.out.print("Importo iniziale: ");
            BigDecimal importoIniziale = new BigDecimal(scanner.nextLine().trim());
            System.out.print("Fido accordato: ");
            BigDecimal fido = new BigDecimal(scanner.nextLine().trim());

            String sqlInsertConto = "INSERT INTO CONTI (numero_conto, codice_cliente, tipo_conto, saldo, data_apertura, stato, fido) VALUES (?, ?, ?, ?, CURRENT_DATE, ?, ?)";
            try (PreparedStatement psInsert = connection.prepareStatement(sqlInsertConto)) {
                psInsert.setString(1, wsNumeroConto);
                psInsert.setString(2, wsCliente.codice());
                psInsert.setString(3, tipoConto);
                psInsert.setBigDecimal(4, importoIniziale);
                psInsert.setString(5, "A"); // Stato Attivo
                psInsert.setBigDecimal(6, fido);

                int rowsAffected = psInsert.executeUpdate();
                if (rowsAffected > 0) {
                    sqlcode = 0;
                    System.out.println("Conto " + wsNumeroConto + " creato con successo!");

                    // Registra movimento iniziale se l'importo è positivo
                    if (importoIniziale.compareTo(BigDecimal.ZERO) > 0) {
                        wsTipoMovimento = "D";
                        wsCausale = "Deposito iniziale";
                        wsImporto = importoIniziale;
                        registraMovimento(); // Questo metodo non committa
                    }
                    connection.commit();
                } else {
                    throw new SQLException("Creazione conto fallita, nessuna riga inserita.");
                }
            }
        } catch (SQLException e) {
            handleSqlException(e);
            safeRollback();
        } catch (NumberFormatException e) {
            System.out.println("Errore: importo o fido non valido.");
            safeRollback();
        }
    }

    /**
     * Genera un nuovo numero di conto sequenziale.
     * Emula il paragrafo GENERA-NUMERO-CONTO.
     * @return Il nuovo numero di conto o null in caso di errore.
     */
    private String generaNumeroConto() throws SQLException {
        String sql = "SELECT 'IT' || LPAD(CAST(COALESCE(MAX(CAST(SUBSTR(numero_conto, 3) AS INTEGER)), 0) + 1 AS VARCHAR), 10, '0') FROM CONTI WHERE numero_conto LIKE 'IT%'";
        try (PreparedStatement ps = connection.prepareStatement(sql);
             ResultSet rs = ps.executeQuery()) {
            if (rs.next()) {
                return rs.getString(1);
            }
        }
        // Questo blocco non dovrebbe mai essere raggiunto in una query di aggregazione senza GROUP BY
        throw new SQLException("Impossibile generare un nuovo numero di conto.");
    }

    /**
     * Gestisce un'operazione di deposito.
     * Emula il paragrafo DEPOSITO.
     */
    private void deposito() {
        System.out.println("\n=== DEPOSITO ===");
        System.out.print("Numero conto: ");
        wsNumeroConto = scanner.nextLine().trim();

        if (!verificaConto()) {
            return; // Errore già stampato da verificaConto
        }

        try {
            System.out.print("Importo deposito: ");
            wsImporto = new BigDecimal(scanner.nextLine().trim());

            if (wsImporto.compareTo(BigDecimal.ZERO) <= 0) {
                System.out.println("Importo non valido!");
                return;
            }

            System.out.print("Causale: ");
            wsCausale = scanner.nextLine().trim();

            String sqlUpdate = "UPDATE CONTI SET saldo = saldo + ? WHERE numero_conto = ? AND stato = 'A'";
            try (PreparedStatement ps = connection.prepareStatement(sqlUpdate)) {
                ps.setBigDecimal(1, wsImporto);
                ps.setString(2, wsNumeroConto);

                int rowsAffected = ps.executeUpdate();
                if (rowsAffected > 0) {
                    sqlcode = 0;
                    wsTipoMovimento = "D";
                    registraMovimento();
                    connection.commit();
                    System.out.println("Deposito effettuato con successo!");
                } else {
                    throw new SQLException("Deposito fallito. Conto non trovato o non attivo.");
                }
            }
        } catch (SQLException e) {
            handleSqlException(e);
            safeRollback();
        } catch (NumberFormatException e) {
            System.out.println("Errore: importo non valido.");
            safeRollback();
        }
    }

    /**
     * Gestisce un'operazione di prelievo.
     * Emula il paragrafo PRELIEVO.
     */
    private void prelievo() {
        System.out.println("\n=== PRELIEVO ===");
        System.out.print("Numero conto: ");
        wsNumeroConto = scanner.nextLine().trim();

        if (!verificaConto()) {
            return;
        }

        try {
            System.out.print("Importo prelievo: ");
            wsImporto = new BigDecimal(scanner.nextLine().trim());

            if (wsImporto.compareTo(BigDecimal.ZERO) <= 0) {
                System.out.println("Importo non valido!");
                return;
            }

            // Verifica disponibilità (saldo + fido)
            if (wsConto.saldo().subtract(wsImporto).compareTo(wsConto.fido().negate()) < 0) {
                System.out.println("Fondi insufficienti!");
                System.out.println("Saldo attuale: " + currencyFormatter.format(wsConto.saldo()));
                System.out.println("Fido disponibile: " + currencyFormatter.format(wsConto.fido()));
                return;
            }

            System.out.print("Causale: ");
            wsCausale = scanner.nextLine().trim();

            String sqlUpdate = "UPDATE CONTI SET saldo = saldo - ? WHERE numero_conto = ? AND stato = 'A'";
            try (PreparedStatement ps = connection.prepareStatement(sqlUpdate)) {
                ps.setBigDecimal(1, wsImporto);
                ps.setString(2, wsNumeroConto);

                int rowsAffected = ps.executeUpdate();
                if (rowsAffected > 0) {
                    sqlcode = 0;
                    wsTipoMovimento = "P";
                    registraMovimento();
                    connection.commit();
                    System.out.println("Prelievo effettuato con successo!");
                } else {
                    throw new SQLException("Prelievo fallito. Conto non trovato o non attivo.");
                }
            }
        } catch (SQLException e) {
            handleSqlException(e);
            safeRollback();
        } catch (NumberFormatException e) {
            System.out.println("Errore: importo non valido.");
            safeRollback();
        }
    }

    /**
     * Mostra il saldo e le informazioni principali del conto.
     * Emula il paragrafo VISUALIZZA-SALDO.
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
                    BigDecimal saldo = rs.getBigDecimal("saldo");
                    BigDecimal fido = rs.getBigDecimal("fido");
                    String nome = rs.getString("nome");
                    String cognome = rs.getString("cognome");
                    BigDecimal disponibile = saldo.add(fido);

                    System.out.println("\nIntestatario: " + nome + " " + cognome);
                    System.out.println("Saldo attuale: " + currencyFormatter.format(saldo));
                    System.out.println("Fido accordato: " + currencyFormatter.format(fido));
                    System.out.println("Disponibile: " + currencyFormatter.format(disponibile));
                } else {
                    sqlcode = 100;
                    System.out.println("Conto non trovato o non attivo!");
                }
            }
        } catch (SQLException e) {
            handleSqlException(e);
        }
    }

    /**
     * Genera un file di testo con l'estratto conto.
     * Emula il paragrafo ESTRATTO-CONTO.
     */
    private void estrattoConto() {
        System.out.println("\n=== ESTRATTO CONTO ===");
        System.out.print("Numero conto: ");
        wsNumeroConto = scanner.nextLine().trim();

        if (!verificaConto()) {
            return;
        }

        String fileName = "ESTRATTO-CONTO.TXT";
        String sqlCursor = "SELECT data_movimento, tipo_movimento, importo, causale, saldo_dopo FROM MOVIMENTI WHERE numero_conto = ? ORDER BY data_movimento DESC";

        try (PrintWriter writer = new PrintWriter(new FileWriter(fileName));
             PreparedStatement ps = connection.prepareStatement(sqlCursor)) {

            // Intestazione report
            writer.println(String.format("%66s", "ESTRATTO CONTO BANCARIO"));
            writer.println("-".repeat(132));
            writer.println("Conto: " + wsNumeroConto + "    Cliente: " + wsCliente.nome() + " " + wsCliente.cognome());
            writer.println("Data: " + LocalDate.now() + "    Ora: " + java.time.LocalTime.now().withNano(0));
            writer.println("-".repeat(132));
            writer.println(String.format("%-20s %-5s %15s   %-50s %15s", "DATA/ORA", "TIPO", "IMPORTO", "CAUSALE", "SALDO DOPO"));
            writer.println("-".repeat(132));

            // Esecuzione del cursore
            ps.setString(1, wsNumeroConto);
            try (ResultSet rs = ps.executeQuery()) {
                sqlcode = 0;
                boolean hasRows = false;
                while (rs.next()) {
                    hasRows = true;
                    Movimento mov = new Movimento(
                        rs.getTimestamp("data_movimento"),
                        rs.getString("tipo_movimento"),
                        rs.getBigDecimal("importo"),
                        rs.getString("causale"),
                        rs.getBigDecimal("saldo_dopo")
                    );
                    scriviMovimentoReport(writer, mov);
                }
                if (!hasRows) {
                    sqlcode = 100; // Emula "not found" per il fetch
                }
            }

            writer.println("-".repeat(132));
            writer.println("SALDO FINALE: " + currencyFormatter.format(wsConto.saldo()));
            
            System.out.println("Estratto conto salvato in " + fileName);

        } catch (IOException e) {
            System.err.println("Errore durante la scrittura del file: " + e.getMessage());
        } catch (SQLException e) {
            handleSqlException(e);
        }
    }

    /**
     * Scrive una singola riga di movimento nel file di report.
     * Emula il paragrafo SCRIVI-MOVIMENTO-REPORT.
     * @param writer Il PrintWriter per scrivere sul file.
     * @param mov L'oggetto Movimento da scrivere.
     */
    private void scriviMovimentoReport(PrintWriter writer, Movimento mov) {
        String tipoDesc;
        switch (mov.tipo()) {
            case "D": tipoDesc = "DEP"; break;
            case "P": tipoDesc = "PRE"; break;
            case "B": tipoDesc = "BON"; break;
            case "G": tipoDesc = "GIR"; break;
            default: tipoDesc = "???"; break;
        }

        String causale = mov.causale() != null ? mov.causale() : "";
        if (causale.length() > 50) {
            causale = causale.substring(0, 50);
        }

        String line = String.format("%-20s %-5s %15s   %-50s %15s",
            mov.data().toLocalDateTime().withNano(0),
            tipoDesc,
            currencyFormatter.format(mov.importo()),
            causale,
            currencyFormatter.format(mov.saldoDopo())
        );
        writer.println(line);
    }

    /**
     * Gestisce la chiusura di un conto.
     * Emula il paragrafo CHIUSURA-CONTO.
     */
    private void chiusuraConto() {
        System.out.println("\n=== CHIUSURA CONTO ===");
        System.out.print("Numero conto da chiudere: ");
        wsNumeroConto = scanner.nextLine().trim();

        if (!verificaConto()) {
            return;
        }

        // Verifica saldo zero
        if (wsConto.saldo().compareTo(BigDecimal.ZERO) != 0) {
            System.out.println("Impossibile chiudere: saldo non zero!");
            System.out.println("Saldo attuale: " + currencyFormatter.format(wsConto.saldo()));
            return;
        }

        System.out.print("Confermare chiusura conto (S/N): ");
        String conferma = scanner.nextLine().trim();

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
                    throw new SQLException("Chiusura conto fallita.");
                }
            } catch (SQLException e) {
                handleSqlException(e);
                safeRollback();
            }
        } else {
            System.out.println("Chiusura annullata.");
        }
    }

    /**
     * Verifica l'esistenza e lo stato di un conto, popolando le variabili di stato.
     * Emula il paragrafo VERIFICA-CONTO.
     * @return true se il conto è valido e attivo, false altrimenti.
     */
    private boolean verificaConto() {
        wsEsito = "KO";
        String sql = "SELECT c.saldo, c.stato, c.fido, cl.nome, cl.cognome, cl.codice_cliente FROM CONTI c JOIN CLIENTI cl ON c.codice_cliente = cl.codice_cliente WHERE c.numero_conto = ?";
        try (PreparedStatement ps = connection.prepareStatement(sql)) {
            ps.setString(1, wsNumeroConto);
            try (ResultSet rs = ps.executeQuery()) {
                if (!rs.next()) {
                    sqlcode = 100;
                    System.out.println("Conto non trovato!");
                    return false;
                }
                
                sqlcode = 0;
                String stato = rs.getString("stato");
                wsConto = new Conto(
                    wsNumeroConto,
                    rs.getString("codice_cliente"),
                    null, // tipo non necessario qui
                    rs.getBigDecimal("saldo"),
                    null, // data apertura non necessaria qui
                    stato,
                    rs.getBigDecimal("fido")
                );
                wsCliente = new Cliente(
                    rs.getString("codice_cliente"),
                    rs.getString("nome"),
                    rs.getString("cognome")
                );

                if (!"A".equals(stato)) {
                    System.out.println("Conto non attivo! (Stato: " + stato + ")");
                    return false;
                }
            }
            wsEsito = "OK";
            return true;
        } catch (SQLException e) {
            handleSqlException(e);
            return false;
        }
    }

    /**
     * Registra un movimento nella tabella MOVIMENTI.
     * Questa operazione fa parte di una transazione più grande e non esegue commit.
     * Emula il paragrafo REGISTRA-MOVIMENTO.
     */
    private void registraMovimento() throws SQLException {
        // Recupera il saldo aggiornato
        String sqlSaldo = "SELECT saldo FROM CONTI WHERE numero_conto = ?";
        try (PreparedStatement psSaldo = connection.prepareStatement(sqlSaldo)) {
            psSaldo.setString(1, wsNumeroConto);
            try (ResultSet rs = psSaldo.executeQuery()) {
                if (rs.next()) {
                    wsSaldo = rs.getBigDecimal("saldo");
                } else {
                    throw new SQLException("Impossibile recuperare il saldo aggiornato per il conto " + wsNumeroConto);
                }
            }
        }

        // Inserisce il movimento
        String sqlInsert = "INSERT INTO MOVIMENTI (numero_conto, tipo_movimento, importo, causale, saldo_dopo, eseguito_da) VALUES (?, ?, ?, ?, ?, ?)";
        try (PreparedStatement psInsert = connection.prepareStatement(sqlInsert)) {
            psInsert.setString(1, wsNumeroConto);
            psInsert.setString(2, wsTipoMovimento);
            psInsert.setBigDecimal(3, wsImporto);
            
            // Gestione del valore NULL per la causale
            if (wsCausale == null || wsCausale.trim().isEmpty()) {
                psInsert.setNull(4, Types.VARCHAR);
            } else {
                psInsert.setString(4, wsCausale);
            }
            
            psInsert.setBigDecimal(5, wsSaldo);
            psInsert.setString(6, "SISTEMA");
            psInsert.executeUpdate();
        }
    }

    /**
     * Gestisce le eccezioni SQL, emulando l'impostazione di SQLCODE.
     * @param e L'eccezione SQL catturata.
     */
    private void handleSqlException(SQLException e) {
        // "02000" è lo SQLState standard per "no data found"
        if ("02000".equals(e.getSQLState())) {
            sqlcode = 100;
        } else {
            sqlcode = e.getErrorCode(); // Usa il codice di errore del driver
            if (sqlcode == 0) sqlcode = -1; // Assicura un valore negativo per errore generico
        }
        System.err.println("Errore database: SQLCODE=" + sqlcode + ", SQLState=" + e.getSQLState() + ", Messaggio=" + e.getMessage());
    }

    /**
     * Esegue un rollback sicuro, gestendo eventuali eccezioni durante il rollback stesso.
     */
    private void safeRollback() {
        if (connection != null) {
            try {
                System.err.println("Transazione fallita. Esecuzione del rollback.");
                connection.rollback();
            } catch (SQLException ex) {
                System.err.println("Errore critico durante il rollback: " + ex.getMessage());
            }
        }
    }
}