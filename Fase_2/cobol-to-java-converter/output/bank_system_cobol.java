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
 * @author Annalisa Egidi (Original COBOL Author)
 * @author Advanced COBOL-to-Java Compiler (Translator)
 * @version 2025-05-20
 */
public class GestioneConti {

    // --- Configurazione Database ---
    // NOTA: Assicurarsi che il database 'banca' esista e che le credenziali siano corrette.
    private static final String DB_URL = "jdbc:postgresql://localhost:5432/banca";
    private static final String DB_USER = "postgres";
    private static final String DB_PASSWORD = "password";

    private Connection connection;
    private final Scanner scanner = new Scanner(System.in);

    // --- Mappatura della WORKING-STORAGE SECTION ---
    private int wsScelta = 0;
    private String wsContinua = "S";
    private String wsEsito = "";

    // Variabili per database (host variables)
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
    // Altri campi cliente non usati attivamente nel flusso principale ma presenti in COBOL
    // private String wsCliCf = "";
    // private String wsCliDataNascita = "";
    // ...

    // Strutture dati per conto
    private String wsConNumero = "";
    private String wsConCliente = "";
    private String wsConTipo = "";
    private BigDecimal wsConSaldo = BigDecimal.ZERO;
    private String wsConDataApertura = "";
    private String wsConStato = "";
    private BigDecimal wsConFido = BigDecimal.ZERO;

    // Strutture dati per movimento (usate nel cursore)
    private Timestamp wsMovData;
    private String wsMovTipo = "";
    private BigDecimal wsMovImporto = BigDecimal.ZERO;
    private String wsMovCausale = "";
    private BigDecimal wsMovSaldoDopo = BigDecimal.ZERO;

    // Variabile per gestire l'esito delle operazioni SQL
    private int sqlcode = 0;

    // Formatter per importi monetari
    private final DecimalFormat currencyFormatter = new DecimalFormat("€ #,##0.00");
    private final DecimalFormat reportCurrencyFormatter = new DecimalFormat("###,###,##0.00");

    /**
     * Metodo main, punto di ingresso dell'applicazione.
     * @param args Argomenti della riga di comando (non usati).
     */
    public static void main(String[] args) {
        GestioneConti programma = new GestioneConti();
        programma.mainLogic();
    }

    /**
     * Contiene la logica principale del programma, equivalente al paragrafo MAIN-LOGIC del COBOL.
     */
    public void mainLogic() {
        if (!connectDatabase()) {
            return; // Interrompe l'esecuzione se la connessione fallisce
        }

        do {
            visualizzaMenu();
            try {
                wsScelta = Integer.parseInt(scanner.nextLine());
                elaboraScelta();
            } catch (NumberFormatException e) {
                System.out.println("Errore: Inserire un numero valido.");
                wsScelta = -1; // Scelta non valida
            }

            if (wsScelta != 0) {
                System.out.println();
                System.out.print("Continuare? (S/N): ");
                wsContinua = scanner.nextLine().toUpperCase();
            } else {
                wsContinua = "N"; // Esce dal ciclo se la scelta è 0
            }

        } while (wsContinua.equalsIgnoreCase("S"));

        disconnectDatabase();
        scanner.close();
        System.out.println("Programma terminato.");
    }

    /**
     * Stabilisce la connessione al database.
     * @return true se la connessione ha successo, false altrimenti.
     */
    private boolean connectDatabase() {
        try {
            connection = DriverManager.getConnection(DB_URL, DB_USER, DB_PASSWORD);
            // Impostiamo la gestione manuale delle transazioni
            connection.setAutoCommit(false);
            System.out.println("Connessione al database stabilita.");
            return true;
        } catch (SQLException e) {
            System.err.println("Errore di connessione al database: " + e.getMessage());
            handleSqlException(e);
            return false;
        }
    }

    /**
     * Chiude la connessione al database.
     */
    private void disconnectDatabase() {
        if (connection != null) {
            try {
                connection.close();
                System.out.println("Disconnesso dal database.");
            } catch (SQLException e) {
                System.err.println("Errore durante la disconnessione dal database: " + e.getMessage());
            }
        }
    }

    /**
     * Mostra il menu principale delle operazioni.
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
     */
    private void aperturaConto() {
        System.out.println("\n=== APERTURA NUOVO CONTO ===");
        System.out.print("Codice cliente: ");
        wsConCliente = scanner.nextLine().trim();

        String sqlCheckCliente = "SELECT codice_cliente, nome, cognome FROM CLIENTI WHERE codice_cliente = ?";
        try (PreparedStatement ps = connection.prepareStatement(sqlCheckCliente)) {
            ps.setString(1, wsConCliente);
            try (ResultSet rs = ps.executeQuery()) {
                if (rs.next()) {
                    wsCliCodice = rs.getString("codice_cliente");
                    wsCliNome = rs.getString("nome");
                    wsCliCognome = rs.getString("cognome");
                    sqlcode = 0;
                } else {
                    sqlcode = 100; // Cliente non trovato
                }
            }
        } catch (SQLException e) {
            handleSqlException(e);
            System.err.println("Errore database durante la verifica del cliente.");
            return;
        }

        if (sqlcode == 100) {
            System.out.println("Cliente non trovato!");
            return;
        }
        
        System.out.println("Cliente: " + wsCliNome.trim() + " " + wsCliCognome.trim());

        // Genera nuovo numero conto
        if (!generaNumeroConto()) {
            return; // Errore durante la generazione
        }

        System.out.print("Tipo conto (C=Corrente, D=Deposito): ");
        wsConTipo = scanner.nextLine().trim().toUpperCase();
        System.out.print("Importo iniziale: ");
        wsConSaldo = new BigDecimal(scanner.nextLine().trim());
        System.out.print("Fido accordato: ");
        wsConFido = new BigDecimal(scanner.nextLine().trim());
        
        wsConDataApertura = LocalDate.now().toString();
        wsConStato = "A";

        String sqlInsertConto = "INSERT INTO CONTI (numero_conto, codice_cliente, tipo_conto, saldo, data_apertura, stato, fido) VALUES (?, ?, ?, ?, ?, ?, ?)";
        try (PreparedStatement ps = connection.prepareStatement(sqlInsertConto)) {
            ps.setString(1, wsConNumero);
            ps.setString(2, wsConCliente);
            ps.setString(3, wsConTipo);
            ps.setBigDecimal(4, wsConSaldo);
            ps.setDate(5, Date.valueOf(LocalDate.now()));
            ps.setString(6, wsConStato);
            ps.setBigDecimal(7, wsConFido);
            
            int rowsAffected = ps.executeUpdate();
            if (rowsAffected > 0) {
                System.out.println("Conto " + wsConNumero.trim() + " creato con successo!");
                
                // Registra movimento iniziale se c'è un saldo
                if (wsConSaldo.compareTo(BigDecimal.ZERO) > 0) {
                    wsTipoMovimento = "D";
                    wsCausale = "Deposito iniziale";
                    wsImporto = wsConSaldo;
                    wsNumeroConto = wsConNumero;
                    registraMovimento(); // Questa chiamata gestirà il proprio commit/rollback
                }
                
                connection.commit();
            } else {
                System.err.println("Errore: la creazione del conto non ha avuto effetto.");
                connection.rollback();
            }
        } catch (SQLException e) {
            System.err.println("Errore creazione conto: " + e.getMessage());
            handleSqlException(e);
            rollbackTransaction();
        }
    }

    /**
     * Genera un nuovo numero di conto sequenziale.
     * @return true se la generazione ha successo, false altrimenti.
     */
    private boolean generaNumeroConto() {
        String sql = "SELECT 'IT' || LPAD(CAST(COALESCE(MAX(CAST(SUBSTR(numero_conto, 3) AS INTEGER)), 0) + 1 AS VARCHAR), 10, '0') FROM CONTI WHERE numero_conto LIKE 'IT%'";
        try (PreparedStatement ps = connection.prepareStatement(sql);
             ResultSet rs = ps.executeQuery()) {
            if (rs.next()) {
                wsConNumero = rs.getString(1);
                return true;
            }
        } catch (SQLException e) {
            System.err.println("Errore durante la generazione del numero conto: " + e.getMessage());
            handleSqlException(e);
        }
        return false;
    }

    /**
     * Gestisce un'operazione di deposito su un conto.
     */
    private void deposito() {
        System.out.println("\n=== DEPOSITO ===");
        System.out.print("Numero conto: ");
        wsNumeroConto = scanner.nextLine().trim();

        if (!verificaConto()) {
            return;
        }

        System.out.print("Importo deposito: ");
        try {
            wsImporto = new BigDecimal(scanner.nextLine().trim());
        } catch (NumberFormatException e) {
            System.out.println("Importo non valido!");
            return;
        }

        if (wsImporto.compareTo(BigDecimal.ZERO) <= 0) {
            System.out.println("L'importo deve essere positivo!");
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
                wsTipoMovimento = "D";
                registraMovimento();
                connection.commit();
                System.out.println("Deposito effettuato con successo!");
            } else {
                System.err.println("Errore durante il deposito: conto non trovato o non attivo.");
                connection.rollback();
            }
        } catch (SQLException e) {
            System.err.println("Errore database durante il deposito: " + e.getMessage());
            handleSqlException(e);
            rollbackTransaction();
        }
    }

    /**
     * Gestisce un'operazione di prelievo da un conto.
     */
    private void prelievo() {
        System.out.println("\n=== PRELIEVO ===");
        System.out.print("Numero conto: ");
        wsNumeroConto = scanner.nextLine().trim();

        if (!verificaConto()) { // verificaConto popola wsConSaldo e wsConFido
            return;
        }

        System.out.print("Importo prelievo: ");
        try {
            wsImporto = new BigDecimal(scanner.nextLine().trim());
        } catch (NumberFormatException e) {
            System.out.println("Importo non valido!");
            return;
        }

        if (wsImporto.compareTo(BigDecimal.ZERO) <= 0) {
            System.out.println("L'importo deve essere positivo!");
            return;
        }

        // Verifica disponibilità: saldo - importo >= -fido
        BigDecimal saldoDopoPrelievo = wsConSaldo.subtract(wsImporto);
        if (saldoDopoPrelievo.compareTo(wsConFido.negate()) < 0) {
            System.out.println("Fondi insufficienti!");
            System.out.println("Saldo attuale: " + currencyFormatter.format(wsConSaldo));
            System.out.println("Fido disponibile: " + currencyFormatter.format(wsConFido));
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
                wsTipoMovimento = "P";
                registraMovimento();
                connection.commit();
                System.out.println("Prelievo effettuato con successo!");
            } else {
                System.err.println("Errore durante il prelievo: conto non trovato o non attivo.");
                connection.rollback();
            }
        } catch (SQLException e) {
            System.err.println("Errore database durante il prelievo: " + e.getMessage());
            handleSqlException(e);
            rollbackTransaction();
        }
    }

    /**
     * Visualizza il saldo e le informazioni principali di un conto.
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
                    wsConSaldo = rs.getBigDecimal("saldo");
                    wsConFido = rs.getBigDecimal("fido");
                    wsCliNome = rs.getString("nome");
                    wsCliCognome = rs.getString("cognome");

                    System.out.println("\nIntestatario: " + wsCliNome.trim() + " " + wsCliCognome.trim());
                    System.out.println("Saldo attuale: " + currencyFormatter.format(wsConSaldo));
                    System.out.println("Fido accordato: " + currencyFormatter.format(wsConFido));
                    BigDecimal disponibile = wsConSaldo.add(wsConFido);
                    System.out.println("Disponibile: " + currencyFormatter.format(disponibile));
                } else {
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
    private void estrattoConto() {
        System.out.println("\n=== ESTRATTO CONTO ===");
        System.out.print("Numero conto: ");
        wsNumeroConto = scanner.nextLine().trim();

        if (!verificaConto()) {
            return;
        }

        String fileName = "ESTRATTO-CONTO.TXT";
        try (PrintWriter writer = new PrintWriter(new FileWriter(fileName))) {
            // Intestazione report
            writer.println(String.format("%66s", "ESTRATTO CONTO BANCARIO"));
            writer.println("-".repeat(132));
            writer.println("Conto: " + wsNumeroConto.trim() + "    Cliente: " + wsCliNome.trim() + " " + wsCliCognome.trim());
            writer.println("Data: " + LocalDate.now() + "    Ora: " + java.time.LocalTime.now().format(DateTimeFormatter.ofPattern("HH:mm:ss")));
            writer.println("-".repeat(132));
            writer.println(String.format("%-20s %-5s %15s   %-50s %15s", "DATA/ORA", "TIPO", "IMPORTO", "CAUSALE", "SALDO DOPO"));
            writer.println("-".repeat(132));

            // Cursore per i movimenti
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
                        scriviMovimentoReport(writer);
                    }
                }
            }

            writer.println("-".repeat(132));
            writer.println("SALDO FINALE: " + currencyFormatter.format(wsConSaldo));
            
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
    private void scriviMovimentoReport(PrintWriter writer) {
        String tipoMovimentoDesc;
        switch (wsMovTipo.trim()) {
            case "D": tipoMovimentoDesc = "DEP"; break;
            case "P": tipoMovimentoDesc = "PRE"; break;
            case "B": tipoMovimentoDesc = "BON"; break;
            default: tipoMovimentoDesc = "???"; break;
        }
        
        String dataFormatted = wsMovData.toLocalDateTime().format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"));
        String importoFormatted = reportCurrencyFormatter.format(wsMovImporto);
        String saldoDopoFormatted = reportCurrencyFormatter.format(wsMovSaldoDopo);
        String causaleTruncated = wsMovCausale.length() > 50 ? wsMovCausale.substring(0, 50) : wsMovCausale;

        writer.println(String.format("%-20s %-5s %15s   %-50s %15s",
            dataFormatted,
            tipoMovimentoDesc,
            importoFormatted,
            causaleTruncated,
            saldoDopoFormatted
        ));
    }

    /**
     * Gestisce la chiusura di un conto.
     */
    private void chiusuraConto() {
        System.out.println("\n=== CHIUSURA CONTO ===");
        System.out.print("Numero conto da chiudere: ");
        wsNumeroConto = scanner.nextLine().trim();

        if (!verificaConto()) {
            return;
        }

        // Verifica saldo zero
        if (wsConSaldo.setScale(2, RoundingMode.HALF_UP).compareTo(BigDecimal.ZERO) != 0) {
            System.out.println("Impossibile chiudere: saldo non a zero!");
            System.out.println("Saldo attuale: " + currencyFormatter.format(wsConSaldo));
            return;
        }

        System.out.print("Confermare chiusura conto (S/N): ");
        String conferma = scanner.nextLine().trim();

        if (conferma.equalsIgnoreCase("S")) {
            String sqlUpdate = "UPDATE CONTI SET stato = 'C', data_chiusura = ? WHERE numero_conto = ?";
            try (PreparedStatement ps = connection.prepareStatement(sqlUpdate)) {
                ps.setDate(1, Date.valueOf(LocalDate.now()));
                ps.setString(2, wsNumeroConto);
                
                int rowsAffected = ps.executeUpdate();
                if (rowsAffected > 0) {
                    connection.commit();
                    System.out.println("Conto chiuso con successo!");
                } else {
                    connection.rollback();
                    System.err.println("Errore: la chiusura del conto non ha avuto effetto.");
                }
            } catch (SQLException e) {
                System.err.println("Errore chiusura conto: " + e.getMessage());
                handleSqlException(e);
                rollbackTransaction();
            }
        } else {
            System.out.println("Chiusura annullata.");
        }
    }

    /**
     * Verifica l'esistenza e lo stato di un conto, popolando le variabili di classe.
     * @return true se il conto è valido e attivo, false altrimenti.
     */
    private boolean verificaConto() {
        wsEsito = "OK";
        String sql = "SELECT c.saldo, c.stato, c.fido, cl.nome, cl.cognome FROM CONTI c JOIN CLIENTI cl ON c.codice_cliente = cl.codice_cliente WHERE c.numero_conto = ?";
        try (PreparedStatement ps = connection.prepareStatement(sql)) {
            ps.setString(1, wsNumeroConto);
            try (ResultSet rs = ps.executeQuery()) {
                if (rs.next()) {
                    sqlcode = 0;
                    wsConSaldo = rs.getBigDecimal("saldo");
                    wsConStato = rs.getString("stato");
                    wsConFido = rs.getBigDecimal("fido");
                    wsCliNome = rs.getString("nome");
                    wsCliCognome = rs.getString("cognome");

                    if (!"A".equals(wsConStato.trim())) {
                        System.out.println("Conto non attivo! (Stato: " + wsConStato.trim() + ")");
                        wsEsito = "KO";
                    }
                } else {
                    sqlcode = 100;
                    System.out.println("Conto non trovato!");
                    wsEsito = "KO";
                }
            }
        } catch (SQLException e) {
            handleSqlException(e);
            System.err.println("Errore database durante la verifica del conto.");
            wsEsito = "KO";
        }
        return "OK".equals(wsEsito);
    }

    /**
     * Registra un movimento nella tabella MOVIMENTI.
     * Questa operazione è parte di una transazione più ampia.
     */
    private void registraMovimento() throws SQLException {
        // Ottiene il saldo aggiornato per registrarlo nel movimento
        String sqlGetSaldo = "SELECT saldo FROM CONTI WHERE numero_conto = ?";
        try (PreparedStatement psGetSaldo = connection.prepareStatement(sqlGetSaldo)) {
            psGetSaldo.setString(1, wsNumeroConto);
            try (ResultSet rs = psGetSaldo.executeQuery()) {
                if (rs.next()) {
                    wsSaldo = rs.getBigDecimal("saldo");
                } else {
                    // Questo non dovrebbe accadere se la transazione è gestita correttamente
                    throw new SQLException("Conto non trovato durante la registrazione del movimento.");
                }
            }
        }

        String sqlInsert = "INSERT INTO MOVIMENTI (numero_conto, tipo_movimento, importo, causale, saldo_dopo, eseguito_da) VALUES (?, ?, ?, ?, ?, ?)";
        try (PreparedStatement ps = connection.prepareStatement(sqlInsert)) {
            ps.setString(1, wsNumeroConto);
            ps.setString(2, wsTipoMovimento);
            ps.setBigDecimal(3, wsImporto);
            ps.setString(4, wsCausale);
            ps.setBigDecimal(5, wsSaldo);
            ps.setString(6, "SISTEMA");
            ps.executeUpdate();
        }
    }

    /**
     * Gestisce le eccezioni SQL, impostando il sqlcode.
     * @param e L'eccezione SQL catturata.
     */
    private void handleSqlException(SQLException e) {
        if ("02000".equals(e.getSQLState())) {
            // "No Data Found" in standard SQL
            this.sqlcode = 100;
        } else {
            this.sqlcode = e.getErrorCode();
            System.err.println("SQL Error: " + e.getMessage() + " (SQLState: " + e.getSQLState() + ", ErrorCode: " + e.getErrorCode() + ")");
        }
    }

    /**
     * Esegue il rollback della transazione corrente in modo sicuro.
     */
    private void rollbackTransaction() {
        if (connection != null) {
            try {
                connection.rollback();
                System.out.println("Transazione annullata (rollback).");
            } catch (SQLException ex) {
                System.err.println("Errore critico durante il rollback della transazione: " + ex.getMessage());
            }
        }
    }
}