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
import java.sql.Statement;
import java.sql.Timestamp;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.Locale;
import java.util.Scanner;

/**
 * Traduzione Java del programma COBOL GESTIONE-CONTI.
 * Questo programma gestisce le operazioni base di un sistema di conti correnti bancari,
 * interfacciandosi con un database PostgreSQL tramite JDBC.
 *
 * @author Annalisa Egidi (Original COBOL Author)
 * @author Advanced Source Code Compiler (Java Translator)
 * @version 2025-05-20
 */
public class GestioneConti {

    // --- Configurazione Database ---
    // DA SETTARE CORRETTAMENTE LE CREDENZIALI DEL DATABASE
    private static final String DB_URL = "jdbc:postgresql://localhost:5432/banca";
    private static final String DB_USER = "postgres";
    private static final String DB_PASSWORD = "password";

    // --- Oggetti JDBC e di utility ---
    private Connection connection;
    private final Scanner scanner;
    private int sqlcode; // Simula SQLCODE di COBOL

    // --- Variabili dalla WORKING-STORAGE SECTION ---
    private int wsScelta = 0;
    private String wsContinua = "S";
    
    // Variabili per database
    private String wsNumeroConto;
    private String wsCodiceCliente;
    private BigDecimal wsImporto;
    private BigDecimal wsSaldo;
    private String wsTipoMovimento;
    private String wsCausale;

    // Strutture dati per cliente
    private String wsCliCodice;
    private String wsCliNome;
    private String wsCliCognome;
    
    // Strutture dati per conto
    private String wsConNumero;
    private String wsConCliente;
    private String wsConTipo;
    private BigDecimal wsConSaldo;
    private String wsConDataApertura;
    private String wsConStato;
    private BigDecimal wsConFido;

    // Strutture dati per movimento (usate nel report)
    private Timestamp wsMovData;
    private String wsMovTipo;
    private BigDecimal wsMovImporto;
    private String wsMovCausale;
    private BigDecimal wsMovSaldoDopo;

    // Formattatori per output
    private final DecimalFormat currencyFormatter;

    /**
     * Costruttore della classe. Inizializza lo scanner e il formattatore di valuta.
     */
    public GestioneConti() {
        this.scanner = new Scanner(System.in);
        DecimalFormatSymbols symbols = new DecimalFormatSymbols(Locale.ITALY);
        this.currencyFormatter = new DecimalFormat("###,##0.00", symbols);
    }

    /**
     * Metodo main, punto di ingresso del programma.
     * @param args Argomenti della riga di comando (non usati).
     */
    public static void main(String[] args) {
        GestioneConti programma = new GestioneConti();
        programma.mainLogic();
    }

    /**
     * Logica principale del programma, che orchestra il menu e le operazioni.
     * Corrisponde al paragrafo MAIN-LOGIC del COBOL.
     */
    public void mainLogic() {
        if (!connettiDatabase()) {
            return; // Esce se la connessione fallisce
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

        disconnettiDatabase();
        scanner.close();
        System.out.println("Programma terminato.");
    }

    /**
     * Stabilisce la connessione al database.
     * Corrisponde al paragrafo CONNETTI-DATABASE.
     * @return true se la connessione ha successo, false altrimenti.
     */
    private boolean connettiDatabase() {
        try {
            connection = DriverManager.getConnection(DB_URL, DB_USER, DB_PASSWORD);
            connection.setAutoCommit(false); // Gestione manuale delle transazioni
            sqlcode = 0;
            System.out.println("Connessione al database stabilita.");
            return true;
        } catch (SQLException e) {
            handleSqlException(e);
            System.err.println("Errore fatale di connessione al database: " + sqlcode);
            e.printStackTrace();
            return false;
        }
    }

    /**
     * Chiude la connessione al database.
     * Corrisponde al paragrafo DISCONNETTI-DATABASE.
     */
    private void disconnettiDatabase() {
        if (connection != null) {
            try {
                connection.close();
                System.out.println("Disconnesso dal database.");
            } catch (SQLException e) {
                handleSqlException(e);
                System.err.println("Errore durante la disconnessione: " + sqlcode);
                e.printStackTrace();
            }
        }
    }

    /**
     * Mostra il menu principale delle operazioni.
     * Corrisponde al paragrafo VISUALIZZA-MENU.
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
     * Corrisponde al paragrafo ELABORA-SCELTA.
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
     * Corrisponde al paragrafo APERTURA-CONTO.
     */
    private void aperturaConto() {
        System.out.println("\n=== APERTURA NUOVO CONTO ===");
        System.out.print("Codice cliente: ");
        wsConCliente = scanner.nextLine().trim();

        String sqlVerificaCliente = "SELECT codice_cliente, nome, cognome FROM CLIENTI WHERE codice_cliente = ?";
        try (PreparedStatement stmt = connection.prepareStatement(sqlVerificaCliente)) {
            stmt.setString(1, wsConCliente);
            ResultSet rs = stmt.executeQuery();
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
        } catch (SQLException e) {
            handleSqlException(e);
            System.err.println("Errore database: " + sqlcode);
            return;
        }

        if (!generaNumeroConto()) return;

        System.out.print("Tipo conto (C=Corrente, D=Deposito): ");
        wsConTipo = scanner.nextLine().trim().toUpperCase();
        System.out.print("Importo iniziale: ");
        wsConSaldo = new BigDecimal(scanner.nextLine().replace(',', '.'));
        System.out.print("Fido accordato: ");
        wsConFido = new BigDecimal(scanner.nextLine().replace(',', '.'));
        wsConStato = "A";

        String sqlInsertConto = "INSERT INTO CONTI (numero_conto, codice_cliente, tipo_conto, saldo, data_apertura, stato, fido) VALUES (?, ?, ?, ?, CURRENT_DATE, ?, ?)";
        try (PreparedStatement stmt = connection.prepareStatement(sqlInsertConto)) {
            stmt.setString(1, wsConNumero);
            stmt.setString(2, wsConCliente);
            stmt.setString(3, wsConTipo);
            stmt.setBigDecimal(4, wsConSaldo);
            stmt.setString(5, wsConStato);
            stmt.setBigDecimal(6, wsConFido);

            int rowsAffected = stmt.executeUpdate();
            if (rowsAffected > 0) {
                sqlcode = 0;
                System.out.println("Conto " + wsConNumero + " creato con successo!");

                if (wsConSaldo.compareTo(BigDecimal.ZERO) > 0) {
                    wsTipoMovimento = "D";
                    wsCausale = "Deposito iniziale";
                    wsImporto = wsConSaldo;
                    wsNumeroConto = wsConNumero;
                    try {
                        registraMovimento();
                    } catch (SQLException ex) {
                        throw new SQLException("Errore durante la registrazione del movimento iniziale", ex);
                    } // Questo metodo non gestisce commit/rollback
                }
                connection.commit();
            } else {
                sqlcode = -1; // Errore generico
                System.err.println("Errore creazione conto: Nessuna riga inserita.");
                connection.rollback();
            }
        } catch (SQLException e) {
            handleSqlException(e);
            System.err.println("Errore creazione conto: " + sqlcode);
            try { connection.rollback(); } catch (SQLException ex) { ex.printStackTrace(); }
        }
    }

    /**
     * Genera un nuovo numero di conto sequenziale.
     * Corrisponde al paragrafo GENERA-NUMERO-CONTO.
     * @return true se la generazione ha successo, false altrimenti.
     */
    private boolean generaNumeroConto() {
        String sql = "SELECT 'IT' || LPAD(CAST(COALESCE(MAX(CAST(SUBSTR(numero_conto, 3) AS INTEGER)), 0) + 1 AS VARCHAR), 10, '0') FROM CONTI WHERE numero_conto LIKE 'IT%'";
        try (Statement stmt = connection.createStatement();
             ResultSet rs = stmt.executeQuery(sql)) {
            if (rs.next()) {
                sqlcode = 0;
                wsConNumero = rs.getString(1);
                return true;
            } else {
                sqlcode = 100; // Non dovrebbe mai accadere con questa query
                return false;
            }
        } catch (SQLException e) {
            handleSqlException(e);
            System.err.println("Errore durante la generazione del numero conto: " + sqlcode);
            return false;
        }
    }

    /**
     * Gestisce un'operazione di deposito su un conto.
     * Corrisponde al paragrafo DEPOSITO.
     */
    private void deposito() {
        System.out.println("\n=== DEPOSITO ===");
        System.out.print("Numero conto: ");
        wsNumeroConto = scanner.nextLine().trim();

        if (!verificaConto()) return;

        System.out.print("Importo deposito: ");
        try {
            wsImporto = new BigDecimal(scanner.nextLine().replace(',', '.'));
            if (wsImporto.compareTo(BigDecimal.ZERO) <= 0) {
                System.out.println("Importo non valido!");
                return;
            }
        } catch (NumberFormatException e) {
            System.out.println("Importo non valido!");
            return;
        }

        System.out.print("Causale: ");
        wsCausale = scanner.nextLine().trim();

        String sqlUpdate = "UPDATE CONTI SET saldo = saldo + ? WHERE numero_conto = ? AND stato = 'A'";
        try (PreparedStatement stmt = connection.prepareStatement(sqlUpdate)) {
            stmt.setBigDecimal(1, wsImporto);
            stmt.setString(2, wsNumeroConto);
            int rowsAffected = stmt.executeUpdate();

            if (rowsAffected > 0) {
                sqlcode = 0;
                wsTipoMovimento = "D";
                try {
                    registraMovimento();
                } catch (SQLException ex) {
                    throw new SQLException("Errore durante la registrazione del movimento iniziale", ex);
                }
                connection.commit();
                System.out.println("Deposito effettuato con successo!");
            } else {
                sqlcode = 100;
                System.err.println("Errore durante il deposito: conto non trovato o non attivo.");
                connection.rollback();
            }
        } catch (SQLException e) {
            handleSqlException(e);
            System.err.println("Errore durante il deposito: " + sqlcode);
            try { connection.rollback(); } catch (SQLException ex) { ex.printStackTrace(); }
        }
    }

    /**
     * Gestisce un'operazione di prelievo da un conto.
     * Corrisponde al paragrafo PRELIEVO.
     */
    private void prelievo() {
        System.out.println("\n=== PRELIEVO ===");
        System.out.print("Numero conto: ");
        wsNumeroConto = scanner.nextLine().trim();

        if (!verificaConto()) return;

        System.out.print("Importo prelievo: ");
        try {
            wsImporto = new BigDecimal(scanner.nextLine().replace(',', '.'));
            if (wsImporto.compareTo(BigDecimal.ZERO) <= 0) {
                System.out.println("Importo non valido!");
                return;
            }
        } catch (NumberFormatException e) {
            System.out.println("Importo non valido!");
            return;
        }

        // Verifica disponibilità (il saldo è già stato caricato da verificaConto)
        // wsConSaldo è il saldo attuale, wsConFido è il fido
        BigDecimal disponibilita = wsConSaldo.add(wsConFido);
        if (wsImporto.compareTo(disponibilita) > 0) {
            System.out.println("Fondi insufficienti!");
            System.out.println("Saldo attuale: EUR " + formatCurrency(wsConSaldo));
            System.out.println("Disponibile (con fido): EUR " + formatCurrency(disponibilita));
            return;
        }

        System.out.print("Causale: ");
        wsCausale = scanner.nextLine().trim();

        String sqlUpdate = "UPDATE CONTI SET saldo = saldo - ? WHERE numero_conto = ? AND stato = 'A'";
        try (PreparedStatement stmt = connection.prepareStatement(sqlUpdate)) {
            stmt.setBigDecimal(1, wsImporto);
            stmt.setString(2, wsNumeroConto);
            int rowsAffected = stmt.executeUpdate();

            if (rowsAffected > 0) {
                sqlcode = 0;
                wsTipoMovimento = "P";
                try {
                    registraMovimento();
                } catch (SQLException ex) {
                    throw new SQLException("Errore durante la registrazione del movimento iniziale", ex);
                }
                connection.commit();
                System.out.println("Prelievo effettuato con successo!");
            } else {
                sqlcode = 100;
                System.err.println("Errore durante il prelievo: conto non trovato o non attivo.");
                connection.rollback();
            }
        } catch (SQLException e) {
            handleSqlException(e);
            System.err.println("Errore durante il prelievo: " + sqlcode);
            try { connection.rollback(); } catch (SQLException ex) { ex.printStackTrace(); }
        }
    }

    /**
     * Visualizza il saldo e le informazioni principali di un conto.
     * Corrisponde al paragrafo VISUALIZZA-SALDO.
     */
    private void visualizzaSaldo() {
        System.out.println("\n=== VISUALIZZA SALDO ===");
        System.out.print("Numero conto: ");
        wsNumeroConto = scanner.nextLine().trim();

        String sql = "SELECT c.saldo, c.fido, cl.nome, cl.cognome FROM CONTI c JOIN CLIENTI cl ON c.codice_cliente = cl.codice_cliente WHERE c.numero_conto = ? AND c.stato = 'A'";
        try (PreparedStatement stmt = connection.prepareStatement(sql)) {
            stmt.setString(1, wsNumeroConto);
            ResultSet rs = stmt.executeQuery();

            if (rs.next()) {
                sqlcode = 0;
                wsConSaldo = rs.getBigDecimal("saldo");
                wsConFido = rs.getBigDecimal("fido");
                wsCliNome = rs.getString("nome");
                wsCliCognome = rs.getString("cognome");

                System.out.println();
                System.out.println("Intestatario: " + wsCliNome + " " + wsCliCognome);
                System.out.println("Saldo attuale: EUR " + formatCurrency(wsConSaldo));
                System.out.println("Fido accordato: EUR " + formatCurrency(wsConFido));
                BigDecimal disponibile = wsConSaldo.add(wsConFido);
                System.out.println("Disponibile: EUR " + formatCurrency(disponibile));
            } else {
                sqlcode = 100;
                System.out.println("Conto non trovato o non attivo!");
            }
            connection.commit(); // Commit per terminare la transazione di lettura
        } catch (SQLException e) {
            handleSqlException(e);
            System.err.println("Errore database: " + sqlcode);
        }
    }

    /**
     * Genera un file di testo con l'estratto conto.
     * Corrisponde al paragrafo ESTRATTO-CONTO.
     */
    private void estrattoConto() {
        System.out.println("\n=== ESTRATTO CONTO ===");
        System.out.print("Numero conto: ");
        wsNumeroConto = scanner.nextLine().trim();

        if (!verificaConto()) return;

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

            // Cursore movimenti
            String sqlCursor = "SELECT data_movimento, tipo_movimento, importo, causale, saldo_dopo FROM MOVIMENTI WHERE numero_conto = ? ORDER BY data_movimento DESC";
            try (PreparedStatement stmt = connection.prepareStatement(sqlCursor)) {
                stmt.setString(1, wsNumeroConto);
                ResultSet rs = stmt.executeQuery();

                while (rs.next()) {
                    sqlcode = 0;
                    wsMovData = rs.getTimestamp("data_movimento");
                    wsMovTipo = rs.getString("tipo_movimento");
                    wsMovImporto = rs.getBigDecimal("importo");
                    wsMovCausale = rs.getString("causale");
                    wsMovSaldoDopo = rs.getBigDecimal("saldo_dopo");
                    scriviMovimentoReport(writer);
                }

                connection.commit(); // Termina transazione di lettura

            } catch (SQLException e) {
                handleSqlException(e);
                System.err.println("Errore nel recuperare i movimenti: " + sqlcode);
            }

            writer.println("-".repeat(132));
            // Saldo finale (già caricato da verificaConto)
            writer.println(String.format("%-100s SALDO FINALE: EUR %15s", "", formatCurrency(wsConSaldo)));
            
            System.out.println("Estratto conto salvato in " + fileName);

        } catch (IOException e) {
            System.err.println("Errore durante la scrittura del file di report: " + e.getMessage());
        }
    }

    /**
     * Scrive una singola riga di movimento nel file di report.
     * Corrisponde al paragrafo SCRIVI-MOVIMENTO-REPORT.
     * @param writer L'oggetto PrintWriter per scrivere sul file.
     */
    private void scriviMovimentoReport(PrintWriter writer) {
        String tipoDesc;
        switch (wsMovTipo) {
            case "D": tipoDesc = "DEP"; break;
            case "P": tipoDesc = "PRE"; break;
            case "B": tipoDesc = "BON"; break;
            default: tipoDesc = "???"; break;
        }

        String dataFormatted = wsMovData.toLocalDateTime().format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"));
        String causaleTrunc = wsMovCausale.length() > 50 ? wsMovCausale.substring(0, 50) : wsMovCausale;

        String line = String.format("%-20s %-5s %15s   %-50s %15s",
                dataFormatted,
                tipoDesc,
                formatCurrency(wsMovImporto),
                causaleTrunc,
                formatCurrency(wsMovSaldoDopo));
        writer.println(line);
    }

    /**
     * Gestisce la chiusura di un conto.
     * Corrisponde al paragrafo CHIUSURA-CONTO.
     */
    private void chiusuraConto() {
        System.out.println("\n=== CHIUSURA CONTO ===");
        System.out.print("Numero conto da chiudere: ");
        wsNumeroConto = scanner.nextLine().trim();

        if (!verificaConto()) return;

        if (wsConSaldo.compareTo(BigDecimal.ZERO) != 0) {
            System.out.println("Impossibile chiudere: saldo non a zero!");
            System.out.println("Saldo attuale: EUR " + formatCurrency(wsConSaldo));
            return;
        }

        System.out.print("Confermare chiusura conto (S/N): ");
        String conferma = scanner.nextLine();

        if (conferma.equalsIgnoreCase("S")) {
            String sqlUpdate = "UPDATE CONTI SET stato = 'C', data_chiusura = CURRENT_DATE WHERE numero_conto = ?";
            try (PreparedStatement stmt = connection.prepareStatement(sqlUpdate)) {
                stmt.setString(1, wsNumeroConto);
                int rowsAffected = stmt.executeUpdate();
                if (rowsAffected > 0) {
                    sqlcode = 0;
                    connection.commit();
                    System.out.println("Conto chiuso con successo!");
                } else {
                    sqlcode = 100;
                    connection.rollback();
                    System.err.println("Errore chiusura conto: nessuna riga aggiornata.");
                }
            } catch (SQLException e) {
                handleSqlException(e);
                System.err.println("Errore chiusura conto: " + sqlcode);
                try { connection.rollback(); } catch (SQLException ex) { ex.printStackTrace(); }
            }
        } else {
            System.out.println("Chiusura annullata.");
        }
    }

    /**
     * Verifica l'esistenza e lo stato di un conto, caricandone i dati.
     * Corrisponde al paragrafo VERIFICA-CONTO.
     * @return true se il conto è valido e attivo, false altrimenti.
     */
    private boolean verificaConto() {
        String sql = "SELECT c.saldo, c.stato, c.fido, cl.nome, cl.cognome FROM CONTI c JOIN CLIENTI cl ON c.codice_cliente = cl.codice_cliente WHERE c.numero_conto = ?";
        try (PreparedStatement stmt = connection.prepareStatement(sql)) {
            stmt.setString(1, wsNumeroConto);
            ResultSet rs = stmt.executeQuery();

            if (rs.next()) {
                sqlcode = 0;
                wsConSaldo = rs.getBigDecimal("saldo");
                wsConStato = rs.getString("stato");
                wsConFido = rs.getBigDecimal("fido");
                wsCliNome = rs.getString("nome");
                wsCliCognome = rs.getString("cognome");

                if (!"A".equals(wsConStato)) {
                    System.out.println("Conto non attivo! (Stato: " + wsConStato + ")");
                    return false;
                }
                return true;
            } else {
                sqlcode = 100;
                System.out.println("Conto non trovato!");
                return false;
            }
        } catch (SQLException e) {
            handleSqlException(e);
            System.err.println("Errore database: " + sqlcode);
            return false;
        }
    }

    /**
     * Registra un movimento nella tabella MOVIMENTI.
     * Corrisponde al paragrafo REGISTRA-MOVIMENTO.
     * ATTENZIONE: Questo metodo non gestisce commit/rollback, deve essere
     * chiamato all'interno di una transazione già esistente.
     */
    private void registraMovimento() throws SQLException {
        // Recupera il saldo aggiornato per registrarlo nel movimento
        String sqlSelectSaldo = "SELECT saldo FROM CONTI WHERE numero_conto = ?";
        try (PreparedStatement stmtSaldo = connection.prepareStatement(sqlSelectSaldo)) {
            stmtSaldo.setString(1, wsNumeroConto);
            ResultSet rs = stmtSaldo.executeQuery();
            if (rs.next()) {
                wsSaldo = rs.getBigDecimal("saldo");
            } else {
                throw new SQLException("Impossibile recuperare il saldo aggiornato per il conto " + wsNumeroConto);
            }
        }

        String sqlInsert = "INSERT INTO MOVIMENTI (numero_conto, tipo_movimento, importo, causale, saldo_dopo, eseguito_da) VALUES (?, ?, ?, ?, ?, 'SISTEMA')";
        try (PreparedStatement stmt = connection.prepareStatement(sqlInsert)) {
            stmt.setString(1, wsNumeroConto);
            stmt.setString(2, wsTipoMovimento);
            stmt.setBigDecimal(3, wsImporto);
            stmt.setString(4, wsCausale);
            stmt.setBigDecimal(5, wsSaldo);
            stmt.executeUpdate();
        }
    }

    /**
     * Gestisce le eccezioni SQL, simulando il comportamento di SQLCODE.
     * @param e L'eccezione SQL catturata.
     */
    private void handleSqlException(SQLException e) {
        // "02000" è lo SQLSTATE standard per "no data found"
        if ("02000".equals(e.getSQLState())) {
            this.sqlcode = 100;
        } else {
            this.sqlcode = e.getErrorCode() != 0 ? -e.getErrorCode() : -1;
        }
    }

    /**
     * Formatta un valore BigDecimal come stringa di valuta.
     * @param amount L'importo da formattare.
     * @return La stringa formattata.
     */
    private String formatCurrency(BigDecimal amount) {
        return currencyFormatter.format(amount.setScale(2, RoundingMode.HALF_UP));
    }
}