import org.junit.jupiter.api.*;

import java.io.*;
import java.lang.reflect.Field;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.sql.*;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Unit test per la classe GestioneConti.
 * Utilizza un database H2 in-memory per isolare i test dall'ambiente di produzione
 * e per evitare il mocking diretto di java.sql.Connection, come richiesto.
 * I test sono scritti per JUnit 5 e sono compatibili con Java 21.
 */
class GestioneContiTest {

    private GestioneConti gestioneConti;
    private Connection connection;

    // Stream per catturare l'output della console
    private final ByteArrayOutputStream outContent = new ByteArrayOutputStream();
    private final PrintStream originalOut = System.out;
    private final InputStream originalIn = System.in;

    // Il file di report generato dal programma
    private static final String REPORT_FILE_NAME = "ESTRATTO-CONTO.TXT";
    private final File reportFile = new File(REPORT_FILE_NAME);

    @BeforeEach
    void setUp() throws Exception {
        // 1. Configura il database H2 in-memory in modalità PostgreSQL per la massima compatibilità
        String dbUrl = "jdbc:h2:mem:testdb;DB_CLOSE_DELAY=-1;MODE=PostgreSQL";
        connection = DriverManager.getConnection(dbUrl, "sa", "");
        connection.setAutoCommit(false); // Impostazione cruciale, come nel codice originale

        // 2. Crea lo schema del database
        createDatabaseSchema();

        // 3. Inizializza l'istanza di GestioneConti
        gestioneConti = new GestioneConti();

        // 4. Usa la reflection per iniettare la nostra connessione H2 nell'istanza,
        // bypassando la logica di connessione hardcoded.
        Field connectionField = GestioneConti.class.getDeclaredField("connection");
        connectionField.setAccessible(true);
        connectionField.set(gestioneConti, connection);

        // 5. Reindirizza System.out per catturare l'output
        System.setOut(new PrintStream(outContent, true, StandardCharsets.UTF_8));

        // 6. Assicurati che il file di report non esista prima del test
        if (reportFile.exists()) {
            reportFile.delete();
        }
    }

    @AfterEach
    void tearDown() throws Exception {
        // 1. Ripristina gli stream originali
        System.setOut(originalOut);
        System.setIn(originalIn);

        // 2. Pulisci il database e chiudi la connessione
        if (connection != null && !connection.isClosed()) {
            try (Statement stmt = connection.createStatement()) {
                stmt.execute("DROP ALL OBJECTS");
            }
            connection.close();
        }

        // 3. Pulisci il file di report se è stato creato
        if (reportFile.exists()) {
            reportFile.delete();
        }
    }

    /**
     * Crea le tabelle necessarie per i test nel database H2.
     */
    private void createDatabaseSchema() throws SQLException {
        try (Statement stmt = connection.createStatement()) {
            stmt.execute("CREATE TABLE CLIENTI (" +
                    "codice_cliente VARCHAR(50) PRIMARY KEY, " +
                    "nome VARCHAR(100), " +
                    "cognome VARCHAR(100))");

            stmt.execute("CREATE TABLE CONTI (" +
                    "numero_conto VARCHAR(20) PRIMARY KEY, " +
                    "codice_cliente VARCHAR(50), " +
                    "tipo_conto VARCHAR(1), " +
                    "saldo DECIMAL(15, 2), " +
                    "data_apertura DATE, " +
                    "stato VARCHAR(1), " +
                    "fido DECIMAL(15, 2), " +
                    "data_chiusura DATE, " +
                    "FOREIGN KEY (codice_cliente) REFERENCES CLIENTI(codice_cliente))");

            stmt.execute("CREATE TABLE MOVIMENTI (" +
                    "id_movimento INT AUTO_INCREMENT PRIMARY KEY, " +
                    "numero_conto VARCHAR(20), " +
                    "data_movimento TIMESTAMP, " +
                    "tipo_movimento VARCHAR(1), " +
                    "importo DECIMAL(15, 2), " +
                    "causale VARCHAR(255), " +
                    "saldo_dopo DECIMAL(15, 2), " +
                    "eseguito_da VARCHAR(50), " +
                    "FOREIGN KEY (numero_conto) REFERENCES CONTI(numero_conto))");
            connection.commit();
        }
    }

    /**
     * Fornisce input simulato al programma e resetta il buffer di output.
     * @param data La stringa di input da simulare.
     */
    private void provideInput(String data) {
        outContent.reset();
        String inputWithLineSeparators = data.replace("\n", System.lineSeparator());
        System.setIn(new ByteArrayInputStream(inputWithLineSeparators.getBytes(StandardCharsets.UTF_8)));
    }

    @Test
    @DisplayName("Dovrebbe uscire immediatamente se l'utente sceglie 0")
    void mainLogic_ExitOnChoiceZero() {
        provideInput("0\n");
        gestioneConti.mainLogic();
        String output = outContent.toString(StandardCharsets.UTF_8);
        assertTrue(output.contains("Programma terminato."), "L'output dovrebbe confermare la terminazione del programma.");
    }

    @Test
    @DisplayName("Dovrebbe gestire una scelta non valida nel menu")
    void mainLogic_InvalidChoice() {
        provideInput("99\nN\n");
        gestioneConti.mainLogic();
        String output = outContent.toString(StandardCharsets.UTF_8);
        assertTrue(output.contains("Scelta non valida!"), "Dovrebbe mostrare un messaggio per scelta non valida.");
    }

    @Nested
    @DisplayName("Test di Apertura Conto")
    class OpenAccountTests {
        @BeforeEach
        void setupClient() throws SQLException {
            try (PreparedStatement ps = connection.prepareStatement("INSERT INTO CLIENTI (codice_cliente, nome, cognome) VALUES (?, ?, ?)")) {
                ps.setString(1, "CL001");
                ps.setString(2, "Mario");
                ps.setString(3, "Rossi");
                ps.executeUpdate();
                connection.commit();
            }
        }

        @Test
        @DisplayName("Dovrebbe aprire un nuovo conto con successo")
        void openAccount_Success() throws SQLException {
            provideInput("1\nCL001\nC\n1500.50\n500.00\nN\n");
            gestioneConti.mainLogic();

            String output = outContent.toString(StandardCharsets.UTF_8);
            assertTrue(output.contains("Conto IT0000000001 creato con successo!"));

            // Verifica sul DB
            try (Statement stmt = connection.createStatement();
                 ResultSet rs = stmt.executeQuery("SELECT * FROM CONTI WHERE numero_conto = 'IT0000000001'")) {
                assertTrue(rs.next(), "Il conto dovrebbe esistere nel database.");
                assertEquals("CL001", rs.getString("codice_cliente"));
                assertEquals(0, new BigDecimal("1500.50").compareTo(rs.getBigDecimal("saldo")));
                assertEquals("A", rs.getString("stato"));
            }
            try (Statement stmt = connection.createStatement();
                 ResultSet rs = stmt.executeQuery("SELECT * FROM MOVIMENTI WHERE numero_conto = 'IT0000000001'")) {
                assertTrue(rs.next(), "Dovrebbe esistere un movimento di deposito iniziale.");
                assertEquals("D", rs.getString("tipo_movimento"));
                assertEquals(0, new BigDecimal("1500.50").compareTo(rs.getBigDecimal("importo")));
            }
        }

        @Test
        @DisplayName("Dovrebbe fallire l'apertura se il cliente non esiste")
        void openAccount_ClientNotFound() throws SQLException {
            provideInput("1\nCL999\nN\n");
            gestioneConti.mainLogic();

            String output = outContent.toString(StandardCharsets.UTF_8);
            assertTrue(output.contains("Cliente non trovato!"));

            try (Statement stmt = connection.createStatement();
                 ResultSet rs = stmt.executeQuery("SELECT COUNT(*) FROM CONTI")) {
                rs.next();
                assertEquals(0, rs.getInt(1), "Nessun conto dovrebbe essere stato creato.");
            }
        }
    }

    @Nested
    @DisplayName("Test di Operazioni sul Conto")
    class AccountOperationsTests {
        private final String testAccount = "IT0000000001";

        @BeforeEach
        void setupAccount() throws SQLException {
            try (Statement stmt = connection.createStatement()) {
                stmt.execute("INSERT INTO CLIENTI (codice_cliente, nome, cognome) VALUES ('CL001', 'Mario', 'Rossi')");
                stmt.execute("INSERT INTO CONTI (numero_conto, codice_cliente, tipo_conto, saldo, data_apertura, stato, fido) " +
                        "VALUES ('" + testAccount + "', 'CL001', 'C', 1000.00, CURRENT_DATE, 'A', 200.00)");
                connection.commit();
            }
        }

        @Test
        @DisplayName("Dovrebbe effettuare un deposito con successo")
        void makeDeposit_Success() throws SQLException {
            provideInput("2\n" + testAccount + "\n350.75\nStipendio\nN\n");
            gestioneConti.mainLogic();

            String output = outContent.toString(StandardCharsets.UTF_8);
            assertTrue(output.contains("Deposito effettuato con successo!"));

            try (PreparedStatement ps = connection.prepareStatement("SELECT saldo FROM CONTI WHERE numero_conto = ?")) {
                ps.setString(1, testAccount);
                ResultSet rs = ps.executeQuery();
                assertTrue(rs.next());
                assertEquals(0, new BigDecimal("1350.75").compareTo(rs.getBigDecimal("saldo")));
            }
        }

        @Test
        @DisplayName("Dovrebbe effettuare un prelievo con successo")
        void makeWithdrawal_Success() throws SQLException {
            provideInput("3\n" + testAccount + "\n150.00\nSpesa\nN\n");
            gestioneConti.mainLogic();

            String output = outContent.toString(StandardCharsets.UTF_8);
            assertTrue(output.contains("Prelievo effettuato con successo!"));

            try (PreparedStatement ps = connection.prepareStatement("SELECT saldo FROM CONTI WHERE numero_conto = ?")) {
                ps.setString(1, testAccount);
                ResultSet rs = ps.executeQuery();
                assertTrue(rs.next());
                assertEquals(0, new BigDecimal("850.00").compareTo(rs.getBigDecimal("saldo")));
            }
        }

        @Test
        @DisplayName("Dovrebbe fallire un prelievo per fondi insufficienti")
        void makeWithdrawal_InsufficientFunds() throws SQLException {
            provideInput("3\n" + testAccount + "\n1200.01\nTentativo\nN\n"); // Saldo 1000 + Fido 200 = 1200 max prelievo
            gestioneConti.mainLogic();

            String output = outContent.toString(StandardCharsets.UTF_8);
            assertTrue(output.contains("Fondi insufficienti!"));

            try (PreparedStatement ps = connection.prepareStatement("SELECT saldo FROM CONTI WHERE numero_conto = ?")) {
                ps.setString(1, testAccount);
                ResultSet rs = ps.executeQuery();
                assertTrue(rs.next());
                assertEquals(0, new BigDecimal("1000.00").compareTo(rs.getBigDecimal("saldo")), "Il saldo non deve cambiare.");
            }
        }

        @Test
        @DisplayName("Dovrebbe visualizzare il saldo corretto")
        void viewBalance_Success() {
            provideInput("4\n" + testAccount + "\nN\n");
            gestioneConti.mainLogic();

            String output = outContent.toString(StandardCharsets.UTF_8);
            assertTrue(output.contains("Intestatario: Mario Rossi"));
            assertTrue(output.contains("Saldo attuale: EUR 1.000,00"));
            assertTrue(output.contains("Fido accordato: EUR 200,00"));
            assertTrue(output.contains("Disponibile: EUR 1.200,00"));
        }

        @Test
        @DisplayName("Dovrebbe generare un file di estratto conto")
        void generateAccountStatement_Success() throws IOException {
            provideInput("5\n" + testAccount + "\nN\n");
            gestioneConti.mainLogic();

            String output = outContent.toString(StandardCharsets.UTF_8);
            assertTrue(output.contains("Estratto conto salvato in " + REPORT_FILE_NAME));
            assertTrue(reportFile.exists(), "Il file di report dovrebbe essere stato creato.");

            List<String> lines = Files.readAllLines(reportFile.toPath(), StandardCharsets.UTF_8);
            assertTrue(lines.stream().anyMatch(line -> line.contains("ESTRATTO CONTO BANCARIO")));
            assertTrue(lines.stream().anyMatch(line -> line.contains("Conto: " + testAccount)));
            assertTrue(lines.stream().anyMatch(line -> line.contains("Cliente: Mario Rossi")));
        }

        @Test
        @DisplayName("Dovrebbe chiudere un conto con saldo zero")
        void closeAccount_Success() throws SQLException {
            // Azzera il saldo prima di tentare la chiusura
            try (Statement stmt = connection.createStatement()) {
                stmt.executeUpdate("UPDATE CONTI SET saldo = 0.00 WHERE numero_conto = '" + testAccount + "'");
                connection.commit();
            }

            provideInput("6\n" + testAccount + "\nS\nN\n");
            gestioneConti.mainLogic();

            String output = outContent.toString(StandardCharsets.UTF_8);
            assertTrue(output.contains("Conto chiuso con successo!"));

            try (PreparedStatement ps = connection.prepareStatement("SELECT stato FROM CONTI WHERE numero_conto = ?")) {
                ps.setString(1, testAccount);
                ResultSet rs = ps.executeQuery();
                assertTrue(rs.next());
                assertEquals("C", rs.getString("stato"), "Lo stato del conto dovrebbe essere 'C'.");
            }
        }

        @Test
        @DisplayName("Dovrebbe fallire la chiusura di un conto con saldo non nullo")
        void closeAccount_NonZeroBalance() throws SQLException {
            provideInput("6\n" + testAccount + "\nS\nN\n");
            gestioneConti.mainLogic();

            String output = outContent.toString(StandardCharsets.UTF_8);
            assertTrue(output.contains("Impossibile chiudere: saldo non a zero!"));

            try (PreparedStatement ps = connection.prepareStatement("SELECT stato FROM CONTI WHERE numero_conto = ?")) {
                ps.setString(1, testAccount);
                ResultSet rs = ps.executeQuery();
                assertTrue(rs.next());
                assertEquals("A", rs.getString("stato"), "Lo stato del conto non deve cambiare.");
            }
        }
    }
}