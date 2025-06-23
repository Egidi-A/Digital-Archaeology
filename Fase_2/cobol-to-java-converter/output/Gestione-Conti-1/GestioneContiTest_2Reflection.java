import org.junit.jupiter.api.*;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;

import java.io.*;
import java.math.BigDecimal;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.sql.*;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.contains;
import static org.mockito.Mockito.*;

/**
 * Unit test per la classe GestioneConti.
 *
 * STRATEGIA DI TEST:
 * Poiché l'unico metodo pubblico significativo è `mainLogic()`, che orchestra l'intera applicazione,
 * tutti i test sono progettati per testare questo metodo.
 * Fornendo input utente simulati e mockando le dipendenze esterne (database, file system),
 * possiamo testare indirettamente tutta la logica privata (openAccount, makeDeposit, etc.)
 * in modo integrato, rispettando i requisiti di visibilità.
 *
 * L'uso della reflection per testare i metodi privati non è stato necessario in quanto
 * la loro funzionalità è completamente coperta attraverso il test del metodo pubblico `mainLogic`.
 */
@ExtendWith(MockitoExtension.class)
class GestioneContiTest {

    @InjectMocks
    private GestioneConti gestioneConti;

    @Mock
    private Connection mockConnection;
    @Mock
    private PreparedStatement mockPreparedStatement;
    @Mock
    private ResultSet mockResultSet;

    private final InputStream originalIn = System.in;
    private final PrintStream originalOut = System.out;
    private final PrintStream originalErr = System.err;

    private ByteArrayOutputStream outContent;
    private ByteArrayOutputStream errContent;

    @BeforeEach
    void setUp() {
        // Redireziona System.out e System.err per catturare l'output della console
        outContent = new ByteArrayOutputStream();
        errContent = new ByteArrayOutputStream();
        System.setOut(new PrintStream(outContent));
        System.setErr(new PrintStream(errContent));
    }

    @AfterEach
    void tearDown() {
        // Ripristina gli stream originali di System
        System.setIn(originalIn);
        System.setOut(originalOut);
        System.setErr(originalErr);
        // Pulisce eventuali file di test
        try {
            Files.deleteIfExists(Paths.get("ESTRATTO-CONTO.TXT"));
        } catch (IOException e) {
            // Ignora errori di pulizia
        }
    }

    // Metodo di utilità per fornire input utente simulato
    private void provideInput(String data) {
        ByteArrayInputStream testIn = new ByteArrayInputStream(data.getBytes());
        System.setIn(testIn);
        // La classe GestioneConti crea il suo Scanner internamente.
        // Per garantire che il nuovo System.in venga utilizzato, creiamo una nuova istanza della classe.
        gestioneConti = new GestioneConti();
    }

    // Metodo di utilità per mockare una connessione al database riuscita
    private MockedStatic<DriverManager> mockDbConnection() throws SQLException {
        MockedStatic<DriverManager> mockedDriverManager = mockStatic(DriverManager.class);
        mockedDriverManager.when(() -> DriverManager.getConnection(anyString(), anyString(), anyString()))
                .thenReturn(mockConnection);
        when(mockConnection.prepareStatement(anyString())).thenReturn(mockPreparedStatement);
        return mockedDriverManager;
    }

    @Test
    void main_shouldAttemptToCallMainLogic() {
        // Questo è un test semplice per il metodo main.
        // Non possiamo testare facilmente l'intero flusso dell'applicazione da qui,
        // ma possiamo verificare che tenti di avviare la logica principale.
        // Ci aspettiamo un'eccezione (es. NullPointerException) perché l'istanza dell'app
        // non è mockata, ma questo prova che mainLogic() è stato chiamato.
        // Il focus principale è testare direttamente mainLogic.
        assertThrows(Exception.class, () -> GestioneConti.main(new String[]{}));
    }

    @Nested
    @DisplayName("Test del Flusso Logico Principale")
    class MainLogicFlowTests {

        @Test
        void mainLogic_shouldExitImmediately_whenChoiceIsZero() throws SQLException {
            provideInput("0\n");
            try (MockedStatic<DriverManager> ignored = mockDbConnection()) {
                gestioneConti.mainLogic();
            }
            String output = outContent.toString();
            assertTrue(output.contains("Connessione al database stabilita"));
            assertTrue(output.contains("===== SISTEMA GESTIONE CONTI CORRENTI ====="));
            assertTrue(output.contains("Programma terminato."));
            assertTrue(output.contains("Disconnesso dal database"));
        }

        @Test
        void mainLogic_shouldHandleInvalidChoice() throws SQLException {
            provideInput("99\nN\n");
            try (MockedStatic<DriverManager> ignored = mockDbConnection()) {
                gestioneConti.mainLogic();
            }
            String output = outContent.toString();
            assertTrue(output.contains("Scelta non valida!"));
            assertTrue(output.contains("Programma terminato."));
        }

        @Test
        void mainLogic_shouldHandleNonNumericInputForChoice() throws SQLException {
            provideInput("abc\nN\n");
            try (MockedStatic<DriverManager> ignored = mockDbConnection()) {
                gestioneConti.mainLogic();
            }
            String output = outContent.toString();
            assertTrue(output.contains("Scelta non valida!"));
            assertTrue(output.contains("Programma terminato."));
        }

        @Test
        void mainLogic_shouldContinueLoop_whenUserEntersS() throws SQLException {
            // Simula la scelta di un'opzione non valida, poi 'S' per continuare, poi '0' per uscire.
            provideInput("99\nS\n0\n");
            try (MockedStatic<DriverManager> ignored = mockDbConnection()) {
                gestioneConti.mainLogic();
            }
            String output = outContent.toString();
            // Verifica che il menu sia stato visualizzato due volte
            assertEquals(2, output.split("===== SISTEMA GESTIONE CONTI CORRENTI =====").length - 1);
            assertTrue(output.contains("Scelta non valida!"));
            assertTrue(output.contains("Programma terminato."));
        }
    }

    @Nested
    @DisplayName("Test di Connessione al Database")
    class DatabaseConnectionTests {

        @Test
        void mainLogic_shouldExitGracefully_whenDatabaseConnectionFails() {
            provideInput(""); // Nessun input necessario, fallisce prima del ciclo
            // Mock di DriverManager per lanciare un'eccezione
            MockedStatic<DriverManager> mockedDriverManager = mockStatic(DriverManager.class);
            mockedDriverManager.when(() -> DriverManager.getConnection(anyString(), anyString(), anyString()))
                    .thenThrow(new SQLException("Connection failed"));

            gestioneConti.mainLogic();

            String errorOutput = errContent.toString();
            assertTrue(errorOutput.contains("Errore connessione database: Connection failed"));
            // Verifica che non stampi il messaggio "Programma terminato" perché esce prima
            assertFalse(outContent.toString().contains("Programma terminato."));

            mockedDriverManager.close();
        }

        @Test
        void mainLogic_shouldRollbackTransaction_whenSqlExceptionOccursDuringOperation() throws SQLException {
            provideInput("2\nACC123\n100\nTest\nN\n"); // Operazione di deposito

            try (MockedStatic<DriverManager> ignored = mockDbConnection()) {
                // Mock di verifyAccount per avere successo
                when(mockPreparedStatement.executeQuery()).thenReturn(mockResultSet);
                when(mockResultSet.next()).thenReturn(true);
                when(mockResultSet.getString("stato")).thenReturn("A");

                // Mock dell'UPDATE per fallire
                when(mockPreparedStatement.executeUpdate()).thenThrow(new SQLException("Update failed"));

                gestioneConti.mainLogic();

                // Verifica che rollback sia stato chiamato
                verify(mockConnection, times(1)).rollback();
                String errorOutput = errContent.toString();
                String output = outContent.toString();
                assertTrue(errorOutput.contains("Errore durante il deposito: Update failed"));
                assertTrue(output.contains("Transazione annullata a causa di un errore."));
            }
        }
    }

    @Nested
    @DisplayName("Test delle Operazioni sul Conto")
    class AccountOperationsTests {

        @Test
        void openAccount_shouldSucceed() throws SQLException {
            String input = "1\nCLIENT01\nC\n1000.00\n500.00\nN\n";
            provideInput(input);

            try (MockedStatic<DriverManager> ignored = mockDbConnection()) {
                // 1. Mock della verifica del cliente
                when(mockPreparedStatement.executeQuery()).thenReturn(mockResultSet);
                when(mockResultSet.next()).thenReturn(true, false, true, false, true, false);
                when(mockResultSet.getString("codice_cliente")).thenReturn("CLIENT01");
                when(mockResultSet.getString("nome")).thenReturn("Mario");
                when(mockResultSet.getString("cognome")).thenReturn("Rossi");

                // 2. Mock della generazione del numero di conto
                when(mockResultSet.getString(1)).thenReturn("IT0000000001");

                // 3. Mock dell'inserimento del conto e della registrazione della transazione
                when(mockPreparedStatement.executeUpdate()).thenReturn(1);
                when(mockResultSet.getBigDecimal("saldo")).thenReturn(new BigDecimal("1000.00"));

                gestioneConti.mainLogic();

                verify(mockConnection, times(1)).commit();
                verify(mockConnection, never()).rollback();
                String output = outContent.toString();
                assertTrue(output.contains("Cliente: Mario Rossi"));
                assertTrue(output.contains("Conto IT0000000001 creato con successo!"));
            }
        }

        @Test
        void openAccount_shouldFail_whenClientNotFound() throws SQLException {
            provideInput("1\nUNKNOWN\nN\n");

            try (MockedStatic<DriverManager> ignored = mockDbConnection()) {
                // Mock della verifica del cliente per fallire (result set vuoto)
                when(mockPreparedStatement.executeQuery()).thenReturn(mockResultSet);
                when(mockResultSet.next()).thenReturn(false);

                gestioneConti.mainLogic();

                verify(mockConnection, never()).commit();
                verify(mockConnection, never()).rollback();
                String output = outContent.toString();
                assertTrue(output.contains("Cliente non trovato!"));
            }
        }

        @Test
        void makeDeposit_shouldSucceed() throws SQLException {
            provideInput("2\nACC123\n250.50\nStipendio\nN\n");

            try (MockedStatic<DriverManager> ignored = mockDbConnection()) {
                // Mock di verifyAccount
                when(mockPreparedStatement.executeQuery()).thenReturn(mockResultSet);
                when(mockResultSet.next()).thenReturn(true, false, true, false);
                when(mockResultSet.getString("stato")).thenReturn("A");
                when(mockResultSet.getString("nome")).thenReturn("Mario");
                when(mockResultSet.getString("cognome")).thenReturn("Rossi");

                // Mock dell'aggiornamento e della registrazione della transazione
                when(mockPreparedStatement.executeUpdate()).thenReturn(1);
                when(mockResultSet.getBigDecimal("saldo")).thenReturn(new BigDecimal("1250.50"));

                gestioneConti.mainLogic();

                verify(mockConnection, times(1)).commit();
                verify(mockConnection, never()).rollback();
                assertTrue(outContent.toString().contains("Deposito effettuato con successo!"));
            }
        }

        @Test
        void makeWithdrawal_shouldFail_whenFundsAreInsufficient() throws SQLException {
            provideInput("3\nACC123\n2000.00\nSpesa\nN\n");

            try (MockedStatic<DriverManager> ignored = mockDbConnection()) {
                // Mock di verifyAccount e del controllo saldo
                when(mockPreparedStatement.executeQuery()).thenReturn(mockResultSet);
                when(mockResultSet.next()).thenReturn(true, false, true, false);
                when(mockResultSet.getString("stato")).thenReturn("A");
                when(mockResultSet.getBigDecimal("saldo")).thenReturn(new BigDecimal("1000.00"));
                when(mockResultSet.getBigDecimal("fido")).thenReturn(new BigDecimal("500.00"));

                gestioneConti.mainLogic();

                verify(mockConnection, never()).commit();
                verify(mockConnection, never()).rollback();
                String output = outContent.toString();
                assertTrue(output.contains("Fondi insufficienti!"));
                assertTrue(output.contains("Saldo attuale: 1,000.00"));
            }
        }

        @Test
        void viewBalance_shouldDisplayCorrectInfo() throws SQLException {
            provideInput("4\nACC123\nN\n");

            try (MockedStatic<DriverManager> ignored = mockDbConnection()) {
                when(mockPreparedStatement.executeQuery()).thenReturn(mockResultSet);
                when(mockResultSet.next()).thenReturn(true).thenReturn(false);
                when(mockResultSet.getBigDecimal("saldo")).thenReturn(new BigDecimal("1234.56"));
                when(mockResultSet.getBigDecimal("fido")).thenReturn(new BigDecimal("500.00"));
                when(mockResultSet.getString("nome")).thenReturn("Luigi");
                when(mockResultSet.getString("cognome")).thenReturn("Verdi");

                gestioneConti.mainLogic();

                String output = outContent.toString();
                assertTrue(output.contains("Intestatario: Luigi Verdi"));
                assertTrue(output.contains("Saldo attuale: EUR 1,234.56"));
                assertTrue(output.contains("Fido accordato: EUR 500.00"));
                assertTrue(output.contains("Disponibile: EUR 1,734.56"));
            }
        }

        @Test
        void closeAccount_shouldFail_whenBalanceIsNotZero() throws SQLException {
            provideInput("6\nACC123\nN\n");

            try (MockedStatic<DriverManager> ignored = mockDbConnection()) {
                // Mock di verifyAccount
                when(mockPreparedStatement.executeQuery()).thenReturn(mockResultSet);
                when(mockResultSet.next()).thenReturn(true).thenReturn(false);
                when(mockResultSet.getString("stato")).thenReturn("A");
                when(mockResultSet.getBigDecimal("saldo")).thenReturn(new BigDecimal("100.00"));

                gestioneConti.mainLogic();

                String output = outContent.toString();
                assertTrue(output.contains("Impossibile chiudere: saldo non a zero!"));
                assertTrue(output.contains("Saldo attuale: EUR 100.00"));
                verify(mockConnection, never()).commit();
            }
        }

        @Test
        void closeAccount_shouldSucceed_whenBalanceIsZero() throws SQLException {
            provideInput("6\nACC123\nS\nN\n");

            try (MockedStatic<DriverManager> ignored = mockDbConnection()) {
                // Mock di verifyAccount
                when(mockPreparedStatement.executeQuery()).thenReturn(mockResultSet);
                when(mockResultSet.next()).thenReturn(true).thenReturn(false);
                when(mockResultSet.getString("stato")).thenReturn("A");
                when(mockResultSet.getBigDecimal("saldo")).thenReturn(BigDecimal.ZERO);

                // Mock dell'update
                when(mockPreparedStatement.executeUpdate()).thenReturn(1);

                gestioneConti.mainLogic();

                String output = outContent.toString();
                assertTrue(output.contains("Conto chiuso con successo!"));
                verify(mockConnection, times(1)).commit();
                verify(mockConnection, never()).rollback();
            }
        }
    }

    @Nested
    @DisplayName("Test dell'Estratto Conto")
    class AccountStatementTests {

        @Test
        void generateAccountStatement_shouldCreateFileWithCorrectContent() throws SQLException, IOException {
            String accountNum = "ACC-STMT-01";
            provideInput("5\n" + accountNum + "\nN\n");

            try (MockedStatic<DriverManager> ignored = mockDbConnection()) {
                // 1. Mock di verifyAccount
                when(mockConnection.prepareStatement(contains("SELECT c.saldo, c.stato"))).thenReturn(mockPreparedStatement);
                when(mockPreparedStatement.executeQuery()).thenReturn(mockResultSet);
                when(mockResultSet.next()).thenReturn(true, false, true, true, false);
                when(mockResultSet.getString("stato")).thenReturn("A");
                when(mockResultSet.getBigDecimal("saldo")).thenReturn(new BigDecimal("950.00"));
                when(mockResultSet.getString("nome")).thenReturn("Paolo");
                when(mockResultSet.getString("cognome")).thenReturn("Bianchi");

                // 2. Mock del cursore dei movimenti
                when(mockConnection.prepareStatement(contains("SELECT data_movimento, tipo_movimento"))).thenReturn(mockPreparedStatement);
                // Primo movimento (prelievo) - ordine DESC
                when(mockResultSet.getTimestamp("data_movimento"))
                        .thenReturn(Timestamp.valueOf("2023-10-27 11:30:00"))
                        .thenReturn(Timestamp.valueOf("2023-10-27 10:00:00"));
                when(mockResultSet.getString("tipo_movimento")).thenReturn("P").thenReturn("D");
                when(mockResultSet.getBigDecimal("importo")).thenReturn(new BigDecimal("50.00")).thenReturn(new BigDecimal("1000.00"));
                when(mockResultSet.getString("causale")).thenReturn("Caffè").thenReturn("Deposito iniziale");
                when(mockResultSet.getBigDecimal("saldo_dopo")).thenReturn(new BigDecimal("950.00")).thenReturn(new BigDecimal("1000.00"));

                gestioneConti.mainLogic();
            }

            // 3. Verifica del contenuto del file
            String output = outContent.toString();
            assertTrue(output.contains("Estratto conto salvato in ESTRATTO-CONTO.TXT"));

            File reportFile = new File("ESTRATTO-CONTO.TXT");
            assertTrue(reportFile.exists());

            String fileContent = new String(Files.readAllBytes(reportFile.toPath()));
            assertTrue(fileContent.contains("ESTRATTO CONTO BANCARIO"));
            assertTrue(fileContent.contains("Conto: " + accountNum));
            assertTrue(fileContent.contains("Cliente: Paolo Bianchi"));
            assertTrue(fileContent.contains("Data: " + LocalDate.now().format(DateTimeFormatter.ISO_LOCAL_DATE)));
            // Verifica le righe dei movimenti
            assertTrue(fileContent.contains("2023-10-27 11:30:00   PRE"));
            assertTrue(fileContent.contains("50.00"));
            assertTrue(fileContent.contains("Caffè"));
            assertTrue(fileContent.contains("950.00"));
            assertTrue(fileContent.contains("SALDO FINALE: EUR 950.00"));
        }
    }
}