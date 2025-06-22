import org.junit.jupiter.api.*;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.io.*;
import java.lang.reflect.Field;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.sql.*;
import java.util.Scanner;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.*;

/**
 * Unit test completi per la classe GestioneConti.
 * Utilizza JUnit 5 e Mockito per isolare la logica di business dalle dipendenze esterne
 * come il database e l'input dell'utente.
 */
@ExtendWith(MockitoExtension.class)
class GestioneContiTest {

    private GestioneConti gestioneConti;

    @Mock
    private Connection mockConnection;
    @Mock
    private PreparedStatement mockStatement;
    @Mock
    private ResultSet mockResultSet;
    @Mock
    private Scanner mockScanner;

    private final ByteArrayOutputStream outContent = new ByteArrayOutputStream();
    private final ByteArrayOutputStream errContent = new ByteArrayOutputStream();
    private final PrintStream originalOut = System.out;
    private final PrintStream originalErr = System.err;

    private static final String TEST_ACCOUNT_NUMBER = "IT0000000001";
    private static final String TEST_CLIENT_CODE = "CL001";
    private static final String REPORT_FILE_NAME = "ESTRATTO-CONTO.TXT";

    @BeforeEach
    void setUp() throws Exception {
        // Redirige l'output standard e di errore per poterli asserire nei test
        System.setOut(new PrintStream(outContent));
        System.setErr(new PrintStream(errContent));

        gestioneConti = new GestioneConti();

        // Inietta i mock nelle dipendenze private della classe usando la reflection,
        // poiché non sono iniettabili tramite costruttore.
        injectMock("connection", mockConnection);
        injectMock("scanner", mockScanner);

        // Configurazione di base per i mock del database
        when(mockConnection.prepareStatement(anyString())).thenReturn(mockStatement);
    }

    private void injectMock(String fieldName, Object mock) throws NoSuchFieldException, IllegalAccessException {
        Field field = GestioneConti.class.getDeclaredField(fieldName);
        field.setAccessible(true);
        field.set(gestioneConti, mock);
    }

    @AfterEach
    void tearDown() throws IOException {
        // Ripristina gli stream di output originali
        System.setOut(originalOut);
        System.setErr(originalErr);
        // Pulisce il file di report se è stato creato
        Files.deleteIfExists(Paths.get(REPORT_FILE_NAME));
    }

    @Nested
    @DisplayName("Test per l'apertura di un conto (openAccount)")
    class OpenAccountTests {

        @Test
        void openAccount_shouldCreateAccountSuccessfully_whenClientExistsAndDataIsValid() throws SQLException {
            // Arrange: Simula l'input dell'utente e le risposte del DB
            when(mockScanner.nextLine())
                    .thenReturn(TEST_CLIENT_CODE) // Codice cliente
                    .thenReturn("C") // Tipo conto
                    .thenReturn("1000.00") // Saldo iniziale
                    .thenReturn("200.00"); // Fido

            // 1. Verifica cliente
            when(mockConnection.prepareStatement(contains("SELECT codice_cliente, nome, cognome FROM CLIENTI"))).thenReturn(mockStatement);
            when(mockStatement.executeQuery()).thenReturn(mockResultSet);
            when(mockResultSet.next()).thenReturn(true); // Cliente trovato
            when(mockResultSet.getString("codice_cliente")).thenReturn(TEST_CLIENT_CODE);
            when(mockResultSet.getString("nome")).thenReturn("Mario");
            when(mockResultSet.getString("cognome")).thenReturn("Rossi");

            // 2. Generazione numero conto
            when(mockConnection.prepareStatement(contains("SELECT 'IT' || LPAD"))).thenReturn(mockStatement);
            when(mockResultSet.getString(1)).thenReturn(TEST_ACCOUNT_NUMBER);

            // 3. Inserimento conto
            when(mockConnection.prepareStatement(contains("INSERT INTO CONTI"))).thenReturn(mockStatement);

            // 4. Registrazione movimento (chiamata da recordTransaction)
            when(mockConnection.prepareStatement(contains("SELECT saldo FROM CONTI"))).thenReturn(mockStatement);
            when(mockResultSet.getBigDecimal("saldo")).thenReturn(new BigDecimal("1000.00"));
            when(mockConnection.prepareStatement(contains("INSERT INTO MOVIMENTI"))).thenReturn(mockStatement);

            // Act
            gestioneConti.openAccount();

            // Assert
            String output = outContent.toString();
            assertTrue(output.contains("Cliente: Mario Rossi"));
            assertTrue(output.contains("Conto " + TEST_ACCOUNT_NUMBER + " creato con successo!"));
            verify(mockConnection, times(1)).commit();
            verify(mockConnection, never()).rollback();
        }

        @Test
        void openAccount_shouldFail_whenClientNotFound() throws SQLException {
            // Arrange
            when(mockScanner.nextLine()).thenReturn("CL999"); // Codice cliente non esistente
            when(mockStatement.executeQuery()).thenReturn(mockResultSet);
            when(mockResultSet.next()).thenReturn(false); // Cliente non trovato

            // Act
            gestioneConti.openAccount();

            // Assert
            assertTrue(outContent.toString().contains("Cliente non trovato!"));
            verify(mockConnection, never()).commit();
            verify(mockConnection, never()).rollback();
        }

        @Test
        void openAccount_shouldRollback_whenSqlExceptionOccurs() throws SQLException {
            // Arrange
            when(mockScanner.nextLine())
                    .thenReturn(TEST_CLIENT_CODE)
                    .thenReturn("C")
                    .thenReturn("1000.00")
                    .thenReturn("200.00");

            when(mockStatement.executeQuery()).thenReturn(mockResultSet);
            when(mockResultSet.next()).thenReturn(true); // Cliente trovato
            when(mockResultSet.getString(anyString())).thenReturn("SomeString");

            // Simula un errore durante l'inserimento del conto
            when(mockConnection.prepareStatement(contains("INSERT INTO CONTI"))).thenThrow(new SQLException("DB Error"));

            // Act
            gestioneConti.openAccount();

            // Assert
            assertTrue(errContent.toString().contains("Errore creazione conto: DB Error"));
            assertTrue(outContent.toString().contains("Transazione annullata a causa di un errore."));
            verify(mockConnection, times(1)).rollback();
            verify(mockConnection, never()).commit();
        }
    }

    @Nested
    @DisplayName("Test per il deposito (makeDeposit)")
    class DepositTests {

        @Test
        void makeDeposit_shouldSucceed_whenAccountIsValidAndAmountIsPositive() throws SQLException {
            // Arrange
            when(mockScanner.nextLine())
                    .thenReturn(TEST_ACCOUNT_NUMBER) // Numero conto
                    .thenReturn("500.00") // Importo
                    .thenReturn("Stipendio"); // Causale

            // Mock per verifyAccount
            setupVerifyAccount(true, "A", new BigDecimal("1000.00"));
            // Mock per l'update del saldo
            when(mockStatement.executeUpdate()).thenReturn(1); // 1 riga aggiornata
            // Mock per recordTransaction
            setupRecordTransaction(new BigDecimal("1500.00"));

            // Act
            gestioneConti.makeDeposit();

            // Assert
            assertTrue(outContent.toString().contains("Deposito effettuato con successo!"));
            verify(mockConnection, times(1)).commit();
            verify(mockConnection, never()).rollback();
        }

        @Test
        void makeDeposit_shouldFail_whenAccountIsNotFound() throws SQLException {
            // Arrange
            when(mockScanner.nextLine()).thenReturn(TEST_ACCOUNT_NUMBER);
            setupVerifyAccount(false, null, null); // Conto non trovato

            // Act
            gestioneConti.makeDeposit();

            // Assert
            assertTrue(outContent.toString().contains("Conto non trovato!"));
            verify(mockConnection, never()).commit();
            verify(mockConnection, never()).rollback();
        }

        @Test
        void makeDeposit_shouldFail_whenAmountIsNegative() throws SQLException {
            // Arrange
            when(mockScanner.nextLine())
                    .thenReturn(TEST_ACCOUNT_NUMBER)
                    .thenReturn("-100.00"); // Importo non valido
            setupVerifyAccount(true, "A", new BigDecimal("1000.00"));

            // Act
            gestioneConti.makeDeposit();

            // Assert
            assertTrue(outContent.toString().contains("Importo non valido!"));
            verify(mockConnection, never()).commit();
        }
    }

    @Nested
    @DisplayName("Test per il prelievo (makeWithdrawal)")
    class WithdrawalTests {

        @Test
        void makeWithdrawal_shouldSucceed_whenFundsAreSufficient() throws SQLException {
            // Arrange
            when(mockScanner.nextLine())
                    .thenReturn(TEST_ACCOUNT_NUMBER)
                    .thenReturn("300.00") // Importo prelievo
                    .thenReturn("Spese varie"); // Causale

            setupVerifyAccount(true, "A", new BigDecimal("1000.00"));
            // Mock per il check del saldo e fido
            when(mockConnection.prepareStatement(contains("SELECT saldo, fido FROM CONTI"))).thenReturn(mockStatement);
            when(mockStatement.executeQuery()).thenReturn(mockResultSet);
            when(mockResultSet.next()).thenReturn(true);
            when(mockResultSet.getBigDecimal("saldo")).thenReturn(new BigDecimal("1000.00"));
            when(mockResultSet.getBigDecimal("fido")).thenReturn(new BigDecimal("200.00"));
            // Mock per l'update
            when(mockStatement.executeUpdate()).thenReturn(1);
            // Mock per la registrazione
            setupRecordTransaction(new BigDecimal("700.00"));

            // Act
            gestioneConti.makeWithdrawal();

            // Assert
            assertTrue(outContent.toString().contains("Prelievo effettuato con successo!"));
            verify(mockConnection, times(1)).commit();
        }

        @Test
        void makeWithdrawal_shouldFail_whenFundsAreInsufficient() throws SQLException {
            // Arrange
            when(mockScanner.nextLine())
                    .thenReturn(TEST_ACCOUNT_NUMBER)
                    .thenReturn("1300.00"); // Importo > saldo + fido

            setupVerifyAccount(true, "A", new BigDecimal("1000.00"));
            when(mockConnection.prepareStatement(contains("SELECT saldo, fido FROM CONTI"))).thenReturn(mockStatement);
            when(mockStatement.executeQuery()).thenReturn(mockResultSet);
            when(mockResultSet.next()).thenReturn(true);
            when(mockResultSet.getBigDecimal("saldo")).thenReturn(new BigDecimal("1000.00"));
            when(mockResultSet.getBigDecimal("fido")).thenReturn(new BigDecimal("200.00"));

            // Act
            gestioneConti.makeWithdrawal();

            // Assert
            assertTrue(outContent.toString().contains("Fondi insufficienti!"));
            verify(mockConnection, never()).commit();
            verify(mockConnection, never()).rollback();
        }
    }

    @Nested
    @DisplayName("Test per la visualizzazione del saldo (viewBalance)")
    class ViewBalanceTests {

        @Test
        void viewBalance_shouldDisplayCorrectInfo_whenAccountExists() throws SQLException {
            // Arrange
            when(mockScanner.nextLine()).thenReturn(TEST_ACCOUNT_NUMBER);
            when(mockConnection.prepareStatement(contains("SELECT c.saldo, c.fido, cl.nome, cl.cognome"))).thenReturn(mockStatement);
            when(mockStatement.executeQuery()).thenReturn(mockResultSet);
            when(mockResultSet.next()).thenReturn(true);
            when(mockResultSet.getBigDecimal("saldo")).thenReturn(new BigDecimal("1234.56"));
            when(mockResultSet.getBigDecimal("fido")).thenReturn(new BigDecimal("500.00"));
            when(mockResultSet.getString("nome")).thenReturn("Mario");
            when(mockResultSet.getString("cognome")).thenReturn("Rossi");

            // Act
            gestioneConti.viewBalance();

            // Assert
            String output = outContent.toString();
            assertTrue(output.contains("Intestatario: Mario Rossi"));
            assertTrue(output.contains("Saldo attuale: EUR 1,234.56"));
            assertTrue(output.contains("Fido accordato: EUR 500.00"));
            assertTrue(output.contains("Disponibile: EUR 1,734.56"));
        }

        @Test
        void viewBalance_shouldShowError_whenAccountNotFound() throws SQLException {
            // Arrange
            when(mockScanner.nextLine()).thenReturn(TEST_ACCOUNT_NUMBER);
            when(mockConnection.prepareStatement(anyString())).thenReturn(mockStatement);
            when(mockStatement.executeQuery()).thenReturn(mockResultSet);
            when(mockResultSet.next()).thenReturn(false); // Conto non trovato

            // Act
            gestioneConti.viewBalance();

            // Assert
            assertTrue(outContent.toString().contains("Conto non trovato o non attivo!"));
        }
    }

    @Nested
    @DisplayName("Test per la generazione dell'estratto conto (generateAccountStatement)")
    class AccountStatementTests {

        @Test
        void generateAccountStatement_shouldCreateFileWithCorrectContent() throws SQLException, IOException {
            // Arrange
            when(mockScanner.nextLine()).thenReturn(TEST_ACCOUNT_NUMBER);
            setupVerifyAccount(true, "A", new BigDecimal("950.00"), "Luigi", "Verdi");

            // Mock per il cursore dei movimenti
            when(mockConnection.prepareStatement(contains("SELECT data_movimento, tipo_movimento, importo, causale, saldo_dopo"))).thenReturn(mockStatement);
            when(mockStatement.executeQuery()).thenReturn(mockResultSet);
            // Simula due movimenti
            when(mockResultSet.next()).thenReturn(true, true, false);
            when(mockResultSet.getTimestamp("data_movimento"))
                    .thenReturn(Timestamp.valueOf("2023-10-27 10:00:00"))
                    .thenReturn(Timestamp.valueOf("2023-10-26 15:30:00"));
            when(mockResultSet.getString("tipo_movimento")).thenReturn("P").thenReturn("D");
            when(mockResultSet.getBigDecimal("importo"))
                    .thenReturn(new BigDecimal("50.00"))
                    .thenReturn(new BigDecimal("1000.00"));
            when(mockResultSet.getString("causale")).thenReturn("Prelievo ATM").thenReturn("Deposito iniziale");
            when(mockResultSet.getBigDecimal("saldo_dopo"))
                    .thenReturn(new BigDecimal("950.00"))
                    .thenReturn(new BigDecimal("1000.00"));

            // Act
            gestioneConti.generateAccountStatement();

            // Assert
            File reportFile = new File(REPORT_FILE_NAME);
            assertTrue(reportFile.exists());
            assertTrue(outContent.toString().contains("Estratto conto salvato in " + REPORT_FILE_NAME));

            String fileContent = new String(Files.readAllBytes(reportFile.toPath()));
            assertTrue(fileContent.contains("Conto: " + TEST_ACCOUNT_NUMBER));
            assertTrue(fileContent.contains("Cliente: Luigi Verdi"));
            assertTrue(fileContent.contains("2023-10-27 10:00:00   PRE"));
            assertTrue(fileContent.contains("50.00"));
            assertTrue(fileContent.contains("Prelievo ATM"));
            assertTrue(fileContent.contains("950.00"));
            assertTrue(fileContent.contains("SALDO FINALE: EUR 950.00"));
        }
    }

    @Nested
    @DisplayName("Test per la chiusura del conto (closeAccount)")
    class CloseAccountTests {

        @Test
        void closeAccount_shouldSucceed_whenBalanceIsZeroAndUserConfirms() throws SQLException {
            // Arrange
            when(mockScanner.nextLine())
                    .thenReturn(TEST_ACCOUNT_NUMBER) // Numero conto
                    .thenReturn("S"); // Conferma

            setupVerifyAccount(true, "A", BigDecimal.ZERO);
            when(mockStatement.executeUpdate()).thenReturn(1);

            // Act
            gestioneConti.closeAccount();

            // Assert
            assertTrue(outContent.toString().contains("Conto chiuso con successo!"));
            verify(mockConnection, times(1)).commit();
            verify(mockConnection, never()).rollback();
        }

        @Test
        void closeAccount_shouldFail_whenBalanceIsNotZero() throws SQLException {
            // Arrange
            when(mockScanner.nextLine()).thenReturn(TEST_ACCOUNT_NUMBER);
            setupVerifyAccount(true, "A", new BigDecimal("100.00"));

            // Act
            gestioneConti.closeAccount();

            // Assert
            String output = outContent.toString();
            assertTrue(output.contains("Impossibile chiudere: saldo non a zero!"));
            assertTrue(output.contains("Saldo attuale: EUR 100.00"));
            verify(mockConnection, never()).commit();
        }

        @Test
        void closeAccount_shouldBeCancelled_whenUserSelectsNo() throws SQLException {
            // Arrange
            when(mockScanner.nextLine())
                    .thenReturn(TEST_ACCOUNT_NUMBER)
                    .thenReturn("N"); // Annulla

            setupVerifyAccount(true, "A", BigDecimal.ZERO);

            // Act
            gestioneConti.closeAccount();

            // Assert
            assertTrue(outContent.toString().contains("Chiusura annullata."));
            verify(mockStatement, never()).executeUpdate();
            verify(mockConnection, never()).commit();
        }
    }

    // --- METODI HELPER PER I TEST ---

    /**
     * Configura il mock per il metodo privato verifyAccount.
     * @param found Se il conto deve essere trovato.
     * @param status Lo stato del conto ('A', 'C', etc.).
     * @param balance Il saldo del conto.
     */
    private void setupVerifyAccount(boolean found, String status, BigDecimal balance) throws SQLException {
        setupVerifyAccount(found, status, balance, "Mario", "Rossi");
    }

    private void setupVerifyAccount(boolean found, String status, BigDecimal balance, String nome, String cognome) throws SQLException {
        when(mockConnection.prepareStatement(contains("SELECT c.saldo, c.stato, cl.nome, cl.cognome"))).thenReturn(mockStatement);
        when(mockStatement.executeQuery()).thenReturn(mockResultSet);
        if (found) {
            when(mockResultSet.next()).thenReturn(true);
            when(mockResultSet.getBigDecimal("saldo")).thenReturn(balance);
            when(mockResultSet.getString("stato")).thenReturn(status);
            when(mockResultSet.getString("nome")).thenReturn(nome);
            when(mockResultSet.getString("cognome")).thenReturn(cognome);
        } else {
            when(mockResultSet.next()).thenReturn(false);
        }
    }

    /**
     * Configura il mock per il metodo privato recordTransaction.
     * @param finalBalance Il saldo finale da registrare nel movimento.
     */
    private void setupRecordTransaction(BigDecimal finalBalance) throws SQLException {
        // 1. Mock per il recupero del saldo aggiornato
        PreparedStatement selectSaldoStmt = mock(PreparedStatement.class);
        ResultSet saldoRs = mock(ResultSet.class);
        when(mockConnection.prepareStatement(contains("SELECT saldo FROM CONTI"))).thenReturn(selectSaldoStmt);
        when(selectSaldoStmt.executeQuery()).thenReturn(saldoRs);
        when(saldoRs.next()).thenReturn(true);
        when(saldoRs.getBigDecimal("saldo")).thenReturn(finalBalance);

        // 2. Mock per l'inserimento del movimento
        PreparedStatement insertMovStmt = mock(PreparedStatement.class);
        when(mockConnection.prepareStatement(contains("INSERT INTO MOVIMENTI"))).thenReturn(insertMovStmt);
        when(insertMovStmt.executeUpdate()).thenReturn(1);
    }
}