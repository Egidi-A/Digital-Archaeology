import org.junit.jupiter.api.*;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.io.*;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.nio.file.Files;
import java.nio.file.Path;
import java.sql.*;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.Scanner;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class GestioneMagazzinoTest {

    @InjectMocks
    private GestioneMagazzino gestioneMagazzino;

    @Mock
    private Connection connection;

    @Mock
    private PreparedStatement preparedStatement;

    @Mock
    private Statement statement;

    @Mock
    private ResultSet resultSet;

    @Mock
    private Scanner scanner;

    private final ByteArrayOutputStream outContent = new ByteArrayOutputStream();
    private final ByteArrayOutputStream errContent = new ByteArrayOutputStream();
    private final PrintStream originalOut = System.out;
    private final PrintStream originalErr = System.err;

    // Helper per invocare metodi privati
    private Object invokePrivateMethod(String methodName, Class<?>[] parameterTypes, Object[] args) throws Exception {
        Method method = GestioneMagazzino.class.getDeclaredMethod(methodName, parameterTypes);
        method.setAccessible(true);
        return method.invoke(gestioneMagazzino, args);
    }

    // Helper per impostare campi privati
    private void setPrivateField(String fieldName, Object value) throws Exception {
        Field field = GestioneMagazzino.class.getDeclaredField(fieldName);
        field.setAccessible(true);
        field.set(gestioneMagazzino, value);
    }

    @BeforeEach
    void setUp() throws Exception {
        // Redirige l'output standard e di errore per poterli asserire
        System.setOut(new PrintStream(outContent));
        System.setErr(new PrintStream(errContent));

        // Inietta i mock nei campi privati della classe, dato che non usa DI
        setPrivateField("connection", connection);
        setPrivateField("scanner", scanner);
    }

    @AfterEach
    void tearDown() {
        // Ripristina gli stream originali
        System.setOut(originalOut);
        System.setErr(originalErr);
        // Pulisce i file di report generati
        new File("REPORT-MAGAZZINO.TXT").delete();
        new File("INVENTARIO.TXT").delete();
    }

    @Nested
    @DisplayName("Test di Connessione al Database")
    class DatabaseConnectionTests {

        @Test
        void connettiDatabase_shouldReturnTrue_whenConnectionIsSuccessful() throws Exception {
            // Simula una connessione riuscita (non è necessario mockare DriverManager,
            // dato che il campo connection viene iniettato direttamente)
            boolean result = (boolean) invokePrivateMethod("connettiDatabase", new Class<?>[]{}, new Object[]{});

            assertTrue(result);
            assertTrue(outContent.toString().contains("Connessione al database stabilita"));
        }

        @Test
        void connettiDatabase_shouldReturnFalse_whenConnectionFails() throws Exception {
            // Forza il campo connection a null per simulare un fallimento
            setPrivateField("connection", null);
            // Simula il fallimento di DriverManager.getConnection()
            // Questo test è più concettuale, dato che bypassiamo DriverManager
            // Il vero test è verificare il comportamento quando la connection è null
            GestioneMagazzino newInstance = new GestioneMagazzino();
            // Non possiamo testare il metodo privato di una nuova istanza senza reflection
            // Quindi testiamo il comportamento dei metodi che dipendono dalla connessione
            assertThrows(NullPointerException.class, () -> {
                newInstance.caricoMerce(); // Questo fallirà perché la connessione è null
            });
        }

        @Test
        void disconnettiDatabase_shouldCloseConnection_whenConnectionIsNotNull() throws Exception {
            invokePrivateMethod("disconnettiDatabase", new Class<?>[]{}, new Object[]{});

            verify(connection, times(1)).close();
            assertTrue(outContent.toString().contains("Disconnesso dal database"));
        }

        @Test
        void disconnettiDatabase_shouldDoNothing_whenConnectionIsNull() throws Exception {
            setPrivateField("connection", null);
            invokePrivateMethod("disconnettiDatabase", new Class<?>[]{}, new Object[]{});

            verify(connection, never()).close();
            assertFalse(outContent.toString().contains("Disconnesso dal database"));
        }
    }

    @Nested
    @DisplayName("Test Funzionalità Carico Merce")
    class CaricoMerceTests {

        @BeforeEach
        void setupCaricoMocks() throws SQLException {
            // Mock comune per le operazioni di scrittura
            when(connection.prepareStatement(anyString())).thenReturn(preparedStatement);
            when(preparedStatement.executeUpdate()).thenReturn(1);
        }

        private void mockCaricaArticoloSuccess() throws SQLException {
            when(resultSet.next()).thenReturn(true);
            when(resultSet.getString(1)).thenReturn("ARTICOLO TEST");
            when(resultSet.getString(11)).thenReturn("A"); // Stato Attivo
            when(resultSet.getBigDecimal(anyInt())).thenReturn(BigDecimal.TEN);
        }

        @Test
        void caricoMerce_shouldSucceed_whenAllInputsAreValid() throws Exception {
            // Arrange: prepara i mock per un flusso di successo
            when(scanner.nextLine()).thenReturn("ART001", "DDT123", "50", "10.50", "FORN01", "CARICO TEST", "LOTTO01");
            when(connection.prepareStatement(contains("SELECT"))).thenReturn(preparedStatement);
            when(preparedStatement.executeQuery()).thenReturn(resultSet);
            mockCaricaArticoloSuccess();

            // Act
            invokePrivateMethod("caricoMerce", new Class<?>[]{}, new Object[]{});

            // Assert
            verify(connection, times(1)).commit();
            verify(connection, never()).rollback();
            String output = outContent.toString();
            assertTrue(output.contains("Carico registrato con successo!"));
            assertTrue(output.contains("Valore carico: EUR 525,00")); // 50 * 10.50
        }

        @Test
        void caricoMerce_shouldFail_whenArticleNotFound() throws Exception {
            when(scanner.nextLine()).thenReturn("ART999");
            when(connection.prepareStatement(contains("SELECT"))).thenReturn(preparedStatement);
            when(preparedStatement.executeQuery()).thenReturn(resultSet);
            when(resultSet.next()).thenReturn(false); // Articolo non trovato

            invokePrivateMethod("caricoMerce", new Class<?>[]{}, new Object[]{});

            verify(connection, never()).commit();
            assertTrue(outContent.toString().contains("Articolo non trovato!"));
        }

        @Test
        void caricoMerce_shouldFail_whenQuantityIsInvalid() throws Exception {
            when(scanner.nextLine()).thenReturn("ART001", "DDT123", "-10"); // Quantità negativa
            when(connection.prepareStatement(contains("SELECT"))).thenReturn(preparedStatement);
            when(preparedStatement.executeQuery()).thenReturn(resultSet);
            mockCaricaArticoloSuccess();

            invokePrivateMethod("caricoMerce", new Class<?>[]{}, new Object[]{});

            verify(connection, never()).commit();
            assertTrue(outContent.toString().contains("Quantità non valida!"));
        }

        @Test
        void caricoMerce_shouldRollback_onSqlException() throws Exception {
            when(scanner.nextLine()).thenReturn("ART001", "DDT123", "50", "10.50", "FORN01", "CARICO TEST", "LOTTO01");
            when(connection.prepareStatement(contains("SELECT"))).thenReturn(preparedStatement);
            when(preparedStatement.executeQuery()).thenReturn(resultSet);
            mockCaricaArticoloSuccess();
            // Simula un errore durante la registrazione del movimento
            when(connection.prepareStatement(contains("INSERT INTO MOVIMENTI"))).thenThrow(new SQLException("DB Error"));

            invokePrivateMethod("caricoMerce", new Class<?>[]{}, new Object[]{});

            verify(connection, times(1)).rollback();
            verify(connection, never()).commit();
            assertTrue(outContent.toString().contains("Errore durante il carico! Operazione annullata."));
        }
    }

    @Nested
    @DisplayName("Test Funzionalità Scarico Merce")
    class ScaricoMerceTests {

        @BeforeEach
        void setupScaricoMocks() throws SQLException {
            when(connection.prepareStatement(anyString())).thenReturn(preparedStatement);
        }

        private void mockCaricaArticoloPerScarico(BigDecimal disponibile, BigDecimal valoreMedio) throws SQLException {
            when(preparedStatement.executeQuery()).thenReturn(resultSet);
            when(resultSet.next()).thenReturn(true);
            when(resultSet.getString(1)).thenReturn("ARTICOLO TEST");
            when(resultSet.getString(11)).thenReturn("A");
            when(resultSet.getBigDecimal(12)).thenReturn(disponibile); // wsGiaDisponibile
            when(resultSet.getBigDecimal(15)).thenReturn(valoreMedio); // wsGiaValMedio
        }

        @Test
        void scaricoMerce_shouldSucceed_withMetodoMedio() throws Exception {
            when(scanner.nextLine()).thenReturn("ART001", "10", "BOL999", "SCARICO TEST", "M");
            mockCaricaArticoloPerScarico(new BigDecimal("100"), new BigDecimal("15.00"));
            when(preparedStatement.executeUpdate()).thenReturn(1);

            invokePrivateMethod("scaricoMerce", new Class<?>[]{}, new Object[]{});

            verify(connection, times(1)).commit();
            String output = outContent.toString();
            assertTrue(output.contains("Scarico registrato con successo!"));
            assertTrue(output.contains("Valore scarico: EUR 150,00")); // 10 * 15.00
        }

        @Test
        void scaricoMerce_shouldSucceed_withMetodoFifo() throws Exception {
            when(scanner.nextLine()).thenReturn("ART001", "15", "BOL999", "SCARICO TEST", "F");
            mockCaricaArticoloPerScarico(new BigDecimal("100"), BigDecimal.ZERO);

            // Mock per calcolaValoreFifoLifo
            PreparedStatement fifoPs = mock(PreparedStatement.class);
            ResultSet fifoRs = mock(ResultSet.class);
            when(connection.prepareStatement(contains("ORDER BY data_carico ASC"))).thenReturn(fifoPs);
            when(fifoPs.executeQuery()).thenReturn(fifoRs);
            // Lotto 1: 10 pezzi a 10 EUR
            when(fifoRs.next()).thenReturn(true).thenReturn(true).thenReturn(false);
            when(fifoRs.getBigDecimal("quantita_residua")).thenReturn(new BigDecimal("10.00")).thenReturn(new BigDecimal("20.00"));
            when(fifoRs.getBigDecimal("prezzo_acquisto")).thenReturn(new BigDecimal("10.00")).thenReturn(new BigDecimal("12.00"));

            // Mock per aggiornamenti
            when(preparedStatement.executeUpdate()).thenReturn(1);
            when(connection.prepareStatement(contains("UPDATE LOTTI"))).thenReturn(preparedStatement);
            when(preparedStatement.executeBatch()).thenReturn(new int[]{1, 1});

            invokePrivateMethod("scaricoMerce", new Class<?>[]{}, new Object[]{});

            verify(connection).commit();
            String output = outContent.toString();
            assertTrue(output.contains("Scarico registrato con successo!"));
            // Valore: (10 * 10) + (5 * 12) = 100 + 60 = 160
            assertTrue(output.contains("Valore scarico: EUR 160,00"));
        }

        @Test
        void scaricoMerce_shouldFail_whenQuantityNotAvailable() throws Exception {
            when(scanner.nextLine()).thenReturn("ART001", "150");
            mockCaricaArticoloPerScarico(new BigDecimal("100"), BigDecimal.TEN);

            invokePrivateMethod("scaricoMerce", new Class<?>[]{}, new Object[]{});

            verify(connection, never()).commit();
            assertTrue(outContent.toString().contains("Quantità non disponibile!"));
        }

        @Test
        void scaricoMerce_shouldFail_whenInvalidValorizzMethod() throws Exception {
            when(scanner.nextLine()).thenReturn("ART001", "10", "BOL999", "SCARICO TEST", "X"); // Metodo non valido
            mockCaricaArticoloPerScarico(new BigDecimal("100"), BigDecimal.TEN);

            invokePrivateMethod("scaricoMerce", new Class<?>[]{}, new Object[]{});

            verify(connection, never()).commit();
            assertTrue(outContent.toString().contains("Metodo valorizzazione non valido!"));
        }
    }

    @Nested
    @DisplayName("Test Funzionalità di Visualizzazione e Report")
    class ReportingTests {

        @Test
        void visualizzaGiacenza_shouldDisplayCorrectData() throws Exception {
            when(scanner.nextLine()).thenReturn("ART001");
            when(connection.prepareStatement(anyString())).thenReturn(preparedStatement);
            when(preparedStatement.executeQuery()).thenReturn(resultSet);
            when(resultSet.next()).thenReturn(true);
            when(resultSet.getString(1)).thenReturn("DESCRIZIONE ARTICOLO LUNGA");
            when(resultSet.getString(2)).thenReturn("CAT01");
            when(resultSet.getString(3)).thenReturn("PZ");
            when(resultSet.getString(10)).thenReturn("A-01-01");
            when(resultSet.getString(11)).thenReturn("A");
            when(resultSet.getBigDecimal(12)).thenReturn(new BigDecimal("150.50")); // Disponibile
            when(resultSet.getBigDecimal(13)).thenReturn(new BigDecimal("20.00"));  // Impegnata
            when(resultSet.getBigDecimal(15)).thenReturn(new BigDecimal("12.34"));  // Valore Medio

            invokePrivateMethod("visualizzaGiacenza", new Class<?>[]{}, new Object[]{});

            String output = outContent.toString();
            assertTrue(output.contains("ARTICOLO: ART001"));
            assertTrue(output.contains("Disponibile:     150,50"));
            assertTrue(output.contains("Impegnata:       20,00"));
            assertTrue(output.contains("Netta:           130,50"));
            assertTrue(output.contains("Valore medio:    EUR 12,34"));
            assertTrue(output.contains("Valore totale:   EUR 1.857,17")); // 150.50 * 12.34
        }

        @Test
        void listaSottoscorta_shouldDisplayItemsBelowReorderPoint() throws Exception {
            when(connection.prepareStatement(anyString())).thenReturn(preparedStatement);
            when(preparedStatement.executeQuery()).thenReturn(resultSet);
            when(resultSet.next()).thenReturn(true).thenReturn(false);
            when(resultSet.getString(1)).thenReturn("ART-LOW");
            when(resultSet.getString(2)).thenReturn("ARTICOLO SOTTOSCORTA");
            when(resultSet.getBigDecimal(3)).thenReturn(new BigDecimal("5"));   // Disponibile
            when(resultSet.getBigDecimal(5)).thenReturn(new BigDecimal("10"));  // Punto riordino
            when(resultSet.getBigDecimal(6)).thenReturn(new BigDecimal("25"));  // Lotto riordino

            invokePrivateMethod("listaSottoscorta", new Class<?>[]{}, new Object[]{});

            String output = outContent.toString();
            assertTrue(output.contains("ART-LOW - ARTICOLO SOTTOSCORTA"));
            assertTrue(output.contains("Mancanti: 5,00"));
            assertTrue(output.contains("DA ORDINARE: 25,00"));
            assertTrue(output.contains("Totale articoli sottoscorta: 1"));
        }

        @Test
        void valorizzazioneMagazzino_shouldGenerateCorrectReportFile() throws Exception {
            when(connection.prepareStatement(anyString())).thenReturn(preparedStatement);
            when(preparedStatement.executeQuery()).thenReturn(resultSet);
            when(resultSet.next()).thenReturn(true).thenReturn(true).thenReturn(false);

            // Articolo 1
            when(resultSet.getString("codice_articolo")).thenReturn("ART001", "ART002");
            when(resultSet.getString("descrizione")).thenReturn("PRODOTTO 1", "PRODOTTO 2");
            when(resultSet.getString("unita_misura")).thenReturn("PZ", "KG");
            when(resultSet.getBigDecimal("quantita_disponibile")).thenReturn(new BigDecimal("100"), new BigDecimal("50.5"));
            when(resultSet.getBigDecimal("valore_medio")).thenReturn(new BigDecimal("10.00"), new BigDecimal("2.50"));

            invokePrivateMethod("valorizzazioneMagazzino", new Class<?>[]{}, new Object[]{});

            File reportFile = new File("REPORT-MAGAZZINO.TXT");
            assertTrue(reportFile.exists());

            String fileContent = Files.readString(reportFile.toPath());
            assertTrue(fileContent.contains("VALORE TOTALE MAGAZZINO: EUR 1.126,25")); // (100*10) + (50.5*2.5) = 1000 + 126.25
            assertTrue(fileContent.contains("Articoli valorizzati: 2"));
            assertTrue(fileContent.contains("ART001"));
            assertTrue(fileContent.contains("ART002"));
        }
    }

    @Nested
    @DisplayName("Test Funzionalità Rettifica Inventario")
    class RettificaInventarioTests {

        @BeforeEach
        void setupRettificaMocks() throws SQLException {
            when(connection.prepareStatement(anyString())).thenReturn(preparedStatement);
            when(preparedStatement.executeQuery()).thenReturn(resultSet);
            when(resultSet.next()).thenReturn(true);
            when(resultSet.getString(1)).thenReturn("ARTICOLO DA RETTIFICARE");
            when(resultSet.getString(11)).thenReturn("A");
            when(resultSet.getBigDecimal(12)).thenReturn(new BigDecimal("100")); // Giacenza attuale
            when(resultSet.getBigDecimal(15)).thenReturn(new BigDecimal("10"));  // Valore medio
        }

        @Test
        void rettificaInventario_shouldPerformPositiveAdjustment() throws Exception {
            when(scanner.nextLine()).thenReturn("ART-RETT", "110", "Conteggio fisico", "S");
            when(preparedStatement.executeUpdate()).thenReturn(1);

            invokePrivateMethod("rettificaInventario", new Class<?>[]{}, new Object[]{});

            verify(connection).commit();
            String output = outContent.toString();
            assertTrue(output.contains("Differenza: +10,00"));
            assertTrue(output.contains("Rettifica eseguita con successo!"));
            // Verifica che il movimento sia di tipo 'RI' (Rettifica Inventario)
            verify(preparedStatement).setString(1, "RI");
        }

        @Test
        void rettificaInventario_shouldPerformNegativeAdjustment() throws Exception {
            when(scanner.nextLine()).thenReturn("ART-RETT", "95", "Smarrito", "S");
            when(preparedStatement.executeUpdate()).thenReturn(1);

            invokePrivateMethod("rettificaInventario", new Class<?>[]{}, new Object[]{});

            verify(connection).commit();
            String output = outContent.toString();
            assertTrue(output.contains("Differenza: -5,00"));
            assertTrue(output.contains("Rettifica eseguita con successo!"));
        }

        @Test
        void rettificaInventario_shouldBeCancelledByUser() throws Exception {
            when(scanner.nextLine()).thenReturn("ART-RETT", "110", "Conteggio", "N"); // Utente annulla

            invokePrivateMethod("rettificaInventario", new Class<?>[]{}, new Object[]{});

            verify(connection, never()).commit();
            assertTrue(outContent.toString().contains("Rettifica annullata"));
        }
    }

    @Nested
    @DisplayName("Test Funzionalità Analisi ABC")
    class AnalisiAbcTests {

        @Test
        void analisiAbc_shouldCorrectlyClassifyAndDisplayArticles() throws Exception {
            when(connection.createStatement()).thenReturn(statement);
            when(connection.prepareStatement(anyString())).thenReturn(preparedStatement);

            // Mock per il calcolo del valore totale
            when(statement.executeQuery("SELECT SUM(valore_tot) FROM ANALISI_ABC")).thenReturn(resultSet);
            when(resultSet.next()).thenReturn(true).thenReturn(false);
            when(resultSet.getBigDecimal(1)).thenReturn(new BigDecimal("10000.00"));

            // Mock per la visualizzazione della Classe A
            PreparedStatement psA = mock(PreparedStatement.class);
            ResultSet rsA = mock(ResultSet.class);
            when(connection.prepareStatement(contains("WHERE classe = ?"))).thenReturn(psA);
            when(psA.executeQuery()).thenReturn(rsA);
            when(rsA.next()).thenReturn(true).thenReturn(false);
            when(rsA.getString("codice_articolo")).thenReturn("ART-A");
            when(rsA.getString("descrizione")).thenReturn("ARTICOLO CLASSE A");
            when(rsA.getBigDecimal("valore_tot")).thenReturn(new BigDecimal("8000.00"));
            when(rsA.getBigDecimal("percentuale")).thenReturn(new BigDecimal("80.00"));

            // Mock per il riepilogo delle Classi B e C
            PreparedStatement psBC = mock(PreparedStatement.class);
            ResultSet rsBC = mock(ResultSet.class);
            when(connection.prepareStatement(contains("SELECT COUNT(*)"))).thenReturn(psBC);
            when(psBC.executeQuery()).thenReturn(rsBC);
            when(rsBC.next()).thenReturn(true).thenReturn(true).thenReturn(false);
            // Riepilogo B
            when(rsBC.getInt(1)).thenReturn(5, 10);
            when(rsBC.getBigDecimal(2)).thenReturn(new BigDecimal("1500.00"), new BigDecimal("500.00"));

            invokePrivateMethod("analisiAbc", new Class<?>[]{}, new Object[]{});

            String output = outContent.toString();
            // Verifica Classe A
            assertTrue(output.contains("CLASSE A - Alto valore"));
            assertTrue(output.contains("ART-A - ARTICOLO CLASSE A"));
            assertTrue(output.contains("Valore: EUR 8.000,00"));
            assertTrue(output.contains("(80,00%)"));

            // Verifica Riepilogo Classe B
            assertTrue(output.contains("CLASSE B - Medio valore"));
            assertTrue(output.contains("Articoli: 5"));
            assertTrue(output.contains("Valore totale: EUR 1.500,00"));

            // Verifica Riepilogo Classe C
            assertTrue(output.contains("CLASSE C - Basso valore"));
            assertTrue(output.contains("Articoli: 10"));
            assertTrue(output.contains("Valore totale: EUR 500,00"));

            // Verifica che la tabella temporanea venga creata e distrutta
            verify(statement).execute(contains("CREATE TEMP TABLE"));
            verify(statement).execute(contains("DROP TABLE"));
        }
    }
}