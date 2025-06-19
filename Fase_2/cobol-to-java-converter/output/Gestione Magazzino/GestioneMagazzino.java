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
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Scanner;

/**
 * <h1>Gestione Magazzino</h1>
 * <p>
 * Traduzione Java del programma COBOL GESTIONE-MAGAZZINO.
 * </p>
 * <p>
 * Questo programma gestisce un sistema di magazzino e inventario,
 * includendo operazioni di carico/scarico merce, valorizzazione,
 * gestione ordini a fornitori e reportistica.
 * Utilizza JDBC per connettersi a un database PostgreSQL.
 * </p>
 *
 * @author Annalisa Egidi (Original COBOL Author), AI Translator
 * @version 1.0
 * @since 2025-05-20
 */
public class GestioneMagazzino {

    // --- Configurazione Database ---
    private static final String DB_URL = "jdbc:postgresql://localhost:5432/magazzino";
    private static final String DB_USER = "postgres";
    private static final String DB_PASSWORD = "password";

    private Connection connection;
    private final Scanner scanner;

    // --- Mappatura della WORKING-STORAGE SECTION ---

    // Variabili di controllo
    private int wsScelta = 0;
    private String wsContinua = "S";
    private String wsRisposta = "";
    private String wsEsito = "";
    private String wsMetodoValorizz = "F"; // F=FIFO, L=LIFO, M=Medio ponderato

    // Variabili per articoli
    private String wsArtCodice = "";
    private String wsArtDescrizione = "";
    private String wsArtCategoria = "";
    private String wsArtUm = "";
    private String wsArtFornitore = "";
    private BigDecimal wsArtPrezzoAcq = BigDecimal.ZERO;
    private BigDecimal wsArtPrezzoVen = BigDecimal.ZERO;
    private BigDecimal wsArtScortaMin = BigDecimal.ZERO;
    private BigDecimal wsArtPuntoRiord = BigDecimal.ZERO;
    private BigDecimal wsArtLottoRiord = BigDecimal.ZERO;
    private String wsArtUbicazione = "";
    private String wsArtStato = "";

    // Variabili per giacenze
    private BigDecimal wsGiaDisponibile = BigDecimal.ZERO;
    private BigDecimal wsGiaImpegnata = BigDecimal.ZERO;
    private BigDecimal wsGiaOrdinata = BigDecimal.ZERO;
    private BigDecimal wsGiaValMedio = BigDecimal.ZERO;
    private BigDecimal wsGiaValUltimo = BigDecimal.ZERO;

    // Variabili per movimenti
    private String wsMovTipo = "";
    private String wsMovNumeroDoc = "";
    private String wsMovData = "";
    private String wsMovArticolo = "";
    private BigDecimal wsMovQuantita = BigDecimal.ZERO;
    private BigDecimal wsMovPrezzo = BigDecimal.ZERO;
    private BigDecimal wsMovValore = BigDecimal.ZERO;
    private String wsMovCausale = "";
    private String wsMovFornitore = "";
    private String wsMovOperatore = "";

    // Variabili per ordini
    private String wsOrdNumero = "";
    private String wsOrdData = "";
    private String wsOrdFornitore = "";
    private String wsOrdStato = "";
    private BigDecimal wsOrdTotale = BigDecimal.ZERO;

    // Variabili per lotti (FIFO/LIFO)
    private int wsLotId = 0;
    private String wsLotNumero = "";
    private String wsLotData = "";
    private BigDecimal wsLotQtaIni = BigDecimal.ZERO;
    private BigDecimal wsLotQtaRes = BigDecimal.ZERO;
    private BigDecimal wsLotPrezzo = BigDecimal.ZERO;

    // Variabili di calcolo
    private BigDecimal wsQtaRichiesta = BigDecimal.ZERO;
    private BigDecimal wsQtaPrelevata = BigDecimal.ZERO;
    private BigDecimal wsQtaResidua = BigDecimal.ZERO;
    private BigDecimal wsValoreTot = BigDecimal.ZERO;
    private BigDecimal wsValoreMedio = BigDecimal.ZERO;
    private BigDecimal wsNuovoMedio = BigDecimal.ZERO;

    // Contatori e totali
    private int wsContaArticoli = 0;
    private int wsContaMovimenti = 0;
    private int wsContaSottoscorta = 0;
    private BigDecimal wsValoreMagazzino = BigDecimal.ZERO;

    // Variabili per formattazione (realizzate con DecimalFormat)
    private final DecimalFormat formatoImporto = new DecimalFormat("###,##0.00");
    private final DecimalFormat formatoQuantita = new DecimalFormat("###,##0.00");
    private final DecimalFormat formatoNumero = new DecimalFormat("###,##0");
    private final DecimalFormat formatoPercentuale = new DecimalFormat("##0.00");

    // Area SQL
    private int sqlcode = 0; // Emulazione di SQLCODE

    /**
     * Costruttore della classe. Inizializza lo scanner per l'input.
     */
    public GestioneMagazzino() {
        this.scanner = new Scanner(System.in);
    }

    /**
     * Metodo principale che avvia l'applicazione.
     *
     * @param args Argomenti della riga di comando (non utilizzati).
     */
    public static void main(String[] args) {
        GestioneMagazzino app = new GestioneMagazzino();
        app.mainLogic();
    }

    /**
     * Contiene il flusso principale del programma, equivalente a MAIN-LOGIC in COBOL.
     */
    private void mainLogic() {
        if (!connettiDatabase()) {
            return;
        }

        do {
            visualizzaMenu();
            try {
                wsScelta = Integer.parseInt(scanner.nextLine());
                elaboraScelta();
            } catch (NumberFormatException e) {
                System.out.println("Scelta non valida! Inserire un numero.");
                wsScelta = -1; // Valore non valido per continuare il ciclo
            }

            if (wsScelta != 0) {
                System.out.println();
                System.out.print("Continuare? (S/N): ");
                wsContinua = scanner.nextLine();
            } else {
                wsContinua = "N";
            }

        } while (wsContinua.equalsIgnoreCase("S"));

        disconnettiDatabase();
        scanner.close();
        System.out.println("Programma terminato.");
    }

    /**
     * Stabilisce la connessione al database PostgreSQL.
     *
     * @return true se la connessione ha successo, false altrimenti.
     */
    private boolean connettiDatabase() {
        try {
            connection = DriverManager.getConnection(DB_URL, DB_USER, DB_PASSWORD);
            sqlcode = 0;
            System.out.println("Connessione al database stabilita");
            return true;
        } catch (SQLException e) {
            System.err.println("Errore connessione database: " + e.getMessage());
            sqlcode = e.getErrorCode();
            return false;
        }
    }

    /**
     * Chiude la connessione al database.
     */
    private void disconnettiDatabase() {
        if (connection != null) {
            try {
                connection.close();
                System.out.println("Disconnesso dal database");
            } catch (SQLException e) {
                System.err.println("Errore durante la disconnessione: " + e.getMessage());
            }
        }
    }

    /**
     * Visualizza il menu principale delle opzioni.
     */
    private void visualizzaMenu() {
        System.out.println();
        System.out.println("===== SISTEMA GESTIONE MAGAZZINO =====");
        System.out.println(" 1. Carico merce");
        System.out.println(" 2. Scarico merce");
        System.out.println(" 3. Visualizza giacenza articolo");
        System.out.println(" 4. Lista articoli sottoscorta");
        System.out.println(" 5. Valorizzazione magazzino");
        System.out.println(" 6. Movimenti articolo");
        System.out.println(" 7. Rettifica inventario");
        System.out.println(" 8. Gestione ordini fornitori");
        System.out.println(" 9. Report inventario fisico");
        System.out.println("10. Analisi ABC articoli");
        System.out.println(" 0. Esci");
        System.out.println("======================================");
        System.out.print("Scelta: ");
    }

    /**
     * Esegue l'azione corrispondente alla scelta dell'utente.
     */
    private void elaboraScelta() {
        switch (wsScelta) {
            case 1:
                caricoMerce();
                break;
            case 2:
                scaricoMerce();
                break;
            case 3:
                visualizzaGiacenza();
                break;
            case 4:
                listaSottoscorta();
                break;
            case 5:
                valorizzazioneMagazzino();
                break;
            case 6:
                movimentiArticolo();
                break;
            case 7:
                rettificaInventario();
                break;
            case 8:
                gestioneOrdini();
                break;
            case 9:
                reportInventario();
                break;
            case 10:
                analisiAbc();
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
     * Gestisce il processo di carico della merce a magazzino.
     */
    private void caricoMerce() {
        System.out.println("\n=== CARICO MERCE ===");
        System.out.print("Codice articolo: ");
        wsArtCodice = scanner.nextLine().trim();

        if (!caricaArticolo()) {
            return;
        }

        System.out.println("Articolo: " + wsArtDescrizione.substring(0, Math.min(50, wsArtDescrizione.length())));

        System.out.print("Numero documento (DDT/Fattura): ");
        wsMovNumeroDoc = scanner.nextLine().trim();

        try {
            System.out.print("Quantità da caricare: ");
            wsMovQuantita = new BigDecimal(scanner.nextLine().trim());
            if (wsMovQuantita.compareTo(BigDecimal.ZERO) <= 0) {
                System.out.println("Quantità non valida!");
                return;
            }

            System.out.print("Prezzo unitario: ");
            wsMovPrezzo = new BigDecimal(scanner.nextLine().trim());
            if (wsMovPrezzo.compareTo(BigDecimal.ZERO) <= 0) {
                System.out.println("Prezzo non valido!");
                return;
            }
        } catch (NumberFormatException e) {
            System.out.println("Input numerico non valido!");
            return;
        }

        System.out.print("Codice fornitore: ");
        wsMovFornitore = scanner.nextLine().trim();
        System.out.print("Causale: ");
        wsMovCausale = scanner.nextLine().trim();
        System.out.print("Numero lotto (opzionale): ");
        wsLotNumero = scanner.nextLine().trim();

        wsMovValore = wsMovQuantita.multiply(wsMovPrezzo).setScale(2, RoundingMode.HALF_UP);
        wsEsito = "OK";

        try {
            connection.setAutoCommit(false);

            wsMovTipo = "CA";
            if (registraMovimento()) {
                if (aggiornaGiacenzaCarico()) {
                    if (wsLotNumero != null && !wsLotNumero.isEmpty()) {
                        creaLotto();
                    }
                }
            }

            if ("OK".equals(wsEsito)) {
                connection.commit();
                System.out.println("Carico registrato con successo!");
                System.out.println("Valore carico: EUR " + formatoImporto.format(wsMovValore));
            } else {
                safeRollback();
                System.out.println("Errore durante il carico! Operazione annullata.");
            }

        } catch (SQLException e) {
            safeRollback();
            handleSqlException(e, "caricoMerce");
        } finally {
            try {
                connection.setAutoCommit(true);
            } catch (SQLException e) {
                handleSqlException(e, "caricoMerce finally");
            }
        }
    }

    /**
     * Gestisce il processo di scarico della merce dal magazzino.
     */
    private void scaricoMerce() {
        System.out.println("\n=== SCARICO MERCE ===");
        System.out.print("Codice articolo: ");
        wsArtCodice = scanner.nextLine().trim();

        if (!caricaArticolo()) {
            return;
        }

        System.out.println("Articolo: " + wsArtDescrizione.substring(0, Math.min(50, wsArtDescrizione.length())));
        System.out.println("Disponibile: " + formatoQuantita.format(wsGiaDisponibile) + " " + wsArtUm);

        try {
            System.out.print("Quantità da scaricare: ");
            wsMovQuantita = new BigDecimal(scanner.nextLine().trim());
        } catch (NumberFormatException e) {
            System.out.println("Input numerico non valido!");
            return;
        }

        if (wsMovQuantita.compareTo(BigDecimal.ZERO) <= 0) {
            System.out.println("Quantità non valida!");
            return;
        }
        if (wsMovQuantita.compareTo(wsGiaDisponibile) > 0) {
            System.out.println("Quantità non disponibile!");
            return;
        }

        System.out.print("Numero documento: ");
        wsMovNumeroDoc = scanner.nextLine().trim();
        System.out.print("Causale: ");
        wsMovCausale = scanner.nextLine().trim();
        System.out.print("Metodo valorizzazione (F=FIFO, L=LIFO, M=Medio): ");
        wsMetodoValorizz = scanner.nextLine().trim().toUpperCase();

        wsEsito = "OK";

        try {
            connection.setAutoCommit(false);

            if (calcolaValoreScarico()) {
                wsMovTipo = "SC";
                if (registraMovimento()) {
                    if (aggiornaGiacenzaScarico()) {
                        if (!"M".equals(wsMetodoValorizz)) {
                            aggiornaLottiScarico();
                        }
                    }
                }
            }

            if ("OK".equals(wsEsito)) {
                connection.commit();
                System.out.println("Scarico registrato con successo!");
                System.out.println("Valore scarico: EUR " + formatoImporto.format(wsMovValore));
            } else {
                safeRollback();
                System.out.println("Errore durante lo scarico! Operazione annullata.");
            }

        } catch (SQLException e) {
            safeRollback();
            handleSqlException(e, "scaricoMerce");
        } finally {
            try {
                connection.setAutoCommit(true);
            } catch (SQLException e) {
                handleSqlException(e, "scaricoMerce finally");
            }
        }
    }

    /**
     * Carica i dati di un articolo e la sua giacenza dal database.
     *
     * @return true se l'articolo è stato trovato e caricato, false altrimenti.
     */
    private boolean caricaArticolo() {
        wsEsito = "OK";
        String sql = "SELECT a.descrizione, a.codice_categoria, a.unita_misura, a.codice_fornitore, " +
                     "a.prezzo_acquisto, a.prezzo_vendita, a.scorta_minima, a.punto_riordino, " +
                     "a.lotto_riordino, a.ubicazione, a.stato, " +
                     "COALESCE(g.quantita_disponibile, 0), COALESCE(g.quantita_impegnata, 0), " +
                     "COALESCE(g.quantita_ordinata, 0), COALESCE(g.valore_medio, 0), " +
                     "COALESCE(g.valore_ultimo, 0) " +
                     "FROM ARTICOLI a LEFT JOIN GIACENZE g ON a.codice_articolo = g.codice_articolo " +
                     "WHERE a.codice_articolo = ?";

        try (PreparedStatement pstmt = connection.prepareStatement(sql)) {
            pstmt.setString(1, wsArtCodice);
            try (ResultSet rs = pstmt.executeQuery()) {
                if (rs.next()) {
                    sqlcode = 0;
                    wsArtDescrizione = rs.getString(1);
                    wsArtCategoria = rs.getString(2);
                    wsArtUm = rs.getString(3);
                    wsArtFornitore = rs.getString(4);
                    wsArtPrezzoAcq = rs.getBigDecimal(5);
                    wsArtPrezzoVen = rs.getBigDecimal(6);
                    wsArtScortaMin = rs.getBigDecimal(7);
                    wsArtPuntoRiord = rs.getBigDecimal(8);
                    wsArtLottoRiord = rs.getBigDecimal(9);
                    wsArtUbicazione = rs.getString(10);
                    wsArtStato = rs.getString(11);
                    wsGiaDisponibile = rs.getBigDecimal(12);
                    wsGiaImpegnata = rs.getBigDecimal(13);
                    wsGiaOrdinata = rs.getBigDecimal(14);
                    wsGiaValMedio = rs.getBigDecimal(15);
                    wsGiaValUltimo = rs.getBigDecimal(16);

                    if (!"A".equals(wsArtStato)) {
                        System.out.println("Articolo non attivo!");
                        wsEsito = "KO";
                    }
                } else {
                    sqlcode = 100;
                    System.out.println("Articolo non trovato!");
                    wsEsito = "KO";
                }
            }
        } catch (SQLException e) {
            handleSqlException(e, "caricaArticolo");
            wsEsito = "KO";
        }
        return "OK".equals(wsEsito);
    }

    /**
     * Inserisce un nuovo record nella tabella dei movimenti di magazzino.
     *
     * @return true se l'operazione ha successo, false altrimenti.
     */
    private boolean registraMovimento() {
        wsMovOperatore = System.getProperty("user.name"); // Emula FUNCTION CURRENT-DATE(1:50)
        String sql = "INSERT INTO MOVIMENTI_MAGAZZINO " +
                     "(tipo_movimento, numero_documento, codice_articolo, quantita, " +
                     "prezzo_unitario, valore_totale, causale, codice_fornitore, operatore) " +
                     "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)";

        try (PreparedStatement pstmt = connection.prepareStatement(sql)) {
            pstmt.setString(1, wsMovTipo);
            pstmt.setString(2, wsMovNumeroDoc);
            pstmt.setString(3, wsArtCodice);
            pstmt.setBigDecimal(4, wsMovQuantita);
            pstmt.setBigDecimal(5, wsMovPrezzo);
            pstmt.setBigDecimal(6, wsMovValore);
            pstmt.setString(7, wsMovCausale);
            pstmt.setString(8, wsMovFornitore);
            pstmt.setString(9, wsMovOperatore);

            int rowsAffected = pstmt.executeUpdate();
            if (rowsAffected > 0) {
                sqlcode = 0;
                return true;
            } else {
                sqlcode = -1; // Generic error
                wsEsito = "KO";
                return false;
            }
        } catch (SQLException e) {
            System.err.println("Errore registrazione movimento: " + e.getErrorCode());
            handleSqlException(e, "registraMovimento");
            wsEsito = "KO";
            return false;
        }
    }

    /**
     * Aggiorna la giacenza di un articolo dopo un carico.
     *
     * @return true se l'operazione ha successo, false altrimenti.
     */
    private boolean aggiornaGiacenzaCarico() {
        // Calcola nuovo valore medio ponderato
        BigDecimal qtaTotale = wsGiaDisponibile.add(wsMovQuantita);
        if (qtaTotale.compareTo(BigDecimal.ZERO) > 0) {
            BigDecimal valoreGiacenzaEsistente = wsGiaDisponibile.multiply(wsGiaValMedio);
            BigDecimal valoreCarico = wsMovQuantita.multiply(wsMovPrezzo);
            wsValoreTot = valoreGiacenzaEsistente.add(valoreCarico);
            wsNuovoMedio = wsValoreTot.divide(qtaTotale, 4, RoundingMode.HALF_UP);
        } else {
            wsNuovoMedio = wsMovPrezzo;
        }

        String updateSql = "UPDATE GIACENZE SET quantita_disponibile = quantita_disponibile + ?, " +
                           "valore_medio = ?, valore_ultimo = ?, data_ultimo_carico = CURRENT_DATE " +
                           "WHERE codice_articolo = ?";

        try (PreparedStatement pstmt = connection.prepareStatement(updateSql)) {
            pstmt.setBigDecimal(1, wsMovQuantita);
            pstmt.setBigDecimal(2, wsNuovoMedio);
            pstmt.setBigDecimal(3, wsMovPrezzo);
            pstmt.setString(4, wsArtCodice);

            int rowsAffected = pstmt.executeUpdate();
            if (rowsAffected == 0) { // Giacenza non esiste, creala
                String insertSql = "INSERT INTO GIACENZE (codice_articolo, quantita_disponibile, " +
                                   "valore_medio, valore_ultimo, data_ultimo_carico) " +
                                   "VALUES (?, ?, ?, ?, CURRENT_DATE)";
                try (PreparedStatement insertPstmt = connection.prepareStatement(insertSql)) {
                    insertPstmt.setString(1, wsArtCodice);
                    insertPstmt.setBigDecimal(2, wsMovQuantita);
                    insertPstmt.setBigDecimal(3, wsMovPrezzo);
                    insertPstmt.setBigDecimal(4, wsMovPrezzo);
                    insertPstmt.executeUpdate();
                }
            }
            sqlcode = 0;
            return true;
        } catch (SQLException e) {
            System.err.println("Errore aggiornamento giacenza: " + e.getErrorCode());
            handleSqlException(e, "aggiornaGiacenzaCarico");
            wsEsito = "KO";
            return false;
        }
    }

    /**
     * Aggiorna la giacenza di un articolo dopo uno scarico.
     *
     * @return true se l'operazione ha successo, false altrimenti.
     */
    private boolean aggiornaGiacenzaScarico() {
        String sql = "UPDATE GIACENZE SET quantita_disponibile = quantita_disponibile - ?, " +
                     "data_ultimo_scarico = CURRENT_DATE WHERE codice_articolo = ?";
        try (PreparedStatement pstmt = connection.prepareStatement(sql)) {
            pstmt.setBigDecimal(1, wsMovQuantita);
            pstmt.setString(2, wsArtCodice);
            pstmt.executeUpdate();
            sqlcode = 0;
            return true;
        } catch (SQLException e) {
            System.err.println("Errore aggiornamento giacenza: " + e.getErrorCode());
            handleSqlException(e, "aggiornaGiacenzaScarico");
            wsEsito = "KO";
            return false;
        }
    }

    /**
     * Crea un nuovo lotto per la merce in ingresso.
     *
     * @return true se l'operazione ha successo, false altrimenti.
     */
    private boolean creaLotto() {
        String sql = "INSERT INTO LOTTI (codice_articolo, numero_lotto, data_carico, " +
                     "quantita_iniziale, quantita_residua, prezzo_acquisto) " +
                     "VALUES (?, ?, CURRENT_DATE, ?, ?, ?)";
        try (PreparedStatement pstmt = connection.prepareStatement(sql)) {
            pstmt.setString(1, wsArtCodice);
            pstmt.setString(2, wsLotNumero);
            pstmt.setBigDecimal(3, wsMovQuantita);
            pstmt.setBigDecimal(4, wsMovQuantita);
            pstmt.setBigDecimal(5, wsMovPrezzo);
            pstmt.executeUpdate();
            sqlcode = 0;
            return true;
        } catch (SQLException e) {
            if ("23505".equals(e.getSQLState())) { // Unique violation
                System.out.println("Lotto già esistente!");
            } else {
                System.err.println("Errore creazione lotto: " + e.getErrorCode());
            }
            handleSqlException(e, "creaLotto");
            wsEsito = "KO";
            return false;
        }
    }

    /**
     * Calcola il valore dello scarico in base al metodo scelto (FIFO, LIFO, Medio).
     *
     * @return true se il calcolo ha successo, false altrimenti.
     */
    private boolean calcolaValoreScarico() {
        switch (wsMetodoValorizz) {
            case "F":
                return calcolaValoreFifoLifo(true);
            case "L":
                return calcolaValoreFifoLifo(false);
            case "M":
                wsMovPrezzo = wsGiaValMedio;
                wsMovValore = wsMovQuantita.multiply(wsMovPrezzo).setScale(2, RoundingMode.HALF_UP);
                return true;
            default:
                System.out.println("Metodo valorizzazione non valido!");
                wsEsito = "KO";
                return false;
        }
    }

    /**
     * Logica unificata per calcolare il valore di scarico con metodo FIFO o LIFO.
     *
     * @param isFifo true per FIFO, false per LIFO.
     * @return true se il calcolo ha successo, false altrimenti.
     */
    private boolean calcolaValoreFifoLifo(boolean isFifo) {
        wsQtaRichiesta = wsMovQuantita;
        wsMovValore = BigDecimal.ZERO;

        String sql = "SELECT id_lotto, numero_lotto, quantita_residua, prezzo_acquisto, data_carico " +
                     "FROM LOTTI WHERE codice_articolo = ? AND quantita_residua > 0 AND stato = 'A' " +
                     "ORDER BY data_carico " + (isFifo ? "ASC, id_lotto ASC" : "DESC, id_lotto DESC");

        try (PreparedStatement pstmt = connection.prepareStatement(sql)) {
            pstmt.setString(1, wsArtCodice);
            try (ResultSet rs = pstmt.executeQuery()) {
                while (rs.next() && wsQtaRichiesta.compareTo(BigDecimal.ZERO) > 0) {
                    sqlcode = 0;
                    wsLotQtaRes = rs.getBigDecimal("quantita_residua");
                    wsLotPrezzo = rs.getBigDecimal("prezzo_acquisto");

                    BigDecimal qtaDaPrelevare = wsLotQtaRes.min(wsQtaRichiesta);
                    BigDecimal valorePrelievo = qtaDaPrelevare.multiply(wsLotPrezzo);
                    wsMovValore = wsMovValore.add(valorePrelievo);
                    wsQtaRichiesta = wsQtaRichiesta.subtract(qtaDaPrelevare);
                }
                sqlcode = rs.isAfterLast() ? 0 : 100;
            }
        } catch (SQLException e) {
            handleSqlException(e, "calcolaValoreFifoLifo");
            wsEsito = "KO";
            return false;
        }

        if (wsQtaRichiesta.compareTo(BigDecimal.ZERO) > 0) {
            System.out.println("Lotti insufficienti per " + (isFifo ? "FIFO" : "LIFO") + "!");
            wsEsito = "KO";
            return false;
        } else {
            wsMovPrezzo = wsMovValore.divide(wsMovQuantita, 4, RoundingMode.HALF_UP);
            return true;
        }
    }

    /**
     * Aggiorna le quantità residue dei lotti dopo uno scarico FIFO/LIFO.
     *
     * @return true se l'operazione ha successo, false altrimenti.
     */
    private boolean aggiornaLottiScarico() {
        wsQtaRichiesta = wsMovQuantita;
        boolean isFifo = "F".equals(wsMetodoValorizz);

        String selectSql = "SELECT id_lotto, quantita_residua FROM LOTTI " +
                           "WHERE codice_articolo = ? AND quantita_residua > 0 AND stato = 'A' " +
                           "ORDER BY data_carico " + (isFifo ? "ASC, id_lotto ASC" : "DESC, id_lotto DESC");

        String updateSql = "UPDATE LOTTI SET quantita_residua = ?, stato = ? WHERE id_lotto = ?";

        try (PreparedStatement selectPstmt = connection.prepareStatement(selectSql);
             PreparedStatement updatePstmt = connection.prepareStatement(updateSql)) {

            selectPstmt.setString(1, wsArtCodice);
            try (ResultSet rs = selectPstmt.executeQuery()) {
                while (rs.next() && wsQtaRichiesta.compareTo(BigDecimal.ZERO) > 0) {
                    sqlcode = 0;
                    int lottoId = rs.getInt("id_lotto");
                    BigDecimal qtaResiduaLotto = rs.getBigDecimal("quantita_residua");

                    if (qtaResiduaLotto.compareTo(wsQtaRichiesta) >= 0) {
                        // Lotto copre la richiesta residua
                        BigDecimal nuovaQtaResidua = qtaResiduaLotto.subtract(wsQtaRichiesta);
                        updatePstmt.setBigDecimal(1, nuovaQtaResidua);
                        updatePstmt.setString(2, nuovaQtaResidua.compareTo(BigDecimal.ZERO) == 0 ? "E" : "A");
                        updatePstmt.setInt(3, lottoId);
                        updatePstmt.addBatch();
                        wsQtaRichiesta = BigDecimal.ZERO;
                    } else {
                        // Esaurisci il lotto
                        updatePstmt.setBigDecimal(1, BigDecimal.ZERO);
                        updatePstmt.setString(2, "E");
                        updatePstmt.setInt(3, lottoId);
                        updatePstmt.addBatch();
                        wsQtaRichiesta = wsQtaRichiesta.subtract(qtaResiduaLotto);
                    }
                }
                updatePstmt.executeBatch();
            }
        } catch (SQLException e) {
            handleSqlException(e, "aggiornaLottiScarico");
            wsEsito = "KO";
            return false;
        }
        return true;
    }

    /**
     * Mostra a schermo i dettagli di giacenza, scorte e valori di un articolo.
     */
    private void visualizzaGiacenza() {
        System.out.println("\n=== VISUALIZZA GIACENZA ===");
        System.out.print("Codice articolo: ");
        wsArtCodice = scanner.nextLine().trim();

        if (!caricaArticolo()) {
            return;
        }

        System.out.println("\nARTICOLO: " + wsArtCodice);
        System.out.println("Descrizione: " + wsArtDescrizione.substring(0, Math.min(60, wsArtDescrizione.length())));
        System.out.println("Categoria: " + wsArtCategoria + "  UM: " + wsArtUm);
        System.out.println("Ubicazione: " + wsArtUbicazione);

        System.out.println("\nQUANTITA':");
        System.out.println("  Disponibile:     " + formatoQuantita.format(wsGiaDisponibile));
        System.out.println("  Impegnata:       " + formatoQuantita.format(wsGiaImpegnata));
        System.out.println("  In ordine:       " + formatoQuantita.format(wsGiaOrdinata));
        wsQtaResidua = wsGiaDisponibile.subtract(wsGiaImpegnata);
        System.out.println("  Netta:           " + formatoQuantita.format(wsQtaResidua));

        System.out.println("\nSCORTE:");
        System.out.println("  Scorta minima:   " + formatoQuantita.format(wsArtScortaMin));
        System.out.println("  Punto riordino:  " + formatoQuantita.format(wsArtPuntoRiord));
        System.out.println("  Lotto riordino:  " + formatoQuantita.format(wsArtLottoRiord));

        System.out.println("\nVALORI:");
        System.out.println("  Valore medio:    EUR " + formatoImporto.format(wsGiaValMedio));
        System.out.println("  Valore ultimo:   EUR " + formatoImporto.format(wsGiaValUltimo));
        wsValoreTot = wsGiaDisponibile.multiply(wsGiaValMedio);
        System.out.println("  Valore totale:   EUR " + formatoImporto.format(wsValoreTot));

        if (wsGiaDisponibile.compareTo(wsArtPuntoRiord) <= 0 && wsArtPuntoRiord.compareTo(BigDecimal.ZERO) > 0) {
            System.out.println("\n*** ATTENZIONE: PUNTO RIORDINO RAGGIUNTO ***");
        } else if (wsGiaDisponibile.compareTo(wsArtScortaMin) <= 0 && wsArtScortaMin.compareTo(BigDecimal.ZERO) > 0) {
            System.out.println("\n*** ATTENZIONE: SCORTA MINIMA RAGGIUNTA ***");
        }
    }

    /**
     * Stampa a video la lista degli articoli che sono sotto il punto di riordino.
     */
    private void listaSottoscorta() {
        System.out.println("\n=== ARTICOLI SOTTOSCORTA ===\n");
        wsContaSottoscorta = 0;

        String sql = "SELECT a.codice_articolo, a.descrizione, g.quantita_disponibile, " +
                     "g.quantita_ordinata, a.punto_riordino, a.lotto_riordino, f.ragione_sociale " +
                     "FROM ARTICOLI a " +
                     "JOIN GIACENZE g ON a.codice_articolo = g.codice_articolo " +
                     "LEFT JOIN FORNITORI f ON a.codice_fornitore = f.codice_fornitore " +
                     "WHERE a.stato = 'A' AND g.quantita_disponibile <= a.punto_riordino AND a.punto_riordino > 0 " +
                     "ORDER BY (a.punto_riordino - g.quantita_disponibile) DESC";

        try (PreparedStatement pstmt = connection.prepareStatement(sql);
             ResultSet rs = pstmt.executeQuery()) {

            while (rs.next()) {
                sqlcode = 0;
                wsContaSottoscorta++;
                wsArtCodice = rs.getString(1);
                wsArtDescrizione = rs.getString(2);
                wsGiaDisponibile = rs.getBigDecimal(3);
                wsGiaOrdinata = rs.getBigDecimal(4);
                wsArtPuntoRiord = rs.getBigDecimal(5);
                wsArtLottoRiord = rs.getBigDecimal(6);
                String fornitore = rs.getString(7);

                System.out.println(wsArtCodice + " - " + wsArtDescrizione.substring(0, Math.min(40, wsArtDescrizione.length())));
                System.out.println("  Disponibile: " + formatoQuantita.format(wsGiaDisponibile));

                wsQtaResidua = wsArtPuntoRiord.subtract(wsGiaDisponibile);
                System.out.println("  Punto riordino: " + formatoQuantita.format(wsArtPuntoRiord) +
                                   "  Mancanti: " + formatoQuantita.format(wsQtaResidua));

                if (wsGiaOrdinata.compareTo(BigDecimal.ZERO) > 0) {
                    System.out.println("  In ordine: " + formatoQuantita.format(wsGiaOrdinata));
                } else {
                    System.out.println("  DA ORDINARE: " + formatoQuantita.format(wsArtLottoRiord) +
                                       " (Fornitore: " + (fornitore != null ? fornitore : "N/D") + ")");
                }
                System.out.println();
            }
            sqlcode = wsContaSottoscorta > 0 ? 0 : 100;

        } catch (SQLException e) {
            handleSqlException(e, "listaSottoscorta");
        }
        System.out.println("Totale articoli sottoscorta: " + formatoNumero.format(wsContaSottoscorta));
    }

    /**
     * Genera un report testuale con la valorizzazione del magazzino.
     */
    private void valorizzazioneMagazzino() {
        System.out.println("\n=== VALORIZZAZIONE MAGAZZINO ===\n");
        String fileName = "REPORT-MAGAZZINO.TXT";

        try (PrintWriter writer = new PrintWriter(new FileWriter(fileName))) {
            writer.println("REPORT VALORIZZAZIONE MAGAZZINO");
            writer.println("===============================");
            writer.println("Data: " + LocalDate.now() + "    Ora: " +
                           LocalDateTime.now().format(DateTimeFormatter.ofPattern("HH:mm:ss")));
            writer.println();

            wsContaArticoli = 0;
            wsValoreMagazzino = BigDecimal.ZERO;

            String sql = "SELECT a.codice_articolo, a.descrizione, a.unita_misura, a.ubicazione, " +
                         "g.quantita_disponibile, g.valore_medio " +
                         "FROM ARTICOLI a JOIN GIACENZE g ON a.codice_articolo = g.codice_articolo " +
                         "WHERE a.stato = 'A' ORDER BY a.codice_articolo";

            try (PreparedStatement pstmt = connection.prepareStatement(sql);
                 ResultSet rs = pstmt.executeQuery()) {

                while (rs.next()) {
                    sqlcode = 0;
                    wsGiaDisponibile = rs.getBigDecimal("quantita_disponibile");
                    if (wsGiaDisponibile.compareTo(BigDecimal.ZERO) > 0) {
                        wsContaArticoli++;
                        wsArtCodice = rs.getString("codice_articolo");
                        wsArtDescrizione = rs.getString("descrizione");
                        wsArtUm = rs.getString("unita_misura");
                        wsGiaValMedio = rs.getBigDecimal("valore_medio");

                        wsValoreTot = wsGiaDisponibile.multiply(wsGiaValMedio);
                        wsValoreMagazzino = wsValoreMagazzino.add(wsValoreTot);

                        writer.println(String.format("%-10s %-40s", wsArtCodice, wsArtDescrizione));
                        writer.println(String.format("  Qtà: %-15s %-5s Val.medio: %-15s Totale: EUR %s",
                                                     formatoQuantita.format(wsGiaDisponibile),
                                                     wsArtUm,
                                                     formatoImporto.format(wsGiaValMedio),
                                                     formatoImporto.format(wsValoreTot)));
                    }
                }
            } catch (SQLException e) {
                handleSqlException(e, "valorizzazioneMagazzino");
            }

            writer.println();
            writer.println("-------------------------------");
            writer.println("Articoli valorizzati: " + formatoNumero.format(wsContaArticoli));
            writer.println("VALORE TOTALE MAGAZZINO: EUR " + formatoImporto.format(wsValoreMagazzino));

            System.out.println("Report salvato in " + fileName);
            System.out.println();
            System.out.println("Articoli valorizzati: " + formatoNumero.format(wsContaArticoli));
            System.out.println("Valore totale: EUR " + formatoImporto.format(wsValoreMagazzino));

        } catch (IOException e) {
            System.err.println("Errore durante la scrittura del file di report: " + e.getMessage());
        }
    }

    /**
     * Mostra gli ultimi 50 movimenti di magazzino per un dato articolo.
     */
    private void movimentiArticolo() {
        System.out.println("\n=== MOVIMENTI ARTICOLO ===");
        System.out.print("Codice articolo: ");
        wsArtCodice = scanner.nextLine().trim();

        if (!caricaArticolo()) {
            return;
        }

        System.out.println("\nUltimi movimenti di: " + wsArtDescrizione.substring(0, Math.min(50, wsArtDescrizione.length())));
        System.out.println();
        System.out.printf("%-5s %-20s %-20s %12s %15s%n", "TIPO", "DATA/ORA", "DOCUMENTO", "QUANTITA", "VALORE");
        System.out.println("-".repeat(75));

        String sql = "SELECT tipo_movimento, data_movimento, numero_documento, " +
                     "quantita, valore_totale, causale " +
                     "FROM MOVIMENTI_MAGAZZINO " +
                     "WHERE codice_articolo = ? " +
                     "ORDER BY data_movimento DESC LIMIT 50";

        try (PreparedStatement pstmt = connection.prepareStatement(sql)) {
            pstmt.setString(1, wsArtCodice);
            try (ResultSet rs = pstmt.executeQuery()) {
                while (rs.next()) {
                    sqlcode = 0;
                    wsMovTipo = rs.getString("tipo_movimento");
                    Timestamp dataMov = rs.getTimestamp("data_movimento");
                    wsMovNumeroDoc = rs.getString("numero_documento");
                    wsMovQuantita = rs.getBigDecimal("quantita");
                    wsMovValore = rs.getBigDecimal("valore_totale");
                    wsMovCausale = rs.getString("causale");

                    String segno = (wsMovTipo.equals("SC") || wsMovTipo.equals("RE")) ? "-" : "+";
                    String dataFormatted = dataMov.toLocalDateTime().format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm"));

                    System.out.printf("%-5s %-20s %-20s %s%11s %15s%n",
                                      wsMovTipo,
                                      dataFormatted,
                                      wsMovNumeroDoc,
                                      segno,
                                      formatoQuantita.format(wsMovQuantita),
                                      "EUR " + formatoImporto.format(wsMovValore));
                    System.out.println("      " + wsMovCausale.substring(0, Math.min(50, wsMovCausale.length())));
                    System.out.println();
                }
            }
        } catch (SQLException e) {
            handleSqlException(e, "movimentiArticolo");
        }
    }

    /**
     * Permette di rettificare la giacenza di un articolo a seguito di un conteggio fisico.
     */
    private void rettificaInventario() {
        System.out.println("\n=== RETTIFICA INVENTARIO ===");
        System.out.print("Codice articolo: ");
        wsArtCodice = scanner.nextLine().trim();

        if (!caricaArticolo()) {
            return;
        }

        System.out.println("Articolo: " + wsArtDescrizione.substring(0, Math.min(50, wsArtDescrizione.length())));
        System.out.println("Giacenza attuale: " + formatoQuantita.format(wsGiaDisponibile) + " " + wsArtUm);

        try {
            System.out.print("Giacenza rilevata: ");
            wsQtaRichiesta = new BigDecimal(scanner.nextLine().trim());
        } catch (NumberFormatException e) {
            System.out.println("Input numerico non valido!");
            return;
        }

        BigDecimal differenza = wsQtaRichiesta.subtract(wsGiaDisponibile);

        if (differenza.compareTo(BigDecimal.ZERO) == 0) {
            System.out.println("Nessuna differenza rilevata");
            return;
        }

        wsMovQuantita = differenza.abs();
        if (differenza.compareTo(BigDecimal.ZERO) > 0) {
            System.out.println("Differenza: +" + formatoQuantita.format(differenza));
            wsMovTipo = "RI";
            wsMovCausale = "Rettifica inventario positiva";
        } else {
            System.out.println("Differenza: " + formatoQuantita.format(differenza));
            wsMovTipo = "RI";
            wsMovCausale = "Rettifica inventario negativa";
        }

        System.out.print("Note rettifica: ");
        String note = scanner.nextLine().trim();
        wsMovCausale += " - " + note;

        System.out.print("Confermare rettifica (S/N): ");
        wsRisposta = scanner.nextLine().trim();

        if (wsRisposta.equalsIgnoreCase("S")) {
            wsMovNumeroDoc = "INV" + LocalDate.now().format(DateTimeFormatter.ofPattern("yyyyMMdd"));
            wsMovPrezzo = wsGiaValMedio;
            wsMovValore = wsMovQuantita.multiply(wsMovPrezzo).setScale(2, RoundingMode.HALF_UP);
            wsEsito = "OK";

            try {
                connection.setAutoCommit(false);
                if (registraMovimento()) {
                    String sqlUpdate = "UPDATE GIACENZE SET quantita_disponibile = ?, " +
                                       "data_ultimo_inventario = CURRENT_DATE WHERE codice_articolo = ?";
                    try (PreparedStatement pstmt = connection.prepareStatement(sqlUpdate)) {
                        pstmt.setBigDecimal(1, wsQtaRichiesta);
                        pstmt.setString(2, wsArtCodice);
                        pstmt.executeUpdate();
                        sqlcode = 0;
                    } catch (SQLException e) {
                        handleSqlException(e, "rettificaInventario.update");
                        wsEsito = "KO";
                    }
                }

                if ("OK".equals(wsEsito)) {
                    connection.commit();
                    System.out.println("Rettifica eseguita con successo!");
                } else {
                    safeRollback();
                    System.out.println("Errore durante la rettifica!");
                }
            } catch (SQLException e) {
                safeRollback();
                handleSqlException(e, "rettificaInventario.transaction");
            } finally {
                try {
                    connection.setAutoCommit(true);
                } catch (SQLException e) {
                    handleSqlException(e, "rettificaInventario.finally");
                }
            }
        } else {
            System.out.println("Rettifica annullata");
        }
    }

    /**
     * Menu per la gestione degli ordini a fornitori.
     */
    private void gestioneOrdini() {
        System.out.println("\n=== GESTIONE ORDINI FORNITORI ===");
        System.out.println("1. Nuovo ordine");
        System.out.println("2. Visualizza ordini aperti");
        System.out.println("3. Ricevi merce da ordine");
        System.out.println("4. Stato ordine");
        System.out.print("\nScelta: ");
        try {
            int sceltaOrdini = Integer.parseInt(scanner.nextLine());
            switch (sceltaOrdini) {
                case 1: nuovoOrdine(); break;
                case 2: visualizzaOrdiniAperti(); break;
                case 3: riceviMerceOrdine(); break;
                case 4: statoOrdine(); break;
                default: System.out.println("Scelta non valida!");
            }
        } catch (NumberFormatException e) {
            System.out.println("Scelta non valida!");
        }
    }

    /**
     * Crea un nuovo ordine a fornitore.
     */
    private void nuovoOrdine() {
        System.out.println("\n=== NUOVO ORDINE FORNITORE ===");
        System.out.print("Codice fornitore: ");
        wsOrdFornitore = scanner.nextLine().trim();

        String sqlCheckFornitore = "SELECT ragione_sociale FROM FORNITORI WHERE codice_fornitore = ? AND stato = 'A'";
        try (PreparedStatement pstmt = connection.prepareStatement(sqlCheckFornitore)) {
            pstmt.setString(1, wsOrdFornitore);
            try (ResultSet rs = pstmt.executeQuery()) {
                if (rs.next()) {
                    System.out.println("Fornitore: " + rs.getString(1));
                } else {
                    System.out.println("Fornitore non trovato o non attivo!");
                    return;
                }
            }
        } catch (SQLException e) {
            handleSqlException(e, "nuovoOrdine.checkFornitore");
            return;
        }

        wsOrdNumero = "ORD" + LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyyMMddHHmmss"));
        wsOrdStato = "A";
        wsOrdTotale = BigDecimal.ZERO;

        String sqlInsertOrdine = "INSERT INTO ORDINI (numero_ordine, data_ordine, codice_fornitore, stato_ordine, totale_ordine) " +
                                 "VALUES (?, CURRENT_DATE, ?, ?, ?)";
        try (PreparedStatement pstmt = connection.prepareStatement(sqlInsertOrdine)) {
            pstmt.setString(1, wsOrdNumero);
            pstmt.setString(2, wsOrdFornitore);
            pstmt.setString(3, wsOrdStato);
            pstmt.setBigDecimal(4, wsOrdTotale);
            pstmt.executeUpdate();
            System.out.println("Ordine " + wsOrdNumero + " creato!");
            aggiungiRigheOrdine();
        } catch (SQLException e) {
            System.err.println("Errore creazione ordine: " + e.getErrorCode());
            handleSqlException(e, "nuovoOrdine.insert");
        }
    }

    /**
     * Aggiunge righe a un ordine appena creato.
     */
    private void aggiungiRigheOrdine() {
        String continuaAggiunta = "S";
        wsOrdTotale = BigDecimal.ZERO;

        String sqlInsertRiga = "INSERT INTO RIGHE_ORDINE (numero_ordine, codice_articolo, quantita_ordinata, prezzo_unitario, importo_riga) VALUES (?, ?, ?, ?, ?)";
        String sqlUpdateGiacenza = "UPDATE GIACENZE SET quantita_ordinata = quantita_ordinata + ? WHERE codice_articolo = ?";

        try (PreparedStatement psInsert = connection.prepareStatement(sqlInsertRiga);
             PreparedStatement psUpdate = connection.prepareStatement(sqlUpdateGiacenza)) {

            do {
                System.out.print("Codice articolo: ");
                wsArtCodice = scanner.nextLine().trim();
                if (caricaArticolo()) {
                    System.out.println("Articolo: " + wsArtDescrizione.substring(0, Math.min(40, wsArtDescrizione.length())));
                    System.out.print("Quantità da ordinare: ");
                    try {
                        wsMovQuantita = new BigDecimal(scanner.nextLine().trim());
                        if (wsMovQuantita.compareTo(BigDecimal.ZERO) > 0) {
                            wsMovPrezzo = wsArtPrezzoAcq;
                            wsMovValore = wsMovQuantita.multiply(wsMovPrezzo).setScale(2, RoundingMode.HALF_UP);

                            connection.setAutoCommit(false);

                            psInsert.setString(1, wsOrdNumero);
                            psInsert.setString(2, wsArtCodice);
                            psInsert.setBigDecimal(3, wsMovQuantita);
                            psInsert.setBigDecimal(4, wsMovPrezzo);
                            psInsert.setBigDecimal(5, wsMovValore);
                            psInsert.executeUpdate();

                            psUpdate.setBigDecimal(1, wsMovQuantita);
                            psUpdate.setString(2, wsArtCodice);
                            psUpdate.executeUpdate();

                            connection.commit();

                            wsOrdTotale = wsOrdTotale.add(wsMovValore);
                            System.out.println("Riga ordine aggiunta!");
                        }
                    } catch (NumberFormatException e) {
                        System.out.println("Quantità non valida.");
                    } catch (SQLException e) {
                        safeRollback();
                        System.err.println("Errore aggiunta riga: " + e.getErrorCode());
                        handleSqlException(e, "aggiungiRigheOrdine");
                    } finally {
                        connection.setAutoCommit(true);
                    }
                }
                System.out.print("Aggiungere altro articolo (S/N): ");
                continuaAggiunta = scanner.nextLine().trim();
            } while (continuaAggiunta.equalsIgnoreCase("S"));

        } catch (SQLException e) {
            handleSqlException(e, "aggiungiRigheOrdine.prepare");
        }

        String sqlUpdateTotale = "UPDATE ORDINI SET totale_ordine = ? WHERE numero_ordine = ?";
        try (PreparedStatement pstmt = connection.prepareStatement(sqlUpdateTotale)) {
            pstmt.setBigDecimal(1, wsOrdTotale);
            pstmt.setString(2, wsOrdNumero);
            pstmt.executeUpdate();
            System.out.println("\nOrdine completato. Totale: EUR " + formatoImporto.format(wsOrdTotale));
        } catch (SQLException e) {
            handleSqlException(e, "aggiungiRigheOrdine.updateTotale");
        }
    }

    /**
     * Visualizza tutti gli ordini aperti, confermati o parzialmente evasi.
     */
    private void visualizzaOrdiniAperti() {
        System.out.println("\n=== ORDINI APERTI ===\n");
        String sql = "SELECT o.numero_ordine, o.data_ordine, f.ragione_sociale, o.totale_ordine, o.stato_ordine " +
                     "FROM ORDINI o JOIN FORNITORI f ON o.codice_fornitore = f.codice_fornitore " +
                     "WHERE o.stato_ordine IN ('A', 'C', 'P') ORDER BY o.data_ordine DESC";
        int count = 0;
        try (PreparedStatement pstmt = connection.prepareStatement(sql);
             ResultSet rs = pstmt.executeQuery()) {
            while (rs.next()) {
                count++;
                String statoDesc;
                switch (rs.getString("stato_ordine")) {
                    case "A": statoDesc = "APERTO"; break;
                    case "C": statoDesc = "CONFERMATO"; break;
                    case "P": statoDesc = "PARZIALMENTE EVASO"; break;
                    default: statoDesc = "SCONOSCIUTO";
                }
                System.out.println(rs.getString("numero_ordine") + " del " + rs.getDate("data_ordine"));
                System.out.println("  Fornitore: " + rs.getString("ragione_sociale"));
                System.out.println("  Totale: EUR " + formatoImporto.format(rs.getBigDecimal("totale_ordine")));
                System.out.println("  Stato: " + statoDesc);
                System.out.println();
            }
            if (count == 0) {
                System.out.println("Nessun ordine aperto");
            }
        } catch (SQLException e) {
            handleSqlException(e, "visualizzaOrdiniAperti");
        }
    }

    /**
     * Gestisce il processo di ricevimento merce da un ordine esistente.
     */
    private void riceviMerceOrdine() {
        System.out.println("\n=== RICEVI MERCE DA ORDINE ===");
        System.out.print("Numero ordine: ");
        wsOrdNumero = scanner.nextLine().trim();

        String sqlCheckOrdine = "SELECT stato_ordine, codice_fornitore FROM ORDINI WHERE numero_ordine = ?";
        try (PreparedStatement pstmt = connection.prepareStatement(sqlCheckOrdine)) {
            pstmt.setString(1, wsOrdNumero);
            try (ResultSet rs = pstmt.executeQuery()) {
                if (rs.next()) {
                    wsOrdStato = rs.getString("stato_ordine");
                    wsOrdFornitore = rs.getString("codice_fornitore");
                    if ("E".equals(wsOrdStato)) {
                        System.out.println("Ordine già completamente evaso!");
                        return;
                    }
                } else {
                    System.out.println("Ordine non trovato!");
                    return;
                }
            }
        } catch (SQLException e) {
            handleSqlException(e, "riceviMerceOrdine.check");
            return;
        }

        System.out.println("\nRighe ordine da ricevere:\n");
        String sqlRighe = "SELECT r.codice_articolo, a.descrizione, r.quantita_ordinata, " +
                          "r.quantita_ricevuta, r.prezzo_unitario " +
                          "FROM RIGHE_ORDINE r JOIN ARTICOLI a ON r.codice_articolo = a.codice_articolo " +
                          "WHERE r.numero_ordine = ? AND r.quantita_ricevuta < r.quantita_ordinata AND r.stato_riga <> 'C'";

        try (PreparedStatement pstmt = connection.prepareStatement(sqlRighe)) {
            pstmt.setString(1, wsOrdNumero);
            try (ResultSet rs = pstmt.executeQuery()) {
                while (rs.next()) {
                    wsArtCodice = rs.getString("codice_articolo");
                    wsArtDescrizione = rs.getString("descrizione");
                    BigDecimal qtaOrdinata = rs.getBigDecimal("quantita_ordinata");
                    BigDecimal qtaRicevuta = rs.getBigDecimal("quantita_ricevuta");
                    wsMovPrezzo = rs.getBigDecimal("prezzo_unitario");
                    BigDecimal qtaDaRicevere = qtaOrdinata.subtract(qtaRicevuta);

                    System.out.println(wsArtCodice + " - " + wsArtDescrizione);
                    System.out.println("  Da ricevere: " + formatoQuantita.format(qtaDaRicevere));
                    System.out.print("  Quantità ricevuta: ");
                    try {
                        wsMovQuantita = new BigDecimal(scanner.nextLine().trim());
                        if (wsMovQuantita.compareTo(BigDecimal.ZERO) > 0) {
                            if (wsMovQuantita.compareTo(qtaDaRicevere) > 0) {
                                System.out.println("  Quantità eccessiva! Impostata a " + formatoQuantita.format(qtaDaRicevere));
                                wsMovQuantita = qtaDaRicevere;
                            }
                            processaRigaRicevuta();
                        }
                    } catch (NumberFormatException e) {
                        System.out.println("Input non valido.");
                    }
                    System.out.println();
                }
            }
        } catch (SQLException e) {
            handleSqlException(e, "riceviMerceOrdine.righe");
        }
        aggiornaStatoOrdine();
    }

    /**
     * Processa la ricezione di una singola riga d'ordine, registrando il carico e aggiornando i dati.
     */
    private void processaRigaRicevuta() {
        wsMovFornitore = wsOrdFornitore;
        wsMovCausale = "Ricevimento ordine " + wsOrdNumero;
        wsMovNumeroDoc = wsOrdNumero;
        wsMovValore = wsMovQuantita.multiply(wsMovPrezzo).setScale(2, RoundingMode.HALF_UP);
        wsEsito = "OK";

        try {
            connection.setAutoCommit(false);
            wsMovTipo = "CA";
            if (registraMovimento()) {
                if (aggiornaGiacenzaCarico()) {
                    // Aggiorna riga ordine
                    String sqlUpdateRiga = "UPDATE RIGHE_ORDINE SET quantita_ricevuta = quantita_ricevuta + ?, " +
                                           "stato_riga = CASE WHEN quantita_ricevuta + ? >= quantita_ordinata THEN 'E' ELSE 'P' END " +
                                           "WHERE numero_ordine = ? AND codice_articolo = ?";
                    try (PreparedStatement ps = connection.prepareStatement(sqlUpdateRiga)) {
                        ps.setBigDecimal(1, wsMovQuantita);
                        ps.setBigDecimal(2, wsMovQuantita);
                        ps.setString(3, wsOrdNumero);
                        ps.setString(4, wsArtCodice);
                        ps.executeUpdate();
                    }

                    // Aggiorna quantità ordinata in giacenze
                    String sqlUpdateGiacenza = "UPDATE GIACENZE SET quantita_ordinata = GREATEST(0, quantita_ordinata - ?) " +
                                               "WHERE codice_articolo = ?";
                    try (PreparedStatement ps = connection.prepareStatement(sqlUpdateGiacenza)) {
                        ps.setBigDecimal(1, wsMovQuantita);
                        ps.setString(2, wsArtCodice);
                        ps.executeUpdate();
                    }
                }
            }

            if ("OK".equals(wsEsito)) {
                connection.commit();
                System.out.println("  Carico registrato!");
            } else {
                safeRollback();
                System.out.println("  Errore durante la registrazione del carico.");
            }
        } catch (SQLException e) {
            safeRollback();
            handleSqlException(e, "processaRigaRicevuta");
        } finally {
            try {
                connection.setAutoCommit(true);
            } catch (SQLException e) {
                handleSqlException(e, "processaRigaRicevuta.finally");
            }
        }
    }

    /**
     * Aggiorna lo stato generale dell'ordine (es. da Parziale a Evaso).
     */
    private void aggiornaStatoOrdine() {
        String sql = "UPDATE ORDINI SET stato_ordine = " +
                     "CASE WHEN NOT EXISTS (SELECT 1 FROM RIGHE_ORDINE WHERE numero_ordine = ? AND quantita_ricevuta < quantita_ordinata AND stato_riga <> 'C') THEN 'E' " +
                     "WHEN EXISTS (SELECT 1 FROM RIGHE_ORDINE WHERE numero_ordine = ? AND quantita_ricevuta > 0) THEN 'P' " +
                     "ELSE stato_ordine END " +
                     "WHERE numero_ordine = ?";
        try (PreparedStatement pstmt = connection.prepareStatement(sql)) {
            pstmt.setString(1, wsOrdNumero);
            pstmt.setString(2, wsOrdNumero);
            pstmt.setString(3, wsOrdNumero);
            pstmt.executeUpdate();
        } catch (SQLException e) {
            handleSqlException(e, "aggiornaStatoOrdine");
        }
    }

    /**
     * Mostra lo stato dettagliato di un singolo ordine e delle sue righe.
     */
    private void statoOrdine() {
        System.out.println("\n=== STATO ORDINE ===");
        System.out.print("Numero ordine: ");
        wsOrdNumero = scanner.nextLine().trim();

        String sqlOrdine = "SELECT o.data_ordine, o.stato_ordine, f.ragione_sociale, o.totale_ordine " +
                           "FROM ORDINI o JOIN FORNITORI f ON o.codice_fornitore = f.codice_fornitore " +
                           "WHERE o.numero_ordine = ?";
        try (PreparedStatement pstmt = connection.prepareStatement(sqlOrdine)) {
            pstmt.setString(1, wsOrdNumero);
            try (ResultSet rs = pstmt.executeQuery()) {
                if (rs.next()) {
                    String statoDesc;
                    switch (rs.getString("stato_ordine")) {
                        case "A": statoDesc = "APERTO"; break;
                        case "C": statoDesc = "CONFERMATO"; break;
                        case "P": statoDesc = "PARZIALMENTE EVASO"; break;
                        case "E": statoDesc = "EVASO"; break;
                        default: statoDesc = "SCONOSCIUTO";
                    }
                    System.out.println("\nOrdine: " + wsOrdNumero);
                    System.out.println("Data: " + rs.getDate("data_ordine"));
                    System.out.println("Fornitore: " + rs.getString("ragione_sociale"));
                    System.out.println("Totale: EUR " + formatoImporto.format(rs.getBigDecimal("totale_ordine")));
                    System.out.println("Stato: " + statoDesc);
                } else {
                    System.out.println("Ordine non trovato!");
                    return;
                }
            }
        } catch (SQLException e) {
            handleSqlException(e, "statoOrdine.header");
            return;
        }

        System.out.println("\nDettaglio righe:\n");
        String sqlRighe = "SELECT r.codice_articolo, a.descrizione, r.quantita_ordinata, " +
                          "r.quantita_ricevuta, r.importo_riga, r.stato_riga " +
                          "FROM RIGHE_ORDINE r JOIN ARTICOLI a ON r.codice_articolo = a.codice_articolo " +
                          "WHERE r.numero_ordine = ? ORDER BY r.codice_articolo";
        try (PreparedStatement pstmt = connection.prepareStatement(sqlRighe)) {
            pstmt.setString(1, wsOrdNumero);
            try (ResultSet rs = pstmt.executeQuery()) {
                while (rs.next()) {
                    String statoRigaDesc;
                    switch (rs.getString("stato_riga")) {
                        case "A": statoRigaDesc = "APERTA"; break;
                        case "P": statoRigaDesc = "PARZIALE"; break;
                        case "E": statoRigaDesc = "EVASA"; break;
                        case "C": statoRigaDesc = "CANCELLATA"; break;
                        default: statoRigaDesc = "SCONOSCIUTO";
                    }
                    System.out.println(rs.getString("codice_articolo") + " - " + rs.getString("descrizione"));
                    System.out.println("  Ordinata: " + formatoQuantita.format(rs.getBigDecimal("quantita_ordinata")));
                    System.out.println("  Ricevuta: " + formatoQuantita.format(rs.getBigDecimal("quantita_ricevuta")) + " (" + statoRigaDesc + ")");
                    System.out.println();
                }
            }
        } catch (SQLException e) {
            handleSqlException(e, "statoOrdine.righe");
        }
    }

    /**
     * Genera un report testuale per il conteggio fisico dell'inventario.
     */
    private void reportInventario() {
        System.out.println("\n=== REPORT INVENTARIO FISICO ===");
        String fileName = "INVENTARIO.TXT";
        try (PrintWriter writer = new PrintWriter(new FileWriter(fileName))) {
            writer.println("LISTA INVENTARIO FISICO");
            writer.println("=======================");
            writer.println("Data: " + LocalDate.now() + "    Ora: " +
                           LocalDateTime.now().format(DateTimeFormatter.ofPattern("HH:mm:ss")));
            writer.println();
            writer.printf("%-12s %-30s %-6s %12s %15s %15s%n", "CODICE", "DESCRIZIONE", "UB.", "QTA TEORICA", "QTA RILEVATA", "DIFFERENZA");
            writer.println("-".repeat(95));

            String sql = "SELECT a.codice_articolo, a.descrizione, a.ubicazione, g.quantita_disponibile " +
                         "FROM ARTICOLI a JOIN GIACENZE g ON a.codice_articolo = g.codice_articolo " +
                         "WHERE a.stato = 'A' ORDER BY a.ubicazione, a.codice_articolo";
            try (PreparedStatement pstmt = connection.prepareStatement(sql);
                 ResultSet rs = pstmt.executeQuery()) {
                while (rs.next()) {
                    writer.printf("%-12s %-30.30s %-6.5s %12s %15s %15s%n",
                                  rs.getString("codice_articolo"),
                                  rs.getString("descrizione"),
                                  rs.getString("ubicazione"),
                                  formatoQuantita.format(rs.getBigDecimal("quantita_disponibile")),
                                  "______________",
                                  "______________");
                }
            } catch (SQLException e) {
                handleSqlException(e, "reportInventario");
            }

            writer.println();
            writer.println("-".repeat(95));
            writer.println("Rilevato da: ________________  Data: ________  Firma: ________________");

            System.out.println("Report salvato in " + fileName);
        } catch (IOException e) {
            System.err.println("Errore durante la scrittura del file di inventario: " + e.getMessage());
        }
    }

    /**
     * Esegue un'analisi ABC degli articoli basata sul valore della giacenza.
     */
    private void analisiAbc() {
        System.out.println("\n=== ANALISI ABC ARTICOLI ===\n");
        try (Statement stmt = connection.createStatement()) {
            // Usare una tabella temporanea è un approccio valido, ma per evitare problemi
            // di visibilità della tabella tra diverse connessioni/sessioni,
            // e per una maggiore portabilità, si potrebbe usare una Common Table Expression (CTE).
            // Tuttavia, per aderire alla logica COBOL, usiamo una tabella temporanea.
            stmt.execute("DROP TABLE IF EXISTS ANALISI_ABC");
            stmt.execute("CREATE TEMP TABLE ANALISI_ABC AS " +
                         "SELECT a.codice_articolo, a.descrizione, " +
                         "(g.quantita_disponibile * g.valore_medio) as valore_tot " +
                         "FROM ARTICOLI a JOIN GIACENZE g ON a.codice_articolo = g.codice_articolo " +
                         "WHERE a.stato = 'A' AND g.quantita_disponibile > 0");

            // Calcola totale e percentuali
            BigDecimal valoreTotaleMagazzino;
            try (ResultSet rs = stmt.executeQuery("SELECT SUM(valore_tot) FROM ANALISI_ABC")) {
                valoreTotaleMagazzino = rs.next() ? rs.getBigDecimal(1) : BigDecimal.ZERO;
            }

            if (valoreTotaleMagazzino.compareTo(BigDecimal.ZERO) == 0) {
                System.out.println("Nessun articolo da analizzare.");
                return;
            }

            // Calcola percentuali e percentuali cumulate usando Window Functions (più efficiente)
            stmt.execute("ALTER TABLE ANALISI_ABC ADD COLUMN percentuale NUMERIC, ADD COLUMN perc_cumulata NUMERIC, ADD COLUMN classe CHAR(1)");
            
            String updateSql = "WITH PercentData AS ( " +
                               "  SELECT codice_articolo, " +
                               "         (valore_tot / ?) * 100 AS perc " +
                               "  FROM ANALISI_ABC " +
                               "), CumulatedData AS ( " +
                               "  SELECT codice_articolo, perc, " +
                               "         SUM(perc) OVER (ORDER BY valore_tot DESC, codice_articolo) AS cum_perc " +
                               "  FROM ANALISI_ABC JOIN PercentData USING(codice_articolo) " +
                               ") " +
                               "UPDATE ANALISI_ABC " +
                               "SET percentuale = CumulatedData.perc, " +
                               "    perc_cumulata = CumulatedData.cum_perc, " +
                               "    classe = CASE " +
                               "               WHEN CumulatedData.cum_perc <= 80 THEN 'A' " +
                               "               WHEN CumulatedData.cum_perc <= 95 THEN 'B' " +
                               "               ELSE 'C' " +
                               "             END " +
                               "FROM CumulatedData " +
                               "WHERE ANALISI_ABC.codice_articolo = CumulatedData.codice_articolo";

            try (PreparedStatement pstmt = connection.prepareStatement(updateSql)) {
                pstmt.setBigDecimal(1, valoreTotaleMagazzino);
                pstmt.executeUpdate();
            }

            // Visualizza Classe A
            System.out.println("CLASSE A - Alto valore (fino a 80% del valore)");
            System.out.println("-".repeat(50));
            visualizzaClasseAbc("A");

            // Visualizza riepilogo Classe B e C
            System.out.println("\nCLASSE B - Medio valore (da 80% a 95% del valore)");
            System.out.println("-".repeat(50));
            visualizzaRiepilogoClasseAbc("B");

            System.out.println("\nCLASSE C - Basso valore (oltre 95% del valore)");
            System.out.println("-".repeat(50));
            visualizzaRiepilogoClasseAbc("C");

            stmt.execute("DROP TABLE ANALISI_ABC");

        } catch (SQLException e) {
            handleSqlException(e, "analisiAbc");
        }
    }

    /**
     * Metodo helper per visualizzare i dettagli di una classe ABC.
     * @param classe La classe da visualizzare ('A', 'B', o 'C').
     */
    private void visualizzaClasseAbc(String classe) throws SQLException {
        String sql = "SELECT codice_articolo, descrizione, valore_tot, percentuale " +
                     "FROM ANALISI_ABC WHERE classe = ? ORDER BY valore_tot DESC";
        int count = 0;
        try (PreparedStatement pstmt = connection.prepareStatement(sql)) {
            pstmt.setString(1, classe);
            try (ResultSet rs = pstmt.executeQuery()) {
                while (rs.next()) {
                    count++;
                    System.out.println(rs.getString("codice_articolo") + " - " + rs.getString("descrizione"));
                    System.out.printf("  Valore: EUR %-15s (%s%%)%n",
                                      formatoImporto.format(rs.getBigDecimal("valore_tot")),
                                      formatoPercentuale.format(rs.getBigDecimal("percentuale")));
                }
            }
        }
        System.out.println("\nArticoli classe " + classe + ": " + formatoNumero.format(count));
    }

    /**
     * Metodo helper per visualizzare il riepilogo di una classe ABC.
     * @param classe La classe di cui visualizzare il riepilogo.
     */
    private void visualizzaRiepilogoClasseAbc(String classe) throws SQLException {
        String sql = "SELECT COUNT(*), SUM(valore_tot) FROM ANALISI_ABC WHERE classe = ?";
        try (PreparedStatement pstmt = connection.prepareStatement(sql)) {
            pstmt.setString(1, classe);
            try (ResultSet rs = pstmt.executeQuery()) {
                if (rs.next()) {
                    System.out.printf("Articoli: %-10s Valore totale: EUR %s%n",
                                      formatoNumero.format(rs.getInt(1)),
                                      formatoImporto.format(rs.getBigDecimal(2)));
                }
            }
        }
    }

    // --- Metodi di Utilità ---

    /**
     * Gestisce le eccezioni SQL, emulando il comportamento di SQLCODE.
     *
     * @param e       L'eccezione SQL catturata.
     * @param context Il nome del metodo dove si è verificato l'errore.
     */
    private void handleSqlException(SQLException e, String context) {
        if ("02000".equals(e.getSQLState())) {
            sqlcode = 100; // Not found
        } else {
            sqlcode = -e.getErrorCode();
            System.err.printf("Errore SQL nel contesto '%s': SQLCODE=%d, SQLSTATE=%s, Messaggio=%s%n",
                              context, sqlcode, e.getSQLState(), e.getMessage());
        }
    }

    /**
     * Esegue un rollback della transazione in modo sicuro, gestendo eventuali eccezioni.
     */
    private void safeRollback() {
        if (connection != null) {
            try {
                connection.rollback();
                System.out.println("Transazione annullata (rollback).");
            } catch (SQLException ex) {
                System.err.println("Errore critico durante il rollback: " + ex.getMessage());
            }
        }
    }
}