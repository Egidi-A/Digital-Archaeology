package com;

Assolutamente. Ecco la traduzione completa del programma COBOL `GESTIONE-MAGAZZINO` in un singolo file Java moderno, utilizzando JDBC per le operazioni sul database PostgreSQL. Il codice è stato strutturato per essere leggibile, manutenibile e compilabile.

```java
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.sql.Timestamp;
import java.sql.Types;
import java.text.DecimalFormat;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

/**
 * <h1>Gestione Magazzino</h1>
 * <p>
 * Traduzione Java del programma COBOL GESTIONE-MAGAZZINO.
 * Questo programma gestisce le operazioni di un magazzino, inclusi carico/scarico merce,
 * gestione ordini, inventario e reportistica, interfacciandosi con un database PostgreSQL tramite JDBC.
 * </p>
 * <p><b>Autore Originale COBOL:</b> ANNALISA-EGIDI</p>
 * <p><b>Traduzione Java:</b> Compilatore Avanzato AI</p>
 */
public class GestioneMagazzino {

    // --- Configurazione Database ---
    private static final String DB_URL = "jdbc:postgresql://localhost:5432/magazzino";
    private static final String DB_USER = "postgres";
    private static final String DB_PASSWORD = "password";

    // --- Connessione e I/O ---
    private Connection connection;
    private final Scanner scanner;
    private int sqlcode = 0;

    // --- Variabili della Working-Storage Section ---
    private int wsScelta = 0;
    private String wsContinua = "S";
    private String wsRisposta = "";
    private String wsEsito = "";
    private String wsMetodoValorizz = "F";

    // Articolo
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

    // Giacenza
    private BigDecimal wsGiaDisponibile = BigDecimal.ZERO;
    private BigDecimal wsGiaImpegnata = BigDecimal.ZERO;
    private BigDecimal wsGiaOrdinata = BigDecimal.ZERO;
    private BigDecimal wsGiaValMedio = BigDecimal.ZERO;
    private BigDecimal wsGiaValUltimo = BigDecimal.ZERO;

    // Movimento
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

    // Ordine
    private String wsOrdNumero = "";
    private String wsOrdData = "";
    private String wsOrdFornitore = "";
    private String wsOrdStato = "";
    private BigDecimal wsOrdTotale = BigDecimal.ZERO;

    // Lotto
    private int wsLotId = 0;
    private String wsLotNumero = "";
    private String wsLotData = "";
    private BigDecimal wsLotQtaIni = BigDecimal.ZERO;
    private BigDecimal wsLotQtaRes = BigDecimal.ZERO;
    private BigDecimal wsLotPrezzo = BigDecimal.ZERO;

    // Calcoli
    private BigDecimal wsQtaRichiesta = BigDecimal.ZERO;
    private BigDecimal wsQtaPrelevata = BigDecimal.ZERO;
    private BigDecimal wsQtaResidua = BigDecimal.ZERO;
    private BigDecimal wsValoreTot = BigDecimal.ZERO;
    private BigDecimal wsValoreMedio = BigDecimal.ZERO;
    private BigDecimal wsNuovoMedio = BigDecimal.ZERO;

    // Contatori
    private int wsContaArticoli = 0;
    private int wsContaSottoscorta = 0;
    private BigDecimal wsValoreMagazzino = BigDecimal.ZERO;

    // --- Formattatori per l'output ---
    private final DecimalFormat formatoImporto = new DecimalFormat("€ #,##0.00");
    private final DecimalFormat formatoQuantita = new DecimalFormat("#,##0.00");
    private final DecimalFormat formatoNumero = new DecimalFormat("#,##0");
    private final DecimalFormat formatoPercentuale = new DecimalFormat("0.00");

    /**
     * Costruttore della classe. Inizializza lo scanner per l'input.
     */
    public GestioneMagazzino() {
        this.scanner = new Scanner(System.in);
    }

    /**
     * Metodo main, punto di ingresso del programma.
     *
     * @param args Argomenti della riga di comando (non usati).
     */
    public static void main(String[] args) {
        GestioneMagazzino programma = new GestioneMagazzino();
        programma.mainLogic();
    }

    /**
     * Logica principale del programma, che emula la PROCEDURE DIVISION.
     */
    private void mainLogic() {
        if (connettiDatabase()) {
            do {
                visualizzaMenu();
                elaboraScelta();

                if (!wsContinua.equalsIgnoreCase("N")) {
                    System.out.println();
                    System.out.print("Continuare? (S/N): ");
                    wsContinua = scanner.nextLine();
                }

            } while (!wsContinua.equalsIgnoreCase("N"));

            disconnettiDatabase();
        }
        scanner.close();
        System.out.println("Programma terminato.");
    }

    /**
     * Corrisponde al paragrafo CONNETTI-DATABASE.
     * Stabilisce la connessione al database PostgreSQL.
     *
     * @return true se la connessione ha successo, false altrimenti.
     */
    private boolean connettiDatabase() {
        try {
            connection = DriverManager.getConnection(DB_URL, DB_USER, DB_PASSWORD);
            System.out.println("Connessione al database stabilita");
            return true;
        } catch (SQLException e) {
            System.err.println("Errore connessione database: " + e.getMessage());
            sqlcode = e.getErrorCode();
            return false;
        }
    }

    /**
     * Corrisponde al paragrafo DISCONNETTI-DATABASE.
     * Chiude la connessione al database.
     */
    private void disconnettiDatabase() {
        try {
            if (connection != null && !connection.isClosed()) {
                connection.close();
                System.out.println("Disconnesso dal database");
            }
        } catch (SQLException e) {
            System.err.println("Errore durante la disconnessione: " + e.getMessage());
        }
    }

    /**
     * Corrisponde al paragrafo VISUALIZZA-MENU.
     * Mostra il menu principale delle opzioni.
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
        try {
            wsScelta = Integer.parseInt(scanner.nextLine());
        } catch (NumberFormatException e) {
            wsScelta = -1; // Scelta non valida
        }
    }

    /**
     * Corrisponde al paragrafo ELABORA-SCELTA.
     * Esegue l'azione scelta dall'utente.
     */
    private void elaboraScelta() {
        switch (wsScelta) {
            case 1: caricoMerce(); break;
            case 2: scaricoMerce(); break;
            case 3: visualizzaGiacenza(); break;
            case 4: listaSottoscorta(); break;
            case 5: valorizzazioneMagazzino(); break;
            case 6: movimentiArticolo(); break;
            case 7: rettificaInventario(); break;
            case 8: gestioneOrdini(); break;
            case 9: reportInventario(); break;
            case 10: analisiAbc(); break;
            case 0: wsContinua = "N"; break;
            default: System.out.println("Scelta non valida!"); break;
        }
    }

    /**
     * Corrisponde al paragrafo CARICO-MERCE.
     */
    private void caricoMerce() {
        System.out.println("\n=== CARICO MERCE ===");
        System.out.print("Codice articolo: ");
        wsArtCodice = scanner.nextLine().trim();

        if (!caricaArticolo()) {
            return;
        }
        System.out.println("Articolo: " + wsArtDescrizione);

        System.out.print("Numero documento (DDT/Fattura): ");
        wsMovNumeroDoc = scanner.nextLine().trim();
        
        System.out.print("Quantità da caricare: ");
        try {
            wsMovQuantita = new BigDecimal(scanner.nextLine());
            if (wsMovQuantita.compareTo(BigDecimal.ZERO) <= 0) {
                System.out.println("Quantità non valida!");
                return;
            }
        } catch (NumberFormatException e) {
            System.out.println("Input quantità non valido!");
            return;
        }

        System.out.print("Prezzo unitario: ");
        try {
            wsMovPrezzo = new BigDecimal(scanner.nextLine());
            if (wsMovPrezzo.compareTo(BigDecimal.ZERO) <= 0) {
                System.out.println("Prezzo non valido!");
                return;
            }
        } catch (NumberFormatException e) {
            System.out.println("Input prezzo non valido!");
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

            registraMovimento("CA");
            if ("OK".equals(wsEsito)) {
                aggiornaGiacenzaCarico();
            }
            if ("OK".equals(wsEsito) && !wsLotNumero.isEmpty()) {
                creaLotto();
            }

            if ("OK".equals(wsEsito)) {
                connection.commit();
                System.out.println("Carico registrato con successo!");
                System.out.println("Valore carico: " + formatoImporto.format(wsMovValore));
            } else {
                throw new SQLException("Errore logico durante il carico, rollback in corso.");
            }

        } catch (SQLException e) {
            System.err.println("Errore durante il carico! " + e.getMessage());
            rollbackTransazione();
        } finally {
            resetAutoCommit();
        }
    }

    /**
     * Corrisponde al paragrafo SCARICO-MERCE.
     */
    private void scaricoMerce() {
        System.out.println("\n=== SCARICO MERCE ===");
        System.out.print("Codice articolo: ");
        wsArtCodice = scanner.nextLine().trim();

        if (!caricaArticolo()) {
            return;
        }
        System.out.println("Articolo: " + wsArtDescrizione);
        System.out.println("Disponibile: " + formatoQuantita.format(wsGiaDisponibile) + " " + wsArtUm);

        System.out.print("Quantità da scaricare: ");
        try {
            wsMovQuantita = new BigDecimal(scanner.nextLine());
        } catch (NumberFormatException e) {
            System.out.println("Input quantità non valido!");
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

            calcolaValoreScarico();
            if ("OK".equals(wsEsito)) {
                registraMovimento("SC");
            }
            if ("OK".equals(wsEsito)) {
                aggiornaGiacenzaScarico();
            }
            if ("OK".equals(wsEsito) && !"M".equals(wsMetodoValorizz)) {
                aggiornaLottiScarico();
            }

            if ("OK".equals(wsEsito)) {
                connection.commit();
                System.out.println("Scarico registrato con successo!");
                System.out.println("Valore scarico: " + formatoImporto.format(wsMovValore));
            } else {
                throw new SQLException("Errore logico durante lo scarico, rollback in corso.");
            }

        } catch (SQLException e) {
            System.err.println("Errore durante lo scarico! " + e.getMessage());
            rollbackTransazione();
        } finally {
            resetAutoCommit();
        }
    }

    /**
     * Corrisponde al paragrafo CARICA-ARTICOLO.
     * Carica i dati di un articolo e la sua giacenza dal database.
     *
     * @return true se l'articolo è stato trovato e caricato, false altrimenti.
     */
    private boolean caricaArticolo() {
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
            ResultSet rs = pstmt.executeQuery();

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
                    return false;
                }
                wsEsito = "OK";
                return true;
            } else {
                sqlcode = 100;
                System.out.println("Articolo non trovato!");
                wsEsito = "KO";
                return false;
            }
        } catch (SQLException e) {
            handleSqlException(e, "Errore database in caricaArticolo");
            wsEsito = "KO";
            return false;
        }
    }

    /**
     * Corrisponde al paragrafo REGISTRA-MOVIMENTO.
     * @param tipoMovimento Il tipo di movimento (es. "CA", "SC").
     */
    private void registraMovimento(String tipoMovimento) {
        wsMovTipo = tipoMovimento;
        wsMovOperatore = System.getProperty("user.name");
        String sql = "INSERT INTO MOVIMENTI_MAGAZZINO " +
                     "(tipo_movimento, numero_documento, codice_articolo, quantita, " +
                     "prezzo_unitario, valore_totale, causale, codice_fornitore, operatore) " +
                     "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)";

        try (PreparedStatement pstmt = connection.prepareStatement(sql)) {
            pstmt.setString(1, wsMovTipo);
            pstmt.setString(2, wsMovNumeroDoc);
            pstmt.setString(3, wsArtCodice);
            pstmt.setBigDecimal(4, wsMovQuantita);
            pstmt.setBigDecimal(5, wsMovPrezzo.setScale(4, RoundingMode.HALF_UP));
            pstmt.setBigDecimal(6, wsMovValore.setScale(2, RoundingMode.HALF_UP));
            pstmt.setString(7, wsMovCausale);
            
            if (wsMovFornitore != null && !wsMovFornitore.trim().isEmpty()) {
                pstmt.setString(8, wsMovFornitore);
            } else {
                pstmt.setNull(8, Types.CHAR);
            }
            
            pstmt.setString(9, wsMovOperatore);

            int rowsAffected = pstmt.executeUpdate();
            if (rowsAffected == 0) {
                throw new SQLException("Inserimento movimento fallito, nessuna riga modificata.");
            }
            sqlcode = 0;
        } catch (SQLException e) {
            handleSqlException(e, "Errore registrazione movimento");
            wsEsito = "KO";
        }
    }

    /**
     * Corrisponde al paragrafo AGGIORNA-GIACENZA-CARICO.
     */
    private void aggiornaGiacenzaCarico() throws SQLException {
        if (wsGiaDisponibile.add(wsMovQuantita).compareTo(BigDecimal.ZERO) != 0) {
            wsValoreTot = (wsGiaDisponibile.multiply(wsGiaValMedio)).add(wsMovQuantita.multiply(wsMovPrezzo));
            wsNuovoMedio = wsValoreTot.divide(wsGiaDisponibile.add(wsMovQuantita), 4, RoundingMode.HALF_UP);
        } else {
            wsNuovoMedio = wsMovPrezzo;
        }

        String updateSql = "UPDATE GIACENZE SET quantita_disponibile = quantita_disponibile + ?, " +
                           "valore_medio = ?, valore_ultimo = ?, data_ultimo_carico = CURRENT_DATE " +
                           "WHERE codice_articolo = ?";
        
        try (PreparedStatement pstmt = connection.prepareStatement(updateSql)) {
            pstmt.setBigDecimal(1, wsMovQuantita);
            pstmt.setBigDecimal(2, wsNuovoMedio);
            pstmt.setBigDecimal(3, wsMovPrezzo.setScale(4, RoundingMode.HALF_UP));
            pstmt.setString(4, wsArtCodice);
            
            int rowsAffected = pstmt.executeUpdate();
            if (rowsAffected == 0) {
                // Giacenza non esiste, creala
                String insertSql = "INSERT INTO GIACENZE (codice_articolo, quantita_disponibile, " +
                                   "valore_medio, valore_ultimo, data_ultimo_carico) " +
                                   "VALUES (?, ?, ?, ?, CURRENT_DATE)";
                try (PreparedStatement insertPstmt = connection.prepareStatement(insertSql)) {
                    insertPstmt.setString(1, wsArtCodice);
                    insertPstmt.setBigDecimal(2, wsMovQuantita);
                    insertPstmt.setBigDecimal(3, wsMovPrezzo.setScale(4, RoundingMode.HALF_UP));
                    insertPstmt.setBigDecimal(4, wsMovPrezzo.setScale(4, RoundingMode.HALF_UP));
                    insertPstmt.executeUpdate();
                }
            }
            sqlcode = 0;
        } catch (SQLException e) {
            handleSqlException(e, "Errore aggiornamento giacenza");
            wsEsito = "KO";
            throw e; // Rilancia per il rollback
        }
    }

    /**
     * Corrisponde al paragrafo AGGIORNA-GIACENZA-SCARICO.
     */
    private void aggiornaGiacenzaScarico() throws SQLException {
        String sql = "UPDATE GIACENZE SET quantita_disponibile = quantita_disponibile - ?, " +
                     "data_ultimo_scarico = CURRENT_DATE WHERE codice_articolo = ?";
        try (PreparedStatement pstmt = connection.prepareStatement(sql)) {
            pstmt.setBigDecimal(1, wsMovQuantita);
            pstmt.setString(2, wsArtCodice);
            int rowsAffected = pstmt.executeUpdate();
            if (rowsAffected == 0) {
                throw new SQLException("Aggiornamento giacenza scarico fallito.");
            }
            sqlcode = 0;
        } catch (SQLException e) {
            handleSqlException(e, "Errore aggiornamento giacenza");
            wsEsito = "KO";
            throw e;
        }
    }

    /**
     * Corrisponde al paragrafo CREA-LOTTO.
     */
    private void creaLotto() throws SQLException {
        String sql = "INSERT INTO LOTTI (codice_articolo, numero_lotto, data_carico, " +
                     "quantita_iniziale, quantita_residua, prezzo_acquisto) " +
                     "VALUES (?, ?, CURRENT_DATE, ?, ?, ?)";
        try (PreparedStatement pstmt = connection.prepareStatement(sql)) {
            pstmt.setString(1, wsArtCodice);
            pstmt.setString(2, wsLotNumero);
            pstmt.setBigDecimal(3, wsMovQuantita);
            pstmt.setBigDecimal(4, wsMovQuantita);
            pstmt.setBigDecimal(5, wsMovPrezzo.setScale(4, RoundingMode.HALF_UP));
            pstmt.executeUpdate();
            sqlcode = 0;
        } catch (SQLException e) {
            if ("23505".equals(e.getSQLState())) { // Unique violation
                System.out.println("Lotto già esistente!");
            } else {
                handleSqlException(e, "Errore creazione lotto");
            }
            wsEsito = "KO";
            throw e;
        }
    }

    /**
     * Corrisponde al paragrafo CALCOLA-VALORE-SCARICO.
     */
    private void calcolaValoreScarico() throws SQLException {
        switch (wsMetodoValorizz) {
            case "F":
                calcolaValoreFifoLifo(true);
                break;
            case "L":
                calcolaValoreFifoLifo(false);
                break;
            case "M":
                wsMovPrezzo = wsGiaValMedio;
                wsMovValore = wsMovQuantita.multiply(wsMovPrezzo).setScale(2, RoundingMode.HALF_UP);
                break;
            default:
                System.out.println("Metodo valorizzazione non valido!");
                wsEsito = "KO";
                break;
        }
    }

    /**
     * Unifica la logica di CALCOLA-VALORE-FIFO e CALCOLA-VALORE-LIFO.
     * @param isFifo true per FIFO, false per LIFO.
     */
    private void calcolaValoreFifoLifo(boolean isFifo) throws SQLException {
        String sql = "SELECT id_lotto, numero_lotto, quantita_residua, prezzo_acquisto, data_carico " +
                     "FROM LOTTI WHERE codice_articolo = ? AND quantita_residua > 0 AND stato = 'A' " +
                     "ORDER BY data_carico " + (isFifo ? "ASC, id_lotto ASC" : "DESC, id_lotto DESC");

        wsQtaRichiesta = wsMovQuantita;
        wsMovValore = BigDecimal.ZERO;

        try (PreparedStatement pstmt = connection.prepareStatement(sql)) {
            pstmt.setString(1, wsArtCodice);
            ResultSet rs = pstmt.executeQuery();
            
            while (rs.next() && wsQtaRichiesta.compareTo(BigDecimal.ZERO) > 0) {
                sqlcode = 0;
                wsLotQtaRes = rs.getBigDecimal("quantita_residua");
                wsLotPrezzo = rs.getBigDecimal("prezzo_acquisto");

                BigDecimal qtaDaPrelevare = wsLotQtaRes.min(wsQtaRichiesta);
                wsValoreTot = qtaDaPrelevare.multiply(wsLotPrezzo);
                wsMovValore = wsMovValore.add(wsValoreTot);
                wsQtaRichiesta = wsQtaRichiesta.subtract(qtaDaPrelevare);
            }
            sqlcode = rs.isAfterLast() ? 0 : 100;
        }

        if (wsQtaRichiesta.compareTo(BigDecimal.ZERO) > 0) {
            System.out.println("Lotti insufficienti per " + (isFifo ? "FIFO" : "LIFO") + "!");
            wsEsito = "KO";
        } else {
            wsMovPrezzo = wsMovValore.divide(wsMovQuantita, 4, RoundingMode.HALF_UP);
        }
    }

    /**
     * Corrisponde al paragrafo AGGIORNA-LOTTI-SCARICO.
     */
    private void aggiornaLottiScarico() throws SQLException {
        boolean isFifo = "F".equals(wsMetodoValorizz);
        String selectSql = "SELECT id_lotto, quantita_residua FROM LOTTI " +
                           "WHERE codice_articolo = ? AND quantita_residua > 0 AND stato = 'A' " +
                           "ORDER BY data_carico " + (isFifo ? "ASC, id_lotto ASC" : "DESC, id_lotto DESC");
        
        String updateSql = "UPDATE LOTTI SET quantita_residua = ?, stato = ? WHERE id_lotto = ?";
        wsQtaRichiesta = wsMovQuantita;

        // Usiamo una lista per evitare problemi con il ResultSet durante l'update (ResultSet holdability)
        List<LottoDaAggiornare> lotti = new ArrayList<>();
        try (PreparedStatement pstmt = connection.prepareStatement(selectSql)) {
            pstmt.setString(1, wsArtCodice);
            ResultSet rs = pstmt.executeQuery();
            while(rs.next()) {
                lotti.add(new LottoDaAggiornare(rs.getInt("id_lotto"), rs.getBigDecimal("quantita_residua")));
            }
        }

        try (PreparedStatement updatePstmt = connection.prepareStatement(updateSql)) {
            for (LottoDaAggiornare lotto : lotti) {
                if (wsQtaRichiesta.compareTo(BigDecimal.ZERO) <= 0) break;

                BigDecimal qtaDaPrelevare = lotto.quantitaResidua.min(wsQtaRichiesta);
                BigDecimal nuovaQtaResidua = lotto.quantitaResidua.subtract(qtaDaPrelevare);
                String nuovoStato = nuovaQtaResidua.compareTo(BigDecimal.ZERO) > 0 ? "A" : "E";

                updatePstmt.setBigDecimal(1, nuovaQtaResidua);
                updatePstmt.setString(2, nuovoStato);
                updatePstmt.setInt(3, lotto.id);
                updatePstmt.addBatch();

                wsQtaRichiesta = wsQtaRichiesta.subtract(qtaDaPrelevare);
            }
            updatePstmt.executeBatch();
        }
    }
    
    // Classe helper per aggiornamento lotti
    private static class LottoDaAggiornare {
        int id;
        BigDecimal quantitaResidua;
        LottoDaAggiornare(int id, BigDecimal quantitaResidua) {
            this.id = id;
            this.quantitaResidua = quantitaResidua;
        }
    }

    /**
     * Corrisponde al paragrafo VISUALIZZA-GIACENZA.
     */
    private void visualizzaGiacenza() {
        System.out.println("\n=== VISUALIZZA GIACENZA ===");
        System.out.print("Codice articolo: ");
        wsArtCodice = scanner.nextLine().trim();

        if (!caricaArticolo()) {
            return;
        }

        System.out.println("\nARTICOLO: " + wsArtCodice);
        System.out.println("Descrizione: " + wsArtDescrizione);
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
        System.out.println("  Valore medio:    " + formatoImporto.format(wsGiaValMedio));
        System.out.println("  Valore ultimo:   " + formatoImporto.format(wsGiaValUltimo));
        wsValoreTot = wsGiaDisponibile.multiply(wsGiaValMedio);
        System.out.println("  Valore totale:   " + formatoImporto.format(wsValoreTot));

        if (wsGiaDisponibile.compareTo(wsArtPuntoRiord) <= 0 && wsArtPuntoRiord.compareTo(BigDecimal.ZERO) > 0) {
             System.out.println("\n*** ATTENZIONE: PUNTO RIORDINO RAGGIUNTO ***");
        } else if (wsGiaDisponibile.compareTo(wsArtScortaMin) <= 0 && wsArtScortaMin.compareTo(BigDecimal.ZERO) > 0) {
             System.out.println("\n*** ATTENZIONE: SCORTA MINIMA RAGGIUNTA ***");
        }
    }

    /**
     * Corrisponde al paragrafo LISTA-SOTTOSCORTA.
     */
    private void listaSottoscorta() {
        System.out.println("\n=== ARTICOLI SOTTOSCORTA ===");
        String sql = "SELECT a.codice_articolo, a.descrizione, g.quantita_disponibile, " +
                     "g.quantita_ordinata, a.punto_riordino, a.lotto_riordino, f.ragione_sociale " +
                     "FROM ARTICOLI a JOIN GIACENZE g ON a.codice_articolo = g.codice_articolo " +
                     "LEFT JOIN FORNITORI f ON a.codice_fornitore = f.codice_fornitore " +
                     "WHERE a.stato = 'A' AND g.quantita_disponibile <= a.punto_riordino AND a.punto_riordino > 0 " +
                     "ORDER BY (a.punto_riordino - g.quantita_disponibile) DESC";
        
        wsContaSottoscorta = 0;
        try (PreparedStatement pstmt = connection.prepareStatement(sql);
             ResultSet rs = pstmt.executeQuery()) {
            
            while (rs.next()) {
                wsContaSottoscorta++;
                wsArtCodice = rs.getString(1);
                wsArtDescrizione = rs.getString(2);
                wsGiaDisponibile = rs.getBigDecimal(3);
                wsGiaOrdinata = rs.getBigDecimal(4);
                wsArtPuntoRiord = rs.getBigDecimal(5);
                wsArtLottoRiord = rs.getBigDecimal(6);
                String fornitore = rs.getString(7);

                System.out.println("\n" + wsArtCodice + " - " + wsArtDescrizione);
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
            }
            System.out.println("\nTotale articoli sottoscorta: " + formatoNumero.format(wsContaSottoscorta));

        } catch (SQLException e) {
            handleSqlException(e, "Errore in listaSottoscorta");
        }
    }

    /**
     * Corrisponde al paragrafo VALORIZZAZIONE-MAGAZZINO.
     */
    private void valorizzazioneMagazzino() {
        System.out.println("\n=== VALORIZZAZIONE MAGAZZINO ===");
        String sql = "SELECT a.codice_articolo, a.descrizione, a.unita_misura, a.ubicazione, " +
                     "g.quantita_disponibile, g.valore_medio " +
                     "FROM ARTICOLI a JOIN GIACENZE g ON a.codice_articolo = g.codice_articolo " +
                     "WHERE a.stato = 'A' ORDER BY a.codice_articolo";

        try (PrintWriter writer = new PrintWriter(new FileWriter("REPORT-MAGAZZINO.TXT"));
             PreparedStatement pstmt = connection.prepareStatement(sql);
             ResultSet rs = pstmt.executeQuery()) {

            writer.println("REPORT VALORIZZAZIONE MAGAZZINO");
            writer.println("===============================");
            writer.println("Data: " + LocalDate.now() + " Ora: " + LocalDateTime.now().format(DateTimeFormatter.ofPattern("HH:mm:ss")));
            writer.println();

            wsContaArticoli = 0;
            wsValoreMagazzino = BigDecimal.ZERO;

            while (rs.next()) {
                wsGiaDisponibile = rs.getBigDecimal("quantita_disponibile");
                if (wsGiaDisponibile.compareTo(BigDecimal.ZERO) > 0) {
                    wsContaArticoli++;
                    wsArtCodice = rs.getString("codice_articolo");
                    wsArtDescrizione = rs.getString("descrizione");
                    wsArtUm = rs.getString("unita_misura");
                    wsGiaValMedio = rs.getBigDecimal("valore_medio");
                    
                    wsValoreTot = wsGiaDisponibile.multiply(wsGiaValMedio);
                    wsValoreMagazzino = wsValoreMagazzino.add(wsValoreTot);

                    writer.printf("%-10s %-40s%n", wsArtCodice, wsArtDescrizione);
                    writer.printf("  Qtà: %-15s UM: %-5s Val.medio: %-15s Totale: %s%n",
                        formatoQuantita.format(wsGiaDisponibile),
                        wsArtUm,
                        formatoImporto.format(wsGiaValMedio),
                        formatoImporto.format(wsValoreTot));
                }
            }

            writer.println();
            writer.println("-------------------------------");
            writer.println("Articoli valorizzati: " + formatoNumero.format(wsContaArticoli));
            writer.println("VALORE TOTALE MAGAZZINO: " + formatoImporto.format(wsValoreMagazzino));

            System.out.println("Report salvato in REPORT-MAGAZZINO.TXT");
            System.out.println("Articoli valorizzati: " + formatoNumero.format(wsContaArticoli));
            System.out.println("Valore totale: " + formatoImporto.format(wsValoreMagazzino));

        } catch (IOException e) {
            System.err.println("Errore durante la scrittura del file di report: " + e.getMessage());
        } catch (SQLException e) {
            handleSqlException(e, "Errore in valorizzazioneMagazzino");
        }
    }
    
    /**
     * Corrisponde al paragrafo MOVIMENTI-ARTICOLO.
     */
    private void movimentiArticolo() {
        System.out.println("\n=== MOVIMENTI ARTICOLO ===");
        System.out.print("Codice articolo: ");
        wsArtCodice = scanner.nextLine().trim();

        if (!caricaArticolo()) {
            return;
        }

        System.out.println("\nUltimi 50 movimenti di: " + wsArtDescrizione);
        System.out.println();
        System.out.printf("%-5s %-20s %-20s %12s %15s%n", "TIPO", "DATA/ORA", "DOCUMENTO", "QUANTITA", "VALORE");
        System.out.println("-".repeat(75));

        String sql = "SELECT tipo_movimento, data_movimento, numero_documento, quantita, valore_totale, causale " +
                     "FROM MOVIMENTI_MAGAZZINO WHERE codice_articolo = ? " +
                     "ORDER BY data_movimento DESC LIMIT 50";

        try (PreparedStatement pstmt = connection.prepareStatement(sql)) {
            pstmt.setString(1, wsArtCodice);
            ResultSet rs = pstmt.executeQuery();

            while (rs.next()) {
                wsMovTipo = rs.getString("tipo_movimento");
                Timestamp dataMov = rs.getTimestamp("data_movimento");
                wsMovNumeroDoc = rs.getString("numero_documento");
                wsMovQuantita = rs.getBigDecimal("quantita");
                wsMovValore = rs.getBigDecimal("valore_totale");
                wsMovCausale = rs.getString("causale");

                String segno = (wsMovTipo.equals("SC") || wsMovTipo.equals("RE")) ? "-" : "+";
                
                System.out.printf("%-5s %-20s %-20s %s%11s %15s%n",
                    wsMovTipo,
                    dataMov.toLocalDateTime().format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm")),
                    wsMovNumeroDoc,
                    segno,
                    formatoQuantita.format(wsMovQuantita),
                    formatoImporto.format(wsMovValore));
                System.out.println("      Causale: " + wsMovCausale);
                System.out.println();
            }
        } catch (SQLException e) {
            handleSqlException(e, "Errore in movimentiArticolo");
        }
    }

    /**
     * Corrisponde al paragrafo RETTIFICA-INVENTARIO.
     */
    private void rettificaInventario() {
        System.out.println("\n=== RETTIFICA INVENTARIO ===");
        System.out.print("Codice articolo: ");
        wsArtCodice = scanner.nextLine().trim();

        if (!caricaArticolo()) return;

        System.out.println("Articolo: " + wsArtDescrizione);
        System.out.println("Giacenza attuale: " + formatoQuantita.format(wsGiaDisponibile) + " " + wsArtUm);
        
        System.out.print("Giacenza rilevata: ");
        try {
            wsQtaRichiesta = new BigDecimal(scanner.nextLine());
        } catch (NumberFormatException e) {
            System.out.println("Input non valido.");
            return;
        }

        BigDecimal differenza = wsQtaRichiesta.subtract(wsGiaDisponibile);
        if (differenza.compareTo(BigDecimal.ZERO) == 0) {
            System.out.println("Nessuna differenza rilevata.");
            return;
        }

        wsMovQuantita = differenza.abs();
        if (differenza.compareTo(BigDecimal.ZERO) > 0) {
            System.out.println("Differenza: +" + formatoQuantita.format(differenza));
            wsMovCausale = "Rettifica inventario positiva";
        } else {
            System.out.println("Differenza: " + formatoQuantita.format(differenza));
            wsMovCausale = "Rettifica inventario negativa";
        }
        
        System.out.print("Note rettifica: ");
        String note = scanner.nextLine();
        wsMovCausale += ". Note: " + note;

        System.out.print("Confermare rettifica (S/N): ");
        wsRisposta = scanner.nextLine();

        if (wsRisposta.equalsIgnoreCase("S")) {
            wsMovNumeroDoc = "INV" + LocalDate.now().format(DateTimeFormatter.ofPattern("yyyyMMdd"));
            wsMovPrezzo = wsGiaValMedio;
            wsMovValore = wsMovQuantita.multiply(wsMovPrezzo).setScale(2, RoundingMode.HALF_UP);
            wsEsito = "OK";

            try {
                connection.setAutoCommit(false);
                registraMovimento("RI");

                if ("OK".equals(wsEsito)) {
                    String sqlUpdate = "UPDATE GIACENZE SET quantita_disponibile = ?, " +
                                       "data_ultimo_inventario = CURRENT_DATE WHERE codice_articolo = ?";
                    try (PreparedStatement pstmt = connection.prepareStatement(sqlUpdate)) {
                        pstmt.setBigDecimal(1, wsQtaRichiesta);
                        pstmt.setString(2, wsArtCodice);
                        if (pstmt.executeUpdate() == 0) {
                            throw new SQLException("Aggiornamento giacenza per rettifica fallito.");
                        }
                    }
                }
                
                if ("OK".equals(wsEsito)) {
                    connection.commit();
                    System.out.println("Rettifica eseguita con successo!");
                } else {
                    throw new SQLException("Errore logico durante la rettifica.");
                }

            } catch (SQLException e) {
                handleSqlException(e, "Errore durante la rettifica");
                rollbackTransazione();
            } finally {
                resetAutoCommit();
            }
        } else {
            System.out.println("Rettifica annullata.");
        }
    }

    /**
     * Corrisponde al paragrafo GESTIONE-ORDINI.
     */
    private void gestioneOrdini() {
        System.out.println("\n=== GESTIONE ORDINI FORNITORI ===");
        System.out.println("1. Nuovo ordine");
        System.out.println("2. Visualizza ordini aperti");
        System.out.println("3. Ricevi merce da ordine");
        System.out.println("4. Stato ordine");
        System.out.print("\nScelta: ");
        try {
            wsScelta = Integer.parseInt(scanner.nextLine());
        } catch (NumberFormatException e) {
            wsScelta = -1;
        }

        switch (wsScelta) {
            case 1: nuovoOrdine(); break;
            case 2: visualizzaOrdiniAperti(); break;
            case 3: riceviMerceOrdine(); break;
            case 4: statoOrdine(); break;
            default: System.out.println("Scelta non valida!"); break;
        }
    }

    /**
     * Corrisponde al paragrafo NUOVO-ORDINE.
     */
    private void nuovoOrdine() {
        System.out.println("\n=== NUOVO ORDINE FORNITORE ===");
        System.out.print("Codice fornitore: ");
        wsOrdFornitore = scanner.nextLine().trim();

        String sqlCheckFornitore = "SELECT ragione_sociale FROM FORNITORI WHERE codice_fornitore = ? AND stato = 'A'";
        try (PreparedStatement pstmt = connection.prepareStatement(sqlCheckFornitore)) {
            pstmt.setString(1, wsOrdFornitore);
            ResultSet rs = pstmt.executeQuery();
            if (!rs.next()) {
                System.out.println("Fornitore non trovato o non attivo!");
                return;
            }
            System.out.println("Fornitore: " + rs.getString(1));
        } catch (SQLException e) {
            handleSqlException(e, "Errore verifica fornitore");
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
            handleSqlException(e, "Errore creazione ordine");
        }
    }

    /**
     * Corrisponde al paragrafo AGGIUNGI-RIGHE-ORDINE.
     */
    private void aggiungiRigheOrdine() {
        wsContinua = "S";
        wsOrdTotale = BigDecimal.ZERO;

        String sqlInsertRiga = "INSERT INTO RIGHE_ORDINE (numero_ordine, codice_articolo, quantita_ordinata, prezzo_unitario, importo_riga) VALUES (?, ?, ?, ?, ?)";
        String sqlUpdateGiacenza = "UPDATE GIACENZE SET quantita_ordinata = quantita_ordinata + ? WHERE codice_articolo = ?";
        
        try (PreparedStatement pstmtRiga = connection.prepareStatement(sqlInsertRiga);
             PreparedStatement pstmtGiacenza = connection.prepareStatement(sqlUpdateGiacenza)) {
            
            connection.setAutoCommit(false);

            while (wsContinua.equalsIgnoreCase("S")) {
                System.out.print("\nCodice articolo: ");
                wsArtCodice = scanner.nextLine().trim();
                if (caricaArticolo()) {
                    System.out.println("Articolo: " + wsArtDescrizione);
                    System.out.print("Quantità da ordinare: ");
                    try {
                        wsMovQuantita = new BigDecimal(scanner.nextLine());
                        if (wsMovQuantita.compareTo(BigDecimal.ZERO) > 0) {
                            wsMovPrezzo = wsArtPrezzoAcq;
                            wsMovValore = wsMovQuantita.multiply(wsMovPrezzo).setScale(2, RoundingMode.HALF_UP);

                            // Inserisci riga ordine
                            pstmtRiga.setString(1, wsOrdNumero);
                            pstmtRiga.setString(2, wsArtCodice);
                            pstmtRiga.setBigDecimal(3, wsMovQuantita);
                            pstmtRiga.setBigDecimal(4, wsMovPrezzo);
                            pstmtRiga.setBigDecimal(5, wsMovValore);
                            pstmtRiga.executeUpdate();

                            wsOrdTotale = wsOrdTotale.add(wsMovValore);

                            // Aggiorna giacenza
                            pstmtGiacenza.setBigDecimal(1, wsMovQuantita);
                            pstmtGiacenza.setString(2, wsArtCodice);
                            pstmtGiacenza.executeUpdate();

                            System.out.println("Riga ordine aggiunta!");
                        }
                    } catch (NumberFormatException e) {
                        System.out.println("Quantità non valida.");
                    }
                }
                System.out.print("Aggiungere altro articolo (S/N): ");
                wsContinua = scanner.nextLine();
            }

            // Aggiorna totale ordine
            String sqlUpdateOrdine = "UPDATE ORDINI SET totale_ordine = ? WHERE numero_ordine = ?";
            try (PreparedStatement pstmtOrdine = connection.prepareStatement(sqlUpdateOrdine)) {
                pstmtOrdine.setBigDecimal(1, wsOrdTotale);
                pstmtOrdine.setString(2, wsOrdNumero);
                pstmtOrdine.executeUpdate();
            }
            
            connection.commit();
            System.out.println("\nOrdine completato. Totale: " + formatoImporto.format(wsOrdTotale));

        } catch (SQLException e) {
            handleSqlException(e, "Errore aggiunta righe ordine");
            rollbackTransazione();
        } finally {
            resetAutoCommit();
        }
    }

    /**
     * Corrisponde al paragrafo VISUALIZZA-ORDINI-APERTI.
     */
    private void visualizzaOrdiniAperti() {
        System.out.println("\n=== ORDINI APERTI ===");
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
                    default: statoDesc = "SCONOSCIUTO"; break;
                }
                System.out.println("\n" + rs.getString("numero_ordine") + " del " + rs.getDate("data_ordine"));
                System.out.println("  Fornitore: " + rs.getString("ragione_sociale"));
                System.out.println("  Totale: " + formatoImporto.format(rs.getBigDecimal("totale_ordine")));
                System.out.println("  Stato: " + statoDesc);
            }
            if (count == 0) {
                System.out.println("Nessun ordine aperto.");
            }
        } catch (SQLException e) {
            handleSqlException(e, "Errore in visualizzaOrdiniAperti");
        }
    }

    /**
     * Corrisponde al paragrafo RICEVI-MERCE-ORDINE.
     */
    private void riceviMerceOrdine() {
        System.out.println("\n=== RICEVI MERCE DA ORDINE ===");
        System.out.print("Numero ordine: ");
        wsOrdNumero = scanner.nextLine().trim();

        try {
            // Verifica ordine
            String sqlCheck = "SELECT stato_ordine, codice_fornitore FROM ORDINI WHERE numero_ordine = ?";
            try (PreparedStatement pstmt = connection.prepareStatement(sqlCheck)) {
                pstmt.setString(1, wsOrdNumero);
                ResultSet rs = pstmt.executeQuery();
                if (!rs.next()) {
                    System.out.println("Ordine non trovato!");
                    return;
                }
                wsOrdStato = rs.getString("stato_ordine");
                wsOrdFornitore = rs.getString("codice_fornitore");
                if ("E".equals(wsOrdStato)) {
                    System.out.println("Ordine già completamente evaso!");
                    return;
                }
            }

            System.out.println("\nRighe ordine da ricevere:");
            String sqlRighe = "SELECT r.codice_articolo, a.descrizione, r.quantita_ordinata, " +
                              "r.quantita_ricevuta, r.prezzo_unitario " +
                              "FROM RIGHE_ORDINE r JOIN ARTICOLI a ON r.codice_articolo = a.codice_articolo " +
                              "WHERE r.numero_ordine = ? AND r.quantita_ricevuta < r.quantita_ordinata AND r.stato_riga <> 'C'";
            
            try (PreparedStatement pstmtRighe = connection.prepareStatement(sqlRighe)) {
                pstmtRighe.setString(1, wsOrdNumero);
                ResultSet rsRighe = pstmtRighe.executeQuery();
                while (rsRighe.next()) {
                    wsArtCodice = rsRighe.getString("codice_articolo");
                    wsArtDescrizione = rsRighe.getString("descrizione");
                    wsQtaRichiesta = rsRighe.getBigDecimal("quantita_ordinata");
                    wsQtaPrelevata = rsRighe.getBigDecimal("quantita_ricevuta");
                    wsMovPrezzo = rsRighe.getBigDecimal("prezzo_unitario");
                    wsQtaResidua = wsQtaRichiesta.subtract(wsQtaPrelevata);

                    System.out.println("\n" + wsArtCodice + " - " + wsArtDescrizione);
                    System.out.println("  Da ricevere: " + formatoQuantita.format(wsQtaResidua));
                    System.out.print("  Quantità ricevuta: ");
                    
                    try {
                        wsMovQuantita = new BigDecimal(scanner.nextLine());
                        if (wsMovQuantita.compareTo(BigDecimal.ZERO) > 0) {
                            if (wsMovQuantita.compareTo(wsQtaResidua) > 0) {
                                System.out.println("  Quantità eccessiva! Impostata a " + formatoQuantita.format(wsQtaResidua));
                                wsMovQuantita = wsQtaResidua;
                            }
                            processaRigaRicevuta();
                        }
                    } catch (NumberFormatException e) {
                        System.out.println("Input non valido, riga saltata.");
                    }
                }
            }
            aggiornaStatoOrdine();

        } catch (SQLException e) {
            handleSqlException(e, "Errore in riceviMerceOrdine");
        }
    }

    /**
     * Logica interna per processare una singola riga di merce ricevuta.
     */
    private void processaRigaRicevuta() {
        wsMovFornitore = wsOrdFornitore;
        wsMovCausale = "Ricevimento ordine " + wsOrdNumero;
        wsMovNumeroDoc = wsOrdNumero;
        wsMovValore = wsMovQuantita.multiply(wsMovPrezzo).setScale(2, RoundingMode.HALF_UP);
        wsEsito = "OK";

        try {
            connection.setAutoCommit(false);
            registraMovimento("CA");
            if ("OK".equals(wsEsito)) {
                aggiornaGiacenzaCarico();
            }
            if ("OK".equals(wsEsito)) {
                // Aggiorna riga ordine
                String sqlUpdateRiga = "UPDATE RIGHE_ORDINE SET quantita_ricevuta = quantita_ricevuta + ?, " +
                                       "stato_riga = CASE WHEN quantita_ricevuta + ? >= quantita_ordinata THEN 'E' ELSE 'P' END " +
                                       "WHERE numero_ordine = ? AND codice_articolo = ?";
                try (PreparedStatement pstmt = connection.prepareStatement(sqlUpdateRiga)) {
                    pstmt.setBigDecimal(1, wsMovQuantita);
                    pstmt.setBigDecimal(2, wsMovQuantita);
                    pstmt.setString(3, wsOrdNumero);
                    pstmt.setString(4, wsArtCodice);
                    pstmt.executeUpdate();
                }
                // Aggiorna giacenza ordinata
                String sqlUpdateGiacenza = "UPDATE GIACENZE SET quantita_ordinata = GREATEST(0, quantita_ordinata - ?) " +
                                           "WHERE codice_articolo = ?";
                try (PreparedStatement pstmt = connection.prepareStatement(sqlUpdateGiacenza)) {
                    pstmt.setBigDecimal(1, wsMovQuantita);
                    pstmt.setString(2, wsArtCodice);
                    pstmt.executeUpdate();
                }
            }

            if ("OK".equals(wsEsito)) {
                connection.commit();
                System.out.println("  Carico registrato!");
            } else {
                throw new SQLException("Errore logico durante la ricezione della riga.");
            }
        } catch (SQLException e) {
            handleSqlException(e, "Errore processamento riga ricevuta");
            rollbackTransazione();
        } finally {
            resetAutoCommit();
        }
    }

    /**
     * Corrisponde al paragrafo AGGIORNA-STATO-ORDINE.
     */
    private void aggiornaStatoOrdine() throws SQLException {
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
        }
    }

    /**
     * Corrisponde al paragrafo STATO-ORDINE.
     */
    private void statoOrdine() {
        System.out.println("\n=== STATO ORDINE ===");
        System.out.print("Numero ordine: ");
        wsOrdNumero = scanner.nextLine().trim();

        String sqlHeader = "SELECT o.data_ordine, o.stato_ordine, f.ragione_sociale, o.totale_ordine " +
                           "FROM ORDINI o JOIN FORNITORI f ON o.codice_fornitore = f.codice_fornitore " +
                           "WHERE o.numero_ordine = ?";
        try (PreparedStatement pstmt = connection.prepareStatement(sqlHeader)) {
            pstmt.setString(1, wsOrdNumero);
            ResultSet rs = pstmt.executeQuery();
            if (!rs.next()) {
                System.out.println("Ordine non trovato!");
                return;
            }
            String statoDesc;
            switch (rs.getString("stato_ordine")) {
                case "A": statoDesc = "APERTO"; break;
                case "C": statoDesc = "CONFERMATO"; break;
                case "P": statoDesc = "PARZIALMENTE EVASO"; break;
                case "E": statoDesc = "EVASO"; break;
                default: statoDesc = "SCONOSCIUTO"; break;
            }
            System.out.println("\nOrdine: " + wsOrdNumero);
            System.out.println("Data: " + rs.getDate("data_ordine"));
            System.out.println("Fornitore: " + rs.getString("ragione_sociale"));
            System.out.println("Totale: " + formatoImporto.format(rs.getBigDecimal("totale_ordine")));
            System.out.println("Stato: " + statoDesc);
        } catch (SQLException e) {
            handleSqlException(e, "Errore recupero testata ordine");
            return;
        }

        System.out.println("\nDettaglio righe:");
        String sqlRighe = "SELECT r.codice_articolo, a.descrizione, r.quantita_ordinata, r.quantita_ricevuta, r.stato_riga " +
                          "FROM RIGHE_ORDINE r JOIN ARTICOLI a ON r.codice_articolo = a.codice_articolo " +
                          "WHERE r.numero_ordine = ? ORDER BY r.codice_articolo";
        try (PreparedStatement pstmt = connection.prepareStatement(sqlRighe)) {
            pstmt.setString(1, wsOrdNumero);
            ResultSet rs = pstmt.executeQuery();
            while (rs.next()) {
                String statoRigaDesc;
                switch (rs.getString("stato_riga")) {
                    case "A": statoRigaDesc = "APERTA"; break;
                    case "P": statoRigaDesc = "PARZIALE"; break;
                    case "E": statoRigaDesc = "EVASA"; break;
                    case "C": statoRigaDesc = "CANCELLATA"; break;
                    default: statoRigaDesc = "SCONOSCIUTO"; break;
                }
                System.out.println("\n" + rs.getString("codice_articolo") + " - " + rs.getString("descrizione"));
                System.out.println("  Ordinata: " + formatoQuantita.format(rs.getBigDecimal("quantita_ordinata")));
                System.out.println("  Ricevuta: " + formatoQuantita.format(rs.getBigDecimal("quantita_ricevuta")) + " (" + statoRigaDesc + ")");
            }
        } catch (SQLException e) {
            handleSqlException(e, "Errore recupero righe ordine");
        }
    }

    /**
     * Corrisponde al paragrafo REPORT-INVENTARIO.
     */
    private void reportInventario() {
        System.out.println("\n=== REPORT INVENTARIO FISICO ===");
        String sql = "SELECT a.codice_articolo, a.descrizione, a.ubicazione, g.quantita_disponibile " +
                     "FROM ARTICOLI a JOIN GIACENZE g ON a.codice_articolo = g.codice_articolo " +
                     "WHERE a.stato = 'A' ORDER BY a.ubicazione, a.codice_articolo";

        try (PrintWriter writer = new PrintWriter(new FileWriter("INVENTARIO.TXT"));
             PreparedStatement pstmt = connection.prepareStatement(sql);
             ResultSet rs = pstmt.executeQuery()) {

            writer.println("LISTA INVENTARIO FISICO");
            writer.println("=======================");
            writer.println("Data: " + LocalDate.now() + " Ora: " + LocalDateTime.now().format(DateTimeFormatter.ofPattern("HH:mm:ss")));
            writer.println();
            writer.printf("%-12s %-30s %-8s %-15s %-15s %-15s%n", "CODICE", "DESCRIZIONE", "UB.", "QTA TEORICA", "QTA RILEVATA", "DIFFERENZA");
            writer.println("-".repeat(98));

            while (rs.next()) {
                writer.printf("%-12s %-30.30s %-8.8s %15s %-15s %-15s%n",
                    rs.getString("codice_articolo"),
                    rs.getString("descrizione"),
                    rs.getString("ubicazione"),
                    formatoQuantita.format(rs.getBigDecimal("quantita_disponibile")),
                    "______________",
                    "______________");
            }
            writer.println();
            writer.println("-".repeat(98));
            writer.println("Rilevato da: ________________  Data: ________  Firma: ________________");
            
            System.out.println("Report salvato in INVENTARIO.TXT");

        } catch (IOException e) {
            System.err.println("Errore scrittura file inventario: " + e.getMessage());
        } catch (SQLException e) {
            handleSqlException(e, "Errore in reportInventario");
        }
    }

    /**
     * Corrisponde al paragrafo ANALISI-ABC.
     */
    private void analisiAbc() {
        System.out.println("\n=== ANALISI ABC ARTICOLI ===");
        try (Statement stmt = connection.createStatement()) {
            // Usiamo una tabella temporanea
            stmt.execute("DROP TABLE IF EXISTS ANALISI_ABC");
            stmt.execute("CREATE TEMP TABLE ANALISI_ABC AS " +
                         "SELECT a.codice_articolo, a.descrizione, " +
                         "(g.quantita_disponibile * g.valore_medio) as valore_tot " +
                         "FROM ARTICOLI a JOIN GIACENZE g ON a.codice_articolo = g.codice_articolo " +
                         "WHERE a.stato = 'A' AND g.quantita_disponibile > 0");

            // Calcolo totale e aggiunta colonne
            BigDecimal valoreTotaleMagazzino;
            try (ResultSet rs = stmt.executeQuery("SELECT SUM(valore_tot) FROM ANALISI_ABC")) {
                valoreTotaleMagazzino = rs.next() ? rs.getBigDecimal(1) : BigDecimal.ZERO;
            }
            if (valoreTotaleMagazzino.compareTo(BigDecimal.ZERO) == 0) {
                System.out.println("Nessun articolo da analizzare.");
                return;
            }

            stmt.execute("ALTER TABLE ANALISI_ABC ADD COLUMN percentuale NUMERIC, ADD COLUMN perc_cumulata NUMERIC, ADD COLUMN classe CHAR(1)");
            stmt.executeUpdate("UPDATE ANALISI_ABC SET percentuale = (valore_tot / " + valoreTotaleMagazzino + ") * 100");

            // Calcolo cumulata con Window Function (più efficiente della subquery correlata)
            stmt.executeUpdate("UPDATE ANALISI_ABC SET perc_cumulata = s.perc_cumulata FROM " +
                               "(SELECT codice_articolo, SUM(percentuale) OVER (ORDER BY valore_tot DESC) as perc_cumulata FROM ANALISI_ABC) s " +
                               "WHERE ANALISI_ABC.codice_articolo = s.codice_articolo");

            stmt.executeUpdate("UPDATE ANALISI_ABC SET classe = CASE " +
                               "WHEN perc_cumulata <= 80 THEN 'A' " +
                               "WHEN perc_cumulata <= 95 THEN 'B' " +
                               "ELSE 'C' END");

            // Visualizzazione risultati
            visualizzaClasseAbc(stmt, "A", "CLASSE A - Alto valore (80% del valore)");
            visualizzaStatisticheClasse(stmt, "B", "CLASSE B - Medio valore (15% del valore)");
            visualizzaStatisticheClasse(stmt, "C", "CLASSE C - Basso valore (5% del valore)");

            stmt.execute("DROP TABLE ANALISI_ABC");

        } catch (SQLException e) {
            handleSqlException(e, "Errore in analisiAbc");
        }
    }

    private void visualizzaClasseAbc(Statement stmt, String classe, String titolo) throws SQLException {
        System.out.println("\n" + titolo);
        System.out.println("-".repeat(titolo.length()));
        String sql = "SELECT codice_articolo, descrizione, valore_tot, percentuale FROM ANALISI_ABC " +
                     "WHERE classe = '" + classe + "' ORDER BY valore_tot DESC";
        int count = 0;
        try (ResultSet rs = stmt.executeQuery(sql)) {
            while (rs.next()) {
                count++;
                System.out.printf("%-10s - %-35.35s%n", rs.getString("codice_articolo"), rs.getString("descrizione"));
                System.out.printf("  Valore: %-15s (%s%%)%n", 
                    formatoImporto.format(rs.getBigDecimal("valore_tot")),
                    formatoPercentuale.format(rs.getBigDecimal("percentuale")));
            }
        }
        System.out.println("\nArticoli classe " + classe + ": " + formatoNumero.format(count));
    }

    private void visualizzaStatisticheClasse(Statement stmt, String classe, String titolo) throws SQLException {
        System.out.println("\n" + titolo);
        System.out.println("-".repeat(titolo.length()));
        String sql = "SELECT COUNT(*), SUM(valore_tot) FROM ANALISI_ABC WHERE classe = '" + classe + "'";
        try (ResultSet rs = stmt.executeQuery(sql)) {
            if (rs.next()) {
                System.out.println("Articoli: " + formatoNumero.format(rs.getInt(1)) +
                                   "  Valore totale: " + formatoImporto.format(rs.getBigDecimal(2)));
            }
        }
    }

    // --- Metodi di utilità ---

    /**
     * Gestisce le eccezioni SQL, impostando sqlcode e stampando un messaggio.
     * @param e L'eccezione SQL.
     * @param context Il contesto in cui si è verificato l'errore.
     */
    private void handleSqlException(SQLException e, String context) {
        if ("02000".equals(e.getSQLState())) {
            sqlcode = 100; // Not found
        } else {
            sqlcode = e.getErrorCode();
            System.err.println(context + ": SQLCODE=" + sqlcode + ", " + e.getMessage());
        }
    }

    /**
     * Esegue il rollback di una transazione in modo sicuro.
     */
    private void rollbackTransazione() {
        try {
            if (connection != null && !connection.getAutoCommit()) {
                connection.rollback();
                System.out.println("Transazione annullata (ROLLBACK).");
            }
        } catch (SQLException ex) {
            System.err.println("Errore critico durante il rollback: " + ex.getMessage());
        }
    }

    /**
     * Ripristina la modalità auto-commit a true.
     */
    private void resetAutoCommit() {
        try {
            if (connection != null && !connection.getAutoCommit()) {
                connection.setAutoCommit(true);
            }
        } catch (SQLException e) {
            System.err.println("Errore nel ripristino dell'auto-commit: " + e.getMessage());
        }
    }
}