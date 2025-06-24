package com;

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
import java.util.Scanner;

/**
 * Sistema di Gestione Magazzino e Inventario
 * 
 * Questo programma Java è una traduzione del programma COBOL GESTIONE-MAGAZZINO.
 * Implementa le funzionalità di gestione articoli, movimenti di magazzino, ordini a fornitore,
 * valorizzazione e reportistica, utilizzando JDBC per l'interazione con un database PostgreSQL.
 * 
 * @author Annalisa Egidi (Original COBOL Author)
 * @author Advanced COBOL to Java Compiler
 * @version 1.0
 * @date 2025-05-20
 */
public class GestioneMagazzino {

    // --- Configurazione Database ---
    private static final String DB_URL = "jdbc:postgresql://localhost:5432/magazzino";
    private static final String DB_USER = "postgres";
    private static final String DB_PASSWORD = "password";

    private Connection connection;
    private final Scanner scanner;

    // --- Variabili della Working-Storage Section ---
    private int wsScelta = 0;
    private String wsContinua = "S";
    private String wsRisposta = "";
    private String wsEsito = "";
    private String wsMetodoValorizz = "F";

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
    private long wsLotId = 0;
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
    
    // Variabili per analisi ABC
    private BigDecimal wsAliquota = BigDecimal.ZERO;

    // Formattatori
    private final DecimalFormat formatoImporto = new DecimalFormat("#,##0.00");
    private final DecimalFormat formatoQuantita = new DecimalFormat("#,##0.00");
    private final DecimalFormat formatoNumero = new DecimalFormat("###,##0");
    private final DecimalFormat formatoPercentuale = new DecimalFormat("##0.00");

    // Stato SQL
    private int sqlcode = 0;

    /**
     * Costruttore della classe. Inizializza lo scanner per l'input.
     */
    public GestioneMagazzino() {
        this.scanner = new Scanner(System.in);
    }

    /**
     * Metodo principale che avvia l'applicazione.
     * @param args Argomenti della riga di comando (non utilizzati).
     */
    public static void main(String[] args) {
        GestioneMagazzino app = new GestioneMagazzino();
        app.mainLogic();
    }

    /**
     * Logica principale del programma, che gestisce il menu e il ciclo di vita dell'applicazione.
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
                wsScelta = -1; // Valore non valido per evitare l'uscita
            }

            if (wsScelta != 0) {
                System.out.println();
                System.out.print("Continuare? (S/N): ");
                wsContinua = scanner.nextLine();
            }

        } while (!wsContinua.equalsIgnoreCase("N"));

        disconnettiDatabase();
        scanner.close();
        System.out.println("Programma terminato.");
    }

    /**
     * Stabilisce la connessione al database PostgreSQL.
     * @return true se la connessione ha successo, false altrimenti.
     */
    private boolean connettiDatabase() {
        try {
            connection = DriverManager.getConnection(DB_URL, DB_USER, DB_PASSWORD);
            // Impostiamo l'isolamento delle transazioni per coerenza
            connection.setTransactionIsolation(Connection.TRANSACTION_READ_COMMITTED);
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
     * Gestisce il processo di carico di nuova merce a magazzino.
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

        try {
            System.out.print("Quantità da caricare: ");
            wsMovQuantita = new BigDecimal(scanner.nextLine());
            if (wsMovQuantita.compareTo(BigDecimal.ZERO) <= 0) {
                System.out.println("Quantità non valida!");
                return;
            }

            System.out.print("Prezzo unitario: ");
            wsMovPrezzo = new BigDecimal(scanner.nextLine());
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

        try {
            connection.setAutoCommit(false);
            wsEsito = "OK";

            wsMovTipo = "CA";
            if (registraMovimento()) {
                if (aggiornaGiacenzaCarico()) {
                    if (!wsLotNumero.isEmpty()) {
                        creaLotto();
                    }
                }
            }

            if ("OK".equals(wsEsito)) {
                connection.commit();
                System.out.println("Carico registrato con successo!");
                System.out.println("Valore carico: EUR " + formatoImporto.format(wsMovValore));
            } else {
                System.err.println("Errore durante il carico! Rollback in corso...");
                connection.rollback();
            }
        } catch (SQLException e) {
            handleSqlException(e, "caricoMerce");
        } finally {
            try {
                connection.setAutoCommit(true);
            } catch (SQLException e) {
                handleSqlException(e, "caricoMerce.finally");
            }
        }
    }

    /**
     * Gestisce il processo di scarico merce dal magazzino.
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

        try {
            System.out.print("Quantità da scaricare: ");
            wsMovQuantita = new BigDecimal(scanner.nextLine());
        } catch (NumberFormatException e) {
            System.out.println("Quantità non valida!");
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

        try {
            connection.setAutoCommit(false);
            wsEsito = "OK";

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
                System.err.println("Errore durante lo scarico! Rollback in corso...");
                connection.rollback();
            }
        } catch (SQLException e) {
            handleSqlException(e, "scaricoMerce");
        } finally {
            try {
                connection.setAutoCommit(true);
            } catch (SQLException e) {
                handleSqlException(e, "scaricoMerce.finally");
            }
        }
    }

    /**
     * Carica i dati di un articolo e la sua giacenza dal database.
     * @return true se l'articolo viene trovato e caricato, false altrimenti.
     */
    private boolean caricaArticolo() {
        wsEsito = "KO";
        sqlcode = 0;
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
                    } else {
                        wsEsito = "OK";
                    }
                } else {
                    sqlcode = 100;
                    System.out.println("Articolo non trovato!");
                }
            }
        } catch (SQLException e) {
            handleSqlException(e, "caricaArticolo");
        }
        return "OK".equals(wsEsito);
    }

    /**
     * Inserisce un nuovo record nella tabella dei movimenti di magazzino.
     * @return true se l'operazione ha successo, false altrimenti.
     */
    private boolean registraMovimento() {
        wsMovOperatore = System.getProperty("user.name");
        String sql = "INSERT INTO MOVIMENTI_MAGAZZINO " +
                     "(tipo_movimento, numero_documento, codice_articolo, quantita, prezzo_unitario, " +
                     "valore_totale, causale, codice_fornitore, operatore) " +
                     "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)";

        try (PreparedStatement pstmt = connection.prepareStatement(sql)) {
            pstmt.setString(1, wsMovTipo);
            pstmt.setString(2, wsMovNumeroDoc);
            pstmt.setString(3, wsArtCodice);
            pstmt.setBigDecimal(4, wsMovQuantita);
            pstmt.setBigDecimal(5, wsMovPrezzo);
            pstmt.setBigDecimal(6, wsMovValore);
            pstmt.setString(7, wsMovCausale);
            
            if (wsMovFornitore != null && !wsMovFornitore.trim().isEmpty()) {
                pstmt.setString(8, wsMovFornitore);
            } else {
                pstmt.setNull(8, Types.CHAR);
            }
            
            pstmt.setString(9, wsMovOperatore);

            int affectedRows = pstmt.executeUpdate();
            if (affectedRows == 0) {
                System.err.println("Errore registrazione movimento: nessun record inserito.");
                wsEsito = "KO";
            }
        } catch (SQLException e) {
            handleSqlException(e, "registraMovimento");
            wsEsito = "KO";
        }
        return "OK".equals(wsEsito);
    }

    /**
     * Aggiorna la giacenza di un articolo dopo un carico. Se la giacenza non esiste, la crea.
     * @return true se l'operazione ha successo, false altrimenti.
     */
    private boolean aggiornaGiacenzaCarico() {
        // Calcolo nuovo valore medio ponderato
        if (wsGiaDisponibile.add(wsMovQuantita).compareTo(BigDecimal.ZERO) != 0) {
            BigDecimal valoreEsistente = wsGiaDisponibile.multiply(wsGiaValMedio);
            BigDecimal valoreNuovo = wsMovQuantita.multiply(wsMovPrezzo);
            wsValoreTot = valoreEsistente.add(valoreNuovo);
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
            pstmt.setBigDecimal(3, wsMovPrezzo);
            pstmt.setString(4, wsArtCodice);

            int affectedRows = pstmt.executeUpdate();
            if (affectedRows == 0) { // Giacenza non esiste, creala
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
        } catch (SQLException e) {
            handleSqlException(e, "aggiornaGiacenzaCarico");
            wsEsito = "KO";
        }
        return "OK".equals(wsEsito);
    }

    /**
     * Aggiorna la giacenza di un articolo dopo uno scarico.
     * @return true se l'operazione ha successo, false altrimenti.
     */
    private boolean aggiornaGiacenzaScarico() {
        String sql = "UPDATE GIACENZE SET quantita_disponibile = quantita_disponibile - ?, " +
                     "data_ultimo_scarico = CURRENT_DATE WHERE codice_articolo = ?";
        try (PreparedStatement pstmt = connection.prepareStatement(sql)) {
            pstmt.setBigDecimal(1, wsMovQuantita);
            pstmt.setString(2, wsArtCodice);
            int affectedRows = pstmt.executeUpdate();
            if (affectedRows == 0) {
                System.err.println("Errore aggiornamento giacenza: articolo non trovato.");
                wsEsito = "KO";
            }
        } catch (SQLException e) {
            handleSqlException(e, "aggiornaGiacenzaScarico");
            wsEsito = "KO";
        }
        return "OK".equals(wsEsito);
    }

    /**
     * Crea un nuovo lotto per la tracciabilità.
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
        } catch (SQLException e) {
            if ("23505".equals(e.getSQLState())) { // Unique violation
                System.err.println("Lotto già esistente!");
            } else {
                handleSqlException(e, "creaLotto");
            }
            wsEsito = "KO";
        }
        return "OK".equals(wsEsito);
    }

    /**
     * Calcola il valore dello scarico in base al metodo scelto (FIFO, LIFO, Medio).
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
     * @param isFifo true per FIFO, false per LIFO.
     * @return true se il calcolo ha successo, false altrimenti.
     */
    private boolean calcolaValoreFifoLifo(boolean isFifo) {
        wsQtaRichiesta = wsMovQuantita;
        wsMovValore = BigDecimal.ZERO;

        String sql = "SELECT id_lotto, quantita_residua, prezzo_acquisto FROM LOTTI " +
                     "WHERE codice_articolo = ? AND quantita_residua > 0 AND stato = 'A' " +
                     "ORDER BY data_carico " + (isFifo ? "ASC, id_lotto ASC" : "DESC, id_lotto DESC");

        try (PreparedStatement pstmt = connection.prepareStatement(sql)) {
            pstmt.setString(1, wsArtCodice);
            try (ResultSet rs = pstmt.executeQuery()) {
                while (rs.next() && wsQtaRichiesta.compareTo(BigDecimal.ZERO) > 0) {
                    wsLotQtaRes = rs.getBigDecimal("quantita_residua");
                    wsLotPrezzo = rs.getBigDecimal("prezzo_acquisto");

                    BigDecimal qtaDaPrelevare = wsLotQtaRes.min(wsQtaRichiesta);
                    BigDecimal valorePrelievo = qtaDaPrelevare.multiply(wsLotPrezzo);
                    wsMovValore = wsMovValore.add(valorePrelievo);
                    wsQtaRichiesta = wsQtaRichiesta.subtract(qtaDaPrelevare);
                }
            }
        } catch (SQLException e) {
            handleSqlException(e, "calcolaValore" + (isFifo ? "FIFO" : "LIFO"));
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
     */
    private void aggiornaLottiScarico() {
        wsQtaRichiesta = wsMovQuantita;
        boolean isFifo = "F".equals(wsMetodoValorizz);

        String selectSql = "SELECT id_lotto, quantita_residua FROM LOTTI " +
                           "WHERE codice_articolo = ? AND quantita_residua > 0 AND stato = 'A' " +
                           "ORDER BY data_carico " + (isFifo ? "ASC, id_lotto ASC" : "DESC, id_lotto DESC");
        
        String updateSql = "UPDATE LOTTI SET quantita_residua = ?, stato = ? WHERE id_lotto = ?";

        try (PreparedStatement selectPstmt = connection.prepareStatement(selectSql)) {
            selectPstmt.setString(1, wsArtCodice);
            try (ResultSet rs = selectPstmt.executeQuery()) {
                while (rs.next() && wsQtaRichiesta.compareTo(BigDecimal.ZERO) > 0) {
                    long lottoId = rs.getLong("id_lotto");
                    BigDecimal qtaResiduaLotto = rs.getBigDecimal("quantita_residua");
                    
                    BigDecimal qtaDaPrelevare = qtaResiduaLotto.min(wsQtaRichiesta);
                    BigDecimal nuovaQtaResidua = qtaResiduaLotto.subtract(qtaDaPrelevare);
                    String nuovoStato = (nuovaQtaResidua.compareTo(BigDecimal.ZERO) == 0) ? "E" : "A";

                    try (PreparedStatement updatePstmt = connection.prepareStatement(updateSql)) {
                        updatePstmt.setBigDecimal(1, nuovaQtaResidua);
                        updatePstmt.setString(2, nuovoStato);
                        updatePstmt.setLong(3, lottoId);
                        updatePstmt.executeUpdate();
                    }
                    wsQtaRichiesta = wsQtaRichiesta.subtract(qtaDaPrelevare);
                }
            }
        } catch (SQLException e) {
            handleSqlException(e, "aggiornaLottiScarico");
            wsEsito = "KO";
        }
    }

    /**
     * Mostra a schermo i dettagli completi di un articolo e la sua giacenza.
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
        System.out.printf("  Disponibile:     %15s\n", formatoQuantita.format(wsGiaDisponibile));
        System.out.printf("  Impegnata:       %15s\n", formatoQuantita.format(wsGiaImpegnata));
        System.out.printf("  In ordine:       %15s\n", formatoQuantita.format(wsGiaOrdinata));
        wsQtaResidua = wsGiaDisponibile.subtract(wsGiaImpegnata);
        System.out.printf("  Netta:           %15s\n", formatoQuantita.format(wsQtaResidua));

        System.out.println("\nSCORTE:");
        System.out.printf("  Scorta minima:   %15s\n", formatoQuantita.format(wsArtScortaMin));
        System.out.printf("  Punto riordino:  %15s\n", formatoQuantita.format(wsArtPuntoRiord));
        System.out.printf("  Lotto riordino:  %15s\n", formatoQuantita.format(wsArtLottoRiord));

        System.out.println("\nVALORI:");
        System.out.printf("  Valore medio:    EUR %12s\n", formatoImporto.format(wsGiaValMedio));
        System.out.printf("  Valore ultimo:   EUR %12s\n", formatoImporto.format(wsGiaValUltimo));
        wsValoreTot = wsGiaDisponibile.multiply(wsGiaValMedio);
        System.out.printf("  Valore totale:   EUR %12s\n", formatoImporto.format(wsValoreTot));

        if (wsGiaDisponibile.compareTo(wsArtScortaMin) <= 0 && wsArtScortaMin.compareTo(BigDecimal.ZERO) > 0) {
            System.out.println("\n*** ATTENZIONE: SCORTA MINIMA RAGGIUNTA ***");
        } else if (wsGiaDisponibile.compareTo(wsArtPuntoRiord) <= 0 && wsArtPuntoRiord.compareTo(BigDecimal.ZERO) > 0) {
            System.out.println("\n*** ATTENZIONE: PUNTO RIORDINO RAGGIUNTO ***");
        }
    }

    /**
     * Stampa un elenco degli articoli che sono al di sotto del punto di riordino.
     */
    private void listaSottoscorta() {
        System.out.println("\n=== ARTICOLI SOTTOSCORTA ===\n");
        wsContaSottoscorta = 0;
        String sql = "SELECT a.codice_articolo, a.descrizione, g.quantita_disponibile, " +
                     "g.quantita_ordinata, a.punto_riordino, a.lotto_riordino, f.ragione_sociale " +
                     "FROM ARTICOLI a JOIN GIACENZE g ON a.codice_articolo = g.codice_articolo " +
                     "LEFT JOIN FORNITORI f ON a.codice_fornitore = f.codice_fornitore " +
                     "WHERE a.stato = 'A' AND g.quantita_disponibile <= a.punto_riordino AND a.punto_riordino > 0 " +
                     "ORDER BY (a.punto_riordino - g.quantita_disponibile) DESC";

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

                System.out.println(wsArtCodice + " - " + wsArtDescrizione);
                wsQtaResidua = wsArtPuntoRiord.subtract(wsGiaDisponibile);
                System.out.printf("  Disponibile: %s  Punto riordino: %s  Mancanti: %s\n",
                        formatoQuantita.format(wsGiaDisponibile),
                        formatoQuantita.format(wsArtPuntoRiord),
                        formatoQuantita.format(wsQtaResidua));

                if (wsGiaOrdinata.compareTo(BigDecimal.ZERO) > 0) {
                    System.out.println("  In ordine: " + formatoQuantita.format(wsGiaOrdinata));
                } else {
                    System.out.printf("  DA ORDINARE: %s (Fornitore: %s)\n",
                            formatoQuantita.format(wsArtLottoRiord),
                            fornitore != null ? fornitore : "N/D");
                }
                System.out.println();
            }
        } catch (SQLException e) {
            handleSqlException(e, "listaSottoscorta");
        }
        System.out.println("Totale articoli sottoscorta: " + formatoNumero.format(wsContaSottoscorta));
    }

    /**
     * Genera un report di valorizzazione del magazzino e lo salva su file.
     */
    private void valorizzazioneMagazzino() {
        System.out.println("\n=== VALORIZZAZIONE MAGAZZINO ===\n");
        String sql = "SELECT a.codice_articolo, a.descrizione, a.unita_misura, a.ubicazione, " +
                     "g.quantita_disponibile, g.valore_medio " +
                     "FROM ARTICOLI a JOIN GIACENZE g ON a.codice_articolo = g.codice_articolo " +
                     "WHERE a.stato = 'A' ORDER BY a.codice_articolo";

        try (PrintWriter writer = new PrintWriter(new FileWriter("REPORT-MAGAZZINO.TXT"));
             PreparedStatement pstmt = connection.prepareStatement(sql);
             ResultSet rs = pstmt.executeQuery()) {

            writer.println("REPORT VALORIZZAZIONE MAGAZZINO");
            writer.println("===============================");
            writer.println("Data: " + LocalDate.now() + "    Ora: " + LocalDateTime.now().format(DateTimeFormatter.ofPattern("HH:mm:ss")));
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

                    writer.printf("%-10s %-40s\n", wsArtCodice, wsArtDescrizione);
                    writer.printf("  Qtà: %10s %-5s  Val.medio: %12s  Totale: EUR %15s\n",
                            formatoQuantita.format(wsGiaDisponibile),
                            wsArtUm,
                            formatoImporto.format(wsGiaValMedio),
                            formatoImporto.format(wsValoreTot));
                }
            }

            writer.println();
            writer.println("-------------------------------");
            writer.println("Articoli valorizzati: " + formatoNumero.format(wsContaArticoli));
            writer.println("VALORE TOTALE MAGAZZINO: EUR " + formatoImporto.format(wsValoreMagazzino));

            System.out.println("Report salvato in REPORT-MAGAZZINO.TXT");
            System.out.println("Articoli valorizzati: " + formatoNumero.format(wsContaArticoli));
            System.out.println("Valore totale: EUR " + formatoImporto.format(wsValoreMagazzino));

        } catch (IOException | SQLException e) {
            System.err.println("Errore durante la valorizzazione: " + e.getMessage());
            if (e instanceof SQLException) {
                handleSqlException((SQLException) e, "valorizzazioneMagazzino");
            }
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

        System.out.println("\nUltimi movimenti di: " + wsArtDescrizione);
        System.out.println();
        System.out.printf("%-5s %-20s %-20s %15s %15s\n", "TIPO", "DATA/ORA", "DOCUMENTO", "QUANTITA", "VALORE");
        System.out.println("-".repeat(80));

        String sql = "SELECT tipo_movimento, data_movimento, numero_documento, " +
                     "quantita, valore_totale, causale " +
                     "FROM MOVIMENTI_MAGAZZINO WHERE codice_articolo = ? " +
                     "ORDER BY data_movimento DESC LIMIT 50";

        try (PreparedStatement pstmt = connection.prepareStatement(sql)) {
            pstmt.setString(1, wsArtCodice);
            try (ResultSet rs = pstmt.executeQuery()) {
                while (rs.next()) {
                    wsMovTipo = rs.getString(1);
                    Timestamp dataMov = rs.getTimestamp(2);
                    wsMovNumeroDoc = rs.getString(3);
                    wsMovQuantita = rs.getBigDecimal(4);
                    wsMovValore = rs.getBigDecimal(5);
                    wsMovCausale = rs.getString(6);

                    String segno = (wsMovTipo.equals("SC") || wsMovTipo.equals("RE")) ? "-" : "+";
                    
                    System.out.printf("%-5s %-20s %-20s %s%14s %15s\n",
                            wsMovTipo,
                            dataMov.toLocalDateTime().format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm")),
                            wsMovNumeroDoc,
                            segno,
                            formatoQuantita.format(wsMovQuantita),
                            formatoImporto.format(wsMovValore));
                    System.out.println("      " + wsMovCausale);
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

        System.out.println("Articolo: " + wsArtDescrizione);
        System.out.println("Giacenza attuale: " + formatoQuantita.format(wsGiaDisponibile) + " " + wsArtUm);

        try {
            System.out.print("Giacenza rilevata: ");
            wsQtaRichiesta = new BigDecimal(scanner.nextLine());
        } catch (NumberFormatException e) {
            System.out.println("Quantità non valida!");
            return;
        }

        BigDecimal differenza = wsQtaRichiesta.subtract(wsGiaDisponibile);
        if (differenza.compareTo(BigDecimal.ZERO) == 0) {
            System.out.println("Nessuna differenza rilevata.");
            return;
        }

        System.out.println("Differenza: " + (differenza.compareTo(BigDecimal.ZERO) > 0 ? "+" : "") + formatoQuantita.format(differenza));
        
        wsMovTipo = "RI";
        if (differenza.compareTo(BigDecimal.ZERO) > 0) {
            wsMovCausale = "Rettifica inventario positiva";
            wsMovQuantita = differenza;
        } else {
            wsMovCausale = "Rettifica inventario negativa";
            wsMovQuantita = differenza.abs();
        }
        
        System.out.print("Note rettifica: ");
        String note = scanner.nextLine().trim();
        wsMovCausale += " - " + note;

        System.out.print("Confermare rettifica (S/N): ");
        wsRisposta = scanner.nextLine();

        if (wsRisposta.equalsIgnoreCase("S")) {
            wsMovNumeroDoc = "INV" + LocalDate.now().format(DateTimeFormatter.ofPattern("yyyyMMdd"));
            wsMovPrezzo = wsGiaValMedio;
            wsMovValore = wsMovQuantita.multiply(wsMovPrezzo).setScale(2, RoundingMode.HALF_UP);

            try {
                connection.setAutoCommit(false);
                wsEsito = "OK";

                if (registraMovimento()) {
                    String sqlUpdate = "UPDATE GIACENZE SET quantita_disponibile = ?, " +
                                       "data_ultimo_inventario = CURRENT_DATE WHERE codice_articolo = ?";
                    try (PreparedStatement pstmt = connection.prepareStatement(sqlUpdate)) {
                        pstmt.setBigDecimal(1, wsQtaRichiesta);
                        pstmt.setString(2, wsArtCodice);
                        if (pstmt.executeUpdate() == 0) {
                            wsEsito = "KO";
                        }
                    }
                }

                if ("OK".equals(wsEsito)) {
                    connection.commit();
                    System.out.println("Rettifica eseguita con successo!");
                } else {
                    connection.rollback();
                    System.err.println("Errore durante la rettifica!");
                }
            } catch (SQLException e) {
                handleSqlException(e, "rettificaInventario");
            } finally {
                try {
                    connection.setAutoCommit(true);
                } catch (SQLException e) {
                    handleSqlException(e, "rettificaInventario.finally");
                }
            }
        } else {
            System.out.println("Rettifica annullata.");
        }
    }

    /**
     * Sottomenu per la gestione degli ordini a fornitore.
     */
    private void gestioneOrdini() {
        System.out.println("\n=== GESTIONE ORDINI FORNITORI ===");
        System.out.println("1. Nuovo ordine");
        System.out.println("2. Visualizza ordini aperti");
        System.out.println("3. Ricevi merce da ordine");
        System.out.println("4. Stato ordine");
        System.out.print("\nScelta: ");
        
        try {
            int scelta = Integer.parseInt(scanner.nextLine());
            switch (scelta) {
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
                if (!rs.next()) {
                    System.out.println("Fornitore non trovato o non attivo!");
                    return;
                }
                System.out.println("Fornitore: " + rs.getString(1));
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
            handleSqlException(e, "nuovoOrdine.insert");
        }
    }

    /**
     * Aggiunge righe a un ordine appena creato.
     */
    private void aggiungiRigheOrdine() {
        String continuaAggiunta = "S";
        wsOrdTotale = BigDecimal.ZERO;

        try {
            connection.setAutoCommit(false);

            while (continuaAggiunta.equalsIgnoreCase("S")) {
                System.out.print("Codice articolo: ");
                wsArtCodice = scanner.nextLine().trim();

                if (caricaArticolo()) {
                    System.out.println("Articolo: " + wsArtDescrizione);
                    System.out.print("Quantità da ordinare: ");
                    try {
                        wsMovQuantita = new BigDecimal(scanner.nextLine());
                        if (wsMovQuantita.compareTo(BigDecimal.ZERO) > 0) {
                            wsMovPrezzo = wsArtPrezzoAcq;
                            wsMovValore = wsMovQuantita.multiply(wsMovPrezzo).setScale(2, RoundingMode.HALF_UP);

                            String sqlInsertRiga = "INSERT INTO RIGHE_ORDINE (numero_ordine, codice_articolo, quantita_ordinata, prezzo_unitario, importo_riga) VALUES (?, ?, ?, ?, ?)";
                            try (PreparedStatement pstmt = connection.prepareStatement(sqlInsertRiga)) {
                                pstmt.setString(1, wsOrdNumero);
                                pstmt.setString(2, wsArtCodice);
                                pstmt.setBigDecimal(3, wsMovQuantita);
                                pstmt.setBigDecimal(4, wsMovPrezzo);
                                pstmt.setBigDecimal(5, wsMovValore);
                                pstmt.executeUpdate();
                            }

                            wsOrdTotale = wsOrdTotale.add(wsMovValore);

                            String sqlUpdateGiacenza = "UPDATE GIACENZE SET quantita_ordinata = quantita_ordinata + ? WHERE codice_articolo = ?";
                            try (PreparedStatement pstmt = connection.prepareStatement(sqlUpdateGiacenza)) {
                                pstmt.setBigDecimal(1, wsMovQuantita);
                                pstmt.setString(2, wsArtCodice);
                                pstmt.executeUpdate();
                            }
                            System.out.println("Riga ordine aggiunta!");
                        }
                    } catch (NumberFormatException e) {
                        System.out.println("Quantità non valida.");
                    }
                }
                System.out.print("Aggiungere altro articolo (S/N): ");
                continuaAggiunta = scanner.nextLine();
            }

            String sqlUpdateTotale = "UPDATE ORDINI SET totale_ordine = ? WHERE numero_ordine = ?";
            try (PreparedStatement pstmt = connection.prepareStatement(sqlUpdateTotale)) {
                pstmt.setBigDecimal(1, wsOrdTotale);
                pstmt.setString(2, wsOrdNumero);
                pstmt.executeUpdate();
            }

            connection.commit();
            System.out.println("\nOrdine completato. Totale: EUR " + formatoImporto.format(wsOrdTotale));

        } catch (SQLException e) {
            handleSqlException(e, "aggiungiRigheOrdine");
        } finally {
            try {
                connection.setAutoCommit(true);
            } catch (SQLException e) {
                handleSqlException(e, "aggiungiRigheOrdine.finally");
            }
        }
    }

    /**
     * Visualizza tutti gli ordini aperti o parzialmente evasi.
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
                System.out.println(rs.getString(1) + " del " + rs.getDate(2));
                System.out.println("  Fornitore: " + rs.getString(3));
                System.out.println("  Totale: EUR " + formatoImporto.format(rs.getBigDecimal(4)));
                String stato = rs.getString(5);
                switch (stato) {
                    case "A": System.out.println("  Stato: APERTO"); break;
                    case "C": System.out.println("  Stato: CONFERMATO"); break;
                    case "P": System.out.println("  Stato: PARZIALMENTE EVASO"); break;
                }
                System.out.println();
            }
        } catch (SQLException e) {
            handleSqlException(e, "visualizzaOrdiniAperti");
        }
        if (count == 0) {
            System.out.println("Nessun ordine aperto.");
        }
    }

    /**
     * Gestisce il processo di ricevimento merce da un ordine esistente.
     */
    private void riceviMerceOrdine() {
        System.out.println("\n=== RICEVI MERCE DA ORDINE ===");
        System.out.print("Numero ordine: ");
        wsOrdNumero = scanner.nextLine().trim();

        try {
            String sqlCheckOrdine = "SELECT stato_ordine, codice_fornitore FROM ORDINI WHERE numero_ordine = ?";
            try (PreparedStatement pstmt = connection.prepareStatement(sqlCheckOrdine)) {
                pstmt.setString(1, wsOrdNumero);
                try (ResultSet rs = pstmt.executeQuery()) {
                    if (!rs.next()) {
                        System.out.println("Ordine non trovato!");
                        return;
                    }
                    wsOrdStato = rs.getString(1);
                    wsOrdFornitore = rs.getString(2);
                    if ("E".equals(wsOrdStato)) {
                        System.out.println("Ordine già completamente evaso!");
                        return;
                    }
                }
            }

            System.out.println("\nRighe ordine da ricevere:\n");
            String sqlRighe = "SELECT r.codice_articolo, a.descrizione, r.quantita_ordinata, r.quantita_ricevuta, r.prezzo_unitario " +
                              "FROM RIGHE_ORDINE r JOIN ARTICOLI a ON r.codice_articolo = a.codice_articolo " +
                              "WHERE r.numero_ordine = ? AND r.quantita_ricevuta < r.quantita_ordinata AND r.stato_riga <> 'C'";
            
            try (PreparedStatement pstmtRighe = connection.prepareStatement(sqlRighe)) {
                pstmtRighe.setString(1, wsOrdNumero);
                try (ResultSet rsRighe = pstmtRighe.executeQuery()) {
                    while (rsRighe.next()) {
                        wsArtCodice = rsRighe.getString(1);
                        wsArtDescrizione = rsRighe.getString(2);
                        BigDecimal qtaOrdinata = rsRighe.getBigDecimal(3);
                        BigDecimal qtaRicevuta = rsRighe.getBigDecimal(4);
                        wsMovPrezzo = rsRighe.getBigDecimal(5);
                        wsQtaResidua = qtaOrdinata.subtract(qtaRicevuta);

                        System.out.println(wsArtCodice + " - " + wsArtDescrizione);
                        System.out.println("  Da ricevere: " + formatoQuantita.format(wsQtaResidua));
                        System.out.print("  Quantità ricevuta: ");
                        
                        try {
                            wsMovQuantita = new BigDecimal(scanner.nextLine());
                            if (wsMovQuantita.compareTo(BigDecimal.ZERO) > 0) {
                                if (wsMovQuantita.compareTo(wsQtaResidua) > 0) {
                                    System.out.println("  Quantità eccessiva! Verrà registrata la quantità residua.");
                                    wsMovQuantita = wsQtaResidua;
                                }
                                // Esegui carico per questa riga in una transazione
                                eseguiCaricoRigaOrdine();
                            }
                        } catch (NumberFormatException e) {
                            System.out.println("Input non valido, riga saltata.");
                        }
                        System.out.println();
                    }
                }
            }
            // Aggiorna lo stato finale dell'ordine
            aggiornaStatoOrdine();

        } catch (SQLException e) {
            handleSqlException(e, "riceviMerceOrdine");
        }
    }

    /**
     * Esegue il carico di una singola riga d'ordine in una transazione.
     */
    private void eseguiCaricoRigaOrdine() {
        wsMovFornitore = wsOrdFornitore;
        wsMovCausale = "Ricevimento ordine " + wsOrdNumero;
        wsMovNumeroDoc = wsOrdNumero;
        wsMovValore = wsMovQuantita.multiply(wsMovPrezzo).setScale(2, RoundingMode.HALF_UP);

        try {
            connection.setAutoCommit(false);
            wsEsito = "OK";
            wsMovTipo = "CA";

            if (registraMovimento()) {
                if (caricaArticolo() && aggiornaGiacenzaCarico()) {
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

                    // Aggiorna giacenza (quantità ordinata)
                    String sqlUpdateGiacenza = "UPDATE GIACENZE SET quantita_ordinata = GREATEST(0, quantita_ordinata - ?) " +
                                               "WHERE codice_articolo = ?";
                    try (PreparedStatement pstmt = connection.prepareStatement(sqlUpdateGiacenza)) {
                        pstmt.setBigDecimal(1, wsMovQuantita);
                        pstmt.setString(2, wsArtCodice);
                        pstmt.executeUpdate();
                    }
                } else {
                    wsEsito = "KO";
                }
            }

            if ("OK".equals(wsEsito)) {
                connection.commit();
                System.out.println("  Carico registrato!");
            } else {
                connection.rollback();
                System.err.println("  Errore durante il carico della riga!");
            }
        } catch (SQLException e) {
            handleSqlException(e, "eseguiCaricoRigaOrdine");
        } finally {
            try {
                connection.setAutoCommit(true);
            } catch (SQLException e) {
                handleSqlException(e, "eseguiCaricoRigaOrdine.finally");
            }
        }
    }

    /**
     * Aggiorna lo stato di un ordine (es. da Parziale a Evaso).
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
     * Mostra lo stato dettagliato di un singolo ordine.
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
            try (ResultSet rs = pstmt.executeQuery()) {
                if (!rs.next()) {
                    System.out.println("Ordine non trovato!");
                    return;
                }
                System.out.println("\nOrdine: " + wsOrdNumero);
                System.out.println("Data: " + rs.getDate(1));
                System.out.println("Fornitore: " + rs.getString(3));
                System.out.println("Totale: EUR " + formatoImporto.format(rs.getBigDecimal(4)));
                String stato = rs.getString(2);
                switch (stato) {
                    case "A": System.out.println("Stato: APERTO"); break;
                    case "C": System.out.println("Stato: CONFERMATO"); break;
                    case "P": System.out.println("Stato: PARZIALMENTE EVASO"); break;
                    case "E": System.out.println("Stato: EVASO"); break;
                }
            }
        } catch (SQLException e) {
            handleSqlException(e, "statoOrdine.header");
            return;
        }

        System.out.println("\nDettaglio righe:\n");
        String sqlDetail = "SELECT r.codice_articolo, a.descrizione, r.quantita_ordinata, r.quantita_ricevuta, r.stato_riga " +
                           "FROM RIGHE_ORDINE r JOIN ARTICOLI a ON r.codice_articolo = a.codice_articolo " +
                           "WHERE r.numero_ordine = ? ORDER BY r.codice_articolo";
        try (PreparedStatement pstmt = connection.prepareStatement(sqlDetail)) {
            pstmt.setString(1, wsOrdNumero);
            try (ResultSet rs = pstmt.executeQuery()) {
                while (rs.next()) {
                    System.out.println(rs.getString(1) + " - " + rs.getString(2));
                    String statoRiga = rs.getString(5);
                    String statoRigaDesc = "";
                    switch (statoRiga) {
                        case "A": statoRigaDesc = "(APERTA)"; break;
                        case "P": statoRigaDesc = "(PARZIALE)"; break;
                        case "E": statoRigaDesc = "(EVASA)"; break;
                        case "C": statoRigaDesc = "(CANCELLATA)"; break;
                    }
                    System.out.printf("  Ordinata: %10s  Ricevuta: %10s %s\n\n",
                            formatoQuantita.format(rs.getBigDecimal(3)),
                            formatoQuantita.format(rs.getBigDecimal(4)),
                            statoRigaDesc);
                }
            }
        } catch (SQLException e) {
            handleSqlException(e, "statoOrdine.detail");
        }
    }

    /**
     * Genera un report per l'inventario fisico, da stampare e compilare manualmente.
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
            writer.println("Data: " + LocalDate.now() + "    Ora: " + LocalDateTime.now().format(DateTimeFormatter.ofPattern("HH:mm:ss")));
            writer.println();
            writer.printf("%-12s %-30s %-10s %15s %15s %15s\n", "CODICE", "DESCRIZIONE", "UBICAZIONE", "QTA TEORICA", "QTA RILEVATA", "DIFFERENZA");
            writer.println("-".repeat(100));

            while (rs.next()) {
                writer.printf("%-12s %-30.30s %-10.10s %15s %15s %15s\n",
                        rs.getString(1),
                        rs.getString(2),
                        rs.getString(3),
                        formatoQuantita.format(rs.getBigDecimal(4)),
                        "______________",
                        "______________");
            }

            writer.println("\n" + "-".repeat(100));
            writer.println("\nRilevato da: ________________  Data: ________  Firma: ________________");

            System.out.println("Report salvato in INVENTARIO.TXT");

        } catch (IOException | SQLException e) {
            System.err.println("Errore durante la generazione del report inventario: " + e.getMessage());
            if (e instanceof SQLException) {
                handleSqlException((SQLException) e, "reportInventario");
            }
        }
    }

    /**
     * Esegue un'analisi ABC degli articoli basata sul valore della giacenza.
     */
    private void analisiAbc() {
        System.out.println("\n=== ANALISI ABC ARTICOLI ===\n");
        String tempTable = "ANALISI_ABC_" + System.currentTimeMillis(); // Nome univoco per sessioni concorrenti

        try (Statement stmt = connection.createStatement()) {
            // 1. Creazione e popolamento della tabella temporanea
            stmt.execute("CREATE TEMP TABLE " + tempTable + " AS " +
                         "SELECT a.codice_articolo, a.descrizione, " +
                         "(g.quantita_disponibile * g.valore_medio) as valore_tot " +
                         "FROM ARTICOLI a JOIN GIACENZE g ON a.codice_articolo = g.codice_articolo " +
                         "WHERE a.stato = 'A' AND g.quantita_disponibile > 0 AND g.valore_medio > 0");

            // 2. Calcolo del valore totale del magazzino
            try (ResultSet rs = stmt.executeQuery("SELECT SUM(valore_tot) FROM " + tempTable)) {
                if (rs.next()) {
                    wsValoreMagazzino = rs.getBigDecimal(1);
                }
            }
            if (wsValoreMagazzino == null || wsValoreMagazzino.compareTo(BigDecimal.ZERO) == 0) {
                System.out.println("Nessun articolo valorizzabile trovato.");
                return;
            }

            // 3. Calcolo percentuali e classificazione
            String sqlClassify = "WITH ranked_items AS ( " +
                                 "  SELECT codice_articolo, descrizione, valore_tot, " +
                                 "         (valore_tot / ? * 100) as percentuale, " +
                                 "         SUM(valore_tot) OVER (ORDER BY valore_tot DESC) as valore_cumulato " +
                                 "  FROM " + tempTable +
                                 "), " +
                                 "classified_items AS ( " +
                                 "  SELECT codice_articolo, descrizione, valore_tot, percentuale, " +
                                 "         (valore_cumulato / ? * 100) as perc_cumulata, " +
                                 "         CASE " +
                                 "           WHEN (valore_cumulato / ? * 100) <= 80 THEN 'A' " +
                                 "           WHEN (valore_cumulato / ? * 100) <= 95 THEN 'B' " +
                                 "           ELSE 'C' " +
                                 "         END as classe " +
                                 "  FROM ranked_items " +
                                 ") SELECT * FROM classified_items";

            try (PreparedStatement pstmt = connection.prepareStatement(sqlClassify)) {
                pstmt.setBigDecimal(1, wsValoreMagazzino);
                pstmt.setBigDecimal(2, wsValoreMagazzino);
                pstmt.setBigDecimal(3, wsValoreMagazzino);
                pstmt.setBigDecimal(4, wsValoreMagazzino);

                try (ResultSet rs = pstmt.executeQuery()) {
                    System.out.println("CLASSE A - Alto valore (80% del valore)");
                    System.out.println("-".repeat(40));
                    int contaA = 0;
                    while (rs.next() && "A".equals(rs.getString("classe"))) {
                        contaA++;
                        System.out.printf("%-10s - %-35.35s\n", rs.getString("codice_articolo"), rs.getString("descrizione"));
                        System.out.printf("  Valore: EUR %12s  (%s%%)\n",
                                formatoImporto.format(rs.getBigDecimal("valore_tot")),
                                formatoPercentuale.format(rs.getBigDecimal("percentuale")));
                    }
                    System.out.println("\nArticoli classe A: " + formatoNumero.format(contaA));

                    System.out.println("\nCLASSE B - Medio valore (15% del valore)");
                    System.out.println("-".repeat(40));
                    int contaB = 0;
                    BigDecimal valoreB = BigDecimal.ZERO;
                    do {
                        if ("B".equals(rs.getString("classe"))) {
                            contaB++;
                            valoreB = valoreB.add(rs.getBigDecimal("valore_tot"));
                        } else {
                            break; // Fine classe B
                        }
                    } while (rs.next());
                    System.out.println("Articoli: " + formatoNumero.format(contaB) + "  Valore totale: EUR " + formatoImporto.format(valoreB));

                    System.out.println("\nCLASSE C - Basso valore (5% del valore)");
                    System.out.println("-".repeat(40));
                    int contaC = 0;
                    BigDecimal valoreC = BigDecimal.ZERO;
                    do {
                        if ("C".equals(rs.getString("classe"))) {
                            contaC++;
                            valoreC = valoreC.add(rs.getBigDecimal("valore_tot"));
                        }
                    } while (rs.next());
                    System.out.println("Articoli: " + formatoNumero.format(contaC) + "  Valore totale: EUR " + formatoImporto.format(valoreC));
                }
            }
        } catch (SQLException e) {
            handleSqlException(e, "analisiAbc");
        }
    }

    /**
     * Gestore centralizzato per le eccezioni SQL.
     * @param e L'eccezione SQL catturata.
     * @param context Il nome del metodo in cui si è verificato l'errore.
     */
    private void handleSqlException(SQLException e, String context) {
        sqlcode = e.getErrorCode();
        System.err.printf("Errore SQL nel contesto '%s':\n", context);
        System.err.println("  SQLCODE: " + sqlcode);
        System.err.println("  SQLSTATE: " + e.getSQLState());
        System.err.println("  Messaggio: " + e.getMessage());
        
        // Tenta il rollback se siamo in una transazione
        try {
            if (connection != null && !connection.getAutoCommit()) {
                System.err.println("Tentativo di rollback della transazione...");
                connection.rollback();
                System.err.println("Rollback eseguito.");
            }
        } catch (SQLException ex) {
            System.err.println("ERRORE CRITICO: Impossibile eseguire il rollback. " + ex.getMessage());
        }
    }
}