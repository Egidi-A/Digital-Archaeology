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
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

/**
 * Traduzione Java del programma COBOL GESTIONE-MAGAZZINO.
 * Questo programma gestisce un sistema di inventario e magazzino,
 * interfacciandosi con un database PostgreSQL tramite JDBC.
 *
 * @author ANNALISA-EGIDI (Original COBOL Author)
 * @translator Advanced COBOL to Java Compiler
 * @version 2025-05-20
 */
public class GestioneMagazzino {

    // --- Configurazione Database ---
    private static final String DB_URL = "jdbc:postgresql://localhost:5432/magazzino";
    private static final String DB_USER = "postgres";
    private static final String DB_PASSWORD = "password";

    private Connection connection;
    private final Scanner scanner;

    // --- Variabili dalla WORKING-STORAGE SECTION ---
    private int wsScelta = 0;
    private String wsContinua = "S";
    private String wsRisposta = "";
    private String wsEsito = "";
    private String wsMetodoValorizz = "F"; // F=FIFO, L=LIFO, M=Medio ponderato

    // Variabili per articoli (WS-ARTICOLO)
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

    // Variabili per giacenze (WS-GIACENZA)
    private BigDecimal wsGiaDisponibile = BigDecimal.ZERO;
    private BigDecimal wsGiaImpegnata = BigDecimal.ZERO;
    private BigDecimal wsGiaOrdinata = BigDecimal.ZERO;
    private BigDecimal wsGiaValMedio = BigDecimal.ZERO;
    private BigDecimal wsGiaValUltimo = BigDecimal.ZERO;

    // Variabili per movimenti (WS-MOVIMENTO)
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

    // Variabili per ordini (WS-ORDINE)
    private String wsOrdNumero = "";
    private String wsOrdData = "";
    private String wsOrdFornitore = "";
    private String wsOrdStato = "";
    private BigDecimal wsOrdTotale = BigDecimal.ZERO;

    // Variabili per lotti (WS-LOTTO)
    private int wsLotId = 0;
    private String wsLotNumero = "";
    private String wsLotData = "";
    private BigDecimal wsLotQtaIni = BigDecimal.ZERO;
    private BigDecimal wsLotQtaRes = BigDecimal.ZERO;
    private BigDecimal wsLotPrezzo = BigDecimal.ZERO;

    // Variabili di calcolo (WS-CALCOLI)
    private BigDecimal wsQtaRichiesta = BigDecimal.ZERO;
    private BigDecimal wsQtaPrelevata = BigDecimal.ZERO;
    private BigDecimal wsQtaResidua = BigDecimal.ZERO;
    private BigDecimal wsValoreTot = BigDecimal.ZERO;
    private BigDecimal wsValoreMedio = BigDecimal.ZERO;
    private BigDecimal wsNuovoMedio = BigDecimal.ZERO;

    // Contatori e totali (WS-CONTATORI)
    private int wsContaArticoli = 0;
    private int wsContaMovimenti = 0;
    private int wsContaSottoscorta = 0;
    private BigDecimal wsValoreMagazzino = BigDecimal.ZERO;
    
    // Variabili per Analisi ABC
    private BigDecimal wsAliquota = BigDecimal.ZERO;

    // Variabili per formattazione
    private final DecimalFormat formatoImporto = new DecimalFormat("###,##0.00");
    private final DecimalFormat formatoQuantita = new DecimalFormat("###,##0.00");
    private final DecimalFormat formatoNumero = new DecimalFormat("###,##0");
    private final DecimalFormat formatoPercentuale = new DecimalFormat("##0.00");


    // --- Emulazione SQLCODE ---
    private int sqlcode = 0;

    public GestioneMagazzino() {
        this.scanner = new Scanner(System.in);
    }

    public static void main(String[] args) {
        GestioneMagazzino programma = new GestioneMagazzino();
        programma.mainLogic();
    }

    /**
     * Logica principale del programma, equivalente a MAIN-LOGIC in COBOL.
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
                System.out.println(" ");
                System.out.print("Continuare? (S/N): ");
                wsContinua = scanner.nextLine().toUpperCase();
            } else {
                wsContinua = "N";
            }

        } while (wsContinua.equalsIgnoreCase("S"));

        disconnettiDatabase();
        scanner.close();
        System.out.println("Programma terminato.");
    }

    /**
     * Stabilisce la connessione al database.
     * @return true se la connessione ha successo, altrimenti false.
     */
    private boolean connettiDatabase() {
        try {
            connection = DriverManager.getConnection(DB_URL, DB_USER, DB_PASSWORD);
            // Impostazione di default per le transazioni
            connection.setAutoCommit(true);
            System.out.println("Connessione al database stabilita");
            sqlcode = 0;
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
     * Gestisce le eccezioni SQL, impostando l'emulazione di SQLCODE.
     * @param e L'eccezione SQL catturata.
     * @param operationDesc Descrizione dell'operazione fallita.
     */
    private void handleSqlException(SQLException e, String operationDesc) {
        // "02000" è lo standard SQLSTATE per "no data found"
        if ("02000".equals(e.getSQLState())) {
            sqlcode = 100;
        } else {
            sqlcode = e.getErrorCode();
            System.err.println("Errore SQL durante '" + operationDesc + "': " + sqlcode + " - " + e.getMessage());
        }
    }
    
    /**
     * Esegue un rollback della transazione in modo sicuro.
     */
    private void rollbackDatabase() {
        if (connection != null) {
            try {
                connection.rollback();
                System.err.println("Transazione annullata (ROLLBACK).");
            } catch (SQLException ex) {
                System.err.println("Errore critico durante il rollback: " + ex.getMessage());
            }
        }
    }

    /**
     * Visualizza il menu principale delle operazioni.
     */
    private void visualizzaMenu() {
        System.out.println(" ");
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
     * Gestisce il processo di carico di nuova merce a magazzino.
     */
    private void caricoMerce() {
        System.out.println(" ");
        System.out.println("=== CARICO MERCE ===");

        System.out.print("Codice articolo: ");
        wsArtCodice = scanner.nextLine().trim();

        caricaArticolo();
        if (!"OK".equals(wsEsito)) {
            return;
        }

        System.out.println("Articolo: " + wsArtDescrizione.substring(0, Math.min(wsArtDescrizione.length(), 50)));

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
        wsEsito = "OK";

        try {
            connection.setAutoCommit(false);

            wsMovTipo = "CA";
            registraMovimento();

            if ("OK".equals(wsEsito)) {
                aggiornaGiacenzaCarico();
            }

            if ("OK".equals(wsEsito) && wsLotNumero != null && !wsLotNumero.isEmpty()) {
                creaLotto();
            }

            if ("OK".equals(wsEsito)) {
                connection.commit();
                System.out.println("Carico registrato con successo!");
                System.out.println("Valore carico: EUR " + formatoImporto.format(wsMovValore));
            } else {
                rollbackDatabase();
                System.out.println("Errore durante il carico!");
            }

        } catch (SQLException e) {
            handleSqlException(e, "Transazione Carico Merce");
            rollbackDatabase();
            System.out.println("Errore SQL durante il carico!");
        } finally {
            try {
                connection.setAutoCommit(true);
            } catch (SQLException e) {
                System.err.println("Errore nel ripristino di auto-commit: " + e.getMessage());
            }
        }
    }

    /**
     * Gestisce il processo di scarico merce dal magazzino.
     */
    private void scaricoMerce() {
        System.out.println(" ");
        System.out.println("=== SCARICO MERCE ===");

        System.out.print("Codice articolo: ");
        wsArtCodice = scanner.nextLine().trim();

        caricaArticolo();
        if (!"OK".equals(wsEsito)) {
            return;
        }

        System.out.println("Articolo: " + wsArtDescrizione.substring(0, Math.min(wsArtDescrizione.length(), 50)));
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
        wsMetodoValorizz = scanner.nextLine().toUpperCase().trim();

        wsEsito = "OK";

        try {
            connection.setAutoCommit(false);

            calcolaValoreScarico();

            if ("OK".equals(wsEsito)) {
                wsMovTipo = "SC";
                registraMovimento();
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
                System.out.println("Valore scarico: EUR " + formatoImporto.format(wsMovValore));
            } else {
                rollbackDatabase();
                System.out.println("Errore durante lo scarico!");
            }

        } catch (SQLException e) {
            handleSqlException(e, "Transazione Scarico Merce");
            rollbackDatabase();
            System.out.println("Errore SQL durante lo scarico!");
        } finally {
            try {
                connection.setAutoCommit(true);
            } catch (SQLException e) {
                System.err.println("Errore nel ripristino di auto-commit: " + e.getMessage());
            }
        }
    }

    /**
     * Carica i dati di un articolo e la sua giacenza dal database.
     */
    private void caricaArticolo() {
        wsEsito = "KO";
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
                    } else {
                        wsEsito = "OK";
                    }
                } else {
                    sqlcode = 100;
                    System.out.println("Articolo non trovato!");
                }
            }
        } catch (SQLException e) {
            handleSqlException(e, "Carica Articolo");
        }
    }

    /**
     * Inserisce un nuovo record nella tabella dei movimenti di magazzino.
     */
    private void registraMovimento() {
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
            pstmt.setBigDecimal(5, wsMovPrezzo);
            pstmt.setBigDecimal(6, wsMovValore);
            pstmt.setString(7, wsMovCausale);
            
            if (wsMovFornitore != null && !wsMovFornitore.isEmpty()) {
                pstmt.setString(8, wsMovFornitore);
            } else {
                pstmt.setNull(8, Types.CHAR);
            }
            
            pstmt.setString(9, wsMovOperatore);

            int rowsAffected = pstmt.executeUpdate();
            if (rowsAffected > 0) {
                sqlcode = 0;
            } else {
                sqlcode = -1; // Errore generico
                wsEsito = "KO";
                System.out.println("Errore: nessuna riga inserita in movimenti.");
            }
        } catch (SQLException e) {
            handleSqlException(e, "Registra Movimento");
            wsEsito = "KO";
        }
    }

    /**
     * Aggiorna la giacenza di un articolo dopo un carico.
     */
    private void aggiornaGiacenzaCarico() {
        // Calcolo nuovo valore medio ponderato
        if (wsGiaDisponibile.add(wsMovQuantita).compareTo(BigDecimal.ZERO) != 0) {
            BigDecimal valoreGiacenzaEsistente = wsGiaDisponibile.multiply(wsGiaValMedio);
            BigDecimal valoreCarico = wsMovQuantita.multiply(wsMovPrezzo);
            wsValoreTot = valoreGiacenzaEsistente.add(valoreCarico);
            wsNuovoMedio = wsValoreTot.divide(wsGiaDisponibile.add(wsMovQuantita), 4, RoundingMode.HALF_UP);
        } else {
            wsNuovoMedio = wsMovPrezzo;
        }

        String updateSql = "UPDATE GIACENZE SET quantita_disponibile = quantita_disponibile + ?, " +
                           "valore_medio = ?, valore_ultimo = ?, data_ultimo_carico = CURRENT_DATE " +
                           "WHERE codice_articolo = ?";
        
        try (PreparedStatement pstmtUpdate = connection.prepareStatement(updateSql)) {
            pstmtUpdate.setBigDecimal(1, wsMovQuantita);
            pstmtUpdate.setBigDecimal(2, wsNuovoMedio);
            pstmtUpdate.setBigDecimal(3, wsMovPrezzo);
            pstmtUpdate.setString(4, wsArtCodice);
            
            int rowsAffected = pstmtUpdate.executeUpdate();
            if (rowsAffected > 0) {
                sqlcode = 0;
            } else {
                // Giacenza non esiste, creala
                String insertSql = "INSERT INTO GIACENZE (codice_articolo, quantita_disponibile, " +
                                   "valore_medio, valore_ultimo, data_ultimo_carico) " +
                                   "VALUES (?, ?, ?, ?, CURRENT_DATE)";
                try (PreparedStatement pstmtInsert = connection.prepareStatement(insertSql)) {
                    pstmtInsert.setString(1, wsArtCodice);
                    pstmtInsert.setBigDecimal(2, wsMovQuantita);
                    pstmtInsert.setBigDecimal(3, wsMovPrezzo);
                    pstmtInsert.setBigDecimal(4, wsMovPrezzo);
                    pstmtInsert.executeUpdate();
                    sqlcode = 0;
                }
            }
        } catch (SQLException e) {
            handleSqlException(e, "Aggiorna Giacenza Carico");
            wsEsito = "KO";
        }
    }

    /**
     * Aggiorna la giacenza di un articolo dopo uno scarico.
     */
    private void aggiornaGiacenzaScarico() {
        String sql = "UPDATE GIACENZE SET quantita_disponibile = quantita_disponibile - ?, " +
                     "data_ultimo_scarico = CURRENT_DATE WHERE codice_articolo = ?";
        try (PreparedStatement pstmt = connection.prepareStatement(sql)) {
            pstmt.setBigDecimal(1, wsMovQuantita);
            pstmt.setString(2, wsArtCodice);
            int rowsAffected = pstmt.executeUpdate();
            if (rowsAffected > 0) {
                sqlcode = 0;
            } else {
                sqlcode = 100;
                wsEsito = "KO";
                System.out.println("Errore aggiornamento giacenza: articolo non trovato.");
            }
        } catch (SQLException e) {
            handleSqlException(e, "Aggiorna Giacenza Scarico");
            wsEsito = "KO";
        }
    }

    /**
     * Crea un nuovo lotto per la merce in ingresso.
     */
    private void creaLotto() {
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
        } catch (SQLException e) {
            if ("23505".equals(e.getSQLState())) { // Unique violation
                System.out.println("Lotto già esistente!");
                sqlcode = -803;
            } else {
                handleSqlException(e, "Crea Lotto");
            }
            wsEsito = "KO";
        }
    }

    /**
     * Calcola il valore dello scarico in base al metodo scelto (FIFO, LIFO, Medio).
     */
    private void calcolaValoreScarico() {
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
                wsEsito = "OK";
                break;
            default:
                System.out.println("Metodo valorizzazione non valido!");
                wsEsito = "KO";
                break;
        }
    }

    /**
     * Logica unificata per calcolare il valore di scarico per FIFO e LIFO.
     * @param isFifo true per FIFO, false per LIFO.
     */
    private void calcolaValoreFifoLifo(boolean isFifo) {
        String sql = "SELECT id_lotto, numero_lotto, quantita_residua, prezzo_acquisto, data_carico " +
                     "FROM LOTTI WHERE codice_articolo = ? AND quantita_residua > 0 AND stato = 'A' " +
                     "ORDER BY data_carico " + (isFifo ? "ASC, id_lotto ASC" : "DESC, id_lotto DESC");

        wsQtaRichiesta = wsMovQuantita;
        wsMovValore = BigDecimal.ZERO;
        wsEsito = "KO";

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
                if (wsQtaRichiesta.compareTo(BigDecimal.ZERO) <= 0) {
                    wsEsito = "OK";
                    wsMovPrezzo = wsMovValore.divide(wsMovQuantita, 4, RoundingMode.HALF_UP);
                } else {
                    System.out.println("Lotti insufficienti per " + (isFifo ? "FIFO" : "LIFO") + "!");
                }
            }
        } catch (SQLException e) {
            handleSqlException(e, "Calcola Valore " + (isFifo ? "FIFO" : "LIFO"));
        }
    }

    /**
     * Aggiorna le quantità residue dei lotti dopo uno scarico FIFO/LIFO.
     */
    private void aggiornaLottiScarico() {
        String selectSql = "SELECT id_lotto, quantita_residua FROM LOTTI " +
                           "WHERE codice_articolo = ? AND quantita_residua > 0 AND stato = 'A' " +
                           "ORDER BY data_carico " + ("F".equals(wsMetodoValorizz) ? "ASC, id_lotto ASC" : "DESC, id_lotto DESC");
        
        String updateSql = "UPDATE LOTTI SET quantita_residua = ?, stato = ? WHERE id_lotto = ?";

        wsQtaRichiesta = wsMovQuantita;
        
        // Usiamo una lista per evitare problemi con il ResultSet mentre aggiorniamo
        List<int[]> lottiDaAggiornare = new ArrayList<>();
        List<BigDecimal> quantitaDaAggiornare = new ArrayList<>();

        try (PreparedStatement pstmtSelect = connection.prepareStatement(selectSql)) {
            pstmtSelect.setString(1, wsArtCodice);
            ResultSet rs = pstmtSelect.executeQuery();

            while (rs.next() && wsQtaRichiesta.compareTo(BigDecimal.ZERO) > 0) {
                int lottoId = rs.getInt("id_lotto");
                BigDecimal qtaResiduaLotto = rs.getBigDecimal("quantita_residua");

                if (qtaResiduaLotto.compareTo(wsQtaRichiesta) >= 0) {
                    BigDecimal nuovaQtaResidua = qtaResiduaLotto.subtract(wsQtaRichiesta);
                    lottiDaAggiornare.add(new int[]{lottoId, (nuovaQtaResidua.compareTo(BigDecimal.ZERO) > 0) ? 'A' : 'E'});
                    quantitaDaAggiornare.add(nuovaQtaResidua);
                    wsQtaRichiesta = BigDecimal.ZERO;
                } else {
                    lottiDaAggiornare.add(new int[]{lottoId, 'E'}); // Esaurito
                    quantitaDaAggiornare.add(BigDecimal.ZERO);
                    wsQtaRichiesta = wsQtaRichiesta.subtract(qtaResiduaLotto);
                }
            }
            rs.close();

            // Esegui gli aggiornamenti
            try (PreparedStatement pstmtUpdate = connection.prepareStatement(updateSql)) {
                for (int i = 0; i < lottiDaAggiornare.size(); i++) {
                    pstmtUpdate.setBigDecimal(1, quantitaDaAggiornare.get(i));
                    pstmtUpdate.setString(2, String.valueOf((char)lottiDaAggiornare.get(i)[1]));
                    pstmtUpdate.setInt(3, lottiDaAggiornare.get(i)[0]);
                    pstmtUpdate.addBatch();
                }
                pstmtUpdate.executeBatch();
                sqlcode = 0;
            }

        } catch (SQLException e) {
            handleSqlException(e, "Aggiorna Lotti Scarico");
            wsEsito = "KO";
        }
    }

    /**
     * Mostra a schermo i dettagli di giacenza di un articolo.
     */
    private void visualizzaGiacenza() {
        System.out.println(" ");
        System.out.println("=== VISUALIZZA GIACENZA ===");
        System.out.print("Codice articolo: ");
        wsArtCodice = scanner.nextLine().trim();

        caricaArticolo();
        if (!"OK".equals(wsEsito)) {
            return;
        }

        System.out.println(" ");
        System.out.println("ARTICOLO: " + wsArtCodice);
        System.out.println("Descrizione: " + wsArtDescrizione.substring(0, Math.min(wsArtDescrizione.length(), 60)));
        System.out.println("Categoria: " + wsArtCategoria + "  UM: " + wsArtUm);
        System.out.println("Ubicazione: " + wsArtUbicazione);
        System.out.println(" ");
        System.out.println("QUANTITA':");
        System.out.println("  Disponibile:     " + formatoQuantita.format(wsGiaDisponibile));
        System.out.println("  Impegnata:       " + formatoQuantita.format(wsGiaImpegnata));
        System.out.println("  In ordine:       " + formatoQuantita.format(wsGiaOrdinata));
        wsQtaResidua = wsGiaDisponibile.subtract(wsGiaImpegnata);
        System.out.println("  Netta:           " + formatoQuantita.format(wsQtaResidua));
        System.out.println(" ");
        System.out.println("SCORTE:");
        System.out.println("  Scorta minima:   " + formatoQuantita.format(wsArtScortaMin));
        System.out.println("  Punto riordino:  " + formatoQuantita.format(wsArtPuntoRiord));
        System.out.println("  Lotto riordino:  " + formatoQuantita.format(wsArtLottoRiord));
        System.out.println(" ");
        System.out.println("VALORI:");
        System.out.println("  Valore medio:    EUR " + formatoImporto.format(wsGiaValMedio));
        System.out.println("  Valore ultimo:   EUR " + formatoImporto.format(wsGiaValUltimo));
        wsValoreTot = wsGiaDisponibile.multiply(wsGiaValMedio);
        System.out.println("  Valore totale:   EUR " + formatoImporto.format(wsValoreTot));

        if (wsGiaDisponibile.compareTo(wsArtScortaMin) <= 0 && wsArtScortaMin.compareTo(BigDecimal.ZERO) > 0) {
            System.out.println(" ");
            System.out.println("*** ATTENZIONE: SCORTA MINIMA RAGGIUNTA ***");
        } else if (wsGiaDisponibile.compareTo(wsArtPuntoRiord) <= 0 && wsArtPuntoRiord.compareTo(BigDecimal.ZERO) > 0) {
            System.out.println(" ");
            System.out.println("*** ATTENZIONE: PUNTO RIORDINO RAGGIUNTO ***");
        }
    }

    /**
     * Stampa una lista degli articoli che sono sotto il punto di riordino.
     */
    private void listaSottoscorta() {
        System.out.println(" ");
        System.out.println("=== ARTICOLI SOTTOSCORTA ===");
        System.out.println(" ");

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
                sqlcode = 0;
                wsContaSottoscorta++;
                wsArtCodice = rs.getString(1);
                wsArtDescrizione = rs.getString(2);
                wsGiaDisponibile = rs.getBigDecimal(3);
                wsGiaOrdinata = rs.getBigDecimal(4);
                wsArtPuntoRiord = rs.getBigDecimal(5);
                wsArtLottoRiord = rs.getBigDecimal(6);
                String fornitoreRagSoc = rs.getString(7);

                System.out.println(wsArtCodice + " - " + wsArtDescrizione.substring(0, Math.min(wsArtDescrizione.length(), 40)));
                System.out.println("  Disponibile: " + formatoQuantita.format(wsGiaDisponibile));
                
                wsQtaResidua = wsArtPuntoRiord.subtract(wsGiaDisponibile);
                System.out.println("  Punto riordino: " + formatoQuantita.format(wsArtPuntoRiord) + 
                                   "  Mancanti: " + formatoQuantita.format(wsQtaResidua));

                if (wsGiaOrdinata.compareTo(BigDecimal.ZERO) > 0) {
                    System.out.println("  In ordine: " + formatoQuantita.format(wsGiaOrdinata));
                } else {
                    System.out.println("  DA ORDINARE: " + formatoQuantita.format(wsArtLottoRiord) +
                                       " (Fornitore: " + (fornitoreRagSoc != null ? fornitoreRagSoc : "N/D") + ")");
                }
                System.out.println(" ");
            }
            if (wsContaSottoscorta == 0) {
                sqlcode = 100;
            }
        } catch (SQLException e) {
            handleSqlException(e, "Lista Sottoscorta");
        }
        System.out.println("Totale articoli sottoscorta: " + formatoNumero.format(wsContaSottoscorta));
    }

    /**
     * Genera un report di valorizzazione del magazzino e lo salva su file.
     */
    private void valorizzazioneMagazzino() {
        System.out.println(" ");
        System.out.println("=== VALORIZZAZIONE MAGAZZINO ===");
        System.out.println(" ");

        String sql = "SELECT a.codice_articolo, a.descrizione, a.unita_misura, a.ubicazione, " +
                     "g.quantita_disponibile, g.valore_medio " +
                     "FROM ARTICOLI a JOIN GIACENZE g ON a.codice_articolo = g.codice_articolo " +
                     "WHERE a.stato = 'A' ORDER BY a.codice_articolo";

        try (PrintWriter writer = new PrintWriter(new FileWriter("REPORT-MAGAZZINO.TXT"));
             PreparedStatement pstmt = connection.prepareStatement(sql);
             ResultSet rs = pstmt.executeQuery()) {

            writer.println("REPORT VALORIZZAZIONE MAGAZZINO");
            writer.println("====================================================================================================================================");
            writer.println("Data: " + LocalDate.now() + "    Ora: " + LocalDateTime.now().format(DateTimeFormatter.ofPattern("HH:mm:ss")));
            writer.println();

            wsContaArticoli = 0;
            wsValoreMagazzino = BigDecimal.ZERO;

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

                    writer.println(String.format("%-10s %-40s", wsArtCodice, wsArtDescrizione.substring(0, Math.min(wsArtDescrizione.length(), 40))));
                    writer.println(String.format("  Qtà: %12s %-5s  Val.medio: %12s  Totale: EUR %15s",
                        formatoQuantita.format(wsGiaDisponibile),
                        wsArtUm,
                        formatoImporto.format(wsGiaValMedio),
                        formatoImporto.format(wsValoreTot)));
                }
            }
            if (wsContaArticoli == 0) sqlcode = 100;

            writer.println();
            writer.println("------------------------------------------------------------------------------------------------------------------------------------");
            writer.println("Articoli valorizzati: " + formatoNumero.format(wsContaArticoli));
            writer.println("VALORE TOTALE MAGAZZINO: EUR " + formatoImporto.format(wsValoreMagazzino));

            System.out.println("Report salvato in REPORT-MAGAZZINO.TXT");
            System.out.println(" ");
            System.out.println("Articoli valorizzati: " + formatoNumero.format(wsContaArticoli));
            System.out.println("Valore totale: EUR " + formatoImporto.format(wsValoreMagazzino));

        } catch (IOException e) {
            System.err.println("Errore durante la scrittura del file di report: " + e.getMessage());
        } catch (SQLException e) {
            handleSqlException(e, "Valorizzazione Magazzino");
        }
    }

    /**
     * Mostra gli ultimi 50 movimenti per un dato articolo.
     */
    private void movimentiArticolo() {
        System.out.println(" ");
        System.out.println("=== MOVIMENTI ARTICOLO ===");
        System.out.print("Codice articolo: ");
        wsArtCodice = scanner.nextLine().trim();

        caricaArticolo();
        if (!"OK".equals(wsEsito)) {
            return;
        }

        System.out.println(" ");
        System.out.println("Ultimi movimenti di: " + wsArtDescrizione.substring(0, Math.min(wsArtDescrizione.length(), 50)));
        System.out.println(" ");
        System.out.println("TIPO  DATA/ORA              DOCUMENTO         QUANTITA        VALORE");
        System.out.println("--------------------------------------------------------------------------------");

        String sql = "SELECT tipo_movimento, data_movimento, numero_documento, " +
                     "quantita, valore_totale, causale " +
                     "FROM MOVIMENTI_MAGAZZINO WHERE codice_articolo = ? " +
                     "ORDER BY data_movimento DESC LIMIT 50";

        try (PreparedStatement pstmt = connection.prepareStatement(sql)) {
            pstmt.setString(1, wsArtCodice);
            try (ResultSet rs = pstmt.executeQuery()) {
                boolean found = false;
                while (rs.next()) {
                    found = true;
                    sqlcode = 0;
                    wsMovTipo = rs.getString("tipo_movimento");
                    Timestamp movTimestamp = rs.getTimestamp("data_movimento");
                    wsMovNumeroDoc = rs.getString("numero_documento");
                    wsMovQuantita = rs.getBigDecimal("quantita");
                    wsMovValore = rs.getBigDecimal("valore_totale");
                    wsMovCausale = rs.getString("causale");

                    String segno = (wsMovTipo.equals("SC") || wsMovTipo.equals("RE")) ? "-" : "+";
                    String dataFormatted = movTimestamp.toLocalDateTime().format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"));

                    System.out.printf("%-4s  %-19s   %-15s %s%12s  EUR %12s%n",
                        wsMovTipo,
                        dataFormatted,
                        wsMovNumeroDoc != null ? wsMovNumeroDoc.substring(0, Math.min(wsMovNumeroDoc.length(), 15)) : "",
                        segno,
                        formatoQuantita.format(wsMovQuantita),
                        formatoImporto.format(wsMovValore));
                    System.out.println("      " + (wsMovCausale != null ? wsMovCausale.substring(0, Math.min(wsMovCausale.length(), 50)) : ""));
                    System.out.println(" ");
                }
                if (!found) {
                    sqlcode = 100;
                    System.out.println("Nessun movimento trovato per questo articolo.");
                }
            }
        } catch (SQLException e) {
            handleSqlException(e, "Movimenti Articolo");
        }
    }

    /**
     * Permette di rettificare la giacenza di un articolo a seguito di un conteggio fisico.
     */
    private void rettificaInventario() {
        System.out.println(" ");
        System.out.println("=== RETTIFICA INVENTARIO ===");
        System.out.print("Codice articolo: ");
        wsArtCodice = scanner.nextLine().trim();

        caricaArticolo();
        if (!"OK".equals(wsEsito)) {
            return;
        }

        System.out.println("Articolo: " + wsArtDescrizione.substring(0, Math.min(wsArtDescrizione.length(), 50)));
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
            System.out.println("Nessuna differenza rilevata");
            return;
        }

        wsMovQuantita = differenza.abs();
        wsMovTipo = "RI";
        if (differenza.compareTo(BigDecimal.ZERO) > 0) {
            System.out.println("Differenza: +" + formatoQuantita.format(differenza));
            wsMovCausale = "Rettifica inventario positiva";
        } else {
            System.out.println("Differenza: " + formatoQuantita.format(differenza));
            wsMovCausale = "Rettifica inventario negativa";
        }

        System.out.print("Note rettifica: ");
        String note = scanner.nextLine();
        wsMovCausale += " - " + note;

        System.out.print("Confermare rettifica (S/N): ");
        wsRisposta = scanner.nextLine();

        if (wsRisposta.equalsIgnoreCase("S")) {
            wsMovNumeroDoc = "INV" + LocalDate.now().format(DateTimeFormatter.ofPattern("yyyyMMdd"));
            wsMovPrezzo = wsGiaValMedio;
            wsMovValore = wsMovQuantita.multiply(wsMovPrezzo).setScale(2, RoundingMode.HALF_UP);
            wsEsito = "OK";

            try {
                connection.setAutoCommit(false);
                registraMovimento();

                if ("OK".equals(wsEsito)) {
                    String sqlUpdate = "UPDATE GIACENZE SET quantita_disponibile = ?, " +
                                       "data_ultimo_inventario = CURRENT_DATE WHERE codice_articolo = ?";
                    try (PreparedStatement pstmt = connection.prepareStatement(sqlUpdate)) {
                        pstmt.setBigDecimal(1, wsQtaRichiesta);
                        pstmt.setString(2, wsArtCodice);
                        int rows = pstmt.executeUpdate();
                        if (rows > 0) {
                            sqlcode = 0;
                            connection.commit();
                            System.out.println("Rettifica eseguita con successo!");
                        } else {
                            sqlcode = 100;
                            rollbackDatabase();
                            System.out.println("Errore durante la rettifica!");
                        }
                    }
                } else {
                    rollbackDatabase();
                }
            } catch (SQLException e) {
                handleSqlException(e, "Transazione Rettifica Inventario");
                rollbackDatabase();
            } finally {
                try {
                    connection.setAutoCommit(true);
                } catch (SQLException e) {
                    System.err.println("Errore nel ripristino di auto-commit: " + e.getMessage());
                }
            }
        } else {
            System.out.println("Rettifica annullata");
        }
    }

    /**
     * Menu per la gestione degli ordini a fornitore.
     */
    private void gestioneOrdini() {
        System.out.println(" ");
        System.out.println("=== GESTIONE ORDINI FORNITORI ===");
        System.out.println(" ");
        System.out.println("1. Nuovo ordine");
        System.out.println("2. Visualizza ordini aperti");
        System.out.println("3. Ricevi merce da ordine");
        System.out.println("4. Stato ordine");
        System.out.println(" ");
        System.out.print("Scelta: ");
        
        try {
            int scelta = Integer.parseInt(scanner.nextLine());
            switch (scelta) {
                case 1: nuovoOrdine(); break;
                case 2: visualizzaOrdiniAperti(); break;
                case 3: riceviMerceOrdine(); break;
                case 4: statoOrdine(); break;
                default: System.out.println("Scelta non valida!"); break;
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
            ResultSet rs = pstmt.executeQuery();
            if (rs.next()) {
                System.out.println("Fornitore: " + rs.getString(1));
            } else {
                System.out.println("Fornitore non trovato o non attivo!");
                return;
            }
        } catch (SQLException e) {
            handleSqlException(e, "Verifica Fornitore");
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
            System.out.println(" ");
            aggiungiRigheOrdine();
        } catch (SQLException e) {
            handleSqlException(e, "Creazione Ordine");
        }
    }

    /**
     * Aggiunge righe di dettaglio a un ordine appena creato.
     */
    private void aggiungiRigheOrdine() {
        String continuaAggiunta = "S";
        wsOrdTotale = BigDecimal.ZERO;

        do {
            System.out.print("Codice articolo: ");
            wsArtCodice = scanner.nextLine().trim();
            caricaArticolo();

            if ("OK".equals(wsEsito)) {
                System.out.println("Articolo: " + wsArtDescrizione.substring(0, Math.min(wsArtDescrizione.length(), 40)));
                try {
                    System.out.print("Quantità da ordinare: ");
                    wsMovQuantita = new BigDecimal(scanner.nextLine());
                    if (wsMovQuantita.compareTo(BigDecimal.ZERO) > 0) {
                        wsMovPrezzo = wsArtPrezzoAcq;
                        wsMovValore = wsMovQuantita.multiply(wsMovPrezzo).setScale(2, RoundingMode.HALF_UP);

                        try {
                            connection.setAutoCommit(false);
                            String sqlInsertRiga = "INSERT INTO RIGHE_ORDINE (numero_ordine, codice_articolo, quantita_ordinata, prezzo_unitario, importo_riga) " +
                                                   "VALUES (?, ?, ?, ?, ?)";
                            String sqlUpdateGiacenza = "UPDATE GIACENZE SET quantita_ordinata = quantita_ordinata + ? WHERE codice_articolo = ?";

                            try (PreparedStatement pstmtInsert = connection.prepareStatement(sqlInsertRiga);
                                 PreparedStatement pstmtUpdate = connection.prepareStatement(sqlUpdateGiacenza)) {
                                
                                pstmtInsert.setString(1, wsOrdNumero);
                                pstmtInsert.setString(2, wsArtCodice);
                                pstmtInsert.setBigDecimal(3, wsMovQuantita);
                                pstmtInsert.setBigDecimal(4, wsMovPrezzo);
                                pstmtInsert.setBigDecimal(5, wsMovValore);
                                pstmtInsert.executeUpdate();

                                pstmtUpdate.setBigDecimal(1, wsMovQuantita);
                                pstmtUpdate.setString(2, wsArtCodice);
                                pstmtUpdate.executeUpdate();

                                connection.commit();
                                wsOrdTotale = wsOrdTotale.add(wsMovValore);
                                System.out.println("Riga ordine aggiunta!");
                            }
                        } catch (SQLException e) {
                            rollbackDatabase();
                            handleSqlException(e, "Aggiunta Riga Ordine");
                        } finally {
                            connection.setAutoCommit(true);
                        }
                    }
                } catch (Exception e) {
                    System.out.println("Input non valido.");
                }
            }
            System.out.print("Aggiungere altro articolo (S/N): ");
            continuaAggiunta = scanner.nextLine();
        } while (continuaAggiunta.equalsIgnoreCase("S"));

        String sqlUpdateTotale = "UPDATE ORDINI SET totale_ordine = ? WHERE numero_ordine = ?";
        try (PreparedStatement pstmt = connection.prepareStatement(sqlUpdateTotale)) {
            pstmt.setBigDecimal(1, wsOrdTotale);
            pstmt.setString(2, wsOrdNumero);
            pstmt.executeUpdate();
            System.out.println("\nOrdine completato. Totale: EUR " + formatoImporto.format(wsOrdTotale));
        } catch (SQLException e) {
            handleSqlException(e, "Aggiornamento Totale Ordine");
        }
    }

    /**
     * Mostra una lista di tutti gli ordini aperti o parzialmente evasi.
     */
    private void visualizzaOrdiniAperti() {
        System.out.println("\n=== ORDINI APERTI ===");
        String sql = "SELECT o.numero_ordine, o.data_ordine, f.ragione_sociale, o.totale_ordine, o.stato_ordine " +
                     "FROM ORDINI o JOIN FORNITORI f ON o.codice_fornitore = f.codice_fornitore " +
                     "WHERE o.stato_ordine IN ('A', 'C', 'P') ORDER BY o.data_ordine DESC";
        
        int count = 0;
        try (Statement stmt = connection.createStatement(); ResultSet rs = stmt.executeQuery(sql)) {
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
                System.out.println("  Totale: EUR " + formatoImporto.format(rs.getBigDecimal("totale_ordine")));
                System.out.println("  Stato: " + statoDesc);
            }
            if (count == 0) {
                System.out.println("Nessun ordine aperto.");
            }
        } catch (SQLException e) {
            handleSqlException(e, "Visualizza Ordini Aperti");
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
            ResultSet rs = pstmt.executeQuery();
            if (rs.next()) {
                wsOrdStato = rs.getString(1);
                wsOrdFornitore = rs.getString(2);
                if ("E".equals(wsOrdStato)) {
                    System.out.println("Ordine già completamente evaso!");
                    return;
                }
            } else {
                System.out.println("Ordine non trovato!");
                return;
            }
        } catch (SQLException e) {
            handleSqlException(e, "Verifica Ordine per Ricevimento");
            return;
        }

        System.out.println("\nRighe ordine da ricevere:");
        String sqlRighe = "SELECT r.codice_articolo, a.descrizione, r.quantita_ordinata, " +
                          "r.quantita_ricevuta, r.prezzo_unitario " +
                          "FROM RIGHE_ORDINE r JOIN ARTICOLI a ON r.codice_articolo = a.codice_articolo " +
                          "WHERE r.numero_ordine = ? AND r.quantita_ricevuta < r.quantita_ordinata AND r.stato_riga <> 'C'";
        
        try (PreparedStatement pstmt = connection.prepareStatement(sqlRighe)) {
            pstmt.setString(1, wsOrdNumero);
            ResultSet rs = pstmt.executeQuery();
            while (rs.next()) {
                wsArtCodice = rs.getString("codice_articolo");
                wsArtDescrizione = rs.getString("descrizione");
                wsQtaRichiesta = rs.getBigDecimal("quantita_ordinata");
                wsQtaPrelevata = rs.getBigDecimal("quantita_ricevuta");
                wsMovPrezzo = rs.getBigDecimal("prezzo_unitario");
                wsQtaResidua = wsQtaRichiesta.subtract(wsQtaPrelevata);

                System.out.println("\n" + wsArtCodice + " - " + wsArtDescrizione);
                System.out.println("  Da ricevere: " + formatoQuantita.format(wsQtaResidua));
                System.out.print("  Quantità ricevuta: ");
                
                try {
                    wsMovQuantita = new BigDecimal(scanner.nextLine());
                    if (wsMovQuantita.compareTo(BigDecimal.ZERO) > 0) {
                        if (wsMovQuantita.compareTo(wsQtaResidua) > 0) {
                            System.out.println("  Quantità eccessiva! Verrà registrata la quantità residua.");
                            wsMovQuantita = wsQtaResidua;
                        }
                        
                        // Esegui carico come transazione
                        eseguiCaricoDaOrdine();
                    }
                } catch (NumberFormatException e) {
                    System.out.println("Input non valido. Riga saltata.");
                }
            }
        } catch (SQLException e) {
            handleSqlException(e, "Lettura Righe Ordine per Ricevimento");
        }
        
        aggiornaStatoOrdine();
    }

    /**
     * Esegue il carico di una singola riga d'ordine come transazione.
     */
    private void eseguiCaricoDaOrdine() {
        wsMovFornitore = wsOrdFornitore;
        wsMovCausale = "Ricevimento ordine " + wsOrdNumero;
        wsMovNumeroDoc = wsOrdNumero;
        wsMovValore = wsMovQuantita.multiply(wsMovPrezzo).setScale(2, RoundingMode.HALF_UP);
        wsEsito = "OK";

        try {
            connection.setAutoCommit(false);
            
            wsMovTipo = "CA";
            registraMovimento();
            
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

                // Aggiorna quantità ordinata in giacenze
                String sqlUpdateGiacenza = "UPDATE GIACENZE SET quantita_ordinata = GREATEST(0, quantita_ordinata - ?) " +
                                           "WHERE codice_articolo = ?";
                try (PreparedStatement pstmt = connection.prepareStatement(sqlUpdateGiacenza)) {
                    pstmt.setBigDecimal(1, wsMovQuantita);
                    pstmt.setString(2, wsArtCodice);
                    pstmt.executeUpdate();
                }
                
                connection.commit();
                System.out.println("  Carico registrato!");
            } else {
                rollbackDatabase();
            }
        } catch (SQLException e) {
            handleSqlException(e, "Transazione Carico da Ordine");
            rollbackDatabase();
        } finally {
            try {
                connection.setAutoCommit(true);
            } catch (SQLException ex) {
                System.err.println("Errore nel ripristino di auto-commit: " + ex.getMessage());
            }
        }
    }

    /**
     * Aggiorna lo stato generale dell'ordine (Parziale, Evaso) dopo un ricevimento.
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
            handleSqlException(e, "Aggiornamento Stato Ordine");
        }
    }

    /**
     * Mostra lo stato dettagliato di un singolo ordine.
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
            ResultSet rs = pstmt.executeQuery();
            if (rs.next()) {
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
                System.out.println("Totale: EUR " + formatoImporto.format(rs.getBigDecimal("totale_ordine")));
                System.out.println("Stato: " + statoDesc);
            } else {
                System.out.println("Ordine non trovato!");
                return;
            }
        } catch (SQLException e) {
            handleSqlException(e, "Lettura Dati Ordine");
            return;
        }

        System.out.println("\nDettaglio righe:");
        String sqlRighe = "SELECT r.codice_articolo, a.descrizione, r.quantita_ordinata, " +
                          "r.quantita_ricevuta, r.importo_riga, r.stato_riga " +
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
            handleSqlException(e, "Lettura Righe Ordine");
        }
    }

    /**
     * Genera un report per il conteggio fisico dell'inventario.
     */
    private void reportInventario() {
        System.out.println("\n=== REPORT INVENTARIO FISICO ===");
        String sql = "SELECT a.codice_articolo, a.descrizione, a.ubicazione, g.quantita_disponibile " +
                     "FROM ARTICOLI a JOIN GIACENZE g ON a.codice_articolo = g.codice_articolo " +
                     "WHERE a.stato = 'A' ORDER BY a.ubicazione, a.codice_articolo";

        try (PrintWriter writer = new PrintWriter(new FileWriter("INVENTARIO.TXT"));
             Statement stmt = connection.createStatement();
             ResultSet rs = stmt.executeQuery(sql)) {

            writer.println("LISTA INVENTARIO FISICO");
            writer.println("====================================================================================================");
            writer.println("Data: " + LocalDate.now() + "    Ora: " + LocalDateTime.now().format(DateTimeFormatter.ofPattern("HH:mm:ss")));
            writer.println();
            writer.println(String.format("%-11s %-30s %-6s %-15s %-15s %-15s",
                "CODICE", "DESCRIZIONE", "UB.", "QTA TEORICA", "QTA RILEVATA", "DIFFERENZA"));
            writer.println("----------------------------------------------------------------------------------------------------");

            while (rs.next()) {
                writer.println(String.format("%-11s %-30s %-6s %15s %-15s %-15s",
                    rs.getString("codice_articolo"),
                    rs.getString("descrizione").substring(0, Math.min(rs.getString("descrizione").length(), 30)),
                    rs.getString("ubicazione") != null ? rs.getString("ubicazione").substring(0, Math.min(rs.getString("ubicazione").length(), 5)) : "",
                    formatoQuantita.format(rs.getBigDecimal("quantita_disponibile")),
                    "______________",
                    "______________"));
            }

            writer.println();
            writer.println("----------------------------------------------------------------------------------------------------");
            writer.println("Rilevato da: ________________  Data: ________  Firma: ________________");
            System.out.println("Report salvato in INVENTARIO.TXT");

        } catch (IOException e) {
            System.err.println("Errore durante la scrittura del file di inventario: " + e.getMessage());
        } catch (SQLException e) {
            handleSqlException(e, "Report Inventario");
        }
    }

    /**
     * Esegue un'analisi ABC degli articoli basata sul valore di giacenza.
     */
    private void analisiAbc() {
        System.out.println("\n=== ANALISI ABC ARTICOLI ===");
        Statement stmt = null;
        try {
            stmt = connection.createStatement();
            // Usiamo una tabella temporanea per l'analisi
            stmt.execute("DROP TABLE IF EXISTS ANALISI_ABC");
            stmt.execute("CREATE TEMP TABLE ANALISI_ABC AS " +
                         "SELECT a.codice_articolo, a.descrizione, " +
                         "(g.quantita_disponibile * g.valore_medio) as valore_tot " +
                         "FROM ARTICOLI a JOIN GIACENZE g ON a.codice_articolo = g.codice_articolo " +
                         "WHERE a.stato = 'A' AND g.quantita_disponibile > 0 AND g.valore_medio > 0");

            // Calcolo totale valore magazzino
            ResultSet rs = stmt.executeQuery("SELECT SUM(valore_tot) FROM ANALISI_ABC");
            if (rs.next()) {
                wsValoreMagazzino = rs.getBigDecimal(1);
            }
            if (wsValoreMagazzino == null || wsValoreMagazzino.compareTo(BigDecimal.ZERO) == 0) {
                System.out.println("Nessun articolo da valorizzare per l'analisi.");
                return;
            }

            // Calcolo percentuali e classi
            String sqlAnalisi = "WITH Valori AS ( " +
                                "  SELECT codice_articolo, descrizione, valore_tot, " +
                                "         (valore_tot / ?) * 100 AS percentuale " +
                                "  FROM ANALISI_ABC " +
                                "), " +
                                "Cumulato AS ( " +
                                "  SELECT codice_articolo, descrizione, valore_tot, percentuale, " +
                                "         SUM(percentuale) OVER (ORDER BY valore_tot DESC) as perc_cumulata " +
                                "  FROM Valori " +
                                ") " +
                                "SELECT codice_articolo, descrizione, valore_tot, percentuale, perc_cumulata, " +
                                "       CASE " +
                                "           WHEN perc_cumulata <= 80 THEN 'A' " +
                                "           WHEN perc_cumulata <= 95 THEN 'B' " +
                                "           ELSE 'C' " +
                                "       END as classe " +
                                "FROM Cumulato ORDER BY valore_tot DESC";

            PreparedStatement pstmtAnalisi = connection.prepareStatement(sqlAnalisi);
            pstmtAnalisi.setBigDecimal(1, wsValoreMagazzino);
            rs = pstmtAnalisi.executeQuery();

            System.out.println("\nCLASSE A - Alto valore (80% del valore)");
            System.out.println("-----------------------------------------");
            int contaA = 0, contaB = 0, contaC = 0;
            BigDecimal valoreB = BigDecimal.ZERO, valoreC = BigDecimal.ZERO;

            while (rs.next()) {
                String classe = rs.getString("classe");
                if ("A".equals(classe)) {
                    contaA++;
                    System.out.println(rs.getString("codice_articolo") + " - " + rs.getString("descrizione").substring(0, Math.min(rs.getString("descrizione").length(), 35)));
                    System.out.println("  Valore: EUR " + formatoImporto.format(rs.getBigDecimal("valore_tot")) +
                                       "  (" + formatoPercentuale.format(rs.getBigDecimal("percentuale")) + "%)");
                } else if ("B".equals(classe)) {
                    contaB++;
                    valoreB = valoreB.add(rs.getBigDecimal("valore_tot"));
                } else {
                    contaC++;
                    valoreC = valoreC.add(rs.getBigDecimal("valore_tot"));
                }
            }
            
            System.out.println("\nArticoli classe A: " + formatoNumero.format(contaA));
            
            System.out.println("\nCLASSE B - Medio valore (15% del valore)");
            System.out.println("-----------------------------------------");
            System.out.println("Articoli: " + formatoNumero.format(contaB) + "  Valore totale: EUR " + formatoImporto.format(valoreB));

            System.out.println("\nCLASSE C - Basso valore (5% del valore)");
            System.out.println("-----------------------------------------");
            System.out.println("Articoli: " + formatoNumero.format(contaC) + "  Valore totale: EUR " + formatoImporto.format(valoreC));

            rs.close();
            pstmtAnalisi.close();
            stmt.execute("DROP TABLE ANALISI_ABC");

        } catch (SQLException e) {
            handleSqlException(e, "Analisi ABC");
        } finally {
            if (stmt != null) {
                try {
                    stmt.close();
                } catch (SQLException e) { /* ignore */ }
            }
        }
    }
}