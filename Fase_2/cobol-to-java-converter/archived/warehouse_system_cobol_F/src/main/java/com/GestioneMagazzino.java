package com;

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
import java.sql.Types;
import java.text.DecimalFormat;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Scanner;

/**
 * Traduzione Java del programma COBOL GESTIONE-MAGAZZINO.
 * <p>
 * Questo programma gestisce le operazioni di un magazzino, inclusi carico/scarico merce,
 * gestione ordini, inventario e reportistica, interfacciandosi con un database PostgreSQL tramite JDBC.
 *
 * @author ANNALISA-EGIDI (Original COBOL Author)
 * @author Advanced COBOL to Java Compiler (Translation)
 * @version 2025-05-20
 */
public class GestioneMagazzino {

    // --- Configurazione Database ---
    private static final String DB_URL = "jdbc:postgresql://localhost/magazzino";
    private static final String DB_USER = "postgres";
    private static final String DB_PASSWORD = "password";

    // --- Connessione e stato SQL ---
    private Connection connection;
    private int sqlcode = 0;
    private final Scanner scanner = new Scanner(System.in);

    // --- Variabili della WORKING-STORAGE SECTION ---
    private int wsScelta = 0;
    private String wsContinua = "S";
    private String wsRisposta = "";
    private String wsEsito = "";
    private String wsMetodoValorizz = "F"; // F=FIFO, L=LIFO, M=Medio ponderato

    // --- Strutture dati raggruppate ---
    private final WsArticolo wsArticolo = new WsArticolo();
    private final WsGiacenza wsGiacenza = new WsGiacenza();
    private final WsMovimento wsMovimento = new WsMovimento();
    private final WsOrdine wsOrdine = new WsOrdine();
    private final WsLotto wsLotto = new WsLotto();
    private final WsCalcoli wsCalcoli = new WsCalcoli();
    private final WsContatori wsContatori = new WsContatori();

    // --- Formattatori per output ---
    private static final DecimalFormat FORMATO_IMPORTO = new DecimalFormat("###,##0.00");
    private static final DecimalFormat FORMATO_QUANTITA = new DecimalFormat("###,##0.00");
    private static final DecimalFormat FORMATO_NUMERO = new DecimalFormat("###,##0");
    private static final DecimalFormat FORMATO_PERCENTUALE = new DecimalFormat("##0.00");

    // --- Strutture dati annidate per rispecchiare il COBOL ---

    static class WsArticolo {
        String wsArtCodice = "";
        String wsArtDescrizione = "";
        String wsArtCategoria = "";
        String wsArtUm = "";
        String wsArtFornitore = "";
        BigDecimal wsArtPrezzoAcq = BigDecimal.ZERO;
        BigDecimal wsArtPrezzoVen = BigDecimal.ZERO;
        BigDecimal wsArtScortaMin = BigDecimal.ZERO;
        BigDecimal wsArtPuntoRiord = BigDecimal.ZERO;
        BigDecimal wsArtLottoRiord = BigDecimal.ZERO;
        String wsArtUbicazione = "";
        String wsArtStato = "";
    }

    static class WsGiacenza {
        BigDecimal wsGiaDisponibile = BigDecimal.ZERO;
        BigDecimal wsGiaImpegnata = BigDecimal.ZERO;
        BigDecimal wsGiaOrdinata = BigDecimal.ZERO;
        BigDecimal wsGiaValMedio = BigDecimal.ZERO;
        BigDecimal wsGiaValUltimo = BigDecimal.ZERO;
    }

    static class WsMovimento {
        String wsMovTipo = "";
        String wsMovNumeroDoc = "";
        String wsMovData = "";
        BigDecimal wsMovQuantita = BigDecimal.ZERO;
        BigDecimal wsMovPrezzo = BigDecimal.ZERO;
        BigDecimal wsMovValore = BigDecimal.ZERO;
        String wsMovCausale = "";
        String wsMovFornitore = "";
        String wsMovOperatore = "";
    }

    static class WsOrdine {
        String wsOrdNumero = "";
        String wsOrdData = "";
        String wsOrdFornitore = "";
        String wsOrdStato = "";
        BigDecimal wsOrdTotale = BigDecimal.ZERO;
    }

    static class WsLotto {
        long wsLotId = 0;
        String wsLotNumero = "";
        String wsLotData = "";
        BigDecimal wsLotQtaIni = BigDecimal.ZERO;
        BigDecimal wsLotQtaRes = BigDecimal.ZERO;
        BigDecimal wsLotPrezzo = BigDecimal.ZERO;
    }

    static class WsCalcoli {
        BigDecimal wsQtaRichiesta = BigDecimal.ZERO;
        BigDecimal wsQtaPrelevata = BigDecimal.ZERO;
        BigDecimal wsQtaResidua = BigDecimal.ZERO;
        BigDecimal wsValoreTot = BigDecimal.ZERO;
        BigDecimal wsValoreMedio = BigDecimal.ZERO;
        BigDecimal wsNuovoMedio = BigDecimal.ZERO;
    }

    static class WsContatori {
        int wsContaArticoli = 0;
        int wsContaMovimenti = 0;
        int wsContaSottoscorta = 0;
        BigDecimal wsValoreMagazzino = BigDecimal.ZERO;
    }

    /**
     * Punto di ingresso del programma.
     *
     * @param args Argomenti della riga di comando (non usati).
     */
    public static void main(String[] args) {
        GestioneMagazzino programma = new GestioneMagazzino();
        programma.mainLogic();
    }

    /**
     * Logica principale del programma, che gestisce il menu e il ciclo di vita dell'applicazione.
     */
    public void mainLogic() {
        connettiDatabase();

        do {
            visualizzaMenu();
            elaboraScelta();

            if (wsScelta != 0) {
                System.out.print("\nContinuare? (S/N): ");
                wsContinua = scanner.nextLine().toUpperCase();
            }
        } while (wsContinua.equals("S") && wsScelta != 0);

        disconnettiDatabase();
        scanner.close();
        System.out.println("Programma terminato.");
    }

    // =================================================================
    // GESTIONE DATABASE
    // =================================================================

    private void connettiDatabase() {
        try {
            connection = DriverManager.getConnection(DB_URL, DB_USER, DB_PASSWORD);
            sqlcode = 0;
            System.out.println("Connessione al database stabilita");
        } catch (SQLException e) {
            System.err.println("Errore fatale connessione database: " + e.getErrorCode());
            e.printStackTrace();
            System.exit(1);
        }
    }

    private void disconnettiDatabase() {
        try {
            if (connection != null && !connection.isClosed()) {
                connection.close();
                System.out.println("Disconnesso dal database");
            }
        } catch (SQLException e) {
            System.err.println("Errore durante la disconnessione: " + e.getErrorCode());
        }
    }

    private void handleSqlException(SQLException e, String routine) {
        sqlcode = e.getErrorCode();
        System.err.printf("Errore SQL nella routine %s: SQLCODE=%d, SQLSTATE=%s%n", routine, sqlcode, e.getSQLState());
        System.err.println("Messaggio: " + e.getMessage());
        // SQLSTATE "02000" corrisponde a "no data found", equivalente a SQLCODE 100
        if ("02000".equals(e.getSQLState())) {
            sqlcode = 100;
        }
    }

    private void beginTransaction() throws SQLException {
        if (connection != null) {
            connection.setAutoCommit(false);
        }
    }

    private void commitTransaction() throws SQLException {
        if (connection != null) {
            connection.commit();
        }
    }

    private void rollbackTransaction() {
        if (connection != null) {
            try {
                connection.rollback();
            } catch (SQLException ex) {
                System.err.println("Errore critico durante il rollback della transazione.");
                ex.printStackTrace();
            }
        }
    }

    private void endTransaction() {
        if (connection != null) {
            try {
                connection.setAutoCommit(true);
            } catch (SQLException e) {
                System.err.println("Errore nel ripristino di auto-commit.");
                e.printStackTrace();
            }
        }
    }

    // =================================================================
    // MENU E LOGICA PRINCIPALE
    // =================================================================

    private void visualizzaMenu() {
        System.out.println("\n===== SISTEMA GESTIONE MAGAZZINO =====");
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
        wsScelta = readInt("Scelta: ");
    }

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

    // =================================================================
    // PARAGRAPH IMPLEMENTATIONS (PROCEDURE DIVISION)
    // =================================================================

    private void caricoMerce() {
        System.out.println("\n=== CARICO MERCE ===");
        wsArticolo.wsArtCodice = readString("Codice articolo: ");
        caricaArticolo();
        if (!"OK".equals(wsEsito)) return;

        System.out.println("Articolo: " + wsArticolo.wsArtDescrizione.substring(0, Math.min(50, wsArticolo.wsArtDescrizione.length())));
        wsMovimento.wsMovNumeroDoc = readString("Numero documento (DDT/Fattura): ");
        wsMovimento.wsMovQuantita = readBigDecimal("Quantità da caricare: ");
        if (wsMovimento.wsMovQuantita.compareTo(BigDecimal.ZERO) <= 0) {
            System.out.println("Quantità non valida!");
            return;
        }
        wsMovimento.wsMovPrezzo = readBigDecimal("Prezzo unitario: ");
        if (wsMovimento.wsMovPrezzo.compareTo(BigDecimal.ZERO) <= 0) {
            System.out.println("Prezzo non valido!");
            return;
        }
        wsMovimento.wsMovFornitore = readString("Codice fornitore: ");
        wsMovimento.wsMovCausale = readString("Causale: ");
        wsLotto.wsLotNumero = readString("Numero lotto (opzionale): ");

        wsMovimento.wsMovValore = wsMovimento.wsMovQuantita.multiply(wsMovimento.wsMovPrezzo).setScale(2, RoundingMode.HALF_UP);

        try {
            beginTransaction();
            wsMovimento.wsMovTipo = "CA";
            registraMovimento();

            if ("OK".equals(wsEsito)) {
                aggiornaGiacenzaCarico();
            }
            if ("OK".equals(wsEsito) && wsLotto.wsLotNumero != null && !wsLotto.wsLotNumero.isBlank()) {
                creaLotto();
            }

            if ("OK".equals(wsEsito)) {
                commitTransaction();
                System.out.println("Carico registrato con successo!");
                System.out.println("Valore carico: EUR " + formatImporto(wsMovimento.wsMovValore));
            } else {
                System.out.println("Errore durante il carico! Rollback in corso...");
                rollbackTransaction();
            }
        } catch (SQLException e) {
            handleSqlException(e, "caricoMerce");
            rollbackTransaction();
        } finally {
            endTransaction();
        }
    }

    private void scaricoMerce() {
        System.out.println("\n=== SCARICO MERCE ===");
        wsArticolo.wsArtCodice = readString("Codice articolo: ");
        caricaArticolo();
        if (!"OK".equals(wsEsito)) return;

        System.out.println("Articolo: " + wsArticolo.wsArtDescrizione.substring(0, Math.min(50, wsArticolo.wsArtDescrizione.length())));
        System.out.println("Disponibile: " + formatQuantita(wsGiacenza.wsGiaDisponibile) + " " + wsArticolo.wsArtUm);

        wsMovimento.wsMovQuantita = readBigDecimal("Quantità da scaricare: ");
        if (wsMovimento.wsMovQuantita.compareTo(BigDecimal.ZERO) <= 0) {
            System.out.println("Quantità non valida!");
            return;
        }
        if (wsMovimento.wsMovQuantita.compareTo(wsGiacenza.wsGiaDisponibile) > 0) {
            System.out.println("Quantità non disponibile!");
            return;
        }

        wsMovimento.wsMovNumeroDoc = readString("Numero documento: ");
        wsMovimento.wsMovCausale = readString("Causale: ");
        wsMetodoValorizz = readString("Metodo valorizzazione (F=FIFO, L=LIFO, M=Medio): ").toUpperCase();

        try {
            beginTransaction();
            calcolaValoreScarico();

            if ("OK".equals(wsEsito)) {
                wsMovimento.wsMovTipo = "SC";
                registraMovimento();
            }
            if ("OK".equals(wsEsito)) {
                aggiornaGiacenzaScarico();
            }
            if ("OK".equals(wsEsito) && !"M".equals(wsMetodoValorizz)) {
                aggiornaLottiScarico();
            }

            if ("OK".equals(wsEsito)) {
                commitTransaction();
                System.out.println("Scarico registrato con successo!");
                System.out.println("Valore scarico: EUR " + formatImporto(wsMovimento.wsMovValore));
            } else {
                System.out.println("Errore durante lo scarico! Rollback in corso...");
                rollbackTransaction();
            }
        } catch (SQLException e) {
            handleSqlException(e, "scaricoMerce");
            rollbackTransaction();
        } finally {
            endTransaction();
        }
    }

    private void caricaArticolo() {
        wsEsito = "OK";
        String sql = "SELECT a.descrizione, a.codice_categoria, a.unita_misura, a.codice_fornitore, " +
                     "a.prezzo_acquisto, a.prezzo_vendita, a.scorta_minima, a.punto_riordino, " +
                     "a.lotto_riordino, a.ubicazione, a.stato, g.quantita_disponibile, g.quantita_impegnata, " +
                     "g.quantita_ordinata, g.valore_medio, g.valore_ultimo " +
                     "FROM ARTICOLI a LEFT JOIN GIACENZE g ON a.codice_articolo = g.codice_articolo " +
                     "WHERE a.codice_articolo = ?";

        try (PreparedStatement ps = connection.prepareStatement(sql)) {
            ps.setString(1, wsArticolo.wsArtCodice.trim());
            try (ResultSet rs = ps.executeQuery()) {
                if (rs.next()) {
                    sqlcode = 0;
                    wsArticolo.wsArtDescrizione = rs.getString("descrizione");
                    wsArticolo.wsArtCategoria = rs.getString("codice_categoria");
                    wsArticolo.wsArtUm = rs.getString("unita_misura");
                    wsArticolo.wsArtFornitore = rs.getString("codice_fornitore");
                    wsArticolo.wsArtPrezzoAcq = rs.getBigDecimal("prezzo_acquisto");
                    wsArticolo.wsArtPrezzoVen = rs.getBigDecimal("prezzo_vendita");
                    wsArticolo.wsArtScortaMin = rs.getBigDecimal("scorta_minima");
                    wsArticolo.wsArtPuntoRiord = rs.getBigDecimal("punto_riordino");
                    wsArticolo.wsArtLottoRiord = rs.getBigDecimal("lotto_riordino");
                    wsArticolo.wsArtUbicazione = rs.getString("ubicazione");
                    wsArticolo.wsArtStato = rs.getString("stato");

                    wsGiacenza.wsGiaDisponibile = rs.getBigDecimal("quantita_disponibile");
                    if (rs.wasNull()) wsGiacenza.wsGiaDisponibile = BigDecimal.ZERO;
                    wsGiacenza.wsGiaImpegnata = rs.getBigDecimal("quantita_impegnata");
                    if (rs.wasNull()) wsGiacenza.wsGiaImpegnata = BigDecimal.ZERO;
                    wsGiacenza.wsGiaOrdinata = rs.getBigDecimal("quantita_ordinata");
                    if (rs.wasNull()) wsGiacenza.wsGiaOrdinata = BigDecimal.ZERO;
                    wsGiacenza.wsGiaValMedio = rs.getBigDecimal("valore_medio");
                    if (rs.wasNull()) wsGiacenza.wsGiaValMedio = BigDecimal.ZERO;
                    wsGiacenza.wsGiaValUltimo = rs.getBigDecimal("valore_ultimo");
                    if (rs.wasNull()) wsGiacenza.wsGiaValUltimo = BigDecimal.ZERO;

                    if (!"A".equals(wsArticolo.wsArtStato.trim())) {
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
    }

    private void registraMovimento() {
        wsMovimento.wsMovOperatore = System.getProperty("user.name");
        String sql = "INSERT INTO MOVIMENTI_MAGAZZINO (tipo_movimento, numero_documento, codice_articolo, " +
                     "quantita, prezzo_unitario, valore_totale, causale, codice_fornitore, operatore) " +
                     "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)";
        try (PreparedStatement ps = connection.prepareStatement(sql)) {
            ps.setString(1, wsMovimento.wsMovTipo);
            ps.setString(2, wsMovimento.wsMovNumeroDoc);
            ps.setString(3, wsArticolo.wsArtCodice.trim());
            ps.setBigDecimal(4, wsMovimento.wsMovQuantita);
            ps.setBigDecimal(5, wsMovimento.wsMovPrezzo);
            ps.setBigDecimal(6, wsMovimento.wsMovValore);
            ps.setString(7, wsMovimento.wsMovCausale);
            
            if (wsMovimento.wsMovFornitore != null && !wsMovimento.wsMovFornitore.isBlank()) {
                ps.setString(8, wsMovimento.wsMovFornitore.trim());
            } else {
                ps.setNull(8, Types.CHAR);
            }
            
            ps.setString(9, wsMovimento.wsMovOperatore);
            ps.executeUpdate();
            sqlcode = 0;
        } catch (SQLException e) {
            handleSqlException(e, "registraMovimento");
            System.out.println("Errore registrazione movimento: " + sqlcode);
            wsEsito = "KO";
        }
    }

    private void aggiornaGiacenzaCarico() {
        BigDecimal qtaTotale = wsGiacenza.wsGiaDisponibile.add(wsMovimento.wsMovQuantita);
        if (qtaTotale.compareTo(BigDecimal.ZERO) != 0) {
            BigDecimal valoreGiacenza = wsGiacenza.wsGiaDisponibile.multiply(wsGiacenza.wsGiaValMedio);
            BigDecimal valoreCarico = wsMovimento.wsMovQuantita.multiply(wsMovimento.wsMovPrezzo);
            wsCalcoli.wsValoreTot = valoreGiacenza.add(valoreCarico);
            wsCalcoli.wsNuovoMedio = wsCalcoli.wsValoreTot.divide(qtaTotale, 4, RoundingMode.HALF_UP);
        } else {
            wsCalcoli.wsNuovoMedio = wsMovimento.wsMovPrezzo;
        }

        String updateSql = "UPDATE GIACENZE SET quantita_disponibile = quantita_disponibile + ?, " +
                           "valore_medio = ?, valore_ultimo = ?, data_ultimo_carico = CURRENT_DATE " +
                           "WHERE codice_articolo = ?";
        try (PreparedStatement psUpdate = connection.prepareStatement(updateSql)) {
            psUpdate.setBigDecimal(1, wsMovimento.wsMovQuantita);
            psUpdate.setBigDecimal(2, wsCalcoli.wsNuovoMedio);
            psUpdate.setBigDecimal(3, wsMovimento.wsMovPrezzo);
            psUpdate.setString(4, wsArticolo.wsArtCodice.trim());
            int rowsAffected = psUpdate.executeUpdate();

            if (rowsAffected == 0) { // Giacenza non esiste, creala
                String insertSql = "INSERT INTO GIACENZE (codice_articolo, quantita_disponibile, valore_medio, " +
                                   "valore_ultimo, data_ultimo_carico) VALUES (?, ?, ?, ?, CURRENT_DATE)";
                try (PreparedStatement psInsert = connection.prepareStatement(insertSql)) {
                    psInsert.setString(1, wsArticolo.wsArtCodice.trim());
                    psInsert.setBigDecimal(2, wsMovimento.wsMovQuantita);
                    psInsert.setBigDecimal(3, wsMovimento.wsMovPrezzo);
                    psInsert.setBigDecimal(4, wsMovimento.wsMovPrezzo);
                    psInsert.executeUpdate();
                }
            }
            sqlcode = 0;
        } catch (SQLException e) {
            handleSqlException(e, "aggiornaGiacenzaCarico");
            System.out.println("Errore aggiornamento giacenza: " + sqlcode);
            wsEsito = "KO";
        }
    }

    private void aggiornaGiacenzaScarico() {
        String sql = "UPDATE GIACENZE SET quantita_disponibile = quantita_disponibile - ?, " +
                     "data_ultimo_scarico = CURRENT_DATE WHERE codice_articolo = ?";
        try (PreparedStatement ps = connection.prepareStatement(sql)) {
            ps.setBigDecimal(1, wsMovimento.wsMovQuantita);
            ps.setString(2, wsArticolo.wsArtCodice.trim());
            ps.executeUpdate();
            sqlcode = 0;
        } catch (SQLException e) {
            handleSqlException(e, "aggiornaGiacenzaScarico");
            System.out.println("Errore aggiornamento giacenza: " + sqlcode);
            wsEsito = "KO";
        }
    }

    private void creaLotto() {
        String sql = "INSERT INTO LOTTI (codice_articolo, numero_lotto, data_carico, quantita_iniziale, " +
                     "quantita_residua, prezzo_acquisto) VALUES (?, ?, CURRENT_DATE, ?, ?, ?)";
        try (PreparedStatement ps = connection.prepareStatement(sql)) {
            ps.setString(1, wsArticolo.wsArtCodice.trim());
            ps.setString(2, wsLotto.wsLotNumero);
            ps.setBigDecimal(3, wsMovimento.wsMovQuantita);
            ps.setBigDecimal(4, wsMovimento.wsMovQuantita);
            ps.setBigDecimal(5, wsMovimento.wsMovPrezzo);
            ps.executeUpdate();
            sqlcode = 0;
        } catch (SQLException e) {
            if ("23505".equals(e.getSQLState())) { // Unique violation
                System.out.println("Lotto già esistente!");
            } else {
                handleSqlException(e, "creaLotto");
                System.out.println("Errore creazione lotto: " + sqlcode);
            }
            wsEsito = "KO";
        }
    }

    private void calcolaValoreScarico() {
        wsEsito = "OK";
        switch (wsMetodoValorizz) {
            case "F":
                calcolaValoreFifoLifo(true);
                break;
            case "L":
                calcolaValoreFifoLifo(false);
                break;
            case "M":
                wsMovimento.wsMovPrezzo = wsGiacenza.wsGiaValMedio;
                wsMovimento.wsMovValore = wsMovimento.wsMovQuantita.multiply(wsMovimento.wsMovPrezzo).setScale(2, RoundingMode.HALF_UP);
                break;
            default:
                System.out.println("Metodo valorizzazione non valido!");
                wsEsito = "KO";
                break;
        }
    }

    private void calcolaValoreFifoLifo(boolean isFifo) {
        wsCalcoli.wsQtaRichiesta = wsMovimento.wsMovQuantita;
        wsMovimento.wsMovValore = BigDecimal.ZERO;

        String sql = "SELECT id_lotto, numero_lotto, quantita_residua, prezzo_acquisto, data_carico FROM LOTTI " +
                     "WHERE codice_articolo = ? AND quantita_residua > 0 AND stato = 'A' " +
                     "ORDER BY data_carico " + (isFifo ? "ASC, id_lotto ASC" : "DESC, id_lotto DESC");

        try (PreparedStatement ps = connection.prepareStatement(sql)) {
            ps.setString(1, wsArticolo.wsArtCodice.trim());
            try (ResultSet rs = ps.executeQuery()) {
                while (wsCalcoli.wsQtaRichiesta.compareTo(BigDecimal.ZERO) > 0 && rs.next()) {
                    wsLotto.wsLotQtaRes = rs.getBigDecimal("quantita_residua");
                    wsLotto.wsLotPrezzo = rs.getBigDecimal("prezzo_acquisto");

                    BigDecimal qtaDaPrelevare = wsLotto.wsLotQtaRes.min(wsCalcoli.wsQtaRichiesta);
                    BigDecimal valorePrelievo = qtaDaPrelevare.multiply(wsLotto.wsLotPrezzo);
                    wsMovimento.wsMovValore = wsMovimento.wsMovValore.add(valorePrelievo);
                    wsCalcoli.wsQtaRichiesta = wsCalcoli.wsQtaRichiesta.subtract(qtaDaPrelevare);
                }
            }
        } catch (SQLException e) {
            handleSqlException(e, "calcolaValore" + (isFifo ? "Fifo" : "Lifo"));
            wsEsito = "KO";
            return;
        }

        if (wsCalcoli.wsQtaRichiesta.compareTo(BigDecimal.ZERO) > 0) {
            System.out.println("Lotti insufficienti per " + (isFifo ? "FIFO" : "LIFO") + "!");
            wsEsito = "KO";
        } else {
            wsMovimento.wsMovPrezzo = wsMovimento.wsMovValore.divide(wsMovimento.wsMovQuantita, 4, RoundingMode.HALF_UP);
        }
    }

    private void aggiornaLottiScarico() {
        wsCalcoli.wsQtaRichiesta = wsMovimento.wsMovQuantita;
        boolean isFifo = "F".equals(wsMetodoValorizz);

        String selectSql = "SELECT id_lotto, quantita_residua FROM LOTTI " +
                           "WHERE codice_articolo = ? AND quantita_residua > 0 AND stato = 'A' " +
                           "ORDER BY data_carico " + (isFifo ? "ASC, id_lotto ASC" : "DESC, id_lotto DESC");
        
        String updateSql = "UPDATE LOTTI SET quantita_residua = ?, stato = ? WHERE id_lotto = ?";

        try (PreparedStatement psSelect = connection.prepareStatement(selectSql)) {
            psSelect.setString(1, wsArticolo.wsArtCodice.trim());
            try (ResultSet rs = psSelect.executeQuery()) {
                while (wsCalcoli.wsQtaRichiesta.compareTo(BigDecimal.ZERO) > 0 && rs.next()) {
                    long lottoId = rs.getLong("id_lotto");
                    BigDecimal qtaResiduaLotto = rs.getBigDecimal("quantita_residua");
                    
                    BigDecimal qtaDaPrelevare = qtaResiduaLotto.min(wsCalcoli.wsQtaRichiesta);
                    BigDecimal nuovaQtaResidua = qtaResiduaLotto.subtract(qtaDaPrelevare);
                    String nuovoStato = nuovaQtaResidua.compareTo(BigDecimal.ZERO) == 0 ? "E" : "A";

                    try (PreparedStatement psUpdate = connection.prepareStatement(updateSql)) {
                        psUpdate.setBigDecimal(1, nuovaQtaResidua);
                        psUpdate.setString(2, nuovoStato);
                        psUpdate.setLong(3, lottoId);
                        psUpdate.executeUpdate();
                    }
                    wsCalcoli.wsQtaRichiesta = wsCalcoli.wsQtaRichiesta.subtract(qtaDaPrelevare);
                }
            }
        } catch (SQLException e) {
            handleSqlException(e, "aggiornaLottiScarico");
            wsEsito = "KO";
        }
    }

    private void visualizzaGiacenza() {
        System.out.println("\n=== VISUALIZZA GIACENZA ===");
        wsArticolo.wsArtCodice = readString("Codice articolo: ");
        caricaArticolo();
        if (!"OK".equals(wsEsito)) return;

        System.out.println("\nARTICOLO: " + wsArticolo.wsArtCodice.trim());
        System.out.println("Descrizione: " + wsArticolo.wsArtDescrizione.substring(0, Math.min(60, wsArticolo.wsArtDescrizione.length())));
        System.out.println("Categoria: " + wsArticolo.wsArtCategoria.trim() + "  UM: " + wsArticolo.wsArtUm);
        System.out.println("Ubicazione: " + wsArticolo.wsArtUbicazione);
        
        System.out.println("\nQUANTITA':");
        System.out.println("  Disponibile:     " + formatQuantita(wsGiacenza.wsGiaDisponibile));
        System.out.println("  Impegnata:       " + formatQuantita(wsGiacenza.wsGiaImpegnata));
        System.out.println("  In ordine:       " + formatQuantita(wsGiacenza.wsGiaOrdinata));
        BigDecimal qtaNetta = wsGiacenza.wsGiaDisponibile.subtract(wsGiacenza.wsGiaImpegnata);
        System.out.println("  Netta:           " + formatQuantita(qtaNetta));

        System.out.println("\nSCORTE:");
        System.out.println("  Scorta minima:   " + formatQuantita(wsArticolo.wsArtScortaMin));
        System.out.println("  Punto riordino:  " + formatQuantita(wsArticolo.wsArtPuntoRiord));
        System.out.println("  Lotto riordino:  " + formatQuantita(wsArticolo.wsArtLottoRiord));

        System.out.println("\nVALORI:");
        System.out.println("  Valore medio:    EUR " + formatImporto(wsGiacenza.wsGiaValMedio));
        System.out.println("  Valore ultimo:   EUR " + formatImporto(wsGiacenza.wsGiaValUltimo));
        BigDecimal valoreTotale = wsGiacenza.wsGiaDisponibile.multiply(wsGiacenza.wsGiaValMedio);
        System.out.println("  Valore totale:   EUR " + formatImporto(valoreTotale));

        if (wsGiacenza.wsGiaDisponibile.compareTo(wsArticolo.wsArtScortaMin) <= 0) {
            System.out.println("\n*** ATTENZIONE: SCORTA MINIMA RAGGIUNTA ***");
        } else if (wsGiacenza.wsGiaDisponibile.compareTo(wsArticolo.wsArtPuntoRiord) <= 0) {
            System.out.println("\n*** ATTENZIONE: PUNTO RIORDINO RAGGIUNTO ***");
        }
    }

    private void listaSottoscorta() {
        System.out.println("\n=== ARTICOLI SOTTOSCORTA ===\n");
        wsContatori.wsContaSottoscorta = 0;
        String sql = "SELECT a.codice_articolo, a.descrizione, g.quantita_disponibile, g.quantita_ordinata, " +
                     "a.punto_riordino, a.lotto_riordino, f.ragione_sociale " +
                     "FROM ARTICOLI a JOIN GIACENZE g ON a.codice_articolo = g.codice_articolo " +
                     "LEFT JOIN FORNITORI f ON a.codice_fornitore = f.codice_fornitore " +
                     "WHERE a.stato = 'A' AND g.quantita_disponibile <= a.punto_riordino AND a.punto_riordino > 0 " +
                     "ORDER BY (a.punto_riordino - g.quantita_disponibile) DESC";

        try (Statement stmt = connection.createStatement(); ResultSet rs = stmt.executeQuery(sql)) {
            while (rs.next()) {
                wsContatori.wsContaSottoscorta++;
                String codice = rs.getString("codice_articolo").trim();
                String desc = rs.getString("descrizione");
                BigDecimal disponibile = rs.getBigDecimal("quantita_disponibile");
                BigDecimal ordinata = rs.getBigDecimal("quantita_ordinata");
                BigDecimal puntoRiordino = rs.getBigDecimal("punto_riordino");
                BigDecimal lottoRiordino = rs.getBigDecimal("lotto_riordino");
                String fornitore = rs.getString("ragione_sociale");

                System.out.println(codice + " - " + desc.substring(0, Math.min(40, desc.length())));
                System.out.println("  Disponibile: " + formatQuantita(disponibile));
                BigDecimal mancanti = puntoRiordino.subtract(disponibile);
                System.out.println("  Punto riordino: " + formatQuantita(puntoRiordino) + "  Mancanti: " + formatQuantita(mancanti));

                if (ordinata.compareTo(BigDecimal.ZERO) > 0) {
                    System.out.println("  In ordine: " + formatQuantita(ordinata));
                } else {
                    System.out.println("  DA ORDINARE: " + formatQuantita(lottoRiordino) + " (Fornitore: " + (fornitore != null ? fornitore : "N/D") + ")");
                }
                System.out.println();
            }
            sqlcode = rs.isAfterLast() ? 100 : 0;
        } catch (SQLException e) {
            handleSqlException(e, "listaSottoscorta");
        }
        System.out.println("Totale articoli sottoscorta: " + formatNumero(wsContatori.wsContaSottoscorta));
    }

    private void valorizzazioneMagazzino() {
        System.out.println("\n=== VALORIZZAZIONE MAGAZZINO ===\n");
        String fileName = "REPORT-MAGAZZINO.TXT";
        try (PrintWriter writer = new PrintWriter(new FileWriter(fileName))) {
            writer.println("REPORT VALORIZZAZIONE MAGAZZINO");
            writer.println("===============================");
            writer.printf("Data: %s    Ora: %s%n%n",
                LocalDate.now().format(DateTimeFormatter.ISO_LOCAL_DATE),
                LocalDateTime.now().format(DateTimeFormatter.ofPattern("HH:mm:ss")));

            wsContatori.wsContaArticoli = 0;
            wsContatori.wsValoreMagazzino = BigDecimal.ZERO;

            String sql = "SELECT a.codice_articolo, a.descrizione, a.unita_misura, a.ubicazione, " +
                         "g.quantita_disponibile, g.valore_medio " +
                         "FROM ARTICOLI a JOIN GIACENZE g ON a.codice_articolo = g.codice_articolo " +
                         "WHERE a.stato = 'A' ORDER BY a.codice_articolo";

            try (Statement stmt = connection.createStatement(); ResultSet rs = stmt.executeQuery(sql)) {
                while (rs.next()) {
                    BigDecimal disponibile = rs.getBigDecimal("quantita_disponibile");
                    if (disponibile != null && disponibile.compareTo(BigDecimal.ZERO) > 0) {
                        wsContatori.wsContaArticoli++;
                        String codice = rs.getString("codice_articolo").trim();
                        String desc = rs.getString("descrizione");
                        String um = rs.getString("unita_misura");
                        BigDecimal valMedio = rs.getBigDecimal("valore_medio");
                        BigDecimal valoreTotArt = disponibile.multiply(valMedio);
                        wsContatori.wsValoreMagazzino = wsContatori.wsValoreMagazzino.add(valoreTotArt);

                        writer.println(String.format("%-10s %-40s", codice, desc.substring(0, Math.min(40, desc.length()))));
                        writer.println(String.format("  Qtà: %-15s Val.medio: %-15s Totale: EUR %s",
                            formatQuantita(disponibile) + " " + um,
                            formatImporto(valMedio),
                            formatImporto(valoreTotArt)));
                    }
                }
            } catch (SQLException e) {
                handleSqlException(e, "valorizzazioneMagazzino");
            }

            writer.println("\n-------------------------------");
            writer.println("Articoli valorizzati: " + formatNumero(wsContatori.wsContaArticoli));
            writer.println("VALORE TOTALE MAGAZZINO: EUR " + formatImporto(wsContatori.wsValoreMagazzino));

            System.out.println("Report salvato in " + fileName);
            System.out.println("Articoli valorizzati: " + formatNumero(wsContatori.wsContaArticoli));
            System.out.println("Valore totale: EUR " + formatImporto(wsContatori.wsValoreMagazzino));

        } catch (IOException e) {
            System.err.println("Errore durante la scrittura del file di report: " + e.getMessage());
        }
    }

    private void movimentiArticolo() {
        System.out.println("\n=== MOVIMENTI ARTICOLO ===");
        wsArticolo.wsArtCodice = readString("Codice articolo: ");
        caricaArticolo();
        if (!"OK".equals(wsEsito)) return;

        System.out.println("\nUltimi movimenti di: " + wsArticolo.wsArtDescrizione.substring(0, Math.min(50, wsArticolo.wsArtDescrizione.length())));
        System.out.println("\nTIPO  DATA/ORA            DOCUMENTO             QUANTITA    VALORE");
        System.out.println("--------------------------------------------------------------------------");

        String sql = "SELECT tipo_movimento, data_movimento, numero_documento, quantita, valore_totale, causale " +
                     "FROM MOVIMENTI_MAGAZZINO WHERE codice_articolo = ? " +
                     "ORDER BY data_movimento DESC LIMIT 50";

        try (PreparedStatement ps = connection.prepareStatement(sql)) {
            ps.setString(1, wsArticolo.wsArtCodice.trim());
            try (ResultSet rs = ps.executeQuery()) {
                while (rs.next()) {
                    String tipo = rs.getString("tipo_movimento").trim();
                    Timestamp data = rs.getTimestamp("data_movimento");
                    String doc = rs.getString("numero_documento");
                    BigDecimal qta = rs.getBigDecimal("quantita");
                    BigDecimal valore = rs.getBigDecimal("valore_totale");
                    String causale = rs.getString("causale");

                    String segno = ("SC".equals(tipo) || "RE".equals(tipo)) ? "-" : "+";
                    
                    System.out.printf("%-4s  %-19s %-20s %s%10s  EUR %10s%n",
                        tipo,
                        data.toLocalDateTime().format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")),
                        doc != null ? doc.substring(0, Math.min(20, doc.length())) : "",
                        segno,
                        formatQuantita(qta),
                        formatImporto(valore));
                    System.out.printf("      %s%n%n", causale != null ? causale.substring(0, Math.min(50, causale.length())) : "");
                }
            }
        } catch (SQLException e) {
            handleSqlException(e, "movimentiArticolo");
        }
    }

    private void rettificaInventario() {
        System.out.println("\n=== RETTIFICA INVENTARIO ===");
        wsArticolo.wsArtCodice = readString("Codice articolo: ");
        caricaArticolo();
        if (!"OK".equals(wsEsito)) return;

        System.out.println("Articolo: " + wsArticolo.wsArtDescrizione.substring(0, Math.min(50, wsArticolo.wsArtDescrizione.length())));
        System.out.println("Giacenza attuale: " + formatQuantita(wsGiacenza.wsGiaDisponibile) + " " + wsArticolo.wsArtUm);
        
        BigDecimal giacenzaRilevata = readBigDecimal("Giacenza rilevata: ");
        BigDecimal differenza = giacenzaRilevata.subtract(wsGiacenza.wsGiaDisponibile);

        if (differenza.compareTo(BigDecimal.ZERO) == 0) {
            System.out.println("Nessuna differenza rilevata");
            return;
        }

        wsMovimento.wsMovTipo = "RI";
        if (differenza.compareTo(BigDecimal.ZERO) > 0) {
            System.out.println("Differenza: +" + formatQuantita(differenza));
            wsMovimento.wsMovCausale = "Rettifica inventario positiva";
            wsMovimento.wsMovQuantita = differenza;
        } else {
            System.out.println("Differenza: " + formatQuantita(differenza));
            wsMovimento.wsMovCausale = "Rettifica inventario negativa";
            wsMovimento.wsMovQuantita = differenza.abs();
        }

        String note = readString("Note rettifica: ");
        wsMovimento.wsMovCausale += " - " + note;
        
        wsRisposta = readString("Confermare rettifica (S/N): ").toUpperCase();
        if (!"S".equals(wsRisposta)) {
            System.out.println("Rettifica annullata");
            return;
        }

        wsMovimento.wsMovNumeroDoc = "INV" + LocalDate.now().format(DateTimeFormatter.ofPattern("yyyyMMdd"));
        wsMovimento.wsMovPrezzo = wsGiacenza.wsGiaValMedio;
        wsMovimento.wsMovValore = wsMovimento.wsMovQuantita.multiply(wsMovimento.wsMovPrezzo).setScale(2, RoundingMode.HALF_UP);

        try {
            beginTransaction();
            registraMovimento();

            if ("OK".equals(wsEsito)) {
                String sqlUpdate = "UPDATE GIACENZE SET quantita_disponibile = ?, data_ultimo_inventario = CURRENT_DATE " +
                                   "WHERE codice_articolo = ?";
                try (PreparedStatement ps = connection.prepareStatement(sqlUpdate)) {
                    ps.setBigDecimal(1, giacenzaRilevata);
                    ps.setString(2, wsArticolo.wsArtCodice.trim());
                    ps.executeUpdate();
                    sqlcode = 0;
                } catch (SQLException e) {
                    handleSqlException(e, "rettificaInventario.update");
                    wsEsito = "KO";
                }
            }

            if ("OK".equals(wsEsito)) {
                commitTransaction();
                System.out.println("Rettifica eseguita con successo!");
            } else {
                System.out.println("Errore durante la rettifica!");
                rollbackTransaction();
            }
        } catch (SQLException e) {
            handleSqlException(e, "rettificaInventario.transaction");
            rollbackTransaction();
        } finally {
            endTransaction();
        }
    }

    private void gestioneOrdini() {
        System.out.println("\n=== GESTIONE ORDINI FORNITORI ===");
        System.out.println("1. Nuovo ordine");
        System.out.println("2. Visualizza ordini aperti");
        System.out.println("3. Ricevi merce da ordine");
        System.out.println("4. Stato ordine");
        int scelta = readInt("\nScelta: ");
        switch (scelta) {
            case 1: nuovoOrdine(); break;
            case 2: visualizzaOrdiniAperti(); break;
            case 3: riceviMerceOrdine(); break;
            case 4: statoOrdine(); break;
            default: System.out.println("Scelta non valida!"); break;
        }
    }

    private void nuovoOrdine() {
        System.out.println("\n=== NUOVO ORDINE FORNITORE ===");
        wsOrdine.wsOrdFornitore = readString("Codice fornitore: ");

        String sqlCheckForn = "SELECT ragione_sociale FROM FORNITORI WHERE codice_fornitore = ? AND stato = 'A'";
        try (PreparedStatement ps = connection.prepareStatement(sqlCheckForn)) {
            ps.setString(1, wsOrdine.wsOrdFornitore.trim());
            try (ResultSet rs = ps.executeQuery()) {
                if (!rs.next()) {
                    System.out.println("Fornitore non trovato o non attivo!");
                    return;
                }
                System.out.println("Fornitore: " + rs.getString("ragione_sociale"));
            }
        } catch (SQLException e) {
            handleSqlException(e, "nuovoOrdine.checkFornitore");
            return;
        }

        wsOrdine.wsOrdNumero = "ORD" + LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyyMMddHHmmss"));
        wsOrdine.wsOrdStato = "A";
        wsOrdine.wsOrdTotale = BigDecimal.ZERO;

        String sqlInsertOrd = "INSERT INTO ORDINI (numero_ordine, data_ordine, codice_fornitore, stato_ordine, totale_ordine) " +
                              "VALUES (?, CURRENT_DATE, ?, ?, ?)";
        try (PreparedStatement ps = connection.prepareStatement(sqlInsertOrd)) {
            ps.setString(1, wsOrdine.wsOrdNumero);
            ps.setString(2, wsOrdine.wsOrdFornitore.trim());
            ps.setString(3, wsOrdine.wsOrdStato);
            ps.setBigDecimal(4, wsOrdine.wsOrdTotale);
            ps.executeUpdate();
            System.out.println("Ordine " + wsOrdine.wsOrdNumero + " creato!\n");
            aggiungiRigheOrdine();
        } catch (SQLException e) {
            handleSqlException(e, "nuovoOrdine.insert");
            System.out.println("Errore creazione ordine: " + sqlcode);
        }
    }

    private void aggiungiRigheOrdine() {
        try {
            beginTransaction();
            String continuaAggiunta;
            do {
                wsArticolo.wsArtCodice = readString("Codice articolo: ");
                caricaArticolo();
                if ("OK".equals(wsEsito)) {
                    System.out.println("Articolo: " + wsArticolo.wsArtDescrizione.substring(0, Math.min(40, wsArticolo.wsArtDescrizione.length())));
                    wsMovimento.wsMovQuantita = readBigDecimal("Quantità da ordinare: ");
                    if (wsMovimento.wsMovQuantita.compareTo(BigDecimal.ZERO) > 0) {
                        wsMovimento.wsMovPrezzo = wsArticolo.wsArtPrezzoAcq;
                        wsMovimento.wsMovValore = wsMovimento.wsMovQuantita.multiply(wsMovimento.wsMovPrezzo).setScale(2, RoundingMode.HALF_UP);

                        String sqlInsertRiga = "INSERT INTO RIGHE_ORDINE (numero_ordine, codice_articolo, quantita_ordinata, prezzo_unitario, importo_riga) " +
                                               "VALUES (?, ?, ?, ?, ?)";
                        try (PreparedStatement ps = connection.prepareStatement(sqlInsertRiga)) {
                            ps.setString(1, wsOrdine.wsOrdNumero);
                            ps.setString(2, wsArticolo.wsArtCodice.trim());
                            ps.setBigDecimal(3, wsMovimento.wsMovQuantita);
                            ps.setBigDecimal(4, wsMovimento.wsMovPrezzo);
                            ps.setBigDecimal(5, wsMovimento.wsMovValore);
                            ps.executeUpdate();
                            
                            wsOrdine.wsOrdTotale = wsOrdine.wsOrdTotale.add(wsMovimento.wsMovValore);

                            String sqlUpdateGiac = "UPDATE GIACENZE SET quantita_ordinata = quantita_ordinata + ? WHERE codice_articolo = ?";
                            try (PreparedStatement psGiac = connection.prepareStatement(sqlUpdateGiac)) {
                                psGiac.setBigDecimal(1, wsMovimento.wsMovQuantita);
                                psGiac.setString(2, wsArticolo.wsArtCodice.trim());
                                psGiac.executeUpdate();
                            }
                            System.out.println("Riga ordine aggiunta!");
                        } catch (SQLException e) {
                            handleSqlException(e, "aggiungiRigheOrdine.insert");
                            System.out.println("Errore aggiunta riga: " + sqlcode);
                            throw e; // Propagate to trigger rollback
                        }
                    }
                }
                continuaAggiunta = readString("Aggiungere altro articolo (S/N): ").toUpperCase();
            } while ("S".equals(continuaAggiunta));

            String sqlUpdateTot = "UPDATE ORDINI SET totale_ordine = ? WHERE numero_ordine = ?";
            try (PreparedStatement ps = connection.prepareStatement(sqlUpdateTot)) {
                ps.setBigDecimal(1, wsOrdine.wsOrdTotale);
                ps.setString(2, wsOrdine.wsOrdNumero);
                ps.executeUpdate();
            }
            
            commitTransaction();
            System.out.println("\nOrdine completato. Totale: EUR " + formatImporto(wsOrdine.wsOrdTotale));

        } catch (SQLException e) {
            System.err.println("Transazione ordine annullata a causa di un errore.");
            rollbackTransaction();
        } finally {
            endTransaction();
        }
    }

    private void visualizzaOrdiniAperti() {
        System.out.println("\n=== ORDINI APERTI ===\n");
        String sql = "SELECT o.numero_ordine, o.data_ordine, f.ragione_sociale, o.totale_ordine, o.stato_ordine " +
                     "FROM ORDINI o JOIN FORNITORI f ON o.codice_fornitore = f.codice_fornitore " +
                     "WHERE o.stato_ordine IN ('A', 'C', 'P') ORDER BY o.data_ordine DESC";
        int count = 0;
        try (Statement stmt = connection.createStatement(); ResultSet rs = stmt.executeQuery(sql)) {
            while (rs.next()) {
                count++;
                String statoDesc;
                switch (rs.getString("stato_ordine").trim()) {
                    case "A": statoDesc = "APERTO"; break;
                    case "C": statoDesc = "CONFERMATO"; break;
                    case "P": statoDesc = "PARZIALMENTE EVASO"; break;
                    default: statoDesc = "SCONOSCIUTO"; break;
                }
                System.out.println(rs.getString("numero_ordine") + " del " + rs.getDate("data_ordine"));
                System.out.println("  Fornitore: " + rs.getString("ragione_sociale"));
                System.out.println("  Totale: EUR " + formatImporto(rs.getBigDecimal("totale_ordine")));
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

    private void riceviMerceOrdine() {
        System.out.println("\n=== RICEVI MERCE DA ORDINE ===");
        wsOrdine.wsOrdNumero = readString("Numero ordine: ");

        String sqlCheckOrd = "SELECT stato_ordine, codice_fornitore FROM ORDINI WHERE numero_ordine = ?";
        try (PreparedStatement ps = connection.prepareStatement(sqlCheckOrd)) {
            ps.setString(1, wsOrdine.wsOrdNumero);
            try (ResultSet rs = ps.executeQuery()) {
                if (!rs.next()) {
                    System.out.println("Ordine non trovato!");
                    return;
                }
                wsOrdine.wsOrdStato = rs.getString("stato_ordine").trim();
                wsOrdine.wsOrdFornitore = rs.getString("codice_fornitore").trim();
                if ("E".equals(wsOrdine.wsOrdStato)) {
                    System.out.println("Ordine già completamente evaso!");
                    return;
                }
            }
        } catch (SQLException e) {
            handleSqlException(e, "riceviMerceOrdine.check");
            return;
        }

        System.out.println("\nRighe ordine da ricevere:\n");
        String sqlRighe = "SELECT r.codice_articolo, a.descrizione, r.quantita_ordinata, r.quantita_ricevuta, r.prezzo_unitario " +
                          "FROM RIGHE_ORDINE r JOIN ARTICOLI a ON r.codice_articolo = a.codice_articolo " +
                          "WHERE r.numero_ordine = ? AND r.quantita_ricevuta < r.quantita_ordinata AND r.stato_riga <> 'C'";
        
        try (PreparedStatement psRighe = connection.prepareStatement(sqlRighe)) {
            psRighe.setString(1, wsOrdine.wsOrdNumero);
            try (ResultSet rsRighe = psRighe.executeQuery()) {
                while (rsRighe.next()) {
                    wsArticolo.wsArtCodice = rsRighe.getString("codice_articolo");
                    String desc = rsRighe.getString("descrizione");
                    BigDecimal qtaOrdinata = rsRighe.getBigDecimal("quantita_ordinata");
                    BigDecimal qtaRicevuta = rsRighe.getBigDecimal("quantita_ricevuta");
                    wsMovimento.wsMovPrezzo = rsRighe.getBigDecimal("prezzo_unitario");
                    BigDecimal qtaDaRicevere = qtaOrdinata.subtract(qtaRicevuta);

                    System.out.println(wsArticolo.wsArtCodice.trim() + " - " + desc.substring(0, Math.min(35, desc.length())));
                    System.out.println("  Da ricevere: " + formatQuantita(qtaDaRicevere));
                    wsMovimento.wsMovQuantita = readBigDecimal("  Quantità ricevuta: ");

                    if (wsMovimento.wsMovQuantita.compareTo(BigDecimal.ZERO) > 0) {
                        if (wsMovimento.wsMovQuantita.compareTo(qtaDaRicevere) > 0) {
                            System.out.println("  Quantità eccessiva! Impostata a " + formatQuantita(qtaDaRicevere));
                            wsMovimento.wsMovQuantita = qtaDaRicevere;
                        }
                        
                        // Esegui carico in una transazione
                        eseguiCaricoDaOrdine();
                    }
                    System.out.println();
                }
            }
        } catch (SQLException e) {
            handleSqlException(e, "riceviMerceOrdine.loop");
        }
        
        // Aggiorna stato finale dell'ordine
        aggiornaStatoOrdine();
    }

    private void eseguiCaricoDaOrdine() {
        wsMovimento.wsMovFornitore = wsOrdine.wsOrdFornitore;
        wsMovimento.wsMovCausale = "Ricevimento ordine " + wsOrdine.wsOrdNumero;
        wsMovimento.wsMovNumeroDoc = wsOrdine.wsOrdNumero;
        wsMovimento.wsMovValore = wsMovimento.wsMovQuantita.multiply(wsMovimento.wsMovPrezzo).setScale(2, RoundingMode.HALF_UP);

        try {
            beginTransaction();
            wsMovimento.wsMovTipo = "CA";
            registraMovimento();

            if ("OK".equals(wsEsito)) {
                caricaArticolo(); // Ricarica dati giacenza per aggiornamento
                aggiornaGiacenzaCarico();
            }
            
            if ("OK".equals(wsEsito)) {
                // Aggiorna riga ordine
                String sqlUpdateRiga = "UPDATE RIGHE_ORDINE SET quantita_ricevuta = quantita_ricevuta + ?, " +
                                       "stato_riga = CASE WHEN quantita_ricevuta + ? >= quantita_ordinata THEN 'E' ELSE 'P' END " +
                                       "WHERE numero_ordine = ? AND codice_articolo = ?";
                try(PreparedStatement ps = connection.prepareStatement(sqlUpdateRiga)) {
                    ps.setBigDecimal(1, wsMovimento.wsMovQuantita);
                    ps.setBigDecimal(2, wsMovimento.wsMovQuantita);
                    ps.setString(3, wsOrdine.wsOrdNumero);
                    ps.setString(4, wsArticolo.wsArtCodice.trim());
                    ps.executeUpdate();
                }

                // Aggiorna quantità ordinata in giacenze
                String sqlUpdateGiac = "UPDATE GIACENZE SET quantita_ordinata = GREATEST(0, quantita_ordinata - ?) " +
                                       "WHERE codice_articolo = ?";
                try(PreparedStatement ps = connection.prepareStatement(sqlUpdateGiac)) {
                    ps.setBigDecimal(1, wsMovimento.wsMovQuantita);
                    ps.setString(2, wsArticolo.wsArtCodice.trim());
                    ps.executeUpdate();
                }
            }

            if ("OK".equals(wsEsito)) {
                commitTransaction();
                System.out.println("  Carico registrato!");
            } else {
                rollbackTransaction();
            }
        } catch (SQLException e) {
            handleSqlException(e, "eseguiCaricoDaOrdine");
            rollbackTransaction();
        } finally {
            endTransaction();
        }
    }

    private void aggiornaStatoOrdine() {
        String sql = "UPDATE ORDINI SET stato_ordine = " +
                     "CASE WHEN NOT EXISTS (SELECT 1 FROM RIGHE_ORDINE WHERE numero_ordine = ? AND quantita_ricevuta < quantita_ordinata AND stato_riga <> 'C') THEN 'E' " +
                     "WHEN EXISTS (SELECT 1 FROM RIGHE_ORDINE WHERE numero_ordine = ? AND quantita_ricevuta > 0) THEN 'P' " +
                     "ELSE stato_ordine END " +
                     "WHERE numero_ordine = ?";
        try (PreparedStatement ps = connection.prepareStatement(sql)) {
            ps.setString(1, wsOrdine.wsOrdNumero);
            ps.setString(2, wsOrdine.wsOrdNumero);
            ps.setString(3, wsOrdine.wsOrdNumero);
            ps.executeUpdate();
        } catch (SQLException e) {
            handleSqlException(e, "aggiornaStatoOrdine");
        }
    }

    private void statoOrdine() {
        System.out.println("\n=== STATO ORDINE ===");
        wsOrdine.wsOrdNumero = readString("Numero ordine: ");

        String sqlOrd = "SELECT o.data_ordine, o.stato_ordine, f.ragione_sociale, o.totale_ordine " +
                        "FROM ORDINI o JOIN FORNITORI f ON o.codice_fornitore = f.codice_fornitore " +
                        "WHERE o.numero_ordine = ?";
        try (PreparedStatement ps = connection.prepareStatement(sqlOrd)) {
            ps.setString(1, wsOrdine.wsOrdNumero);
            try (ResultSet rs = ps.executeQuery()) {
                if (!rs.next()) {
                    System.out.println("Ordine non trovato!");
                    return;
                }
                System.out.println("\nOrdine: " + wsOrdine.wsOrdNumero);
                System.out.println("Data: " + rs.getDate("data_ordine"));
                System.out.println("Fornitore: " + rs.getString("ragione_sociale"));
                System.out.println("Totale: EUR " + formatImporto(rs.getBigDecimal("totale_ordine")));
                String stato = rs.getString("stato_ordine").trim();
                String statoDesc;
                switch(stato) {
                    case "A": statoDesc = "APERTO"; break;
                    case "C": statoDesc = "CONFERMATO"; break;
                    case "P": statoDesc = "PARZIALMENTE EVASO"; break;
                    case "E": statoDesc = "EVASO"; break;
                    default: statoDesc = "SCONOSCIUTO"; break;
                }
                System.out.println("Stato: " + statoDesc);
            }
        } catch (SQLException e) {
            handleSqlException(e, "statoOrdine.header");
            return;
        }

        System.out.println("\nDettaglio righe:\n");
        String sqlRighe = "SELECT r.codice_articolo, a.descrizione, r.quantita_ordinata, r.quantita_ricevuta, r.stato_riga " +
                          "FROM RIGHE_ORDINE r JOIN ARTICOLI a ON r.codice_articolo = a.codice_articolo " +
                          "WHERE r.numero_ordine = ? ORDER BY r.codice_articolo";
        try (PreparedStatement ps = connection.prepareStatement(sqlRighe)) {
            ps.setString(1, wsOrdine.wsOrdNumero);
            try (ResultSet rs = ps.executeQuery()) {
                while (rs.next()) {
                    String statoRiga = rs.getString("stato_riga").trim();
                    String statoRigaDesc;
                    switch(statoRiga) {
                        case "A": statoRigaDesc = "APERTA"; break;
                        case "P": statoRigaDesc = "PARZIALE"; break;
                        case "E": statoRigaDesc = "EVASA"; break;
                        case "C": statoRigaDesc = "CANCELLATA"; break;
                        default: statoRigaDesc = "SCONOSCIUTO"; break;
                    }
                    System.out.println(rs.getString("codice_articolo").trim() + " - " + rs.getString("descrizione"));
                    System.out.println("  Ordinata: " + formatQuantita(rs.getBigDecimal("quantita_ordinata")));
                    System.out.println("  Ricevuta: " + formatQuantita(rs.getBigDecimal("quantita_ricevuta")) + " (" + statoRigaDesc + ")");
                    System.out.println();
                }
            }
        } catch (SQLException e) {
            handleSqlException(e, "statoOrdine.righe");
        }
    }

    private void reportInventario() {
        System.out.println("\n=== REPORT INVENTARIO FISICO ===");
        String fileName = "INVENTARIO.TXT";
        try (PrintWriter writer = new PrintWriter(new FileWriter(fileName))) {
            writer.println("LISTA INVENTARIO FISICO");
            writer.println("=======================");
            writer.printf("Data: %s    Ora: %s%n%n",
                LocalDate.now().format(DateTimeFormatter.ISO_LOCAL_DATE),
                LocalDateTime.now().format(DateTimeFormatter.ofPattern("HH:mm:ss")));
            writer.println(String.format("%-11s %-30s %-5s %-12s %-12s %-12s",
                "CODICE", "DESCRIZIONE", "UB.", "QTA TEORICA", "QTA RILEVATA", "DIFFERENZA"));
            writer.println("-".repeat(85));

            String sql = "SELECT a.codice_articolo, a.descrizione, a.ubicazione, g.quantita_disponibile " +
                         "FROM ARTICOLI a JOIN GIACENZE g ON a.codice_articolo = g.codice_articolo " +
                         "WHERE a.stato = 'A' ORDER BY a.ubicazione, a.codice_articolo";
            try (Statement stmt = connection.createStatement(); ResultSet rs = stmt.executeQuery(sql)) {
                while (rs.next()) {
                    writer.println(String.format("%-11s %-30s %-5s %12s %-12s %-12s",
                        rs.getString("codice_articolo").trim(),
                        rs.getString("descrizione").substring(0, Math.min(30, rs.getString("descrizione").length())),
                        rs.getString("ubicazione") != null ? rs.getString("ubicazione").trim() : "",
                        formatQuantita(rs.getBigDecimal("quantita_disponibile")),
                        "____________",
                        "____________"));
                }
            } catch (SQLException e) {
                handleSqlException(e, "reportInventario");
            }

            writer.println("\n" + "-".repeat(85));
            writer.println("\nRilevato da: ________________  Data: ________  Firma: ________________");
            System.out.println("Report salvato in " + fileName);
        } catch (IOException e) {
            System.err.println("Errore durante la scrittura del file di inventario: " + e.getMessage());
        }
    }

    private void analisiAbc() {
        System.out.println("\n=== ANALISI ABC ARTICOLI ===\n");
        String tempTable = "ANALISI_ABC_" + System.currentTimeMillis(); // Unique temp table name
        try (Statement stmt = connection.createStatement()) {
            // Step 1: Create and populate temp table
            stmt.execute("CREATE TEMP TABLE " + tempTable + " AS " +
                         "SELECT a.codice_articolo, a.descrizione, " +
                         "(g.quantita_disponibile * g.valore_medio) as valore_tot " +
                         "FROM ARTICOLI a JOIN GIACENZE g ON a.codice_articolo = g.codice_articolo " +
                         "WHERE a.stato = 'A' AND g.quantita_disponibile > 0 AND g.valore_medio > 0");
            
            stmt.execute("ALTER TABLE " + tempTable + " ADD COLUMN percentuale DECIMAL(10,4), " +
                         "ADD COLUMN perc_cumulata DECIMAL(10,4), ADD COLUMN classe CHAR(1)");

            // Step 2: Calculate total value
            BigDecimal valoreTotaleMagazzino = BigDecimal.ZERO;
            try (ResultSet rs = stmt.executeQuery("SELECT SUM(valore_tot) FROM " + tempTable)) {
                if (rs.next()) {
                    valoreTotaleMagazzino = rs.getBigDecimal(1);
                }
            }
            if (valoreTotaleMagazzino.compareTo(BigDecimal.ZERO) == 0) {
                System.out.println("Nessun articolo valorizzabile trovato.");
                return;
            }

            // Step 3: Update percentages
            stmt.execute("UPDATE " + tempTable + " SET percentuale = (valore_tot / " + valoreTotaleMagazzino + ") * 100");

            // Step 4: Update cumulative percentage using window function (more efficient)
            stmt.execute("UPDATE " + tempTable + " t1 SET perc_cumulata = t2.perc_cumulata FROM " +
                         "(SELECT codice_articolo, SUM(percentuale) OVER (ORDER BY valore_tot DESC) as perc_cumulata FROM " + tempTable + ") t2 " +
                         "WHERE t1.codice_articolo = t2.codice_articolo");

            // Step 5: Assign classes
            stmt.execute("UPDATE " + tempTable + " SET classe = CASE " +
                         "WHEN perc_cumulata <= 80 THEN 'A' " +
                         "WHEN perc_cumulata <= 95 THEN 'B' " +
                         "ELSE 'C' END");

            // Step 6: Display results
            displayClasseAbc("A", "CLASSE A - Alto valore (80% del valore)", stmt);
            displayClasseAbcSummary("B", "CLASSE B - Medio valore (15% del valore)", stmt);
            displayClasseAbcSummary("C", "CLASSE C - Basso valore (5% del valore)", stmt);

        } catch (SQLException e) {
            handleSqlException(e, "analisiAbc");
        } finally {
            try (Statement stmt = connection.createStatement()) {
                stmt.execute("DROP TABLE IF EXISTS " + tempTable);
            } catch (SQLException e) {
                // Ignore, table might not exist if creation failed
            }
        }
    }

    private void displayClasseAbc(String classe, String titolo, Statement stmt) throws SQLException {
        System.out.println(titolo);
        System.out.println("-".repeat(titolo.length()));
        String sql = "SELECT codice_articolo, descrizione, valore_tot, percentuale FROM ANALISI_ABC_" + tempTable.substring(12) +
                     " WHERE classe = '" + classe + "' ORDER BY valore_tot DESC";
        int count = 0;
        try (ResultSet rs = stmt.executeQuery(sql)) {
            while (rs.next()) {
                count++;
                System.out.println(rs.getString("codice_articolo").trim() + " - " + rs.getString("descrizione"));
                System.out.printf("  Valore: EUR %-15s (%s%%)%n",
                    formatImporto(rs.getBigDecimal("valore_tot")),
                    formatPercentuale(rs.getBigDecimal("percentuale")));
            }
        }
        System.out.println("\nArticoli classe " + classe + ": " + formatNumero(count) + "\n");
    }

    private void displayClasseAbcSummary(String classe, String titolo, Statement stmt) throws SQLException {
        System.out.println(titolo);
        System.out.println("-".repeat(titolo.length()));
        String sql = "SELECT COUNT(*), SUM(valore_tot) FROM ANALISI_ABC_" + tempTable.substring(12) +
                     " WHERE classe = '" + classe + "'";
        try (ResultSet rs = stmt.executeQuery(sql)) {
            if (rs.next()) {
                System.out.printf("Articoli: %-10s Valore totale: EUR %s%n%n",
                    formatNumero(rs.getInt(1)),
                    formatImporto(rs.getBigDecimal(2)));
            }
        }
    }

    // =================================================================
    // METODI HELPER
    // =================================================================

    private String readString(String prompt) {
        System.out.print(prompt);
        return scanner.nextLine();
    }

    private int readInt(String prompt) {
        while (true) {
            System.out.print(prompt);
            String input = scanner.nextLine();
            try {
                return Integer.parseInt(input);
            } catch (NumberFormatException e) {
                System.out.println("Formato numero intero non valido. Riprova.");
            }
        }
    }

    private BigDecimal readBigDecimal(String prompt) {
        while (true) {
            System.out.print(prompt);
            String input = scanner.nextLine().trim().replace(',', '.');
            if (input.isEmpty()) {
                return BigDecimal.ZERO;
            }
            try {
                return new BigDecimal(input);
            } catch (NumberFormatException e) {
                System.out.println("Formato numero non valido. Riprova.");
            }
        }
    }

    private String formatImporto(BigDecimal value) {
        return value != null ? FORMATO_IMPORTO.format(value) : "0,00";
    }

    private String formatQuantita(BigDecimal value) {
        return value != null ? FORMATO_QUANTITA.format(value) : "0,00";
    }

    private String formatNumero(int value) {
        return FORMATO_NUMERO.format(value);
    }
    
    private String formatPercentuale(BigDecimal value) {
        return value != null ? FORMATO_PERCENTUALE.format(value) : "0,00";
    }
}