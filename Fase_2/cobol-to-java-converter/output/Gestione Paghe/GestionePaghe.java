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
import java.util.Scanner;

/**
 * <h1>Sistema di Gestione Paghe e Stipendi</h1>
 * <p>
 * Questo programma è una traduzione Java del programma COBOL GESTIONE-PAGHE.
 * Gestisce l'elaborazione degli stipendi mensili, il calcolo delle trattenute,
 * la generazione di cedolini e report, e l'inserimento delle presenze dei dipendenti.
 * </p>
 * <p>
 * Il programma si connette a un database PostgreSQL per leggere e scrivere i dati.
 * Utilizza JDBC per tutte le operazioni SQL, con una gestione attenta delle transazioni.
 * </p>
 *
 * @author Annalisa Egidi (Original COBOL Author)
 * @author Advanced COBOL to Java Compiler (Translator)
 * @version 1.0
 * @since 2025-05-20
 */
public class GestionePaghe {

    // --- Configurazione Database ---
    private static final String DB_URL = "jdbc:postgresql://localhost:5432/paghe";
    private static final String DB_USER = "postgres";
    private static final String DB_PASSWORD = "password";

    // --- Connessione e I/O ---
    private Connection connection;
    private final Scanner scanner = new Scanner(System.in);

    // --- Variabili di stato (dalla WORKING-STORAGE) ---
    private int wsScelta = 0;
    private String wsContinua = "S";
    private String wsEsito = ""; // "OK" o "KO"

    // --- Parametri di elaborazione ---
    private int wsAnno;
    private int wsMese;
    private String wsMatricola;

    // --- Contatori e totali ---
    private int wsContaElaborati = 0;
    private BigDecimal wsTotaleLordo = BigDecimal.ZERO;
    private BigDecimal wsTotaleNetto = BigDecimal.ZERO;

    // --- Strutture dati (modellate come classi interne statiche) ---
    private Dipendente wsDipedente = new Dipendente();
    private Stipendio wsStipendio = new Stipendio();
    private Presenze wsPresenze = new Presenze();
    private Calcoli wsCalcoli = new Calcoli();

    // --- Formattatori per l'output ---
    private final DecimalFormat currencyFormat = new DecimalFormat("€ #,##0.00");
    private final DecimalFormat numberFormat = new DecimalFormat("#,##0");
    private final DecimalFormat decimalFormat = new DecimalFormat("#,##0.00");

    /**
     * Classe interna per rappresentare i dati di un dipendente.
     * Corrisponde a WS-DIPENDENTE in COBOL.
     */
    private static class Dipendente {
        String matricola;
        String nome;
        String cognome;
        String cf;
        String qualifica;
        String livello;
        String reparto;
        BigDecimal stipendioBase;
        String stato;
    }

    /**
     * Classe interna per rappresentare i dati di uno stipendio calcolato.
     * Corrisponde a WS-STIPENDIO in COBOL.
     */
    private static class Stipendio {
        long id;
        int giorni;
        BigDecimal oreOrd = BigDecimal.ZERO;
        BigDecimal oreStr = BigDecimal.ZERO;
        BigDecimal base = BigDecimal.ZERO;
        BigDecimal straordinari = BigDecimal.ZERO;
        BigDecimal altreCompetenze = BigDecimal.ZERO;
        BigDecimal lordo = BigDecimal.ZERO;
        BigDecimal trattenute = BigDecimal.ZERO;
        BigDecimal netto = BigDecimal.ZERO;
    }

    /**
     * Classe interna per rappresentare i dati delle presenze mensili.
     * Corrisponde a WS-PRESENZE in COBOL.
     */
    private static class Presenze {
        int giorniLav = 0;
        int giorniFer = 0;
        int giorniMal = 0;
        int giorniPer = 0;
        BigDecimal oreOrd = BigDecimal.ZERO;
        BigDecimal oreStr = BigDecimal.ZERO;
    }

    /**
     * Classe interna per i calcoli intermedi delle trattenute.
     * Corrisponde a WS-CALCOLI in COBOL.
     */
    private static class Calcoli {
        BigDecimal imponibile = BigDecimal.ZERO;
        BigDecimal irpef = BigDecimal.ZERO;
        BigDecimal inps = BigDecimal.ZERO;
        BigDecimal addReg = BigDecimal.ZERO;
        BigDecimal addCom = BigDecimal.ZERO;
        BigDecimal detrazioni = BigDecimal.ZERO;
    }

    /**
     * Metodo principale che avvia l'applicazione.
     *
     * @param args Argomenti della riga di comando (non utilizzati).
     */
    public static void main(String[] args) {
        GestionePaghe app = new GestionePaghe();
        app.mainLogic();
    }

    /**
     * Logica principale del programma, che gestisce il menu e il ciclo di vita dell'applicazione.
     * Corrisponde al paragrafo MAIN-LOGIC in COBOL.
     */
    private void mainLogic() {
        if (!connectDatabase()) {
            return; // Esce se la connessione fallisce
        }

        do {
            visualizzaMenu();
            try {
                wsScelta = Integer.parseInt(scanner.nextLine());
                elaboraScelta();
            } catch (NumberFormatException e) {
                System.out.println("Input non valido. Inserire un numero.");
                wsScelta = -1; // Valore non valido per evitare l'uscita
            }

            if (wsScelta != 0) {
                System.out.println();
                System.out.print("Continuare? (S/N): ");
                wsContinua = scanner.nextLine();
            } else {
                wsContinua = "N";
            }

        } while (wsContinua.equalsIgnoreCase("S"));

        disconnectDatabase();
        System.out.println("Programma terminato.");
    }

    /**
     * Stabilisce la connessione al database.
     * Corrisponde al paragrafo CONNETTI-DATABASE.
     *
     * @return true se la connessione ha successo, false altrimenti.
     */
    private boolean connectDatabase() {
        try {
            connection = DriverManager.getConnection(DB_URL, DB_USER, DB_PASSWORD);
            System.out.println("Connessione al database stabilita.");
            return true;
        } catch (SQLException e) {
            System.err.println("Errore connessione database: " + e.getMessage());
            return false;
        }
    }

    /**
     * Chiude la connessione al database.
     * Corrisponde al paragrafo DISCONNETTI-DATABASE.
     */
    private void disconnectDatabase() {
        try {
            if (connection != null && !connection.isClosed()) {
                connection.close();
                System.out.println("Disconnesso dal database.");
            }
        } catch (SQLException e) {
            System.err.println("Errore durante la disconnessione: " + e.getMessage());
        }
    }

    /**
     * Mostra il menu principale all'utente.
     * Corrisponde al paragrafo VISUALIZZA-MENU.
     */
    private void visualizzaMenu() {
        System.out.println();
        System.out.println("===== SISTEMA GESTIONE PAGHE E STIPENDI =====");
        System.out.println("1. Elaborazione stipendi mensili");
        System.out.println("2. Calcolo singolo stipendio");
        System.out.println("3. Stampa cedolino");
        System.out.println("4. Report mensile stipendi");
        System.out.println("5. Inserimento presenze");
        System.out.println("6. Visualizza presenze dipendente");
        System.out.println("0. Esci");
        System.out.println("=============================================");
        System.out.print("Scelta: ");
    }

    /**
     * Esegue l'azione scelta dall'utente nel menu.
     * Corrisponde al paragrafo ELABORA-SCELTA.
     */
    private void elaboraScelta() {
        switch (wsScelta) {
            case 1: elaborazioneMensile(); break;
            case 2: calcoloSingolo(); break;
            case 3: stampaCedolino(); break;
            case 4: reportMensile(); break;
            case 5: inserimentoPresenze(); break;
            case 6: visualizzaPresenze(); break;
            case 0: break; // L'uscita è gestita nel ciclo principale
            default: System.out.println("Scelta non valida!"); break;
        }
    }

    /**
     * Esegue l'elaborazione massiva degli stipendi per un dato mese/anno.
     * Corrisponde al paragrafo ELABORAZIONE-MENSILE.
     */
    private void elaborazioneMensile() {
        System.out.println("\n=== ELABORAZIONE STIPENDI MENSILI ===");
        System.out.print("Anno (AAAA): ");
        wsAnno = Integer.parseInt(scanner.nextLine());
        System.out.print("Mese (MM): ");
        wsMese = Integer.parseInt(scanner.nextLine());

        if (wsMese < 1 || wsMese > 12) {
            System.out.println("Mese non valido!");
            return;
        }

        String checkSql = "SELECT COUNT(*) FROM STIPENDI WHERE anno = ? AND mese = ? AND stato_pagamento <> 'A'";
        try (PreparedStatement stmt = connection.prepareStatement(checkSql)) {
            stmt.setInt(1, wsAnno);
            stmt.setInt(2, wsMese);
            ResultSet rs = stmt.executeQuery();
            if (rs.next()) {
                int count = rs.getInt(1);
                if (count > 0) {
                    System.out.printf("Stipendi già elaborati per %02d/%d%n", wsMese, wsAnno);
                    System.out.print("Vuoi rielaborare? (S/N): ");
                    String risposta = scanner.nextLine();
                    if (!risposta.equalsIgnoreCase("S")) {
                        return;
                    }
                }
            }
        } catch (SQLException e) {
            handleSqlException(e, false);
            return;
        }

        wsContaElaborati = 0;
        wsTotaleLordo = BigDecimal.ZERO;
        wsTotaleNetto = BigDecimal.ZERO;

        String cursorSql = "SELECT matricola, nome, cognome, codice_fiscale, qualifica, livello, reparto, stipendio_base FROM DIPENDENTI WHERE stato = 'A' ORDER BY matricola";
        try (PreparedStatement stmt = connection.prepareStatement(cursorSql);
             ResultSet rs = stmt.executeQuery()) {

            while (rs.next()) {
                wsDipedente = new Dipendente();
                wsDipedente.matricola = rs.getString("matricola");
                wsDipedente.nome = rs.getString("nome");
                wsDipedente.cognome = rs.getString("cognome");
                wsDipedente.cf = rs.getString("codice_fiscale");
                wsDipedente.qualifica = rs.getString("qualifica");
                wsDipedente.livello = rs.getString("livello");
                wsDipedente.reparto = rs.getString("reparto");
                wsDipedente.stipendioBase = rs.getBigDecimal("stipendio_base");

                wsMatricola = wsDipedente.matricola;
                calcolaStipendio();

                if ("OK".equals(wsEsito)) {
                    wsContaElaborati++;
                    wsTotaleLordo = wsTotaleLordo.add(wsStipendio.lordo);
                    wsTotaleNetto = wsTotaleNetto.add(wsStipendio.netto);
                }
            }
        } catch (SQLException e) {
            handleSqlException(e, false);
        }

        System.out.println("\nElaborazione completata!");
        System.out.println("Dipendenti elaborati: " + wsContaElaborati);
        System.out.println("Totale lordo: " + currencyFormat.format(wsTotaleLordo));
        System.out.println("Totale netto: " + currencyFormat.format(wsTotaleNetto));
    }

    /**
     * Calcola lo stipendio per un singolo dipendente.
     * Corrisponde al paragrafo CALCOLO-SINGOLO.
     */
    private void calcoloSingolo() {
        System.out.println("\n=== CALCOLO SINGOLO STIPENDIO ===");
        System.out.print("Matricola dipendente: ");
        wsMatricola = scanner.nextLine();
        System.out.print("Anno (AAAA): ");
        wsAnno = Integer.parseInt(scanner.nextLine());
        System.out.print("Mese (MM): ");
        wsMese = Integer.parseInt(scanner.nextLine());

        if (!caricaDipendente()) {
            return;
        }

        calcolaStipendio();

        if ("OK".equals(wsEsito)) {
            System.out.printf("\nStipendio calcolato per %s %s%n", wsDipedente.nome.trim(), wsDipedente.cognome.trim());
            visualizzaDettaglioStipendio();
        }
    }

    /**
     * Carica i dati anagrafici di un dipendente dal database.
     * Corrisponde al paragrafo CARICA-DIPENDENTE.
     *
     * @return true se il dipendente è stato caricato con successo, false altrimenti.
     */
    private boolean caricaDipendente() {
        String sql = "SELECT matricola, nome, cognome, codice_fiscale, qualifica, livello, reparto, stipendio_base, stato FROM DIPENDENTI WHERE matricola = ?";
        try (PreparedStatement stmt = connection.prepareStatement(sql)) {
            stmt.setString(1, wsMatricola);
            ResultSet rs = stmt.executeQuery();

            if (rs.next()) {
                wsDipedente = new Dipendente();
                wsDipedente.matricola = rs.getString("matricola");
                wsDipedente.nome = rs.getString("nome");
                wsDipedente.cognome = rs.getString("cognome");
                wsDipedente.cf = rs.getString("codice_fiscale");
                wsDipedente.qualifica = rs.getString("qualifica");
                wsDipedente.livello = rs.getString("livello");
                wsDipedente.reparto = rs.getString("reparto");
                wsDipedente.stipendioBase = rs.getBigDecimal("stipendio_base");
                wsDipedente.stato = rs.getString("stato");

                if (!"A".equals(wsDipedente.stato)) {
                    System.out.println("Dipendente non attivo!");
                    return false;
                }
                return true;
            } else {
                System.out.println("Dipendente non trovato!");
                return false;
            }
        } catch (SQLException e) {
            handleSqlException(e, false);
            return false;
        }
    }

    /**
     * Esegue il calcolo completo dello stipendio (competenze, trattenute, netto).
     * Corrisponde al paragrafo CALCOLA-STIPENDIO.
     */
    private void calcolaStipendio() {
        wsEsito = "OK";
        wsPresenze = new Presenze();
        wsStipendio = new Stipendio();
        wsCalcoli = new Calcoli();

        caricaPresenze();

        // Stipendio base
        wsStipendio.base = wsDipedente.stipendioBase;

        // Straordinari (25% sulla paga oraria, basata su 168 ore mensili)
        if (wsPresenze.oreStr.compareTo(BigDecimal.ZERO) > 0) {
            BigDecimal pagaOraria = wsDipedente.stipendioBase.divide(new BigDecimal("168"), 4, RoundingMode.HALF_UP);
            BigDecimal maggiorazione = new BigDecimal("1.25");
            wsStipendio.straordinari = pagaOraria.multiply(wsPresenze.oreStr).multiply(maggiorazione);
        }

        // Totale lordo
        wsStipendio.lordo = wsStipendio.base.add(wsStipendio.straordinari).add(wsStipendio.altreCompetenze);

        // Trattenute
        calcolaTrattenute();

        // Netto
        wsStipendio.netto = wsStipendio.lordo.subtract(wsStipendio.trattenute);

        // Salva su DB
        salvaStipendio();
    }

    /**
     * Carica e aggrega le presenze del mese per il dipendente corrente.
     * Corrisponde al paragrafo CARICA-PRESENZE.
     */
    private void caricaPresenze() {
        String sql = "SELECT tipo_giornata, SUM(ore_ordinarie), SUM(ore_straordinarie) " +
                     "FROM PRESENZE " +
                     "WHERE matricola = ? AND EXTRACT(YEAR FROM data_presenza) = ? AND EXTRACT(MONTH FROM data_presenza) = ? " +
                     "GROUP BY tipo_giornata";
        
        try (PreparedStatement stmt = connection.prepareStatement(sql)) {
            stmt.setString(1, wsMatricola);
            stmt.setInt(2, wsAnno);
            stmt.setInt(3, wsMese);
            ResultSet rs = stmt.executeQuery();

            while (rs.next()) {
                String tipoGiornata = rs.getString(1);
                BigDecimal oreOrd = rs.getBigDecimal(2);
                BigDecimal oreStr = rs.getBigDecimal(3);

                wsPresenze.oreOrd = wsPresenze.oreOrd.add(oreOrd);
                wsPresenze.oreStr = wsPresenze.oreStr.add(oreStr);

                switch (tipoGiornata) {
                    case "L": wsPresenze.giorniLav++; break;
                    case "F": wsPresenze.giorniFer++; break;
                    case "M": wsPresenze.giorniMal++; break;
                    case "P": wsPresenze.giorniPer++; break;
                }
            }
        } catch (SQLException e) {
            handleSqlException(e, false);
        }

        wsStipendio.giorni = wsPresenze.giorniLav;
        wsStipendio.oreOrd = wsPresenze.oreOrd;
        wsStipendio.oreStr = wsPresenze.oreStr;
    }

    /**
     * Calcola l'importo totale delle trattenute fiscali e previdenziali.
     * Corrisponde al paragrafo CALCOLA-TRATTENUTE.
     */
    private void calcolaTrattenute() {
        wsCalcoli.imponibile = wsStipendio.lordo;

        // INPS (9.19%)
        wsCalcoli.inps = wsCalcoli.imponibile.multiply(new BigDecimal("0.0919")).setScale(2, RoundingMode.HALF_UP);

        // IRPEF
        calcolaIrpef();

        // Addizionali (calcolate sul lordo mensile e divise per 12)
        wsCalcoli.addReg = wsCalcoli.imponibile.multiply(new BigDecimal("0.0173")).divide(BigDecimal.valueOf(12), 2, RoundingMode.HALF_UP);
        wsCalcoli.addCom = wsCalcoli.imponibile.multiply(new BigDecimal("0.0080")).divide(BigDecimal.valueOf(12), 2, RoundingMode.HALF_UP);

        // Detrazioni
        calcolaDetrazioni();

        // Totale trattenute
        wsStipendio.trattenute = wsCalcoli.inps.add(wsCalcoli.irpef).add(wsCalcoli.addReg).add(wsCalcoli.addCom).subtract(wsCalcoli.detrazioni);

        if (wsStipendio.trattenute.compareTo(BigDecimal.ZERO) < 0) {
            wsStipendio.trattenute = BigDecimal.ZERO;
        }
        wsStipendio.trattenute = wsStipendio.trattenute.setScale(2, RoundingMode.HALF_UP);
    }

    /**
     * Calcola l'IRPEF mensile basata su scaglioni di reddito annuali.
     * Corrisponde al paragrafo CALCOLA-IRPEF.
     */
    private void calcolaIrpef() {
        BigDecimal imponibileAnnuo = wsStipendio.lordo.multiply(BigDecimal.valueOf(12));
        BigDecimal irpefAnnua = BigDecimal.ZERO;

        if (imponibileAnnuo.compareTo(BigDecimal.ZERO) > 0) {
            if (imponibileAnnuo.compareTo(new BigDecimal("15000")) <= 0) {
                irpefAnnua = imponibileAnnuo.multiply(new BigDecimal("0.23"));
            } else {
                irpefAnnua = new BigDecimal("15000").multiply(new BigDecimal("0.23"));
                if (imponibileAnnuo.compareTo(new BigDecimal("28000")) <= 0) {
                    irpefAnnua = irpefAnnua.add(imponibileAnnuo.subtract(new BigDecimal("15000")).multiply(new BigDecimal("0.25")));
                } else {
                    irpefAnnua = irpefAnnua.add(new BigDecimal("13000").multiply(new BigDecimal("0.25"))); // 28000 - 15000
                    if (imponibileAnnuo.compareTo(new BigDecimal("50000")) <= 0) {
                        irpefAnnua = irpefAnnua.add(imponibileAnnuo.subtract(new BigDecimal("28000")).multiply(new BigDecimal("0.35")));
                    } else {
                        irpefAnnua = irpefAnnua.add(new BigDecimal("22000").multiply(new BigDecimal("0.35"))); // 50000 - 28000
                        irpefAnnua = irpefAnnua.add(imponibileAnnuo.subtract(new BigDecimal("50000")).multiply(new BigDecimal("0.43")));
                    }
                }
            }
        }
        wsCalcoli.irpef = irpefAnnua.divide(BigDecimal.valueOf(12), 2, RoundingMode.HALF_UP);
    }

    /**
     * Calcola le detrazioni mensili per lavoro dipendente.
     * Corrisponde al paragrafo CALCOLA-DETRAZIONI.
     */
    private void calcolaDetrazioni() {
        BigDecimal imponibileAnnuo = wsStipendio.lordo.multiply(BigDecimal.valueOf(12));
        BigDecimal detrazioniAnnue = BigDecimal.ZERO;

        if (imponibileAnnuo.compareTo(new BigDecimal("8000")) <= 0) {
            detrazioniAnnue = new BigDecimal("1880");
        } else if (imponibileAnnuo.compareTo(new BigDecimal("28000")) <= 0) {
            detrazioniAnnue = new BigDecimal("1910");
        } else if (imponibileAnnuo.compareTo(new BigDecimal("55000")) <= 0) {
            detrazioniAnnue = new BigDecimal("1370");
        }

        if (detrazioniAnnue.compareTo(BigDecimal.ZERO) > 0) {
            wsCalcoli.detrazioni = detrazioniAnnue.divide(BigDecimal.valueOf(12), 2, RoundingMode.HALF_UP);
        }
    }

    /**
     * Salva o aggiorna i dati dello stipendio calcolato nel database.
     * Gestisce la transazione per salvare anche il dettaglio delle trattenute.
     * Corrisponde al paragrafo SALVA-STIPENDIO.
     */
    private void salvaStipendio() {
        String checkSql = "SELECT id_stipendio, stato_pagamento FROM STIPENDI WHERE matricola = ? AND anno = ? AND mese = ?";
        long existingId = -1;
        String statoPagamento = "";

        try (PreparedStatement stmt = connection.prepareStatement(checkSql)) {
            stmt.setString(1, wsMatricola);
            stmt.setInt(2, wsAnno);
            stmt.setInt(3, wsMese);
            ResultSet rs = stmt.executeQuery();
            if (rs.next()) {
                existingId = rs.getLong("id_stipendio");
                statoPagamento = rs.getString("stato_pagamento");
            }
        } catch (SQLException e) {
            handleSqlException(e, false);
            wsEsito = "KO";
            return;
        }

        try {
            connection.setAutoCommit(false);

            if (existingId != -1) { // Stipendio esistente, si aggiorna
                if ("P".equals(statoPagamento)) {
                    System.out.println("Stipendio già pagato, non modificabile!");
                    wsEsito = "KO";
                    connection.rollback();
                    return;
                }
                wsStipendio.id = existingId;
                String updateSql = "UPDATE STIPENDI SET giorni_lavorati = ?, ore_ordinarie = ?, ore_straordinarie = ?, " +
                                   "stipendio_base = ?, importo_straord = ?, altre_competenze = ?, totale_lordo = ?, " +
                                   "totale_trattenute = ?, netto_pagare = ?, data_elaborazione = ? WHERE id_stipendio = ?";
                try (PreparedStatement stmt = connection.prepareStatement(updateSql)) {
                    stmt.setInt(1, wsStipendio.giorni);
                    stmt.setBigDecimal(2, wsStipendio.oreOrd);
                    stmt.setBigDecimal(3, wsStipendio.oreStr);
                    stmt.setBigDecimal(4, wsStipendio.base);
                    stmt.setBigDecimal(5, wsStipendio.straordinari);
                    stmt.setBigDecimal(6, wsStipendio.altreCompetenze);
                    stmt.setBigDecimal(7, wsStipendio.lordo);
                    stmt.setBigDecimal(8, wsStipendio.trattenute);
                    stmt.setBigDecimal(9, wsStipendio.netto);
                    stmt.setTimestamp(10, Timestamp.valueOf(java.time.LocalDateTime.now()));
                    stmt.setLong(11, wsStipendio.id);
                    stmt.executeUpdate();
                }
            } else { // Stipendio nuovo, si inserisce
                String insertSql = "INSERT INTO STIPENDI (matricola, anno, mese, giorni_lavorati, ore_ordinarie, ore_straordinarie, " +
                                   "stipendio_base, importo_straord, altre_competenze, totale_lordo, totale_trattenute, netto_pagare) " +
                                   "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)";
                try (PreparedStatement stmt = connection.prepareStatement(insertSql, Statement.RETURN_GENERATED_KEYS)) {
                    stmt.setString(1, wsMatricola);
                    stmt.setInt(2, wsAnno);
                    stmt.setInt(3, wsMese);
                    stmt.setInt(4, wsStipendio.giorni);
                    stmt.setBigDecimal(5, wsStipendio.oreOrd);
                    stmt.setBigDecimal(6, wsStipendio.oreStr);
                    stmt.setBigDecimal(7, wsStipendio.base);
                    stmt.setBigDecimal(8, wsStipendio.straordinari);
                    stmt.setBigDecimal(9, wsStipendio.altreCompetenze);
                    stmt.setBigDecimal(10, wsStipendio.lordo);
                    stmt.setBigDecimal(11, wsStipendio.trattenute);
                    stmt.setBigDecimal(12, wsStipendio.netto);
                    stmt.executeUpdate();

                    try (ResultSet generatedKeys = stmt.getGeneratedKeys()) {
                        if (generatedKeys.next()) {
                            wsStipendio.id = generatedKeys.getLong(1);
                        } else {
                            throw new SQLException("Creazione stipendio fallita, nessun ID ottenuto.");
                        }
                    }
                }
            }

            salvaDettaglioTrattenute(); // Eseguito nella stessa transazione
            connection.commit();
            wsEsito = "OK";

        } catch (SQLException e) {
            handleSqlException(e, true); // Esegue il rollback
            wsEsito = "KO";
        } finally {
            try {
                connection.setAutoCommit(true);
            } catch (SQLException e) {
                System.err.println("Errore nel ripristinare autocommit: " + e.getMessage());
            }
        }
    }

    /**
     * Salva il dettaglio delle singole voci di trattenuta.
     * Viene eseguito all'interno della transazione di `salvaStipendio`.
     * Corrisponde al paragrafo SALVA-DETTAGLIO-TRATTENUTE.
     *
     * @throws SQLException se si verifica un errore SQL.
     */
    private void salvaDettaglioTrattenute() throws SQLException {
        // Elimina dettagli esistenti
        String deleteSql = "DELETE FROM TRATTENUTE WHERE id_stipendio = ?";
        try (PreparedStatement stmt = connection.prepareStatement(deleteSql)) {
            stmt.setLong(1, wsStipendio.id);
            stmt.executeUpdate();
        }

        // Inserisce i nuovi dettagli
        String insertSql = "INSERT INTO TRATTENUTE (id_stipendio, tipo_trattenuta, descrizione, importo) VALUES (?, ?, ?, ?)";
        try (PreparedStatement stmt = connection.prepareStatement(insertSql)) {
            // INPS
            stmt.setLong(1, wsStipendio.id);
            stmt.setString(2, "INPS");
            stmt.setString(3, "Contributi previdenziali");
            stmt.setBigDecimal(4, wsCalcoli.inps);
            stmt.addBatch();

            // IRPEF
            stmt.setLong(1, wsStipendio.id);
            stmt.setString(2, "IRPEF");
            stmt.setString(3, "Imposta sul reddito");
            stmt.setBigDecimal(4, wsCalcoli.irpef);
            stmt.addBatch();

            // Addizionale Regionale
            stmt.setLong(1, wsStipendio.id);
            stmt.setString(2, "ADD_REG");
            stmt.setString(3, "Addizionale regionale");
            stmt.setBigDecimal(4, wsCalcoli.addReg);
            stmt.addBatch();

            // Addizionale Comunale
            stmt.setLong(1, wsStipendio.id);
            stmt.setString(2, "ADD_COM");
            stmt.setString(3, "Addizionale comunale");
            stmt.setBigDecimal(4, wsCalcoli.addCom);
            stmt.addBatch();

            // Detrazioni (inserite come importo negativo)
            if (wsCalcoli.detrazioni.compareTo(BigDecimal.ZERO) > 0) {
                stmt.setLong(1, wsStipendio.id);
                stmt.setString(2, "DETRAZ");
                stmt.setString(3, "Detrazioni lavoro dipendente");
                stmt.setBigDecimal(4, wsCalcoli.detrazioni.negate());
                stmt.addBatch();
            }
            stmt.executeBatch();
        }
    }

    /**
     * Mostra a schermo il dettaglio dello stipendio calcolato.
     * Corrisponde al paragrafo VISUALIZZA-DETTAGLIO-STIPENDIO.
     */
    private void visualizzaDettaglioStipendio() {
        System.out.println("\n=== DETTAGLIO STIPENDIO ===");
        System.out.printf("Periodo: %02d/%d%n", wsMese, wsAnno);
        System.out.println("Giorni lavorati: " + numberFormat.format(wsStipendio.giorni));
        System.out.println("Ore straordinario: " + decimalFormat.format(wsStipendio.oreStr));
        System.out.println("\nCOMPETENZE:");
        System.out.printf("  Stipendio base:     %15s%n", currencyFormat.format(wsStipendio.base));
        System.out.printf("  Straordinari:       %15s%n", currencyFormat.format(wsStipendio.straordinari));
        System.out.printf("  TOTALE LORDO:       %15s%n", currencyFormat.format(wsStipendio.lordo));
        System.out.println("\nTRATTENUTE:");
        System.out.printf("  INPS:               %15s%n", currencyFormat.format(wsCalcoli.inps));
        System.out.printf("  IRPEF:              %15s%n", currencyFormat.format(wsCalcoli.irpef));
        System.out.printf("  Add. Regionale:     %15s%n", currencyFormat.format(wsCalcoli.addReg));
        System.out.printf("  Add. Comunale:      %15s%n", currencyFormat.format(wsCalcoli.addCom));
        if (wsCalcoli.detrazioni.compareTo(BigDecimal.ZERO) > 0) {
            System.out.printf("  Detrazioni:        -%15s%n", currencyFormat.format(wsCalcoli.detrazioni));
        }
        System.out.printf("  TOTALE TRATTENUTE:  %15s%n", currencyFormat.format(wsStipendio.trattenute));
        System.out.println();
        System.out.printf("  NETTO A PAGARE:     %15s%n", currencyFormat.format(wsStipendio.netto));
    }

    /**
     * Avvia il processo di stampa di un cedolino su file.
     * Corrisponde al paragrafo STAMPA-CEDOLINO.
     */
    private void stampaCedolino() {
        System.out.println("\n=== STAMPA CEDOLINO ===");
        System.out.print("Matricola: ");
        wsMatricola = scanner.nextLine();
        System.out.print("Anno: ");
        wsAnno = Integer.parseInt(scanner.nextLine());
        System.out.print("Mese: ");
        wsMese = Integer.parseInt(scanner.nextLine());

        String sql = "SELECT s.id_stipendio, s.giorni_lavorati, s.ore_ordinarie, s.ore_straordinarie, " +
                     "s.stipendio_base, s.importo_straord, s.altre_competenze, s.totale_lordo, " +
                     "s.totale_trattenute, s.netto_pagare, d.nome, d.cognome, d.codice_fiscale, " +
                     "d.qualifica, d.livello " +
                     "FROM STIPENDI s JOIN DIPENDENTI d ON s.matricola = d.matricola " +
                     "WHERE s.matricola = ? AND s.anno = ? AND s.mese = ?";

        try (PreparedStatement stmt = connection.prepareStatement(sql)) {
            stmt.setString(1, wsMatricola);
            stmt.setInt(2, wsAnno);
            stmt.setInt(3, wsMese);
            ResultSet rs = stmt.executeQuery();

            if (rs.next()) {
                wsStipendio = new Stipendio();
                wsDipedente = new Dipendente();
                wsStipendio.id = rs.getLong("id_stipendio");
                wsStipendio.giorni = rs.getInt("giorni_lavorati");
                wsStipendio.oreOrd = rs.getBigDecimal("ore_ordinarie");
                wsStipendio.oreStr = rs.getBigDecimal("ore_straordinarie");
                wsStipendio.base = rs.getBigDecimal("stipendio_base");
                wsStipendio.straordinari = rs.getBigDecimal("importo_straord");
                wsStipendio.altreCompetenze = rs.getBigDecimal("altre_competenze");
                wsStipendio.lordo = rs.getBigDecimal("totale_lordo");
                wsStipendio.trattenute = rs.getBigDecimal("totale_trattenute");
                wsStipendio.netto = rs.getBigDecimal("netto_pagare");
                wsDipedente.nome = rs.getString("nome");
                wsDipedente.cognome = rs.getString("cognome");
                wsDipedente.cf = rs.getString("codice_fiscale");
                wsDipedente.qualifica = rs.getString("qualifica");
                wsDipedente.livello = rs.getString("livello");
                wsDipedente.matricola = wsMatricola;

                generaCedolino();
                registraCedolino();
            } else {
                System.out.println("Stipendio non trovato!");
            }
        } catch (SQLException e) {
            handleSqlException(e, false);
        }
    }

    /**
     * Genera il file di testo del cedolino.
     * Corrisponde al paragrafo GENERA-CEDOLINO.
     */
    private void generaCedolino() {
        try (PrintWriter writer = new PrintWriter(new FileWriter("CEDOLINO.TXT"))) {
            writer.println("                    CEDOLINO PAGA");
            writer.println("=======================================================================");
            writer.printf("AZIENDA: ESEMPIO SPA                    Periodo: %02d/%d%n", wsMese, wsAnno);
            writer.println("-----------------------------------------------------------------------");
            writer.printf("Dipendente: %s %s%n", wsDipedente.cognome.trim(), wsDipedente.nome.trim());
            writer.printf("Matricola: %s    C.F.: %s%n", wsDipedente.matricola, wsDipedente.cf);
            writer.printf("Qualifica: %s    Livello: %s%n", wsDipedente.qualifica.trim(), wsDipedente.livello.trim());
            writer.println("-----------------------------------------------------------------------");
            writer.println();
            writer.println("COMPETENZE:");
            writer.printf("  Stipendio base.............. %15s%n", currencyFormat.format(wsStipendio.base));
            if (wsStipendio.straordinari.compareTo(BigDecimal.ZERO) > 0) {
                writer.printf("  Straordinari................ %15s%n", currencyFormat.format(wsStipendio.straordinari));
            }
            writer.println();
            writer.println("TRATTENUTE:");

            String trattenuteSql = "SELECT descrizione, importo FROM TRATTENUTE WHERE id_stipendio = ? ORDER BY tipo_trattenuta";
            try (PreparedStatement stmt = connection.prepareStatement(trattenuteSql)) {
                stmt.setLong(1, wsStipendio.id);
                ResultSet rs = stmt.executeQuery();
                while (rs.next()) {
                    String descrizione = rs.getString("descrizione");
                    BigDecimal importo = rs.getBigDecimal("importo");
                    writer.printf("  %-28.28s... %15s%n", descrizione, currencyFormat.format(importo));
                }
            } catch (SQLException e) {
                writer.println("  Errore nel caricamento dettaglio trattenute.");
                handleSqlException(e, false);
            }

            writer.println("-----------------------------------------------------------------------");
            writer.printf("TOTALE LORDO................. %15s%n", currencyFormat.format(wsStipendio.lordo));
            writer.printf("TOTALE TRATTENUTE............ %15s%n", currencyFormat.format(wsStipendio.trattenute));
            writer.println("-----------------------------------------------------------------------");
            writer.printf("NETTO A PAGARE............... %15s%n", currencyFormat.format(wsStipendio.netto));
            writer.println("=======================================================================");

            System.out.println("Cedolino salvato in CEDOLINO.TXT");
        } catch (IOException e) {
            System.err.println("Errore durante la scrittura del file CEDOLINO.TXT: " + e.getMessage());
        }
    }

    /**
     * Registra l'emissione del cedolino nella tabella CEDOLINI.
     * Corrisponde al paragrafo REGISTRA-CEDOLINO.
     */
    private void registraCedolino() {
        String numeroCedolino = String.format("CED%d%02d%s", wsAnno, wsMese, wsMatricola);
        String sql = "INSERT INTO CEDOLINI (id_stipendio, numero_cedolino, data_emissione) VALUES (?, ?, ?) ON CONFLICT (numero_cedolino) DO NOTHING";
        try (PreparedStatement stmt = connection.prepareStatement(sql)) {
            stmt.setLong(1, wsStipendio.id);
            stmt.setString(2, numeroCedolino);
            stmt.setDate(3, Date.valueOf(LocalDate.now()));
            stmt.executeUpdate();
        } catch (SQLException e) {
            handleSqlException(e, false);
        }
    }

    /**
     * Avvia la generazione del report mensile degli stipendi su file.
     * Corrisponde al paragrafo REPORT-MENSILE.
     */
    private void reportMensile() {
        System.out.println("\n=== REPORT MENSILE STIPENDI ===");
        System.out.print("Anno: ");
        wsAnno = Integer.parseInt(scanner.nextLine());
        System.out.print("Mese: ");
        wsMese = Integer.parseInt(scanner.nextLine());

        try (PrintWriter writer = new PrintWriter(new FileWriter("REPORT-STIPENDI.TXT"))) {
            generaReport(writer);
            System.out.println("Report salvato in REPORT-STIPENDI.TXT");
        } catch (IOException e) {
            System.err.println("Errore durante la scrittura del file REPORT-STIPENDI.TXT: " + e.getMessage());
        }
    }

    /**
     * Genera il contenuto del report mensile.
     * Corrisponde al paragrafo GENERA-REPORT.
     *
     * @param writer Il PrintWriter su cui scrivere il report.
     */
    private void generaReport(PrintWriter writer) {
        writer.printf("REPORT STIPENDI - %02d/%d%n", wsMese, wsAnno);
        writer.println("=======================================================================================");

        String summarySql = "SELECT COUNT(*), SUM(totale_lordo), SUM(totale_trattenute), SUM(netto_pagare) " +
                            "FROM STIPENDI WHERE anno = ? AND mese = ? AND stato_pagamento <> 'A'";
        try (PreparedStatement stmt = connection.prepareStatement(summarySql)) {
            stmt.setInt(1, wsAnno);
            stmt.setInt(2, wsMese);
            ResultSet rs = stmt.executeQuery();
            if (rs.next()) {
                writer.printf("Dipendenti elaborati: %d%n", rs.getInt(1));
                writer.printf("Totale lordo:        %s%n", currencyFormat.format(rs.getBigDecimal(2)));
                writer.printf("Totale trattenute:   %s%n", currencyFormat.format(rs.getBigDecimal(3)));
                writer.printf("Totale netto:        %s%n", currencyFormat.format(rs.getBigDecimal(4)));
            }
        } catch (SQLException e) {
            handleSqlException(e, false);
        }

        writer.println("---------------------------------------------------------------------------------------");
        writer.println("DETTAGLIO PER DIPENDENTE:");
        writer.printf("%-8s %-20s %-20s %15s %15s%n", "Matr.", "Cognome", "Nome", "Lordo", "Netto");
        writer.println("---------------------------------------------------------------------------------------");

        String detailSql = "SELECT d.matricola, d.cognome, d.nome, s.totale_lordo, s.netto_pagare " +
                           "FROM STIPENDI s JOIN DIPENDENTI d ON s.matricola = d.matricola " +
                           "WHERE s.anno = ? AND s.mese = ? AND s.stato_pagamento <> 'A' ORDER BY d.matricola";
        try (PreparedStatement stmt = connection.prepareStatement(detailSql)) {
            stmt.setInt(1, wsAnno);
            stmt.setInt(2, wsMese);
            ResultSet rs = stmt.executeQuery();
            while (rs.next()) {
                writer.printf("%-8s %-20.20s %-20.20s %15s %15s%n",
                        rs.getString("matricola"),
                        rs.getString("cognome"),
                        rs.getString("nome"),
                        currencyFormat.format(rs.getBigDecimal("totale_lordo")),
                        currencyFormat.format(rs.getBigDecimal("netto_pagare")));
            }
        } catch (SQLException e) {
            handleSqlException(e, false);
        }
    }

    /**
     * Gestisce l'inserimento di una singola presenza giornaliera.
     * Corrisponde al paragrafo INSERIMENTO-PRESENZE.
     */
    private void inserimentoPresenze() {
        System.out.println("\n=== INSERIMENTO PRESENZE ===");
        System.out.print("Matricola: ");
        wsMatricola = scanner.nextLine();

        if (!caricaDipendente()) return;

        System.out.printf("Dipendente: %s %s%n", wsDipedente.nome.trim(), wsDipedente.cognome.trim());
        System.out.print("Data presenza (AAAA-MM-GG): ");
        String dataPresenzaStr = scanner.nextLine();
        Date dataPresenza = Date.valueOf(dataPresenzaStr);

        System.out.print("Tipo giornata (L/F/M/P/A): ");
        String tipoGiornata = scanner.nextLine().toUpperCase();

        BigDecimal oreOrd = BigDecimal.ZERO;
        BigDecimal oreStr = BigDecimal.ZERO;

        if ("L".equals(tipoGiornata)) {
            System.out.print("Ore ordinarie: ");
            oreOrd = new BigDecimal(scanner.nextLine());
            System.out.print("Ore straordinarie: ");
            oreStr = new BigDecimal(scanner.nextLine());
        }

        String sql = "INSERT INTO PRESENZE (matricola, data_presenza, tipo_giornata, ore_ordinarie, ore_straordinarie) VALUES (?, ?, ?, ?, ?)";
        try (PreparedStatement stmt = connection.prepareStatement(sql)) {
            stmt.setString(1, wsMatricola);
            stmt.setDate(2, dataPresenza);
            stmt.setString(3, tipoGiornata);
            stmt.setBigDecimal(4, oreOrd);
            stmt.setBigDecimal(5, oreStr);
            stmt.executeUpdate();
            System.out.println("Presenza registrata con successo!");
        } catch (SQLException e) {
            if ("23505".equals(e.getSQLState())) { // Codice per violazione UNIQUE constraint in PostgreSQL
                System.out.println("Presenza già registrata per questa data!");
            } else {
                handleSqlException(e, false);
            }
        }
    }

    /**
     * Visualizza il riepilogo delle presenze mensili di un dipendente.
     * Corrisponde al paragrafo VISUALIZZA-PRESENZE.
     */
    private void visualizzaPresenze() {
        System.out.println("\n=== VISUALIZZA PRESENZE ===");
        System.out.print("Matricola: ");
        wsMatricola = scanner.nextLine();
        System.out.print("Anno: ");
        wsAnno = Integer.parseInt(scanner.nextLine());
        System.out.print("Mese: ");
        wsMese = Integer.parseInt(scanner.nextLine());

        if (!caricaDipendente()) return;

        System.out.printf("\nPresenze di %s %s%n", wsDipedente.nome.trim(), wsDipedente.cognome.trim());
        System.out.printf("Periodo: %02d/%d%n", wsMese, wsAnno);

        wsPresenze = new Presenze();
        caricaPresenze();

        System.out.println("\nRIEPILOGO:");
        System.out.println("Giorni lavorati: " + numberFormat.format(wsPresenze.giorniLav));
        System.out.println("Giorni ferie:    " + numberFormat.format(wsPresenze.giorniFer));
        System.out.println("Giorni malattia: " + numberFormat.format(wsPresenze.giorniMal));
        System.out.println("Giorni permesso: " + numberFormat.format(wsPresenze.giorniPer));
        System.out.println("Ore ordinarie:   " + decimalFormat.format(wsPresenze.oreOrd));
        System.out.println("Ore straord.:    " + decimalFormat.format(wsPresenze.oreStr));
    }

    /**
     * Gestore centralizzato per le eccezioni SQL.
     * Stampa l'errore ed esegue il rollback se richiesto.
     *
     * @param e L'eccezione SQL catturata.
     * @param doRollback true se si deve tentare un rollback della transazione.
     */
    private void handleSqlException(SQLException e, boolean doRollback) {
        System.err.println("--- ERRORE DATABASE ---");
        System.err.println("SQLState: " + e.getSQLState());
        System.err.println("ErrorCode: " + e.getErrorCode());
        System.err.println("Messaggio: " + e.getMessage());
        System.err.println("-----------------------");

        if (doRollback && connection != null) {
            try {
                System.err.println("Tentativo di rollback della transazione...");
                connection.rollback();
                System.err.println("Rollback eseguito con successo.");
            } catch (SQLException ex) {
                System.err.println("!!! ERRORE CRITICO: Rollback fallito: " + ex.getMessage());
            }
        }
    }
}