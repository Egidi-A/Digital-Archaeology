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
import java.sql.Timestamp;
import java.sql.Types;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Locale;
import java.util.Scanner;

/**
 * Traduzione Java del programma COBOL GESTIONE-CONTI.
 * Questo programma gestisce le operazioni base di un conto corrente bancario,
 * interfacciandosi con un database PostgreSQL tramite JDBC.
 *
 * @author Annalisa Egidi (COBOL), Tradotto da Compilatore Avanzato (Java)
 * @version 2025-05-20
 */
public class GestioneConti {

    // --- Configurazione Database ---
    // NOTA: Assicurarsi che il nome del database sia corretto.
    // Il programma COBOL indicava 'banca', quindi usiamo quello.
    private static final String DB_URL = "jdbc:postgresql://localhost:5432/banca";
    private static final String DB_USER = "postgres";
    private static final String DB_PASSWORD = "password";

    // --- Connessione e Utility ---
    private Connection connection;
    private final Scanner scanner = new Scanner(System.in);
    private final DecimalFormat currencyFormatter;

    // --- Variabili di stato (Working-Storage) ---
    private int wsScelta = 0;
    private String wsContinua = "S";
    private int sqlcode = 0; // Simula SQLCODE per la logica di controllo

    public static void main(String[] args) {
        GestioneConti programma = new GestioneConti();
        programma.mainLogic();
    }

    /**
     * Costruttore della classe. Inizializza il formattatore di valuta.
     */
    public GestioneConti() {
        DecimalFormatSymbols symbols = new DecimalFormatSymbols(Locale.ITALY);
        this.currencyFormatter = new DecimalFormat("###,##0.00", symbols);
    }

    /**
     * Contiene il flusso principale del programma, equivalente a MAIN-LOGIC in COBOL.
     */
    public void mainLogic() {
        if (!connettiDatabase()) {
            return; // Interrompe l'esecuzione se la connessione fallisce
        }

        do {
            visualizzaMenu();
            try {
                wsScelta = Integer.parseInt(scanner.nextLine());
                elaboraScelta();
            } catch (NumberFormatException e) {
                System.out.println("Input non valido. Inserire un numero.");
                wsScelta = -1; // Valore non valido per forzare il messaggio di default
            }

            if (wsScelta != 0) {
                System.out.println();
                System.out.print("Continuare? (S/N): ");
                wsContinua = scanner.nextLine().toUpperCase();
            } else {
                wsContinua = "N"; // Esce dal ciclo se la scelta è 0
            }

        } while (wsContinua.equalsIgnoreCase("S"));

        disconnettiDatabase();
        scanner.close();
        System.out.println("Programma terminato.");
    }

    /**
     * Stabilisce la connessione al database.
     * Equivalente a CONNETTI-DATABASE.
     * @return true se la connessione ha successo, altrimenti false.
     */
    private boolean connettiDatabase() {
        try {
            connection = DriverManager.getConnection(DB_URL, DB_USER, DB_PASSWORD);
            // Impostiamo la gestione manuale delle transazioni per operazioni complesse
            connection.setAutoCommit(false);
            System.out.println("Connessione al database stabilita.");
            return true;
        } catch (SQLException e) {
            System.err.println("Errore connessione database: " + e.getMessage());
            sqlcode = e.getErrorCode();
            return false;
        }
    }

    /**
     * Chiude la connessione al database.
     * Equivalente a DISCONNETTI-DATABASE.
     */
    private void disconnettiDatabase() {
        if (connection != null) {
            try {
                connection.close();
                System.out.println("Disconnesso dal database.");
            } catch (SQLException e) {
                System.err.println("Errore durante la disconnessione: " + e.getMessage());
            }
        }
    }

    /**
     * Mostra il menu principale delle operazioni.
     * Equivalente a VISUALIZZA-MENU.
     */
    private void visualizzaMenu() {
        System.out.println();
        System.out.println("===== SISTEMA GESTIONE CONTI CORRENTI =====");
        System.out.println("1. Apertura nuovo conto");
        System.out.println("2. Deposito");
        System.out.println("3. Prelievo");
        System.out.println("4. Visualizza saldo");
        System.out.println("5. Estratto conto");
        System.out.println("6. Chiusura conto");
        System.out.println("0. Esci");
        System.out.println("===========================================");
        System.out.print("Scelta: ");
    }

    /**
     * Esegue l'azione corrispondente alla scelta dell'utente.
     * Equivalente a ELABORA-SCELTA.
     */
    private void elaboraScelta() {
        switch (wsScelta) {
            case 1:
                aperturaConto();
                break;
            case 2:
                deposito();
                break;
            case 3:
                prelievo();
                break;
            case 4:
                visualizzaSaldo();
                break;
            case 5:
                estrattoConto();
                break;
            case 6:
                chiusuraConto();
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
     * Gestisce l'apertura di un nuovo conto corrente.
     * Equivalente a APERTURA-CONTO.
     */
    private void aperturaConto() {
        System.out.println("\n=== APERTURA NUOVO CONTO ===");
        System.out.print("Codice cliente: ");
        String codiceCliente = scanner.nextLine();

        String sqlCheckCliente = "SELECT nome, cognome FROM CLIENTI WHERE codice_cliente = ?";
        try (PreparedStatement ps = connection.prepareStatement(sqlCheckCliente)) {
            ps.setString(1, codiceCliente);
            try (ResultSet rs = ps.executeQuery()) {
                if (!rs.next()) {
                    System.out.println("Cliente non trovato!");
                    return;
                }
                System.out.println("Cliente: " + rs.getString("nome") + " " + rs.getString("cognome"));
            }
        } catch (SQLException e) {
            handleSqlException(e, null);
            return;
        }

        String nuovoNumeroConto = generaNumeroConto();
        if (nuovoNumeroConto == null) {
            System.out.println("Impossibile generare un nuovo numero di conto.");
            return;
        }

        System.out.print("Tipo conto (C=Corrente, D=Deposito): ");
        String tipoConto = scanner.nextLine().toUpperCase();
        System.out.print("Importo iniziale: ");
        BigDecimal importoIniziale = new BigDecimal(scanner.nextLine());
        System.out.print("Fido accordato: ");
        BigDecimal fido = new BigDecimal(scanner.nextLine());

        String sqlInsertConto = "INSERT INTO CONTI (numero_conto, codice_cliente, tipo_conto, saldo, data_apertura, stato, fido) VALUES (?, ?, ?, ?, CURRENT_DATE, 'A', ?)";
        try (PreparedStatement ps = connection.prepareStatement(sqlInsertConto)) {
            ps.setString(1, nuovoNumeroConto);
            ps.setString(2, codiceCliente);
            ps.setString(3, tipoConto);
            ps.setBigDecimal(4, importoIniziale);
            ps.setBigDecimal(5, fido);

            int righeInserite = ps.executeUpdate();
            if (righeInserite > 0) {
                System.out.println("Conto " + nuovoNumeroConto + " creato con successo!");
                // Se c'è un deposito iniziale, lo registriamo come movimento
                if (importoIniziale.compareTo(BigDecimal.ZERO) > 0) {
                    registraMovimento(nuovoNumeroConto, "D", importoIniziale, "Deposito iniziale");
                }
                connection.commit();
            } else {
                throw new SQLException("Creazione conto fallita, nessuna riga inserita.");
            }
        } catch (SQLException e) {
            handleSqlException(e, "Errore creazione conto: ");
        }
    }

    /**
     * Genera un nuovo numero di conto sequenziale.
     * Equivalente a GENERA-NUMERO-CONTO.
     * @return Il nuovo numero di conto o null in caso di errore.
     */
    private String generaNumeroConto() {
        String sql = "SELECT 'IT' || LPAD(CAST(COALESCE(MAX(CAST(SUBSTR(numero_conto, 3) AS INTEGER)), 0) + 1 AS VARCHAR), 10, '0') FROM CONTI WHERE numero_conto LIKE 'IT%'";
        try (PreparedStatement ps = connection.prepareStatement(sql);
             ResultSet rs = ps.executeQuery()) {
            if (rs.next()) {
                return rs.getString(1);
            }
        } catch (SQLException e) {
            handleSqlException(e, null);
        }
        return null;
    }

    /**
     * Gestisce un'operazione di deposito su un conto.
     * Equivalente a DEPOSITO.
     */
    private void deposito() {
        System.out.println("\n=== DEPOSITO ===");
        System.out.print("Numero conto: ");
        String numeroConto = scanner.nextLine();

        if (!verificaContoAttivo(numeroConto)) return;

        System.out.print("Importo deposito: ");
        BigDecimal importo = new BigDecimal(scanner.nextLine());
        if (importo.compareTo(BigDecimal.ZERO) <= 0) {
            System.out.println("Importo non valido!");
            return;
        }

        System.out.print("Causale: ");
        String causale = scanner.nextLine();

        String sqlUpdate = "UPDATE CONTI SET saldo = saldo + ? WHERE numero_conto = ? AND stato = 'A'";
        try (PreparedStatement ps = connection.prepareStatement(sqlUpdate)) {
            ps.setBigDecimal(1, importo);
            ps.setString(2, numeroConto);
            int righeAggiornate = ps.executeUpdate();

            if (righeAggiornate > 0) {
                registraMovimento(numeroConto, "D", importo, causale);
                connection.commit();
                System.out.println("Deposito effettuato con successo!");
            } else {
                throw new SQLException("Deposito fallito, nessuna riga aggiornata.");
            }
        } catch (SQLException e) {
            handleSqlException(e, "Errore durante il deposito: ");
        }
    }

    /**
     * Gestisce un'operazione di prelievo da un conto.
     * Equivalente a PRELIEVO.
     */
    private void prelievo() {
        System.out.println("\n=== PRELIEVO ===");
        System.out.print("Numero conto: ");
        String numeroConto = scanner.nextLine();

        Conto conto = getDatiConto(numeroConto);
        if (conto == null || !conto.stato.equals("A")) {
            System.out.println("Conto non trovato o non attivo.");
            return;
        }

        System.out.print("Importo prelievo: ");
        BigDecimal importo = new BigDecimal(scanner.nextLine());
        if (importo.compareTo(BigDecimal.ZERO) <= 0) {
            System.out.println("Importo non valido!");
            return;
        }

        // Verifica disponibilità: saldo - importo >= -fido
        if (conto.saldo.subtract(importo).compareTo(conto.fido.negate()) < 0) {
            System.out.println("Fondi insufficienti!");
            System.out.println("Saldo attuale: " + currencyFormatter.format(conto.saldo));
            System.out.println("Fido disponibile: " + currencyFormatter.format(conto.fido));
            return;
        }

        System.out.print("Causale: ");
        String causale = scanner.nextLine();

        String sqlUpdate = "UPDATE CONTI SET saldo = saldo - ? WHERE numero_conto = ? AND stato = 'A'";
        try (PreparedStatement ps = connection.prepareStatement(sqlUpdate)) {
            ps.setBigDecimal(1, importo);
            ps.setString(2, numeroConto);
            int righeAggiornate = ps.executeUpdate();

            if (righeAggiornate > 0) {
                registraMovimento(numeroConto, "P", importo, causale);
                connection.commit();
                System.out.println("Prelievo effettuato con successo!");
            } else {
                throw new SQLException("Prelievo fallito, nessuna riga aggiornata.");
            }
        } catch (SQLException e) {
            handleSqlException(e, "Errore durante il prelievo: ");
        }
    }

    /**
     * Mostra il saldo e i dettagli di un conto.
     * Equivalente a VISUALIZZA-SALDO.
     */
    private void visualizzaSaldo() {
        System.out.println("\n=== VISUALIZZA SALDO ===");
        System.out.print("Numero conto: ");
        String numeroConto = scanner.nextLine();

        String sql = "SELECT c.saldo, c.fido, cl.nome, cl.cognome FROM CONTI c JOIN CLIENTI cl ON c.codice_cliente = cl.codice_cliente WHERE c.numero_conto = ? AND c.stato = 'A'";
        try (PreparedStatement ps = connection.prepareStatement(sql)) {
            ps.setString(1, numeroConto);
            try (ResultSet rs = ps.executeQuery()) {
                if (rs.next()) {
                    BigDecimal saldo = rs.getBigDecimal("saldo");
                    BigDecimal fido = rs.getBigDecimal("fido");
                    String nome = rs.getString("nome");
                    String cognome = rs.getString("cognome");
                    BigDecimal disponibile = saldo.add(fido);

                    System.out.println("\nIntestatario: " + nome + " " + cognome);
                    System.out.println("Saldo attuale: EUR " + currencyFormatter.format(saldo));
                    System.out.println("Fido accordato: EUR " + currencyFormatter.format(fido));
                    System.out.println("Disponibile:   EUR " + currencyFormatter.format(disponibile));
                } else {
                    System.out.println("Conto non trovato o non attivo!");
                }
                connection.commit(); // Commit per terminare la transazione di lettura
            }
        } catch (SQLException e) {
            handleSqlException(e, "Errore database: ");
        }
    }

    /**
     * Genera un file di testo con l'estratto conto.
     * Equivalente a ESTRATTO-CONTO.
     */
    private void estrattoConto() {
        System.out.println("\n=== ESTRATTO CONTO ===");
        System.out.print("Numero conto: ");
        String numeroConto = scanner.nextLine();

        Conto conto = getDatiContoConCliente(numeroConto);
        if (conto == null || !conto.stato.equals("A")) {
            System.out.println("Conto non trovato o non attivo.");
            return;
        }

        String fileName = "ESTRATTO-CONTO.TXT";
        try (PrintWriter writer = new PrintWriter(new FileWriter(fileName))) {
            // Intestazione report
            writer.println(String.format("%66s", "ESTRATTO CONTO BANCARIO"));
            writer.println("-".repeat(132));
            writer.println("Conto: " + numeroConto + "    Cliente: " + conto.cliente.nome + " " + conto.cliente.cognome);
            writer.println("Data: " + LocalDate.now().format(DateTimeFormatter.ISO_LOCAL_DATE) + "    Ora: " + LocalDateTime.now().format(DateTimeFormatter.ofPattern("HH:mm:ss")));
            writer.println("-".repeat(132));
            writer.println(String.format("%-20s %-5s %15s   %-50s %15s", "DATA/ORA", "TIPO", "IMPORTO", "CAUSALE", "SALDO DOPO"));
            writer.println("-".repeat(132));

            // Cursore movimenti
            String sqlCursor = "SELECT data_movimento, tipo_movimento, importo, causale, saldo_dopo FROM MOVIMENTI WHERE numero_conto = ? ORDER BY data_movimento DESC";
            try (PreparedStatement ps = connection.prepareStatement(sqlCursor)) {
                ps.setString(1, numeroConto);
                try (ResultSet rs = ps.executeQuery()) {
                    while (rs.next()) {
                        scriviMovimentoReport(writer, rs);
                    }
                }
            }

            writer.println("-".repeat(132));
            writer.println(String.format("SALDO FINALE: EUR %s", currencyFormatter.format(conto.saldo)));
            
            connection.commit(); // Commit per terminare la transazione di lettura
            System.out.println("Estratto conto salvato in " + fileName);

        } catch (IOException e) {
            System.err.println("Errore durante la scrittura del file: " + e.getMessage());
        } catch (SQLException e) {
            handleSqlException(e, "Errore durante la lettura dei movimenti: ");
        }
    }

    /**
     * Formatta e scrive una singola riga di movimento nel report.
     * Equivalente a SCRIVI-MOVIMENTO-REPORT.
     */
    private void scriviMovimentoReport(PrintWriter writer, ResultSet rs) throws SQLException {
        Timestamp dataMov = rs.getTimestamp("data_movimento");
        String tipoMov = rs.getString("tipo_movimento");
        BigDecimal importo = rs.getBigDecimal("importo");
        String causale = rs.getString("causale");
        BigDecimal saldoDopo = rs.getBigDecimal("saldo_dopo");

        String dataFormatted = dataMov.toLocalDateTime().format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"));
        
        String tipoDesc;
        switch (tipoMov) {
            case "D": tipoDesc = "DEP"; break;
            case "P": tipoDesc = "PRE"; break;
            case "B": tipoDesc = "BON"; break;
            default: tipoDesc = "???"; break;
        }

        String riga = String.format("%-20s %-5s %15s   %-50.50s %15s",
            dataFormatted,
            tipoDesc,
            currencyFormatter.format(importo),
            causale,
            currencyFormatter.format(saldoDopo)
        );
        writer.println(riga);
    }

    /**
     * Gestisce la chiusura di un conto.
     * Equivalente a CHIUSURA-CONTO.
     */
    private void chiusuraConto() {
        System.out.println("\n=== CHIUSURA CONTO ===");
        System.out.print("Numero conto da chiudere: ");
        String numeroConto = scanner.nextLine();

        Conto conto = getDatiConto(numeroConto);
        if (conto == null || !conto.stato.equals("A")) {
            System.out.println("Conto non trovato o non attivo.");
            return;
        }

        if (conto.saldo.compareTo(BigDecimal.ZERO) != 0) {
            System.out.println("Impossibile chiudere: saldo non zero!");
            System.out.println("Saldo attuale: EUR " + currencyFormatter.format(conto.saldo));
            return;
        }

        System.out.print("Confermare chiusura conto (S/N): ");
        String conferma = scanner.nextLine();

        if (conferma.equalsIgnoreCase("S")) {
            String sqlUpdate = "UPDATE CONTI SET stato = 'C', data_chiusura = CURRENT_DATE WHERE numero_conto = ?";
            try (PreparedStatement ps = connection.prepareStatement(sqlUpdate)) {
                ps.setString(1, numeroConto);
                int righeAggiornate = ps.executeUpdate();
                if (righeAggiornate > 0) {
                    connection.commit();
                    System.out.println("Conto chiuso con successo!");
                } else {
                    throw new SQLException("Chiusura conto fallita, nessuna riga aggiornata.");
                }
            } catch (SQLException e) {
                handleSqlException(e, "Errore chiusura conto: ");
            }
        } else {
            System.out.println("Chiusura annullata.");
        }
    }

    /**
     * Verifica se un conto esiste ed è attivo.
     * Metodo helper che sostituisce VERIFICA-CONTO.
     * @return true se il conto è valido, altrimenti false.
     */
    private boolean verificaContoAttivo(String numeroConto) {
        Conto conto = getDatiConto(numeroConto);
        if (conto == null) {
            System.out.println("Conto non trovato!");
            return false;
        }
        if (!conto.stato.equals("A")) {
            System.out.println("Conto non attivo!");
            return false;
        }
        return true;
    }

    /**
     * Recupera i dati essenziali di un conto.
     * @return un oggetto Conto o null se non trovato.
     */
    private Conto getDatiConto(String numeroConto) {
        String sql = "SELECT saldo, stato, fido FROM CONTI WHERE numero_conto = ?";
        try (PreparedStatement ps = connection.prepareStatement(sql)) {
            ps.setString(1, numeroConto);
            try (ResultSet rs = ps.executeQuery()) {
                if (rs.next()) {
                    Conto c = new Conto();
                    c.numero = numeroConto;
                    c.saldo = rs.getBigDecimal("saldo");
                    c.stato = rs.getString("stato");
                    c.fido = rs.getBigDecimal("fido");
                    return c;
                }
            }
        } catch (SQLException e) {
            handleSqlException(e, null);
        }
        return null;
    }
    
    /**
     * Recupera i dati di un conto includendo i dettagli del cliente.
     * @return un oggetto Conto con Cliente annesso, o null se non trovato.
     */
    private Conto getDatiContoConCliente(String numeroConto) {
        String sql = "SELECT c.saldo, c.stato, c.fido, cl.nome, cl.cognome FROM CONTI c JOIN CLIENTI cl ON c.codice_cliente = cl.codice_cliente WHERE c.numero_conto = ?";
        try (PreparedStatement ps = connection.prepareStatement(sql)) {
            ps.setString(1, numeroConto);
            try (ResultSet rs = ps.executeQuery()) {
                if (rs.next()) {
                    Conto c = new Conto();
                    c.numero = numeroConto;
                    c.saldo = rs.getBigDecimal("saldo");
                    c.stato = rs.getString("stato");
                    c.fido = rs.getBigDecimal("fido");
                    
                    Cliente cl = new Cliente();
                    cl.nome = rs.getString("nome");
                    cl.cognome = rs.getString("cognome");
                    c.cliente = cl;
                    return c;
                }
            }
        } catch (SQLException e) {
            handleSqlException(e, null);
        }
        return null;
    }

    /**
     * Inserisce un nuovo record nella tabella MOVIMENTI.
     * Equivalente a REGISTRA-MOVIMENTO.
     */
    private void registraMovimento(String numeroConto, String tipoMovimento, BigDecimal importo, String causale) throws SQLException {
        // Prima recupero il saldo aggiornato
        BigDecimal saldoDopo = BigDecimal.ZERO;
        String sqlGetSaldo = "SELECT saldo FROM CONTI WHERE numero_conto = ?";
        try (PreparedStatement ps = connection.prepareStatement(sqlGetSaldo)) {
            ps.setString(1, numeroConto);
            try (ResultSet rs = ps.executeQuery()) {
                if (rs.next()) {
                    saldoDopo = rs.getBigDecimal("saldo");
                } else {
                    throw new SQLException("Conto " + numeroConto + " non trovato durante la registrazione del movimento.");
                }
            }
        }

        String sqlInsert = "INSERT INTO MOVIMENTI (numero_conto, tipo_movimento, importo, causale, saldo_dopo, eseguito_da) VALUES (?, ?, ?, ?, ?, 'SISTEMA')";
        try (PreparedStatement ps = connection.prepareStatement(sqlInsert)) {
            ps.setString(1, numeroConto);
            ps.setString(2, tipoMovimento);
            ps.setBigDecimal(3, importo);
            ps.setString(4, causale);
            ps.setBigDecimal(5, saldoDopo);
            ps.executeUpdate();
        }
    }

    /**
     * Gestore centralizzato per le eccezioni SQL. Esegue il rollback.
     * @param e L'eccezione SQL catturata.
     * @param userMessage Messaggio personalizzato da mostrare all'utente.
     */
    private void handleSqlException(SQLException e, String userMessage) {
        sqlcode = e.getErrorCode();
        if (userMessage != null) {
            System.err.println(userMessage + sqlcode + " - " + e.getMessage());
        } else {
            System.err.println("Errore database: " + sqlcode + " - " + e.getMessage());
        }

        if (connection != null) {
            try {
                connection.rollback();
                System.err.println("Transazione annullata (rollback).");
            } catch (SQLException ex) {
                System.err.println("Errore critico durante il rollback: " + ex.getMessage());
            }
        }
    }

    // --- Strutture Dati (POJO) ---
    // Queste classi interne statiche sostituiscono le strutture 01 in COBOL
    // per una migliore organizzazione e leggibilità del codice Java.

    private static class Cliente {
        String codice = "";
        String nome = "";
        String cognome = "";
        // Altri campi del cliente potrebbero essere aggiunti qui se necessario
    }

    private static class Conto {
        String numero = "";
        String stato = "";
        BigDecimal saldo = BigDecimal.ZERO;
        BigDecimal fido = BigDecimal.ZERO;
        Cliente cliente; // Per contenere i dati del cliente associato
    }
}