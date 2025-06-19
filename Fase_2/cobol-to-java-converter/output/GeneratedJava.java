/*
 * IMPORTANTE:
 * Questo codice Java è stato generato automaticamente da un Abstract Semantic Graph (ASG) XML.
 * L'ASG rappresenta un programma originariamente scritto in un altro linguaggio (probabilmente COBOL)
 * e tradotto in una rappresentazione intermedia.
 *
 * Note sulla generazione:
 * 1. Simulazione SQL: Le operazioni di database (indicate nell'ASG come 'execSqlStatement')
 *    sono state simulate. Questo comporta l'impostazione manuale della variabile 'sqlcode'
 *    (0 per successo, 100 per record non trovato) e la valorizzazione di variabili
 *    con dati fittizi per permettere al flusso del programma di continuare come previsto.
 * 2. Logica e Struttura: In alcuni punti, la struttura dell'ASG era ridondante o illogica
 *    (es. blocchi di codice duplicati, chiamate a metodi ridondanti). Il codice generato
 *    cerca di interpretare l'intento originale del programma, razionalizzando la logica
 *    in costrutti Java standard (es. if-else-if, loop do-while) per garantire
 *    funzionalità e leggibilità. Tali decisioni sono documentate nei commenti.
 * 3. Variabili: Molte variabili usate per lo stato del programma (es. input utente,
 *    risultati di operazioni) non erano definite come campi nell'ASG. Sono state
 *    dichiarate come campi di istanza privati per mantenere lo stato tra le chiamate
 *    ai metodi, rispecchiando il comportamento di un programma procedurale.
 */
package com.generated;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.Scanner;
import java.io.FileWriter;
import java.io.PrintWriter;
import java.io.IOException;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

/**
 * Gestisce le operazioni sui conti correnti, come apertura, deposito, prelievo,
 * e generazione di estratti conto.
 * Questa classe è generata da un Abstract Semantic Graph (ASG) e simula
 * le interazioni con un database per la gestione dei conti.
 */
public class GestioneConti {

    // =========================================================================
    // Campi definiti nell'ASG
    // =========================================================================

    /** Connessione al database (simulata). */
    private java.sql.Connection connection;

    /** Codice di stato SQL per le operazioni sul database (simulato). */
    private int sqlcode;

    /** Codice del cliente. */
    private String wsCliCodice;

    /** Nome del cliente. */
    private String wsCliNome;

    /** Cognome del cliente. */
    private String wsCliCognome;

    /** Codice Fiscale del cliente. */
    private String wsCliCf;

    /** Data di nascita del cliente. */
    private String wsCliDataNascita;

    /** Indirizzo del cliente. */
    private String wsCliIndirizzo;

    /** Città di residenza del cliente. */
    private String wsCliCitta;

    /** CAP del cliente. */
    private String wsCliCap;

    /** Telefono del cliente. */
    private String wsCliTelefono;

    /** Email del cliente. */
    private String wsCliEmail;

    /** Numero del conto. */
    private String wsConNumero;

    /** Codice cliente associato al conto. */
    private String wsConCliente;

    /** Tipo di conto (es. 'C' per Corrente). */
    private String wsConTipo;

    /** Saldo del conto. */
    private BigDecimal wsConSaldo;

    /** Data di apertura del conto. */
    private String wsConDataApertura;

    /** Stato del conto (es. 'A' per Attivo). */
    private String wsConStato;

    /** Fido concesso sul conto. */
    private BigDecimal wsConFido;

    /** Data del movimento. */
    private String wsMovData;

    /** Tipo di movimento (es. 'D' per Deposito). */
    private String wsMovTipo;

    /** Importo del movimento. */
    private BigDecimal wsMovImporto;

    /** Causale del movimento. */
    private String wsMovCausale;

    /** Saldo del conto dopo il movimento. */
    private BigDecimal wsMovSaldoDopo;

    // =========================================================================
    // Campi ausiliari dedotti dall'uso nei metodi dell'ASG
    // =========================================================================

    /** Scanner per l'input dell'utente dalla console. */
    private transient Scanner scanner = new Scanner(System.in);

    /** Memorizza la scelta dell'utente dal menu principale. */
    private String wsScelta;

    /** Flag per controllare il loop principale del programma ('S' per continuare). */
    private String wsContinua;

    /** Esito di un'operazione interna (es. "OK" o "KO"). */
    private String wsEsito;

    /** Variabile temporanea per l'importo inserito dall'utente. */
    private BigDecimal wsImporto;

    /** Variabile temporanea per il numero di conto inserito dall'utente. */
    private String wsNumeroConto;

    /** Variabile temporanea per la causale inserita dall'utente. */
private String wsCausale;

    /** Variabile temporanea per il tipo di movimento. */
    private String wsTipoMovimento;

    /** Variabile temporanea per calcoli sul saldo. */
    private BigDecimal wsSaldo;

    /** Variabile per formattare i valori BigDecimal come stringhe per la visualizzazione. */
    private BigDecimal wsSaldoEdit;


    // =========================================================================
    // Getter e Setter generati dall'ASG
    // =========================================================================

    /**
     * Restituisce il codice del cliente.
     * @return Il codice del cliente.
     */
    public String getWsCliCodice() {
        return wsCliCodice;
    }

    /**
     * Imposta il codice del cliente.
     * @param value Il nuovo codice del cliente.
     */
    public void setWsCliCodice(String value) {
        this.wsCliCodice = value;
    }

    /**
     * Restituisce il nome del cliente.
     * @return Il nome del cliente.
     */
    public String getWsCliNome() {
        return wsCliNome;
    }

    /**
     * Imposta il nome del cliente.
     * @param value Il nuovo nome del cliente.
     */
    public void setWsCliNome(String value) {
        this.wsCliNome = value;
    }

    /**
     * Restituisce il cognome del cliente.
     * @return Il cognome del cliente.
     */
    public String getWsCliCognome() {
        return wsCliCognome;
    }

    /**
     * Imposta il cognome del cliente.
     * @param value Il nuovo cognome del cliente.
     */
    public void setWsCliCognome(String value) {
        this.wsCliCognome = value;
    }

    /**
     * Restituisce il codice fiscale del cliente.
     * @return Il codice fiscale del cliente.
     */
    public String getWsCliCf() {
        return wsCliCf;
    }

    /**
     * Imposta il codice fiscale del cliente.
     * @param value Il nuovo codice fiscale del cliente.
     */
    public void setWsCliCf(String value) {
        this.wsCliCf = value;
    }

    /**
     * Restituisce la data di nascita del cliente.
     * @return La data di nascita del cliente.
     */
    public String getWsCliDataNascita() {
        return wsCliDataNascita;
    }

    /**
     * Imposta la data di nascita del cliente.
     * @param value La nuova data di nascita del cliente.
     */
    public void setWsCliDataNascita(String value) {
        this.wsCliDataNascita = value;
    }

    /**
     * Restituisce l'indirizzo del cliente.
     * @return L'indirizzo del cliente.
     */
    public String getWsCliIndirizzo() {
        return wsCliIndirizzo;
    }

    /**
     * Imposta l'indirizzo del cliente.
     * @param value Il nuovo indirizzo del cliente.
     */
    public void setWsCliIndirizzo(String value) {
        this.wsCliIndirizzo = value;
    }

    /**
     * Restituisce la città del cliente.
     * @return La città del cliente.
     */
    public String getWsCliCitta() {
        return wsCliCitta;
    }

    /**
     * Imposta la città del cliente.
     * @param value La nuova città del cliente.
     */
    public void setWsCliCitta(String value) {
        this.wsCliCitta = value;
    }

    /**
     * Restituisce il CAP del cliente.
     * @return Il CAP del cliente.
     */
    public String getWsCliCap() {
        return wsCliCap;
    }

    /**
     * Imposta il CAP del cliente.
     * @param value Il nuovo CAP del cliente.
     */
    public void setWsCliCap(String value) {
        this.wsCliCap = value;
    }

    /**
     * Restituisce il telefono del cliente.
     * @return Il telefono del cliente.
     */
    public String getWsCliTelefono() {
        return wsCliTelefono;
    }

    /**
     * Imposta il telefono del cliente.
     * @param value Il nuovo telefono del cliente.
     */
    public void setWsCliTelefono(String value) {
        this.wsCliTelefono = value;
    }

    /**
     * Restituisce l'email del cliente.
     * @return L'email del cliente.
     */
    public String getWsCliEmail() {
        return wsCliEmail;
    }

    /**
     * Imposta l'email del cliente.
     * @param value La nuova email del cliente.
     */
    public void setWsCliEmail(String value) {
        this.wsCliEmail = value;
    }

    /**
     * Restituisce il numero del conto.
     * @return Il numero del conto.
     */
    public String getWsConNumero() {
        return wsConNumero;
    }

    /**
     * Imposta il numero del conto.
     * @param value Il nuovo numero del conto.
     */
    public void setWsConNumero(String value) {
        this.wsConNumero = value;
    }

    /**
     * Restituisce il codice cliente associato al conto.
     * @return Il codice cliente.
     */
    public String getWsConCliente() {
        return wsConCliente;
    }

    /**
     * Imposta il codice cliente associato al conto.
     * @param value Il nuovo codice cliente.
     */
    public void setWsConCliente(String value) {
        this.wsConCliente = value;
    }

    /**
     * Restituisce il tipo di conto.
     * @return Il tipo di conto.
     */
    public String getWsConTipo() {
        return wsConTipo;
    }

    /**
     * Imposta il tipo di conto.
     * @param value Il nuovo tipo di conto.
     */
    public void setWsConTipo(String value) {
        this.wsConTipo = value;
    }

    /**
     * Restituisce il saldo del conto.
     * @return Il saldo del conto.
     */
    public BigDecimal getWsConSaldo() {
        return wsConSaldo;
    }

    /**
     * Imposta il saldo del conto.
     * @param value Il nuovo saldo del conto.
     */
    public void setWsConSaldo(BigDecimal value) {
        this.wsConSaldo = value;
    }

    /**
     * Restituisce la data di apertura del conto.
     * @return La data di apertura.
     */
    public String getWsConDataApertura() {
        return wsConDataApertura;
    }

    /**
     * Imposta la data di apertura del conto.
     * @param value La nuova data di apertura.
     */
    public void setWsConDataApertura(String value) {
        this.wsConDataApertura = value;
    }

    /**
     * Restituisce lo stato del conto.
     * @return Lo stato del conto.
     */
    public String getWsConStato() {
        return wsConStato;
    }

    /**
     * Imposta lo stato del conto.
     * @param value Il nuovo stato del conto.
     */
    public void setWsConStato(String value) {
        this.wsConStato = value;
    }

    /**
     * Restituisce il fido del conto.
     * @return Il fido del conto.
     */
    public BigDecimal getWsConFido() {
        return wsConFido;
    }

    /**
     * Imposta il fido del conto.
     * @param value Il nuovo fido del conto.
     */
    public void setWsConFido(BigDecimal value) {
        this.wsConFido = value;
    }

    /**
     * Restituisce la data del movimento.
     * @return La data del movimento.
     */
    public String getWsMovData() {
        return wsMovData;
    }

    /**
     * Imposta la data del movimento.
     * @param value La nuova data del movimento.
     */
    public void setWsMovData(String value) {
        this.wsMovData = value;
    }

    /**
     * Restituisce il tipo di movimento.
     * @return Il tipo di movimento.
     */
    public String getWsMovTipo() {
        return wsMovTipo;
    }

    /**
     * Imposta il tipo di movimento.
     * @param value Il nuovo tipo di movimento.
     */
    public void setWsMovTipo(String value) {
        this.wsMovTipo = value;
    }

    /**
     * Restituisce l'importo del movimento.
     * @return L'importo del movimento.
     */
    public BigDecimal getWsMovImporto() {
        return wsMovImporto;
    }

    /**
     * Imposta l'importo del movimento.
     * @param value Il nuovo importo del movimento.
     */
    public void setWsMovImporto(BigDecimal value) {
        this.wsMovImporto = value;
    }

    /**
     * Restituisce la causale del movimento.
     * @return La causale del movimento.
     */
    public String getWsMovCausale() {
        return wsMovCausale;
    }

    /**
     * Imposta la causale del movimento.
     * @param value La nuova causale del movimento.
     */
    public void setWsMovCausale(String value) {
        this.wsMovCausale = value;
    }

    /**
     * Restituisce il saldo dopo il movimento.
     * @return Il saldo dopo il movimento.
     */
    public BigDecimal getWsMovSaldoDopo() {
        return wsMovSaldoDopo;
    }

    /**
     * Imposta il saldo dopo il movimento.
     * @param value Il nuovo saldo dopo il movimento.
     */
    public void setWsMovSaldoDopo(BigDecimal value) {
        this.wsMovSaldoDopo = value;
    }

    // =========================================================================
    // Metodi di Logica generati dall'ASG
    // =========================================================================

    /**
     * Logica principale dell'applicazione. Gestisce il menu e il ciclo di vita del programma.
     * @throws SQLException In caso di errori SQL (simulati).
     * @throws IOException In caso di errori di I/O durante la scrittura del report.
     */
    private void mainLogic() throws SQLException, IOException {
        connettiDatabase();

        // L'ASG originale suggerisce una sequenza fissa, ma la presenza della variabile
        // 'wsContinua' implica un loop. È stato implementato un loop do-while
        // per riflettere l'intento più probabile di un'applicazione interattiva.
        do {
            visualizzaMenu();
            elaboraScelta();

            // Se la scelta non è 'esci' (0), chiede se continuare.
            if (!"0".equals(wsScelta)) {
                System.out.println(" ");
                System.out.print("Eseguire un'altra operazione? (S/N): ");
                wsContinua = scanner.nextLine();
            } else {
                wsContinua = "N"; // Esce dal loop se l'utente ha scelto 0
            }

        } while ("S".equalsIgnoreCase(wsContinua));

        disconnettiDatabase();
        System.out.println("Programma terminato.");
    }

    /**
     * Simula la connessione al database.
     * @throws SQLException In caso di errori (simulati).
     */
    private void connettiDatabase() throws SQLException {
        // ASG: Simulating database connection.
        // In un'applicazione reale, questo metodo conterrebbe la logica JDBC per
        // stabilire una connessione. Qui, simuliamo il successo.
        sqlcode = 0;

        if (sqlcode != 0) {
            System.out.println("Errore connessione database: " + sqlcode);
            System.exit(1);
        } else {
            System.out.println("Connessione al database stabilita");
        }
        // NOTA: L'ASG originale conteneva codice ridondante/irraggiungibile dopo
        // questo blocco if-else, che è stato omesso per chiarezza e correttezza.
    }

    /**
     * Simula la disconnessione dal database.
     * @throws SQLException In caso di errori (simulati).
     */
    private void disconnettiDatabase() throws SQLException {
        // In un'applicazione reale, questo chiuderebbe la connessione JDBC.
        System.out.println("Disconnesso dal database");
    }

    /**
     * Visualizza il menu principale e acquisisce la scelta dell'utente.
     * @throws SQLException In caso di errori (simulati).
     */
    private void visualizzaMenu() throws SQLException {
        System.out.println(" ");
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
        wsScelta = scanner.nextLine();
    }

    /**
     * Esegue l'azione corrispondente alla scelta dell'utente.
     * @throws SQLException In caso di errori (simulati).
     * @throws IOException In caso di errori di I/O.
     */
    private void elaboraScelta() throws SQLException, IOException {
        switch (wsScelta) {
            case "1":
                aperturaConto();
                break;
            case "2":
                deposito();
                break;
            case "3":
                prelievo();
                break;
            case "4":
                visualizzaSaldo();
                break;
            case "5":
                estrattoConto();
                break;
            case "6":
                chiusuraConto();
                break;
            case "0":
                // L'uscita è gestita nel loop di mainLogic
                break;
            default:
                System.out.println("Scelta non valida!");
        }
        // NOTA: L'ASG originale conteneva chiamate a tutti i metodi dopo lo switch,
        // che è stato interpretato come un artefatto di traduzione e rimosso.
    }

    /**
     * Gestisce la logica per l'apertura di un nuovo conto.
     * @throws SQLException In caso di errori (simulati).
     */
    private void aperturaConto() throws SQLException {
        System.out.println(" ");
        System.out.println("=== APERTURA NUOVO CONTO ===");
        System.out.print("Codice cliente: ");
        wsConCliente = scanner.nextLine();

        // ASG: execSqlStatement (SELECT su cliente)
        // Simula una SELECT per trovare i dati del cliente.
        // Per testare il caso "cliente non trovato", impostare sqlcode = 100.
        sqlcode = 0; // Simuliamo che il cliente sia stato trovato.
        if (sqlcode == 0) {
            wsCliNome = "Mario";
            wsCliCognome = "Rossi";
        }

        if (sqlcode == 100) {
            System.out.println("Cliente non trovato!");
            return; // Esce dal metodo se il cliente non esiste.
        }
        if (sqlcode != 0) {
            System.out.println("Errore database: " + sqlcode);
            return; // Esce per altri errori DB.
        }

        System.out.println("Cliente: " + wsCliNome + " " + wsCliCognome);
        
        generaNumeroConto(); // Genera un nuovo numero di conto

        System.out.print("Tipo conto (C=Corrente, D=Deposito): ");
        wsConTipo = scanner.nextLine().toUpperCase();
        
        System.out.print("Importo iniziale: ");
        wsConSaldo = new BigDecimal(scanner.nextLine());
        
        System.out.print("Fido accordato: ");
        wsConFido = new BigDecimal(scanner.nextLine());

        // Imposta data corrente e stato 'Attivo'
        wsConDataApertura = LocalDate.now().format(DateTimeFormatter.ISO_LOCAL_DATE);
        wsConStato = "A";

        // ASG: execSqlStatement (INSERT su conti)
        // Simula l'inserimento del nuovo conto nel database.
        sqlcode = 0; // Simuliamo successo.

        if (sqlcode == 0) {
            System.out.println("Conto " + wsConNumero + " creato con successo!");
            // Se c'è un saldo iniziale, registra il primo movimento.
            if (wsConSaldo.compareTo(BigDecimal.ZERO) > 0) {
                wsTipoMovimento = "D";
                wsCausale = "Deposito iniziale";
                wsImporto = wsConSaldo;
                wsNumeroConto = wsConNumero;
                registraMovimento();
            }
        } else {
            System.out.println("Errore creazione conto: " + sqlcode);
        }
    }

    /**
     * Simula la generazione di un nuovo numero di conto.
     * @throws SQLException In caso di errori (simulati).
     */
    private void generaNumeroConto() throws SQLException {
        // ASG: execSqlStatement (SELECT per ottenere l'ultimo numero di conto)
        // In un'applicazione reale, questo metodo leggerebbe una sequenza dal DB.
        // Qui, generiamo un numero casuale per la simulazione.
        long randomNum = (long) (Math.random() * 1000000000L) + 1000000000L;
        wsConNumero = "IT" + randomNum;
        sqlcode = 0;
    }

    /**
     * Gestisce la logica per effettuare un deposito.
     * @throws SQLException In caso di errori (simulati).
     */
    private void deposito() throws SQLException {
        System.out.println(" ");
        System.out.println("=== DEPOSITO ===");
        System.out.print("Numero conto: ");
        wsNumeroConto = scanner.nextLine();
        
        verificaConto(); // Controlla se il conto esiste ed è attivo
        if ("KO".equals(wsEsito)) {
            return; // Esce se il conto non è valido
        }

        System.out.print("Importo deposito: ");
        wsImporto = new BigDecimal(scanner.nextLine());

        if (wsImporto.compareTo(BigDecimal.ZERO) <= 0) {
            System.out.println("Importo non valido!");
            return;
        }

        System.out.print("Causale: ");
        wsCausale = scanner.nextLine();

        // ASG: execSqlStatement (UPDATE saldo conto)
        // Simula l'aggiornamento del saldo nel DB.
        sqlcode = 0; // Simuliamo successo.

        if (sqlcode == 0) {
            wsTipoMovimento = "D";
            registraMovimento(); // Registra il movimento
            System.out.println("Deposito effettuato con successo!");
        } else {
            System.out.println("Errore durante il deposito: " + sqlcode);
        }
    }

    /**
     * Gestisce la logica per effettuare un prelievo.
     * @throws SQLException In caso di errori (simulati).
     */
    private void prelievo() throws SQLException {
        System.out.println(" ");
        System.out.println("=== PRELIEVO ===");
        System.out.print("Numero conto: ");
        wsNumeroConto = scanner.nextLine();

        verificaConto(); // Controlla se il conto esiste ed è attivo
        if ("KO".equals(wsEsito)) {
            return; // Esce se il conto non è valido
        }

        System.out.print("Importo prelievo: ");
        wsImporto = new BigDecimal(scanner.nextLine());

        if (wsImporto.compareTo(BigDecimal.ZERO) <= 0) {
            System.out.println("Importo non valido!");
            return;
        }

        // Controlla la disponibilità: Saldo - Prelievo >= -Fido
        if (wsConSaldo.subtract(wsImporto).compareTo(wsConFido.negate()) < 0) {
            System.out.println("Fondi insufficienti!");
            System.out.println("Saldo attuale: " + wsConSaldo);
            System.out.println("Fido disponibile: " + wsConFido);
            return;
        }

        System.out.print("Causale: ");
        wsCausale = scanner.nextLine();

        // ASG: execSqlStatement (UPDATE saldo conto)
        // Simula l'aggiornamento del saldo nel DB.
        sqlcode = 0; // Simuliamo successo.

        if (sqlcode == 0) {
            wsTipoMovimento = "P";
            registraMovimento(); // Registra il movimento
            System.out.println("Prelievo effettuato con successo!");
        } else {
            System.out.println("Errore durante il prelievo: " + sqlcode);
        }
    }

    /**
     * Visualizza il saldo e la disponibilità di un conto.
     * @throws SQLException In caso di errori (simulati).
     */
    private void visualizzaSaldo() throws SQLException {
        System.out.println(" ");
        System.out.println("=== VISUALIZZA SALDO ===");
        System.out.print("Numero conto: ");
        wsNumeroConto = scanner.nextLine();

        // ASG: execSqlStatement (SELECT su conti e clienti)
        // La simulazione avviene dentro verificaConto()
        verificaConto();

        if ("OK".equals(wsEsito)) {
            System.out.println(" ");
            System.out.println("Intestatario: " + wsCliNome + " " + wsCliCognome);
            
            wsSaldoEdit = wsConSaldo;
            System.out.println("Saldo attuale: EUR " + wsSaldoEdit.setScale(2, RoundingMode.HALF_UP));
            
            wsSaldoEdit = wsConFido;
            System.out.println("Fido accordato: EUR " + wsSaldoEdit.setScale(2, RoundingMode.HALF_UP));
            
            wsSaldo = wsConSaldo.add(wsConFido); // Disponibilità totale
            wsSaldoEdit = wsSaldo;
            System.out.println("Disponibile: EUR " + wsSaldoEdit.setScale(2, RoundingMode.HALF_UP));
        }
        // Gli errori sono già stampati da verificaConto()
    }

    /**
     * Genera un file di testo con l'estratto conto.
     * @throws SQLException In caso di errori (simulati).
     * @throws IOException In caso di problemi di scrittura del file.
     */
    private void estrattoConto() throws SQLException, IOException {
        System.out.println(" ");
        System.out.println("=== ESTRATTO CONTO ===");
        System.out.print("Numero conto: ");
        wsNumeroConto = scanner.nextLine();

        verificaConto();
        if ("KO".equals(wsEsito)) {
            return;
        }

        String fileName = "ESTRATTO-CONTO.TXT";
        try (PrintWriter reportFile = new PrintWriter(new FileWriter(fileName))) {
            String wsTitoloReport = "ESTRATTO CONTO";
            String wsLineaSeparatore = "--------------------------------------------------------------------------------";

            reportFile.println(wsTitoloReport);
            reportFile.println(wsLineaSeparatore);
            reportFile.println("Conto: " + wsNumeroConto + "    Cliente: " + wsCliNome + " " + wsCliCognome);
            reportFile.println("Data: " + LocalDate.now() + "    Ora: " + LocalDateTime.now().format(DateTimeFormatter.ofPattern("HH:mm:ss")));
            reportFile.println(wsLineaSeparatore);
            reportFile.printf("%-20s %-5s %12s   %-25s %12s%n", "DATA/ORA", "TIPO", "IMPORTO", "CAUSALE", "SALDO DOPO");
            reportFile.println(wsLineaSeparatore);

            // ASG: execSqlStatement (OPEN CURSOR per movimenti)
            // Simula la lettura di un cursore con i movimenti del conto.
            
            // Simulazione Movimento 1
            sqlcode = 0;
            wsMovData = "2023-10-27 10:00:00";
            wsMovTipo = "D";
            wsMovImporto = new BigDecimal("1200.50");
            wsMovCausale = "Accredito stipendio";
            wsMovSaldoDopo = new BigDecimal("1200.50");
            scriviMovimentoReport(reportFile);

            // Simulazione Movimento 2
            sqlcode = 0;
            wsMovData = "2023-10-28 15:30:10";
            wsMovTipo = "P";
            wsMovImporto = new BigDecimal("-75.00");
            wsMovCausale = "Pagamento bolletta luce";
            wsMovSaldoDopo = new BigDecimal("1125.50");
            scriviMovimentoReport(reportFile);
            
            // Simulazione fine dati
            sqlcode = 100;

            reportFile.println(wsLineaSeparatore);
            wsSaldoEdit = wsConSaldo; // Usa il saldo attuale del conto
            reportFile.println("SALDO FINALE: EUR " + wsSaldoEdit.setScale(2, RoundingMode.HALF_UP));
        }

        System.out.println("Estratto conto salvato in " + fileName);
    }

    /**
     * Scrive una singola riga di movimento nel file di report.
     * @param reportFile Il PrintWriter per scrivere sul file.
     * @throws SQLException In caso di errori (simulati).
     */
    private void scriviMovimentoReport(PrintWriter reportFile) throws SQLException {
        String tipoMovStr;
        switch (wsMovTipo) {
            case "D": tipoMovStr = "DEP"; break;
            case "P": tipoMovStr = "PRE"; break;
            case "B": tipoMovStr = "BON"; break;
            default:  tipoMovStr = "???"; break;
        }

        reportFile.printf("%-20s %-5s %12.2f   %-25s %12.2f%n",
            wsMovData,
            tipoMovStr,
            wsMovImporto,
            wsMovCausale.length() > 25 ? wsMovCausale.substring(0, 25) : wsMovCausale,
            wsMovSaldoDopo
        );
    }

    /**
     * Gestisce la logica per la chiusura di un conto.
     * @throws SQLException In caso di errori (simulati).
     */
    private void chiusuraConto() throws SQLException {
        System.out.println(" ");
        System.out.println("=== CHIUSURA CONTO ===");
        System.out.print("Numero conto da chiudere: ");
        wsNumeroConto = scanner.nextLine();

        verificaConto();
        if ("KO".equals(wsEsito)) {
            return;
        }

        if (wsConSaldo.compareTo(BigDecimal.ZERO) != 0) {
            System.out.println("Impossibile chiudere: saldo non zero!");
            wsSaldoEdit = wsConSaldo;
            System.out.println("Saldo attuale: EUR " + wsSaldoEdit.setScale(2, RoundingMode.HALF_UP));
            return;
        }

        System.out.print("Confermare chiusura conto (S/N): ");
        wsContinua = scanner.nextLine();

        if ("S".equalsIgnoreCase(wsContinua)) {
            // ASG: execSqlStatement (UPDATE per impostare lo stato del conto a 'C')
            sqlcode = 0; // Simuliamo successo

            if (sqlcode == 0) {
                System.out.println("Conto chiuso con successo!");
            } else {
                System.out.println("Errore chiusura conto: " + sqlcode);
            }
        } else {
            System.out.println("Chiusura annullata");
        }
    }

    /**
     * Verifica l'esistenza e lo stato di un conto.
     * Imposta la variabile 'wsEsito' a "OK" o "KO".
     * @throws SQLException In caso di errori (simulati).
     */
    private void verificaConto() throws SQLException {
        wsEsito = "OK";

        // ASG: execSqlStatement (SELECT su conti e clienti)
        // Simula una SELECT per trovare i dati del conto e del cliente associato.
        // Per testare "conto non trovato", impostare sqlcode = 100.
        // Per testare "conto non attivo", impostare sqlcode = 0 e wsConStato = "C".
        sqlcode = 0; // Simuliamo successo
        if (sqlcode == 0) {
            // Popoliamo i campi con dati fittizi come se li avessimo letti dal DB
            this.wsConCliente = "C001";
            this.wsConTipo = "C";
            this.wsConSaldo = new BigDecimal("540.75");
            this.wsConDataApertura = "2022-01-15";
            this.wsConStato = "A"; // 'A' per Attivo
            this.wsConFido = new BigDecimal("500.00");
            this.wsCliNome = "Mario";
            this.wsCliCognome = "Rossi";
        }

        if (sqlcode == 100) {
            System.out.println("Conto non trovato!");
            wsEsito = "KO";
        } else if (sqlcode != 0) {
            System.out.println("Errore database: " + sqlcode);
            wsEsito = "KO";
        } else if (!"A".equals(wsConStato)) {
            System.out.println("Conto non attivo!");
            wsEsito = "KO";
        }
        // NOTA: La struttura if nidificata e ridondante dell'ASG è stata
        // semplificata in una catena if-else if-else logicamente equivalente.
    }

    /**
     * Simula la registrazione di un movimento nel database.
     * @throws SQLException In caso di errori (simulati).
     */
    private void registraMovimento() throws SQLException {
        // ASG: execSqlStatement (INSERT su movimenti)
        // Questo metodo simulerebbe l'inserimento di un record nella tabella dei movimenti.
        // In questa simulazione, non esegue azioni concrete oltre a impostare sqlcode.
        sqlcode = 0; // Simuliamo successo.
        if (sqlcode != 0) {
            System.out.println("Errore registrazione movimento: " + sqlcode);
        }
    }

    /**
     * Punto di ingresso principale dell'applicazione.
     * @param args Argomenti della riga di comando (non utilizzati).
     */
    public static void main(String[] args) {
        GestioneConti app = new GestioneConti();
        try {
            app.mainLogic();
        } catch (Exception e) {
            System.err.println("Si è verificato un errore imprevisto: " + e.getMessage());
            e.printStackTrace();
        }
    }
}