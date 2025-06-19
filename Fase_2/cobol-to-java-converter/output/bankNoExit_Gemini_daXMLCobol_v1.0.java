package com.generated;

import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.math.BigDecimal;
import java.sql.SQLException;
import java.util.Scanner;

/**
 * Classe generata dal programma COBOL GESTIONE-CONTI.
 * Gestisce le operazioni sui conti correnti.
 */
public class GestioneConti {

    private java.sql.Connection connection;
    private int sqlcode;
    private final Scanner scanner = new Scanner(System.in);
    private String wsCliCodice;
    private String wsCliNome;
    private String wsCliCognome;
    private String wsCliCf;
    private String wsCliDataNascita;
    private String wsCliIndirizzo;
    private String wsCliCitta;
    private String wsCliCap;
    private String wsCliTelefono;
    private String wsCliEmail;
    private String wsConNumero;
    private String wsConCliente;
    private String wsConTipo;
    private BigDecimal wsConSaldo = BigDecimal.ZERO;
    private String wsConDataApertura;
    private String wsConStato;
    private BigDecimal wsConFido = BigDecimal.ZERO;
    private String wsMovData;
    private String wsMovTipo;
    private BigDecimal wsMovImporto = BigDecimal.ZERO;
    private String wsMovCausale;
    private BigDecimal wsMovSaldoDopo = BigDecimal.ZERO;
    private String wsScelta;
    private String wsContinua;
    private String wsEsito;
    private String wsNumeroConto;
    private String wsTipoMovimento;
    private String wsCausale;
    private BigDecimal wsImporto = BigDecimal.ZERO;
    private BigDecimal wsSaldo = BigDecimal.ZERO;
    private BigDecimal wsSaldoEdit = BigDecimal.ZERO;
    private PrintWriter reportFile;
    private String wsTitoloReport = "===== ESTRATTO CONTO =====";
    private String wsLineaSeparatore = "==========================================";
    private String reportRecord;


    public String getWsCliCodice() {
        return wsCliCodice;
    }

    public void setWsCliCodice(String value) {
        this.wsCliCodice = value;
    }

    public String getWsCliNome() {
        return wsCliNome;
    }

    public void setWsCliNome(String value) {
        this.wsCliNome = value;
    }

    public String getWsCliCognome() {
        return wsCliCognome;
    }

    public void setWsCliCognome(String value) {
        this.wsCliCognome = value;
    }

    public String getWsCliCf() {
        return wsCliCf;
    }

    public void setWsCliCf(String value) {
        this.wsCliCf = value;
    }

    public String getWsCliDataNascita() {
        return wsCliDataNascita;
    }


    public void setWsCliDataNascita(String value) {
        this.wsCliDataNascita = value;
    }

    public String getWsCliIndirizzo() {
        return wsCliIndirizzo;
    }

    public void setWsCliIndirizzo(String value) {
        this.wsCliIndirizzo = value;
    }


    public String getWsCliCitta() {
        return wsCliCitta;
    }

    public void setWsCliCitta(String value) {
        this.wsCliCitta = value;
    }

    public String getWsCliCap() {
        return wsCliCap;
    }


    public void setWsCliCap(String value) {
        this.wsCliCap = value;
    }


    public String getWsCliTelefono() {
        return wsCliTelefono;
    }


    public void setWsCliTelefono(String value) {
        this.wsCliTelefono = value;
    }


    public String getWsCliEmail() {
        return wsCliEmail;
    }


    public void setWsCliEmail(String value) {
        this.wsCliEmail = value;
    }


    public String getWsConNumero() {
        return wsConNumero;
    }


    public void setWsConNumero(String value) {
        this.wsConNumero = value;
    }


    public String getWsConCliente() {
        return wsConCliente;
    }


    public void setWsConCliente(String value) {
        this.wsConCliente = value;
    }


    public String getWsConTipo() {
        return wsConTipo;
    }


    public void setWsConTipo(String value) {
        this.wsConTipo = value;
    }


    public BigDecimal getWsConSaldo() {
        return wsConSaldo;
    }


    public void setWsConSaldo(BigDecimal value) {
        this.wsConSaldo = value;
    }


    public String getWsConDataApertura() {
        return wsConDataApertura;
    }


    public void setWsConDataApertura(String value) {
        this.wsConDataApertura = value;
    }


    public String getWsConStato() {
        return wsConStato;
    }


    public void setWsConStato(String value) {
        this.wsConStato = value;
    }


    public BigDecimal getWsConFido() {
        return wsConFido;
    }


    public void setWsConFido(BigDecimal value) {
        this.wsConFido = value;
    }


    public String getWsMovData() {
        return wsMovData;
    }


    public void setWsMovData(String value) {
        this.wsMovData = value;
    }


    public String getWsMovTipo() {
        return wsMovTipo;
    }


    public void setWsMovTipo(String value) {
        this.wsMovTipo = value;
    }


    public BigDecimal getWsMovImporto() {
        return wsMovImporto;
    }


    public void setWsMovImporto(BigDecimal value) {
        this.wsMovImporto = value;
    }


    public String getWsMovCausale() {
        return wsMovCausale;
    }


    public void setWsMovCausale(String value) {
        this.wsMovCausale = value;
    }


    public BigDecimal getWsMovSaldoDopo() {
        return wsMovSaldoDopo;
    }


    public void setWsMovSaldoDopo(BigDecimal value) {
        this.wsMovSaldoDopo = value;
    }


    /**
     * Metodo principale del programma COBOL, equivalente al paragrafo MAIN-LOGIC.
     * Gestisce il ciclo principale del programma.
     *
     * @throws SQLException In caso di errori di database simulati.
     */
    private void mainLogic() throws SQLException {
        connettiDatabase();
        visualizzaMenu();

        do {
            elaboraScelta();
            System.out.println(" ");
            System.out.println("Continuare? (S/N): ");
            wsContinua = scanner.nextLine();
        } while (wsContinua.equalsIgnoreCase("S"));

        disconnettiDatabase();
        System.exit(0);
    }


    private void connettiDatabase() throws SQLException {
        // ASG: execSqlStatement (Connessione al database)
        sqlcode = 0; // Simulazione connessione riuscita

        if (sqlcode != 0) {
            System.out.println("Errore connessione database: " + sqlcode);
            System.exit(0);
        } else {
            System.out.println("Connessione al database stabilita");
        }
    }


    private void disconnettiDatabase() throws SQLException {
        System.out.println("Disconnesso dal database");
    }


    private void visualizzaMenu() throws SQLException {
        System.out.println(" ");
        System.out.println("===== SISTEMA GESTIONE CONTI CORRENTI ======");
        System.out.println("1. Apertura nuovo conto");
        System.out.println("2. Deposito");
        System.out.println("3. Prelievo");
        System.out.println("4. Visualizza saldo");
        System.out.println("5. Estratto conto");
        System.out.println("6. Chiusura conto");
        System.out.println("0. Esci");
        System.out.println("===========================================");
        System.out.println("Scelta: ");
        wsScelta = scanner.nextLine();
    }


    private void elaboraScelta() throws SQLException {
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
                wsContinua = "N";
                break;
            default:
                System.out.println("Scelta non valida!");
        }
    }


    private void aperturaConto() throws SQLException {
        System.out.println(" ");
        System.out.println("=== APERTURA NUOVO CONTO ===");
        System.out.println("Codice cliente: ");
        wsConCliente = scanner.nextLine();

        // ASG: execSqlStatement (SELECT su cliente)
        sqlcode = 0; // Simulazione cliente trovato
        if (sqlcode == 100) {
            System.out.println("Cliente non trovato!");
        }


        if (sqlcode != 0) {
            System.out.println("Errore database: " + sqlcode);

        }

        wsCliNome = "Mario"; // Simulazione dati cliente
        wsCliCognome = "Rossi";
        System.out.println("Cliente: " + wsCliNome + " " + wsCliCognome);

        generaNumeroConto();

        System.out.println("Tipo conto (C=Corrente, D=Deposito): ");
        wsConTipo = scanner.nextLine();
        System.out.println("Importo iniziale: ");
        wsConSaldo = new BigDecimal(scanner.nextLine());
        System.out.println("Fido accordato: ");
        wsConFido = new BigDecimal(scanner.nextLine());

        wsConDataApertura = "1";
        wsConStato = "A";

        // ASG: execSqlStatement (INSERT su conto)
        sqlcode = 0; // Simulazione conto creato

        if (sqlcode == 0) {
            System.out.println("Conto " + wsConNumero + " creato con successo!");
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


    private void generaNumeroConto() throws SQLException {
        // ASG: execSqlStatement (Generazione numero conto)
        wsConNumero = "1234567890"; // Simulazione numero conto generato
        sqlcode = 0;
    }

    private void deposito() throws SQLException {
        System.out.println(" ");
        System.out.println("=== DEPOSITO ===");
        System.out.println("Numero conto: ");
        wsNumeroConto = scanner.nextLine();
        verificaConto();
        if (wsEsito.equals("KO")) {
            return; // Esci se il conto non è valido
        }

        System.out.println("Importo deposito: ");
        wsImporto = new BigDecimal(scanner.nextLine());

        if (wsImporto.compareTo(BigDecimal.ZERO) == 0) {
            System.out.println("Importo non valido!");
            return; // Esci se l'importo non è valido
        }


        System.out.println("Causale: ");
        wsCausale = scanner.nextLine();

        // ASG: execSqlStatement (UPDATE su conto e INSERT su movimento)
        sqlcode = 0; // Simulazione deposito riuscito

        if (sqlcode == 0) {
            wsTipoMovimento = "D";
            registraMovimento();
            System.out.println("Deposito effettuato con successo!");
        } else {
            System.out.println("Errore durante il deposito: " + sqlcode);
        }
    }


    private void prelievo() throws SQLException {
        System.out.println(" ");
        System.out.println("=== PRELIEVO ===");
        System.out.println("Numero conto: ");
        wsNumeroConto = scanner.nextLine();

        verificaConto();
        if (wsEsito.equals("KO")) {
            return;
        }

        System.out.println("Importo prelievo: ");
        wsImporto = new BigDecimal(scanner.nextLine());

        if (wsImporto.compareTo(BigDecimal.ZERO) == 0) {
            System.out.println("Importo non valido!");
            return;
        }

        if (wsConSaldo.subtract(wsImporto).compareTo(wsConFido.negate()) < 0) {
            System.out.println("Fondi insufficienti!");
            System.out.println("Saldo attuale: " + wsConSaldo);
            System.out.println("Fido disponibile: " + wsConFido);
            return;
        }

        System.out.println("Causale: ");
        wsCausale = scanner.nextLine();

        // ASG: execSqlStatement (UPDATE su conto e INSERT su movimento)
        sqlcode = 0; // Simulazione prelievo riuscito

        if (sqlcode == 0) {
            wsTipoMovimento = "P";
            registraMovimento();
            System.out.println("Prelievo effettuato con successo!");
        } else {
            System.out.println("Errore durante il prelievo: " + sqlcode);
        }
    }


    private void visualizzaSaldo() throws SQLException {
        System.out.println(" ");
        System.out.println("=== VISUALIZZA SALDO ===");
        System.out.println("Numero conto: ");
        wsNumeroConto = scanner.nextLine();

        // ASG: execSqlStatement (SELECT su conto e cliente)
        sqlcode = 0; // Simulazione conto trovato

        if (sqlcode == 0) {
            wsCliNome = "Mario"; // Simulazione dati cliente
            wsCliCognome = "Rossi";
            wsConSaldo = new BigDecimal("1500.50"); // Simulazione saldo
            wsConFido = new BigDecimal("500.00"); // Simulazione fido

            System.out.println(" ");
            System.out.println("Intestatario: " + wsCliNome + " " + wsCliCognome);
            wsSaldoEdit = wsConSaldo;
            System.out.println("Saldo attuale: EUR " + wsSaldoEdit);
            wsSaldoEdit = wsConFido;
            System.out.println("Fido accordato: EUR " + wsSaldoEdit);
            wsSaldo = wsConSaldo.add(wsConFido);
            wsSaldoEdit = wsSaldo;
            System.out.println("Disponibile: EUR " + wsSaldoEdit);
        } else {
            if (sqlcode == 100) {
                System.out.println("Conto non trovato o non attivo!");
            } else {
                System.out.println("Errore database: " + sqlcode);
            }
        }
    }


    private void estrattoConto() throws SQLException {
        System.out.println(" ");
        System.out.println("=== ESTRATTO CONTO ===");
        System.out.println("Numero conto: ");
        wsNumeroConto = scanner.nextLine();

        verificaConto();
        if (wsEsito.equals("KO")) {
            return;
        }

        try (PrintWriter reportFile = new PrintWriter(new FileWriter("ESTRATTO-CONTO.TXT"))) {
            this.reportFile = reportFile;
            reportFile.println(wsTitoloReport);
            reportFile.println(wsLineaSeparatore);
            reportRecord = "Conto: " + wsNumeroConto + "    Cliente: " + wsCliNome + " " + wsCliCognome;
            reportFile.println(reportRecord);
            reportRecord = "Data: " + 1 + "    Ora: " + 12;
            reportFile.println(reportRecord);
            reportFile.println(wsLineaSeparatore);
            reportRecord = "DATA/ORA            TIPO      IMPORTO     CAUSALE                    SALDO DOPO";
            reportFile.println(reportRecord);
            reportFile.println(wsLineaSeparatore);

            // ASG: execSqlStatement (Apertura cursore movimenti)
            sqlcode = 0;
            int rowCount = 0;
            while (sqlcode == 0 && rowCount < 3) { // Simulazione fetch da cursore
                wsMovData = "20240126"; // Simulazione dati movimento
                wsMovTipo = "D";
                wsMovImporto = new BigDecimal("100.00");
                wsMovCausale = "Deposito";
                wsMovSaldoDopo = new BigDecimal("1600.50");
                scriviMovimentoReport();

                // ASG: execSqlStatement (FETCH cursore movimenti)
                sqlcode = (rowCount < 2) ? 0 : 100; // Simula fine cursore al terzo fetch
                rowCount++;
            }
            // ASG: execSqlStatement (Chiusura cursore movimenti)

            reportFile.println(wsLineaSeparatore);
            wsSaldoEdit = wsConSaldo;
            reportRecord = "SALDO FINALE: EUR " + wsSaldoEdit;
            reportFile.println(reportRecord);

            System.out.println("Estratto conto salvato in ESTRATTO-CONTO.TXT");

        } catch (IOException e) {
            System.err.println("Errore durante la scrittura del file: " + e.getMessage());
        }
    }


    private void scriviMovimentoReport() throws SQLException {
        reportRecord = " ";
        reportRecord = wsMovData + "  ";

        switch (wsMovTipo) {
            case "D":
                reportRecord += "DEP  ";
                break;
            case "P":
                reportRecord += "PRE  ";
                break;
            case "B":
                reportRecord += "BON  ";
                break;
            default:
                reportRecord += "???  ";
        }

        wsSaldoEdit = wsMovImporto;
        reportRecord += String.format("%10.2f", wsSaldoEdit) + " ";
        reportRecord += wsMovCausale + " ";
        wsSaldoEdit = wsMovSaldoDopo;
        reportRecord += String.format("%15.2f", wsSaldoEdit);

        reportFile.println(reportRecord);
    }


    private void chiusuraConto() throws SQLException {
        System.out.println(" ");
        System.out.println("=== CHIUSURA CONTO ===");
        System.out.println("Numero conto da chiudere: ");
        wsNumeroConto = scanner.nextLine();

        verificaConto();
        if (wsEsito.equals("KO")) {
            return;
        }

        if (wsConSaldo.compareTo(BigDecimal.ZERO) != 0) {
            System.out.println("Impossibile chiudere: saldo non zero!");
            wsSaldoEdit = wsConSaldo;
            System.out.println("Saldo attuale: EUR " + wsSaldoEdit);
            return;
        }

        System.out.println("Confermare chiusura conto (S/N): ");
        wsContinua = scanner.nextLine();

        if (wsContinua.equalsIgnoreCase("S")) {
            // ASG: execSqlStatement (UPDATE su conto)
            sqlcode = 0; // Simulazione chiusura riuscita

            if (sqlcode == 0) {
                System.out.println("Conto chiuso con successo!");
            } else {
                System.out.println("Errore chiusura conto: " + sqlcode);
            }
        } else {
            System.out.println("Chiusura annullata");
        }
    }


    private void verificaConto() throws SQLException {
        wsEsito = "OK";

        // ASG: execSqlStatement (SELECT su conto)
        sqlcode = 0; // Simulazione conto trovato e attivo

        if (sqlcode == 100) {
            System.out.println("Conto non trovato!");
            wsEsito = "KO";
        } else if (sqlcode != 0) {
            System.out.println("Errore database: " + sqlcode);
            wsEsito = "KO";
        } else if (!wsConStato.equals("A")) {
            System.out.println("Conto non attivo!");
            wsEsito = "KO";
        }
    }


    private void registraMovimento() throws SQLException {
        // ASG: execSqlStatement (INSERT su movimento)
        sqlcode = 0; // Simulazione movimento registrato
    }


    public static void main(String[] args) {
        try {
            GestioneConti app = new GestioneConti();
            app.mainLogic();
        } catch (SQLException e) {
            System.err.println("Errore SQL: " + e.getMessage());
        }
    }
}