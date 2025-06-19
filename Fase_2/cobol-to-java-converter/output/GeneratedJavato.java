import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Scanner;

/**
 * Classe Java generata dal programma COBOL GESTIONE-CONTI.
 */
public class GestioneConti {

    private String wsScelta = "0";
    private String wsContinua = "S";
    private String wsMessaggio;
    private String wsEsito;
    private String wsNumeroConto;
    private String wsCodiceCliente;
    private BigDecimal wsImporto = BigDecimal.ZERO;
    private BigDecimal wsSaldo = BigDecimal.ZERO;
    private String wsSaldoEdit;
    private String wsTipoMovimento;
    private String wsCausale;
    private String wsDataSistema;
    private String wsOraSistema;
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
    private String wsTitoloReport = "                                                  ESTRATTO CONTO BANCARIO                                                  ";
    private String wsLineaSeparatore = "--------------------------------------------------------------------------------------------------------------------------------";


    private final Scanner scanner = new Scanner(System.in);
    private int sqlcode;

    /**
     * Metodo principale del programma, equivalente al paragrafo MAIN-LOGIC in COBOL.
     */
    private void mainLogic() {
        connettiDatabase();
        do {
            visualizzaMenu();
            elaboraScelta();
            System.out.println(" ");
            System.out.print("Continuare? (S/N): ");
            wsContinua = scanner.nextLine();
        } while (!wsContinua.equalsIgnoreCase("N"));
        disconnettiDatabase();
        System.out.println("Programma terminato.");
    }

    private void connettiDatabase() {
        // ASG: execSqlStatement (CONNECT TO database)
        sqlcode = 0;
        if (sqlcode != 0) {
            System.out.println("Errore connessione database: " + sqlcode);
            System.exit(1);
        } else {
            System.out.println("Connessione al database stabilita");
        }
    }

    private void disconnettiDatabase() {
        // ASG: execSqlStatement (DISCONNECT ALL)
        System.out.println("Disconnesso dal database");
    }

    private void visualizzaMenu() {
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

    private void elaboraScelta() {
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

    private void aperturaConto() {
        System.out.println(" ");
        System.out.println("=== APERTURA NUOVO CONTO ===");
        System.out.print("Codice cliente: ");
        wsConCliente = scanner.nextLine();

        // ASG: execSqlStatement (SELECT su CLIENTI)
        sqlcode = 0;
        if (sqlcode == 0) {
            wsCliCodice = "COD001";
            wsCliNome = "Mario";
            wsCliCognome = "Rossi";
        } else if (sqlcode == 100) {
            System.out.println("Cliente non trovato!");
        } else {
            System.out.println("Errore database: " + sqlcode);
        }


        if (sqlcode == 0) {
            System.out.println("Cliente: " + wsCliNome + " " + wsCliCognome);
            generaNumeroConto();
            System.out.print("Tipo conto (C=Corrente, D=Deposito): ");
            wsConTipo = scanner.nextLine();
            System.out.print("Importo iniziale: ");
            wsConSaldo = new BigDecimal(scanner.nextLine());
            System.out.print("Fido accordato: ");
            wsConFido = new BigDecimal(scanner.nextLine());

            LocalDateTime now = LocalDateTime.now();
            DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd");
            wsConDataApertura = now.format(formatter);

            wsConStato = "A";

            // ASG: execSqlStatement (INSERT su CONTI)
            sqlcode = 0;
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

    }


    private void generaNumeroConto() {
        // ASG: execSqlStatement (Generazione numero conto)
        wsConNumero = "IT0000000001";
    }

    private void deposito() {
        System.out.println(" ");
        System.out.println("=== DEPOSITO ===");
        System.out.print("Numero conto: ");
        wsNumeroConto = scanner.nextLine();
        verificaConto();
        if (wsEsito.equals("KO")) {
            return;
        }
        System.out.print("Importo deposito: ");
        wsImporto = new BigDecimal(scanner.nextLine());

        if (wsImporto.compareTo(BigDecimal.ZERO) <= 0) {
            System.out.println("Importo non valido!");
            return; // o gestione errore più appropriata
        }

        System.out.print("Causale: ");
        wsCausale = scanner.nextLine();

        // ASG: execSqlStatement (UPDATE su CONTI)
        sqlcode = 0;
        if (sqlcode == 0) {
            wsTipoMovimento = "D";
            registraMovimento();
            System.out.println("Deposito effettuato con successo!");
        } else {
            System.out.println("Errore durante il deposito: " + sqlcode);
        }
    }


    private void prelievo() {
        System.out.println(" ");
        System.out.println("=== PRELIEVO ===");
        System.out.print("Numero conto: ");
        wsNumeroConto = scanner.nextLine();
        verificaConto();
        if (wsEsito.equals("KO")) {
            return;
        }

        System.out.print("Importo prelievo: ");
        wsImporto = new BigDecimal(scanner.nextLine());
        if (wsImporto.compareTo(BigDecimal.ZERO) <= 0) {
            System.out.println("Importo non valido!");
            return;
        }

        // ASG: execSqlStatement (SELECT saldo e fido da CONTI)
        sqlcode = 0;
        if (sqlcode == 0) {
            wsConSaldo = new BigDecimal("1000.00");
            wsConFido = new BigDecimal("500.00");
        } else {
            System.out.println("Errore durante il prelievo (lettura saldo): " + sqlcode);
            return;
        }

        if (wsConSaldo.subtract(wsImporto).compareTo(wsConFido.negate()) < 0) {
            System.out.println("Fondi insufficienti!");
            System.out.println("Saldo attuale: " + wsConSaldo);
            System.out.println("Fido disponibile: " + wsConFido);
            return;
        }

        System.out.print("Causale: ");
        wsCausale = scanner.nextLine();

        // ASG: execSqlStatement (UPDATE su CONTI)
        sqlcode = 0;
        if (sqlcode == 0) {
            wsTipoMovimento = "P";
            registraMovimento();
            System.out.println("Prelievo effettuato con successo!");
        } else {
            System.out.println("Errore durante il prelievo: " + sqlcode);
        }
    }

    private void visualizzaSaldo() {
        System.out.println(" ");
        System.out.println("=== VISUALIZZA SALDO ===");
        System.out.print("Numero conto: ");
        wsNumeroConto = scanner.nextLine();

        // ASG: execSqlStatement (SELECT su CONTI e CLIENTI)
        sqlcode = 0;
        if (sqlcode == 0) {
            wsConSaldo = new BigDecimal("1500.50");
            wsConFido = new BigDecimal("500.00");
            wsCliNome = "Mario";
            wsCliCognome = "Rossi";

            System.out.println(" ");
            System.out.println("Intestatario: " + wsCliNome + " " + wsCliCognome);

            wsSaldoEdit = String.format("%,.2f", wsConSaldo);
            System.out.println("Saldo attuale: EUR " + wsSaldoEdit);

            wsSaldoEdit = String.format("%,.2f", wsConFido);
            System.out.println("Fido accordato: EUR " + wsSaldoEdit);

            wsSaldo = wsConSaldo.add(wsConFido);
            wsSaldoEdit = String.format("%,.2f", wsSaldo);
            System.out.println("Disponibile: EUR " + wsSaldoEdit);

        } else if (sqlcode == 100) {
            System.out.println("Conto non trovato o non attivo!");
        } else {
            System.out.println("Errore database: " + sqlcode);
        }
    }

    private void estrattoConto() {
        System.out.println(" ");
        System.out.println("=== ESTRATTO CONTO ===");
        System.out.print("Numero conto: ");
        wsNumeroConto = scanner.nextLine();
        verificaConto();
        if (wsEsito.equals("KO")) {
            return;
        }

        try (FileWriter fileWriter = new FileWriter("ESTRATTO-CONTO.TXT");
             PrintWriter printWriter = new PrintWriter(fileWriter)) {

            printWriter.println(wsTitoloReport);
            printWriter.println(wsLineaSeparatore);

            String intestazione = "Conto: " + wsNumeroConto + "    Cliente: " + wsCliNome + " " + wsCliCognome;
            printWriter.println(intestazione);


            LocalDateTime now = LocalDateTime.now();
            DateTimeFormatter dateFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd");
            DateTimeFormatter timeFormatter = DateTimeFormatter.ofPattern("HH:mm:ss");
            String data = now.format(dateFormatter);
            String ora = now.format(timeFormatter);

            String dataOra = "Data: " + data + "    Ora: " + ora;
            printWriter.println(dataOra);

            printWriter.println(" ");
            printWriter.println(wsLineaSeparatore);
            printWriter.println("DATA/ORA            TIPO      IMPORTO     CAUSALE                    SALDO DOPO");
            printWriter.println(wsLineaSeparatore);


            // ASG: execSqlStatement (OPEN CUR-MOVIMENTI)
            sqlcode = 0;
            int fetchCount = 0;

            do {
                // ASG: execSqlStatement (FETCH CUR-MOVIMENTI)
                if (fetchCount < 3) {
                    sqlcode = 0;
                    wsMovData = "2024-01-15 10:30:00";
                    wsMovTipo = "D";
                    wsMovImporto = new BigDecimal("100.00");
                    wsMovCausale = "Accredito stipendio";
                    wsMovSaldoDopo = new BigDecimal("2000.00");
                    fetchCount++;
                } else {
                    sqlcode = 100;
                }

                if (sqlcode == 0) {
                    scriviMovimentoReport(printWriter);
                }
            } while (sqlcode == 0);

            // ASG: execSqlStatement (CLOSE CUR-MOVIMENTI)


            printWriter.println(wsLineaSeparatore);
            wsSaldoEdit = String.format("%,.2f", wsConSaldo);
            printWriter.println("SALDO FINALE: EUR " + wsSaldoEdit);


            System.out.println("Estratto conto salvato in ESTRATTO-CONTO.TXT");

        } catch (IOException e) {
            System.out.println("Errore durante la scrittura del file: " + e.getMessage());
        }
    }


    private void scriviMovimentoReport(PrintWriter printWriter) {
        String record = wsMovData + "  ";

        switch (wsMovTipo) {
            case "D":
                record += "DEP  ";
                break;
            case "P":
                record += "PRE  ";
                break;
            case "B":
                record += "BON  ";
                break;
            default:
                record += "???  ";
        }

        wsSaldoEdit = String.format("%,.2f", wsMovImporto);
        record += wsSaldoEdit + " ";

        record += wsMovCausale.substring(0, Math.min(wsMovCausale.length(), 30)) + " ";

        wsSaldoEdit = String.format("%,.2f", wsMovSaldoDopo);
        record += wsSaldoEdit;

        printWriter.println(record);
    }


    private void chiusuraConto() {
        System.out.println(" ");
        System.out.println("=== CHIUSURA CONTO ===");
        System.out.print("Numero conto da chiudere: ");
        wsNumeroConto = scanner.nextLine();
        verificaConto();
        if (wsEsito.equals("KO")) {
            return;
        }

        if (wsConSaldo.compareTo(BigDecimal.ZERO) != 0) {
            System.out.println("Impossibile chiudere: saldo non zero!");
            wsSaldoEdit = String.format("%,.2f", wsConSaldo);
            System.out.println("Saldo attuale: EUR " + wsSaldoEdit);
            return;
        }

        System.out.print("Confermare chiusura conto (S/N): ");
        wsContinua = scanner.nextLine();

        if (wsContinua.equalsIgnoreCase("S")) {
            // ASG: execSqlStatement (UPDATE su CONTI - chiusura)
            sqlcode = 0;
            if (sqlcode == 0) {
                System.out.println("Conto chiuso con successo!");
            } else {
                System.out.println("Errore chiusura conto: " + sqlcode);
            }
        } else {
            System.out.println("Chiusura annullata");
        }
    }

    private void verificaConto() {
        wsEsito = "OK";

        // ASG: execSqlStatement (SELECT su CONTI e CLIENTI)
        sqlcode = 0;
        if (sqlcode == 0) {
            wsConSaldo = new BigDecimal("1500.50");
            wsConStato = "A";
            wsCliNome = "Mario";
            wsCliCognome = "Rossi";
        } else if (sqlcode == 100) {
            System.out.println("Conto non trovato!");
            wsEsito = "KO";
        } else {
            System.out.println("Errore database: " + sqlcode);
            wsEsito = "KO";
        }


        if (!wsEsito.equals("KO")) {
            if (!wsConStato.equals("A")) {
                System.out.println("Conto non attivo!");
                wsEsito = "KO";
            }
        }
    }


    private void registraMovimento() {
        // ASG: execSqlStatement (SELECT saldo da CONTI)
        // ASG: execSqlStatement (INSERT su MOVIMENTI)
        sqlcode = 0;
        if (sqlcode == 0) {
            wsSaldo = new BigDecimal("1600.50"); // Simulazione saldo aggiornato
        } else {
            System.out.println("Errore durante la registrazione del movimento: " + sqlcode);
        }
    }


    public static void main(String[] args) {
        GestioneConti gc = new GestioneConti();
        gc.mainLogic();
    }
}