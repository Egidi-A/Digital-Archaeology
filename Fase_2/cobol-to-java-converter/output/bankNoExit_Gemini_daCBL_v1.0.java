import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Scanner;

/**
 * Simulazione Java del programma COBOL GESTIONE-CONTI.
 * Sistema di gestione conti correnti bancari.
 * @author ANNALISA-EGIDI (COBOL)
 * @author GPT-4 (Java Translation)
 */
public class GestioneConti {

    private final Scanner scanner = new Scanner(System.in);
    private int sqlcode;

    private int wsScelta;
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

    private final String wsTitoloReport = "                                          ESTRATTO CONTO BANCARIO                                          ";
    private final String wsLineaSeparatore = "--------------------------------------------------------------------------------------------------------------------";


    public static void main(String[] args) {
        GestioneConti programma = new GestioneConti();
        programma.mainLogic();
    }

    /**
     * Logica principale del programma.
     */
    private void mainLogic() {
        connettiDatabase();

        do {
            visualizzaMenu();
            elaboraScelta();

            System.out.println(" ");
            System.out.print("Continuare? (S/N): ");
            wsContinua = scanner.nextLine();
        } while (wsContinua.equalsIgnoreCase("S"));

        disconnettiDatabase();
    }

    private void connettiDatabase() {
        // EXEC SQL CONNECT TO 'postgresql://localhost/banca' USER 'postgres' USING 'password' END-EXEC
        sqlcode = 0; // Simula connessione riuscita
        if (sqlcode != 0) {
            System.out.println("Errore connessione database: " + sqlcode);
            System.exit(1);
        } else {
            System.out.println("Connessione al database stabilita");
        }
    }

    private void disconnettiDatabase() {
        // EXEC SQL DISCONNECT ALL END-EXEC
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
        wsScelta = Integer.parseInt(scanner.nextLine());
    }

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

        // EXEC SQL SELECT codice_cliente, nome, cognome INTO :WS-CLI-CODICE, :WS-CLI-NOME, :WS-CLI-COGNOME FROM CLIENTI WHERE codice_cliente = :WS-CON-CLIENTE END-EXEC
        if (wsConCliente.equals("12345678")) { // Simula cliente esistente
            sqlcode = 0;
            wsCliCodice = "12345678";
            wsCliNome = "Mario";
            wsCliCognome = "Rossi";
        } else {
            sqlcode = 100; // Simula cliente non trovato
        }


        if (sqlcode == 100) {
            System.out.println("Cliente non trovato!");
        }

        if (sqlcode != 0 && sqlcode != 100) {
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

            wsConDataApertura = LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd"));
            wsConStato = "A";

            // EXEC SQL INSERT INTO CONTI (numero_conto, codice_cliente, tipo_conto, saldo, data_apertura, stato, fido) VALUES (:WS-CON-NUMERO, :WS-CON-CLIENTE, :WS-CON-TIPO, :WS-CON-SALDO, CURRENT_DATE, :WS-CON-STATO, :WS-CON-FIDO) END-EXEC
            sqlcode = 0; // Simula inserimento riuscito

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
        // EXEC SQL SELECT 'IT' || LPAD(CAST(COALESCE(MAX(CAST(SUBSTR(numero_conto, 3) AS INTEGER)), 0) + 1 AS VARCHAR), 10, '0') INTO :WS-CON-NUMERO FROM CONTI WHERE numero_conto LIKE 'IT%' END-EXEC
        wsConNumero = "IT0000000001"; // Simula generazione numero conto
        sqlcode = 0;
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
            return;
        }

        System.out.print("Causale: ");
        wsCausale = scanner.nextLine();

        // EXEC SQL UPDATE CONTI SET saldo = saldo + :WS-IMPORTO WHERE numero_conto = :WS-NUMERO-CONTO AND stato = 'A' END-EXEC
        sqlcode = 0; // Simula aggiornamento riuscito

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

        // EXEC SQL SELECT saldo, fido INTO :WS-CON-SALDO, :WS-CON-FIDO FROM CONTI WHERE numero_conto = :WS-NUMERO-CONTO AND stato = 'A' END-EXEC
        sqlcode = 0; // Simula selezione riuscita
        wsConSaldo = new BigDecimal("1000.00"); // Simula saldo
        wsConFido = new BigDecimal("200.00"); // Simula fido

        if (wsConSaldo.subtract(wsImporto).compareTo(wsConFido.negate()) < 0) {
            System.out.println("Fondi insufficienti!");
            System.out.println("Saldo attuale: " + wsConSaldo);
            System.out.println("Fido disponibile: " + wsConFido);
            return;
        }

        System.out.print("Causale: ");
        wsCausale = scanner.nextLine();

        // EXEC SQL UPDATE CONTI SET saldo = saldo - :WS-IMPORTO WHERE numero_conto = :WS-NUMERO-CONTO AND stato = 'A' END-EXEC
        sqlcode = 0; // Simula aggiornamento riuscito

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

        // EXEC SQL SELECT c.saldo, c.fido, cl.nome, cl.cognome INTO :WS-CON-SALDO, :WS-CON-FIDO, :WS-CLI-NOME, :WS-CLI-COGNOME FROM CONTI c JOIN CLIENTI cl ON c.codice_cliente = cl.codice_cliente WHERE c.numero_conto = :WS-NUMERO-CONTO AND c.stato = 'A' END-EXEC
        if (wsNumeroConto.equals("IT0000000001")) {
            sqlcode = 0;
            wsConSaldo = new BigDecimal("1500.50");
            wsConFido = new BigDecimal("500.00");
            wsCliNome = "Mario";
            wsCliCognome = "Rossi";
        } else {
            sqlcode = 100;
        }


        if (sqlcode == 0) {
            System.out.println(" ");
            System.out.println("Intestatario: " + wsCliNome + " " + wsCliCognome);
            wsSaldoEdit = String.format("%,.2f", wsConSaldo);
            System.out.println("Saldo attuale: EUR " + wsSaldoEdit);
            wsSaldoEdit = String.format("%,.2f", wsConFido);
            System.out.println("Fido accordato: EUR " + wsSaldoEdit);
            wsSaldo = wsConSaldo.add(wsConFido);
            wsSaldoEdit = String.format("%,.2f", wsSaldo);
            System.out.println("Disponibile: EUR " + wsSaldoEdit);
        } else {
            if (sqlcode == 100) {
                System.out.println("Conto non trovato o non attivo!");
            } else {
                System.out.println("Errore database: " + sqlcode);
            }
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

            // Intestazione report
            printWriter.println(wsTitoloReport);
            printWriter.println(wsLineaSeparatore);

            printWriter.println("Conto: " + wsNumeroConto + "    Cliente: " + wsCliNome + " " + wsCliCognome);

            wsDataSistema = LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd"));
            wsOraSistema = LocalDateTime.now().format(DateTimeFormatter.ofPattern("HH:mm:ss"));
            printWriter.println("Data: " + wsDataSistema + "    Ora: " + wsOraSistema);
            printWriter.println(wsLineaSeparatore);

            // Intestazione colonne
            printWriter.println("DATA/ORA            TIPO      IMPORTO     CAUSALE                    SALDO DOPO");
            printWriter.println(wsLineaSeparatore);

            // Apri cursore movimenti
            // EXEC SQL OPEN CUR-MOVIMENTI END-EXEC
            sqlcode = 0;

            int i = 0;
            do {
                // EXEC SQL FETCH CUR-MOVIMENTI INTO :WS-MOV-DATA, :WS-MOV-TIPO, :WS-MOV-IMPORTO, :WS-MOV-CAUSALE, :WS-MOV-SALDO-DOPO END-EXEC
                if (i == 0) {
                    wsMovData = "2024-01-15 10:30:00";
                    wsMovTipo = "D";
                    wsMovImporto = new BigDecimal("1000.00");
                    wsMovCausale = "Deposito iniziale";
                    wsMovSaldoDopo = new BigDecimal("1000.00");
                } else if (i == 1) {
                    wsMovData = "2024-01-20 12:00:00";
                    wsMovTipo = "P";
                    wsMovImporto = new BigDecimal("200.50");
                    wsMovCausale = "Prelievo bancomat";
                    wsMovSaldoDopo = new BigDecimal("799.50");
                } else if (i == 2) {
                    wsMovData = "2024-01-25 15:45:00";
                    wsMovTipo = "B";
                    wsMovImporto = new BigDecimal("500.00");
                    wsMovCausale = "Bonifico ricevuto";
                    wsMovSaldoDopo = new BigDecimal("1299.50");
                } else {
                    sqlcode = 100; // Simula fine dati
                }

                if (sqlcode == 0) {
                    scriviMovimentoReport(printWriter);
                }
                i++;
            } while (sqlcode == 0);

            // EXEC SQL CLOSE CUR-MOVIMENTI END-EXEC

            printWriter.println(wsLineaSeparatore);

            // Saldo finale
            wsSaldoEdit = String.format("%,.2f", wsConSaldo);
            printWriter.println("SALDO FINALE: EUR " + wsSaldoEdit);


            System.out.println("Estratto conto salvato in ESTRATTO-CONTO.TXT");

        } catch (IOException e) {
            System.out.println("Errore durante la scrittura del file: " + e.getMessage());
        }
    }

    private void scriviMovimentoReport(PrintWriter printWriter) {
        StringBuilder reportRecord = new StringBuilder(" ".repeat(132));

        reportRecord.replace(0, 20, wsMovData.substring(0,19));
        reportRecord.append("  ");

        switch (wsMovTipo) {
            case "D":
                reportRecord.replace(20, 25, "DEP  ");
                break;
            case "P":
                reportRecord.replace(20, 25, "PRE  ");
                break;
            case "B":
                reportRecord.replace(20, 25, "BON  ");
                break;
            default:
                reportRecord.replace(20, 25, "???  ");
        }

        wsSaldoEdit = String.format("%,.2f", wsMovImporto);
        reportRecord.replace(25, 45, wsSaldoEdit + " ");

        reportRecord.replace(44, 75, wsMovCausale.substring(0, Math.min(wsMovCausale.length(), 30)) + " ");

        wsSaldoEdit = String.format("%,.2f", wsMovSaldoDopo);
        reportRecord.replace(75, reportRecord.length(), wsSaldoEdit);

        printWriter.println(reportRecord);
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
            // EXEC SQL UPDATE CONTI SET stato = 'C', data_chiusura = CURRENT_DATE WHERE numero_conto = :WS-NUMERO-CONTO END-EXEC
            sqlcode = 0; // Simula aggiornamento riuscito

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

        // EXEC SQL SELECT c.saldo, c.stato, cl.nome, cl.cognome INTO :WS-CON-SALDO, :WS-CON-STATO, :WS-CLI-NOME, :WS-CLI-COGNOME FROM CONTI c JOIN CLIENTI cl ON c.codice_cliente = cl.codice_cliente WHERE c.numero_conto = :WS-NUMERO-CONTO END-EXEC
        if (wsNumeroConto.equals("IT0000000001")) {
            sqlcode = 0;
            wsConSaldo = new BigDecimal("1299.50");
            wsConStato = "A";
            wsCliNome = "Mario";
            wsCliCognome = "Rossi";
        } else {
            sqlcode = 100;
        }

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

    private void registraMovimento() {
        // EXEC SQL SELECT saldo INTO :WS-SALDO FROM CONTI WHERE numero_conto = :WS-NUMERO-CONTO END-EXEC
        wsSaldo = wsConSaldo; // Simula selezione saldo

        // EXEC SQL INSERT INTO MOVIMENTI (numero_conto, tipo_movimento, importo, causale, saldo_dopo, eseguito_da) VALUES (:WS-NUMERO-CONTO, :WS-TIPO-MOVIMENTO, :WS-IMPORTO, :WS-CAUSALE, :WS-SALDO, 'SISTEMA') END-EXEC
        // Simula inserimento movimento
    }
}