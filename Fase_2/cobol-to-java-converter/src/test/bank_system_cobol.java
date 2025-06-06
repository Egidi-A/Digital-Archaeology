/**
 * @author ANNALISA-EGIDI
 * @date 2025-05-20
 * Generated from COBOL program
 */
public class GestioneConti {
    
    // Program ID: GESTIONE-CONTI
    // Environment Division
    // Data Division
    // File Section: Present
    // Working-Storage: 40 items
    
    // Working-Storage Fields
    private int wsScelta = 0; // PIC 9
    private String wsContinua = "S"; // PIC X
    private String wsMessaggio; // PIC X(80)
    private String wsEsito; // PIC X(2)
    private String wsNumeroConto; // PIC X(12)
    private String wsCodiceCliente; // PIC X(8)
    private double wsImporto; // PIC 9(13)V99
    private double wsSaldo; // PIC S9(13)V99
    private int wsSaldoEdit; // PIC Z,ZZZ,ZZZ,ZZ9
    private String wsTipoMovimento; // PIC X
    private String wsCausale; // PIC X(100)
    private String wsDataSistema; // PIC X(10)
    private String wsOraSistema; // PIC X(8)
    private Object wsCliente; // PIC GROUP
    private String wsCliCodice; // PIC X(8)
    private String wsCliNome; // PIC X(50)
    private String wsCliCognome; // PIC X(50)
    private String wsCliCf; // PIC X(16)
    private String wsCliDataNascita; // PIC X(10)
    private String wsCliIndirizzo; // PIC X(100)
    private String wsCliCitta; // PIC X(50)
    private String wsCliCap; // PIC X(5)
    private String wsCliTelefono; // PIC X(15)
    private String wsCliEmail; // PIC X(100)
    private Object wsConto; // PIC GROUP
    private String wsConNumero; // PIC X(12)
    private String wsConCliente; // PIC X(8)
    private String wsConTipo; // PIC X
    private double wsConSaldo; // PIC S9(13)V99
    private String wsConDataApertura; // PIC X(10)
    private String wsConStato; // PIC X
    private double wsConFido; // PIC 9(13)V99
    private Object wsMovimento; // PIC GROUP
    private int wsMovData; // PIC X(19)
    private String wsMovTipo; // PIC X
    private double wsMovImporto; // PIC S9(13)V99
    private String wsMovCausale; // PIC X(100)
    private double wsMovSaldoDopo; // PIC S9(13)V99
    private Object wsTitoloReport; // PIC GROUP
    private String wsLineaSeparatore = "ALL \"-\""; // PIC X(132)
    
    // Procedure Division
    // Paragraphs: 15
    
    public static void main(String[] args) {
        GestioneConti program = new GestioneConti();
        program.mainLogic();
    }
    
    private void mainLogic() {
        // MAIN-LOGIC
        connettiDatabase();
    }
    
    private void connettiDatabase() {
        // CONNETTI-DATABASE
        // TODO: UNKNOWN - EXEC SQL CONNECT TO 'postgresql://localhost/banca' USER 'postgres' USING 'password' END-EXEC IF SQLCODE NOT = 0 DISPLAY "Errore connessione database: " SQLCODE STOP RUN ELSE DISPLAY "Connessione al database stabilita"
    }
    
    private void disconnettiDatabase() {
        // DISCONNETTI-DATABASE
        // TODO: UNKNOWN - EXEC SQL DISCONNECT ALL END-EXEC DISPLAY "Disconnesso dal database"
    }
    
    private void visualizzaMenu() {
        // VISUALIZZA-MENU
        System.out.println(" " DISPLAY "===== SISTEMA GESTIONE CONTI CORRENTI =====" DISPLAY "1. Apertura nuovo conto" DISPLAY "2. Deposito" DISPLAY "3. Prelievo" DISPLAY "4. Visualizza saldo" DISPLAY "5. Estratto conto" DISPLAY "6. Chiusura conto" DISPLAY "0. Esci" DISPLAY "===========================================" DISPLAY "Scelta: " WITH NO ADVANCING ACCEPT WS-SCELTA);
    }
    
    private void elaboraScelta() {
        // ELABORA-SCELTA
        // TODO: EVALUATE - EVALUATE WS-SCELTA WHEN 1 PERFORM APERTURA-CONTO WHEN 2 PERFORM DEPOSITO WHEN 3 PERFORM PRELIEVO WHEN 4 PERFORM VISUALIZZA-SALDO WHEN 5 PERFORM ESTRATTO-CONTO WHEN 6 PERFORM CHIUSURA-CONTO WHEN 0 MOVE 'N' TO WS-CONTINUA WHEN OTHER DISPLAY "Scelta non valida!"
    }
    
    private void aperturaConto() {
        // APERTURA-CONTO
        System.out.println(" " DISPLAY "=== APERTURA NUOVO CONTO ===" DISPLAY "Codice cliente: " WITH NO ADVANCING ACCEPT WS-CON-CLIENTE EXEC SQL SELECT codice_cliente, nome, cognome INTO :WS-CLI-CODICE, :WS-CLI-NOME, :WS-CLI-COGNOME FROM CLIENTI WHERE codice_cliente = :WS-CON-CLIENTE END-EXEC IF SQLCODE = 100 DISPLAY "Cliente non trovato!" EXIT PARAGRAPH END-IF IF SQLCODE NOT = 0 DISPLAY "Errore database: " SQLCODE EXIT PARAGRAPH END-IF DISPLAY "Cliente: " WS-CLI-NOME " " WS-CLI-COGNOME PERFORM GENERA-NUMERO-CONTO DISPLAY "Tipo conto (C=Corrente, D=Deposito): " WITH NO ADVANCING ACCEPT WS-CON-TIPO DISPLAY "Importo iniziale: " WITH NO ADVANCING ACCEPT WS-CON-SALDO DISPLAY "Fido accordato: " WITH NO ADVANCING ACCEPT WS-CON-FIDO MOVE FUNCTION CURRENT-DATE(1:10) TO WS-CON-DATA-APERTURA MOVE 'A' TO WS-CON-STATO EXEC SQL INSERT INTO CONTI (numero_conto, codice_cliente, tipo_conto, saldo, data_apertura, stato, fido) VALUES (:WS-CON-NUMERO, :WS-CON-CLIENTE, :WS-CON-TIPO, :WS-CON-SALDO, CURRENT_DATE, :WS-CON-STATO, :WS-CON-FIDO) END-EXEC IF SQLCODE = 0 DISPLAY "Conto " WS-CON-NUMERO " creato con successo!" IF WS-CON-SALDO > 0 MOVE 'D' TO WS-TIPO-MOVIMENTO MOVE "Deposito iniziale" TO WS-CAUSALE MOVE WS-CON-SALDO TO WS-IMPORTO MOVE WS-CON-NUMERO TO WS-NUMERO-CONTO PERFORM REGISTRA-MOVIMENTO END-IF ELSE DISPLAY "Errore creazione conto: " SQLCODE);
    }
    
    private void generaNumeroConto() {
        // GENERA-NUMERO-CONTO
        // TODO: UNKNOWN - EXEC SQL SELECT 'IT' || LPAD( CAST(COALESCE(MAX(CAST(SUBSTR(numero_conto, 3) AS INTEGER)), 0) + 1 AS VARCHAR), 10, '0') INTO :WS-CON-NUMERO FROM CONTI WHERE numero_conto LIKE 'IT%'
    }
    
    private void deposito() {
        // DEPOSITO
        System.out.println(" " DISPLAY "=== DEPOSITO ===" DISPLAY "Numero conto: " WITH NO ADVANCING ACCEPT WS-NUMERO-CONTO PERFORM VERIFICA-CONTO IF WS-ESITO = "KO" EXIT PARAGRAPH END-IF DISPLAY "Importo deposito: " WITH NO ADVANCING ACCEPT WS-IMPORTO IF WS-IMPORTO <= 0 DISPLAY "Importo non valido!" EXIT PARAGRAPH END-IF DISPLAY "Causale: " WITH NO ADVANCING ACCEPT WS-CAUSALE EXEC SQL UPDATE CONTI SET saldo = saldo + :WS-IMPORTO WHERE numero_conto = :WS-NUMERO-CONTO AND stato = 'A' END-EXEC IF SQLCODE = 0 MOVE 'D' TO WS-TIPO-MOVIMENTO PERFORM REGISTRA-MOVIMENTO DISPLAY "Deposito effettuato con successo!" ELSE DISPLAY "Errore durante il deposito: " SQLCODE);
    }
    
    private void prelievo() {
        // PRELIEVO
        System.out.println(" " DISPLAY "=== PRELIEVO ===" DISPLAY "Numero conto: " WITH NO ADVANCING ACCEPT WS-NUMERO-CONTO PERFORM VERIFICA-CONTO IF WS-ESITO = "KO" EXIT PARAGRAPH END-IF DISPLAY "Importo prelievo: " WITH NO ADVANCING ACCEPT WS-IMPORTO IF WS-IMPORTO <= 0 DISPLAY "Importo non valido!" EXIT PARAGRAPH END-IF EXEC SQL SELECT saldo, fido INTO :WS-CON-SALDO, :WS-CON-FIDO FROM CONTI WHERE numero_conto = :WS-NUMERO-CONTO AND stato = 'A' END-EXEC IF WS-CON-SALDO - WS-IMPORTO < (0 - WS-CON-FIDO) DISPLAY "Fondi insufficienti!" DISPLAY "Saldo attuale: " WS-CON-SALDO DISPLAY "Fido disponibile: " WS-CON-FIDO EXIT PARAGRAPH END-IF DISPLAY "Causale: " WITH NO ADVANCING ACCEPT WS-CAUSALE EXEC SQL UPDATE CONTI SET saldo = saldo - :WS-IMPORTO WHERE numero_conto = :WS-NUMERO-CONTO AND stato = 'A' END-EXEC IF SQLCODE = 0 MOVE 'P' TO WS-TIPO-MOVIMENTO PERFORM REGISTRA-MOVIMENTO DISPLAY "Prelievo effettuato con successo!" ELSE DISPLAY "Errore durante il prelievo: " SQLCODE);
    }
    
    private void visualizzaSaldo() {
        // VISUALIZZA-SALDO
        System.out.println(" " DISPLAY "=== VISUALIZZA SALDO ===" DISPLAY "Numero conto: " WITH NO ADVANCING ACCEPT WS-NUMERO-CONTO EXEC SQL SELECT c.saldo, c.fido, cl.nome, cl.cognome INTO :WS-CON-SALDO, :WS-CON-FIDO, :WS-CLI-NOME, :WS-CLI-COGNOME FROM CONTI c JOIN CLIENTI cl ON c.codice_cliente = cl.codice_cliente WHERE c.numero_conto = :WS-NUMERO-CONTO AND c.stato = 'A' END-EXEC IF SQLCODE = 0 DISPLAY " " DISPLAY "Intestatario: " WS-CLI-NOME " " WS-CLI-COGNOME MOVE WS-CON-SALDO TO WS-SALDO-EDIT DISPLAY "Saldo attuale: EUR " WS-SALDO-EDIT MOVE WS-CON-FIDO TO WS-SALDO-EDIT DISPLAY "Fido accordato: EUR " WS-SALDO-EDIT COMPUTE WS-SALDO = WS-CON-SALDO + WS-CON-FIDO MOVE WS-SALDO TO WS-SALDO-EDIT DISPLAY "Disponibile: EUR " WS-SALDO-EDIT ELSE IF SQLCODE = 100 DISPLAY "Conto non trovato o non attivo!" ELSE DISPLAY "Errore database: " SQLCODE END-IF);
    }
    
    private void estrattoConto() {
        // ESTRATTO-CONTO
        System.out.println(" " DISPLAY "=== ESTRATTO CONTO ===" DISPLAY "Numero conto: " WITH NO ADVANCING ACCEPT WS-NUMERO-CONTO PERFORM VERIFICA-CONTO IF WS-ESITO = "KO" EXIT PARAGRAPH END-IF OPEN OUTPUT REPORT-FILE WRITE REPORT-RECORD FROM WS-TITOLO-REPORT WRITE REPORT-RECORD FROM WS-LINEA-SEPARATORE STRING "Conto: " WS-NUMERO-CONTO "    Cliente: " WS-CLI-NOME " " WS-CLI-COGNOME DELIMITED BY SIZE INTO REPORT-RECORD WRITE REPORT-RECORD STRING "Data: " FUNCTION CURRENT-DATE(1:10) "    Ora: " FUNCTION CURRENT-DATE(12:8) DELIMITED BY SIZE INTO REPORT-RECORD WRITE REPORT-RECORD WRITE REPORT-RECORD FROM WS-LINEA-SEPARATORE STRING "DATA/ORA            TIPO  " "    IMPORTO     CAUSALE" "                    SALDO DOPO" DELIMITED BY SIZE INTO REPORT-RECORD WRITE REPORT-RECORD WRITE REPORT-RECORD FROM WS-LINEA-SEPARATORE EXEC SQL OPEN CUR-MOVIMENTI END-EXEC PERFORM UNTIL SQLCODE NOT = 0 EXEC SQL FETCH CUR-MOVIMENTI INTO :WS-MOV-DATA, :WS-MOV-TIPO, :WS-MOV-IMPORTO, :WS-MOV-CAUSALE, :WS-MOV-SALDO-DOPO END-EXEC IF SQLCODE = 0 PERFORM SCRIVI-MOVIMENTO-REPORT END-IF END-PERFORM EXEC SQL CLOSE CUR-MOVIMENTI END-EXEC WRITE REPORT-RECORD FROM WS-LINEA-SEPARATORE MOVE WS-CON-SALDO TO WS-SALDO-EDIT STRING "SALDO FINALE: EUR " WS-SALDO-EDIT DELIMITED BY SIZE INTO REPORT-RECORD WRITE REPORT-RECORD CLOSE REPORT-FILE DISPLAY "Estratto conto salvato in ESTRATTO-CONTO.TXT");
    }
    
    private void scriviMovimentoReport() {
        // SCRIVI-MOVIMENTO-REPORT
        reportRecord string wsMovData(1:19) " " delimited by size into reportRecord evaluate wsMovTipo when 'd' string reportRecord(1:21) "dep " delimited by size into reportRecord when 'p' string reportRecord(1:21) "pre " delimited by size into reportRecord when 'b' string reportRecord(1:21) "bon " delimited by size into reportRecord when other string reportRecord(1:21) "??? " delimited by size into reportRecord endEvaluate move wsMovImporto to wsSaldoEdit string reportRecord(1:26) wsSaldoEdit " " delimited by size into reportRecord string reportRecord(1:45) wsMovCausale(1:30) " " delimited by size into reportRecord move wsMovSaldoDopo to wsSaldoEdit string reportRecord(1:76) wsSaldoEdit delimited by size into reportRecord write reportRecord = "";
    }
    
    private void chiusuraConto() {
        // CHIUSURA-CONTO
        System.out.println(" " DISPLAY "=== CHIUSURA CONTO ===" DISPLAY "Numero conto da chiudere: " WITH NO ADVANCING ACCEPT WS-NUMERO-CONTO PERFORM VERIFICA-CONTO IF WS-ESITO = "KO" EXIT PARAGRAPH END-IF IF WS-CON-SALDO NOT = 0 DISPLAY "Impossibile chiudere: saldo non zero!" MOVE WS-CON-SALDO TO WS-SALDO-EDIT DISPLAY "Saldo attuale: EUR " WS-SALDO-EDIT EXIT PARAGRAPH END-IF DISPLAY "Confermare chiusura conto (S/N): " WITH NO ADVANCING ACCEPT WS-CONTINUA IF WS-CONTINUA = 'S' OR 's' EXEC SQL UPDATE CONTI SET stato = 'C', data_chiusura = CURRENT_DATE WHERE numero_conto = :WS-NUMERO-CONTO END-EXEC IF SQLCODE = 0 DISPLAY "Conto chiuso con successo!" ELSE DISPLAY "Errore chiusura conto: " SQLCODE END-IF ELSE DISPLAY "Chiusura annullata");
    }
    
    private void verificaConto() {
        // VERIFICA-CONTO
        wsEsito exec sql select c.saldo, c.stato, cl.nome, cl.cognome into :wsConSaldo, :wsConStato, :wsCliNome, :wsCliCognome from conti c join clienti cl on c.codiceCliente = cl.codiceCliente where c.numeroConto = :wsNumeroConto endExec if sqlcode = 100 display "conto non trovato!" move "ko" to wsEsito else if sqlcode not = 0 display "errore database: " sqlcode move "ko" to wsEsito else if wsConStato not = 'a' display "conto non attivo!" move "ko" to wsEsito = "ok";
    }
    
    private void registraMovimento() {
        // REGISTRA-MOVIMENTO
        // TODO: UNKNOWN - EXEC SQL SELECT saldo INTO :WS-SALDO FROM CONTI WHERE numero_conto = :WS-NUMERO-CONTO END-EXEC EXEC SQL INSERT INTO MOVIMENTI (numero_conto, tipo_movimento, importo, causale, saldo_dopo, eseguito_da) VALUES (:WS-NUMERO-CONTO, :WS-TIPO-MOVIMENTO, :WS-IMPORTO, :WS-CAUSALE, :WS-SALDO, 'SISTEMA')
    }
    
}
