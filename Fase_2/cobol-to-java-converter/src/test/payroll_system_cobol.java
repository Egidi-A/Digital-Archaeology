/**
 * @author ANNALISA-EGIDI
 * @date 2025-05-20
 * Generated from COBOL program
 */
public class GestionePaghe {
    
    // Program ID: GESTIONE-PAGHE
    // Environment Division
    // Data Division
    // File Section: Present
    // Working-Storage: 54 items
    
    // Working-Storage Fields
    private int wsScelta = 0; // PIC 9
    private String wsContinua = "S"; // PIC X
    private String wsRisposta; // PIC X
    private String wsEsito; // PIC X(2)
    private int wsAnno; // PIC 9(4)
    private int wsMese; // PIC 99
    private String wsMatricola; // PIC X(6)
    private String wsDataElaborazione; // PIC X(10)
    private int wsContaDipendenti = 0; // PIC 999
    private int wsContaElaborati = 0; // PIC 999
    private double wsTotaleLordo = 0; // PIC 9(9)V99
    private double wsTotaleNetto = 0; // PIC 9(9)V99
    private double wsTotaleTrattenute = 0; // PIC 9(9)V99
    private Object wsDipendente; // PIC GROUP
    private String wsDipMatricola; // PIC X(6)
    private String wsDipNome; // PIC X(50)
    private String wsDipCognome; // PIC X(50)
    private String wsDipCf; // PIC X(16)
    private String wsDipQualifica; // PIC X(50)
    private String wsDipLivello; // PIC X(2)
    private String wsDipReparto; // PIC X(30)
    private double wsDipStipendio; // PIC 9(8)V99
    private String wsDipStato; // PIC X
    private Object wsStipendio; // PIC GROUP
    private int wsStiId; // PIC 9(9)
    private int wsStiGiorni; // PIC 99
    private double wsStiOreOrd; // PIC 999V99
    private double wsStiOreStr; // PIC 999V99
    private double wsStiBase; // PIC 9(8)V99
    private double wsStiStraord; // PIC 9(8)V99
    private double wsStiAltre; // PIC 9(8)V99
    private double wsStiLordo; // PIC 9(8)V99
    private double wsStiTrattenute; // PIC 9(8)V99
    private double wsStiNetto; // PIC 9(8)V99
    private Object wsPresenze; // PIC GROUP
    private int wsPreGiorniLav = 0; // PIC 99
    private int wsPreGiorniFer = 0; // PIC 99
    private int wsPreGiorniMal = 0; // PIC 99
    private int wsPreGiorniPer = 0; // PIC 99
    private double wsPreOreOrd = 0; // PIC 999V99
    private double wsPreOreStr = 0; // PIC 999V99
    private Object wsCalcoli; // PIC GROUP
    private double wsImponibile; // PIC 9(8)V99
    private double wsIrpef; // PIC 9(8)V99
    private double wsInps; // PIC 9(8)V99
    private double wsAddReg; // PIC 9(8)V99
    private double wsAddCom; // PIC 9(8)V99
    private double wsDetrazioni; // PIC 9(8)V99
    private double wsAliquota; // PIC 99V99
    private double wsScaglioneMin; // PIC 9(8)V99
    private double wsScaglioneMax; // PIC 9(8)V99
    private int wsImportoEdit; // PIC Z,ZZZ,ZZ9
    private int wsNumeroEdit; // PIC ZZ9
    private int wsPercEdit; // PIC Z9
    
    // Procedure Division
    // Paragraphs: 23
    
    public static void main(String[] args) {
        GestionePaghe program = new GestionePaghe();
        program.mainLogic();
    }
    
    private void mainLogic() {
        // MAIN-LOGIC
        System.exit(0);
    }
    
    private void connettiDatabase() {
        // CONNETTI-DATABASE
        System.out.println("Connessione al database stabilita");
    }
    
    private void disconnettiDatabase() {
        // DISCONNETTI-DATABASE
        System.out.println("Disconnesso dal database");
    }
    
    private void visualizzaMenu() {
        // VISUALIZZA-MENU
        WS-SCELTA = scanner.nextLine();
    }
    
    private void elaboraScelta() {
        // ELABORA-SCELTA
        System.out.println("Scelta non valida!");
    }
    
    private void elaborazioneMensile() {
        // ELABORAZIONE-MENSILE
        System.out.println("Totale netto: EUR " WS-IMPORTO-EDIT);
    }
    
    private void calcoloSingolo() {
        // CALCOLO-SINGOLO
        visualizzaDettaglioStipendio();
    }
    
    private void caricaDipendente() {
        // CARICA-DIPENDENTE
        wsEsito = "ko";
    }
    
    private void calcolaStipendio() {
        // CALCOLA-STIPENDIO
        salvaStipendio();
    }
    
    private void caricaPresenze() {
        // CARICA-PRESENZE
        wsStiOreStr = wsPreOreStr;
    }
    
    private void calcolaTrattenute() {
        // CALCOLA-TRATTENUTE
        wsStiTrattenute = 0;
    }
    
    private void calcolaIrpef() {
        // CALCOLA-IRPEF
        WS-IRPEF = WS-IRPEF / 12;
    }
    
    private void calcolaDetrazioni() {
        // CALCOLA-DETRAZIONI
        WS-DETRAZIONI = WS-DETRAZIONI / 12;
    }
    
    private void salvaStipendio() {
        // SALVA-STIPENDIO
        }
    }
    
    private void salvaDettaglioTrattenute() {
        // SALVA-DETTAGLIO-TRATTENUTE
        WS-DETRAZIONI = 0 - WS-DETRAZIONI EXEC SQL INSERT INTO TRATTENUTE;
    }
    
    private void visualizzaDettaglioStipendio() {
        // VISUALIZZA-DETTAGLIO-STIPENDIO
        System.out.println("  NETTO A PAGARE:     EUR " WS-IMPORTO-EDIT);
    }
    
    private void stampaCedolino() {
        // STAMPA-CEDOLINO
        registraCedolino();
    }
    
    private void generaCedolino() {
        // GENERA-CEDOLINO
        // TODO: WRITE - WRITE CEDOLINO-RECORD
    }
    
    private void registraCedolino() {
        // REGISTRA-CEDOLINO
        StringBuilder sb = new StringBuilder();
// STRING components: "CED" WS-ANNO WS-MESE WS-MATRICOLA DELIMITED BY SIZE
WS-CAUSALE = sb.toString();
    }
    
    private void reportMensile() {
        // REPORT-MENSILE
        System.out.println("Report salvato in REPORT-STIPENDI.TXT");
    }
    
    private void generaReport() {
        // GENERA-REPORT
        // TODO: CLOSE - CLOSE CUR-REPORT
    }
    
    private void inserimentoPresenze() {
        // INSERIMENTO-PRESENZE
        System.out.println("Errore inserimento: " SQLCODE);
    }
    
    private void visualizzaPresenze() {
        // VISUALIZZA-PRESENZE
        System.out.println("Ore straord.:    " WS-IMPORTO-EDIT);
    }
    
}
