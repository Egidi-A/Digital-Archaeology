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
    
    public static void main(String[] args) {
        // Main program logic goes here
        System.out.println("GestioneConti - Generated from COBOL");
    }
}
