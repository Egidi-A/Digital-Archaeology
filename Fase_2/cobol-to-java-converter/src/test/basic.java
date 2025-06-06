/**
 * @author JOHN SMITH
 * @date 2024-01-15
 * Installation: ACME CORPORATION
 * Security: CONFIDENTIAL
 * Generated from COBOL program
 */
public class CustomerReport {
    
    // Program ID: CUSTOMER-REPORT
    // Environment Division
    // Source Computer: IBM-370
    // Object Computer: IBM-370
    // Data Division
    // Working-Storage: 7 items
    
    // Working-Storage Fields
    private String wsCustomerName; // PIC X(30)
    private int wsCustomerId; // PIC 9(6)
    private double wsTotalSales; // PIC 9(7)V99
    private Object wsDate; // PIC GROUP
    private int wsYear; // PIC 9(4)
    private int wsMonth; // PIC 9(2)
    private int wsDay; // PIC 9(2)
    
    // Procedure Division
    // Paragraphs: 1
    
    public static void main(String[] args) {
        CustomerReport program = new CustomerReport();
        program.mainProcedure();
    }
    
    private void mainProcedure() {
        // MAIN-PROCEDURE
        System.out.println("CUSTOMER REPORT SYSTEM");
        System.out.println("PROCESSING...");
        System.exit(0);
    }
    
}
