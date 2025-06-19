package com.generated;

import java.math.BigDecimal;

/**
 * Manages employee data and performs calculations based on a predefined process.
 * This class is automatically generated from an Abstract Semantic Graph (ASG)
 * and represents the logic for processing employee records, calculating
 * total salaries, and displaying summary results. It includes fields for
 * employee details, getters and setters for those fields, and methods
 * that define the main business logic.
 */
public class EmployeeManager {

    // --- Class Fields ---

    /**
     * The employee's unique identifier.
     */
    private int wsEmpId;

    /**
     * The employee's full name.
     */
    private String wsEmpName;

    /**
     * The employee's salary, represented as a BigDecimal for precision.
     */
    private BigDecimal wsEmpSalary;

    /**
     * The department to which the employee belongs.
     */
    private String wsEmpDept;

    /**
     * The running total of all processed employee salaries.
     * Initialized to zero.
     */
    private BigDecimal wsTotalSalary = new BigDecimal("0");

    /**
     * The total count of employees processed.
     * Initialized to zero.
     */
    private int wsEmpCount = 0;

    /**
     * A flag to indicate the end of data processing, similar to an EOF marker.
     * Initialized to "N".
     */
    private String wsEof = "N";


    // --- Getters and Setters ---

    /**
     * Gets the employee's ID.
     * @return The current employee ID.
     */
    public int getWsEmpId() {
        return wsEmpId;
    }

    /**
     * Sets the employee's ID.
     * @param value The new integer value for the employee ID.
     */
    public void setWsEmpId(int value) {
        this.wsEmpId = value;
    }

    /**
     * Gets the employee's name.
     * @return The current employee name.
     */
    public String getWsEmpName() {
        return wsEmpName;
    }

    /**
     * Sets the employee's name.
     * @param value The new String value for the employee name.
     */
    public void setWsEmpName(String value) {
        this.wsEmpName = value;
    }

    /**
     * Gets the employee's salary.
     * @return The current employee salary as a BigDecimal.
     */
    public BigDecimal getWsEmpSalary() {
        return wsEmpSalary;
    }

    /**
     * Sets the employee's salary.
     * @param value The new BigDecimal value for the employee salary.
     */
    public void setWsEmpSalary(BigDecimal value) {
        this.wsEmpSalary = value;
    }

    /**
     * Gets the employee's department.
     * @return The current employee department.
     */
    public String getWsEmpDept() {
        return wsEmpDept;
    }

    /**
     * Sets the employee's department.
     * @param value The new String value for the employee department.
     */
    public void setWsEmpDept(String value) {
        this.wsEmpDept = value;
    }

    /**
     * Gets the total salary of all employees.
     * @return The total salary as a BigDecimal.
     */
    public BigDecimal getWsTotalSalary() {
        return wsTotalSalary;
    }

    /**
     * Sets the total salary of all employees.
     * @param value The new BigDecimal value for the total salary.
     */
    public void setWsTotalSalary(BigDecimal value) {
        this.wsTotalSalary = value;
    }

    /**
     * Gets the total count of employees.
     * @return The current count of employees.
     */
    public int getWsEmpCount() {
        return wsEmpCount;
    }

    /**
     * Sets the total count of employees.
     * @param value The new integer value for the employee count.
     */
    public void setWsEmpCount(int value) {
        this.wsEmpCount = value;
    }

    /**
     * Gets the end-of-file marker.
     * @return The current EOF marker string.
     */
    public String getWsEof() {
        return wsEof;
    }

    /**
     * Sets the end-of-file marker.
     * @param value The new String value for the EOF marker.
     */
    public void setWsEof(String value) {
        this.wsEof = value;
    }


    // --- Business Logic Methods ---

    /**
     * Main control paragraph that orchestrates the program flow.
     * It calls initialization, processing, and result display routines in sequence,
     * and then terminates the program.
     */
    private void mainParagraph() {
        // COBOL equivalent: PERFORM initParagraph
        initParagraph();
        // COBOL equivalent: PERFORM processEmployees
        processEmployees();
        // COBOL equivalent: PERFORM displayResults
        displayResults();
        // COBOL equivalent: STOP RUN
        System.exit(0);
    }

    /**
     * Initializes the program state by resetting counters and totals.
     * This method is called once at the beginning of the execution.
     */
    private void initParagraph() {
        // COBOL equivalent: DISPLAY
        System.out.println("Starting Employee Processing");
        // COBOL equivalent: MOVE ZEROS TO ws-total-salary
        this.wsTotalSalary = BigDecimal.ZERO;
        // COBOL equivalent: MOVE ZEROS TO ws-emp-count
        this.wsEmpCount = 0;
    }

    /**
     * Processes a hardcoded employee record.
     * In a real-world scenario, this method would likely contain a loop
     * to read and process multiple records from a data source.
     */
    private void processEmployees() {
        // Hardcoded data for a single employee
        // COBOL equivalent: MOVE 12345 TO ws-emp-id
        this.wsEmpId = 12345;
        // COBOL equivalent: MOVE "John Smith" TO ws-emp-name
        this.wsEmpName = "John Smith";
        // COBOL equivalent: MOVE 55000.00 TO ws-emp-salary
        this.wsEmpSalary = new BigDecimal("55000.00");
        // COBOL equivalent: MOVE "IT" TO ws-emp-dept
        this.wsEmpDept = "IT";

        // Perform calculations
        // COBOL equivalent: ADD ws-emp-salary TO ws-total-salary
        this.wsTotalSalary = this.wsTotalSalary.add(this.wsEmpSalary);
        // COBOL equivalent: ADD 1 TO ws-emp-count
        this.wsEmpCount++;

        // Display details for the processed employee
        // COBOL equivalent: DISPLAY
        System.out.println("Employee: " + this.wsEmpName);
        // COBOL equivalent: DISPLAY
        System.out.println("Salary: " + this.wsEmpSalary);
    }

    /**
     * Displays the final calculated results to the console.
     * This method is called after all processing is complete.
     */
    private void displayResults() {
        // COBOL equivalent: DISPLAY
        System.out.println("Total Employees: " + this.wsEmpCount);
        // COBOL equivalent: DISPLAY
        System.out.println("Total Salary: " + this.wsTotalSalary);
    }


    // --- Application Entry Point ---

    /**
     * The main entry point for the Java application.
     * It creates an instance of the EmployeeManager class and starts the main processing logic.
     * @param args Command line arguments (not used in this application).
     */
    public static void main(String[] args) {
        // Instantiate the main application class
        EmployeeManager app = new EmployeeManager();
        // Invoke the primary control method
        app.mainParagraph();
    }
}