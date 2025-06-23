       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMPLOYEE-MANAGER.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-EMPLOYEE.
          05 WS-EMP-ID        PIC 9(5).
          05 WS-EMP-NAME      PIC X(30).
          05 WS-EMP-SALARY    PIC 9(7)V99.
          05 WS-EMP-DEPT      PIC X(10).
       
       01 WS-COUNTERS.
          05 WS-TOTAL-SALARY  PIC 9(9)V99 VALUE ZERO.
          05 WS-EMP-COUNT     PIC 9(3) VALUE ZERO.
       
       01 WS-FLAGS.
          05 WS-EOF           PIC X VALUE 'N'.
       
       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           PERFORM INIT-PARAGRAPH.
           PERFORM PROCESS-EMPLOYEES.
           PERFORM DISPLAY-RESULTS.
           STOP RUN.
       
       INIT-PARAGRAPH.
           DISPLAY "Starting Employee Processing".
           MOVE ZERO TO WS-TOTAL-SALARY.
           MOVE ZERO TO WS-EMP-COUNT.
       
       PROCESS-EMPLOYEES.
           MOVE 12345 TO WS-EMP-ID.
           MOVE "John Smith" TO WS-EMP-NAME.
           MOVE 55000.00 TO WS-EMP-SALARY.
           MOVE "IT" TO WS-EMP-DEPT.
           
           ADD WS-EMP-SALARY TO WS-TOTAL-SALARY.
           ADD 1 TO WS-EMP-COUNT.
           
           DISPLAY "Employee: " WS-EMP-NAME.
           DISPLAY "Salary: " WS-EMP-SALARY.
       
       DISPLAY-RESULTS.
           DISPLAY "Total Employees: " WS-EMP-COUNT.
           DISPLAY "Total Salary: " WS-TOTAL-SALARY.