       IDENTIFICATION DIVISION.
       PROGRAM-ID. TWOPHASE.
       AUTHOR.
       REMARKS.
      *****************************************************************
      *                                                               *
      * MODULE NAME = TWOPHASE                                        *
      *                                                               *
      * DESCRIPTIVE NAME = DB2 SAMPLE APPLICATION USING               *
      *                    TWO PHASE COMMIT AND THE DRDA DISTRIBUTED  *
      *                    ACCESS METHOD WITH CONNECT STATEMENTS      *
      *                                                               *
      * COPYRIGHT = 5665-DB2 (C) COPYRIGHT IBM CORP 1982, 1989        *
      * REFER TO COPYRIGHT INSTRUCTIONS FORM NUMBER G120-2083         *
      *                                                               *
      * STATUS = VERSION 5                                            *
      *                                                               *
      * FUNCTION = THIS MODULE DEMONSTRATES DISTRIBUTED DATA ACCESS   *
      *            USING 2 PHASE COMMIT BY TRANSFERRING AN EMPLOYEE   *
      *            FROM ONE LOCATION TO ANOTHER.                      *
      *                                                               *
      *            NOTE: THIS PROGRAM ASSUMES THE EXISTENCE OF THE    *
      *                  TABLE SYSADM.EMP AT LOCATIONS STLEC1 AND     *
      *                  STLEC2.                                      *
      *                                                               *
      * MODULE TYPE = COBOL PROGRAM                                   *
      *    PROCESSOR   = DB2 PRECOMPILER, ENTERPRISE COBOL FOR Z/OS   *
      *    MODULE SIZE = SEE LINK EDIT                                *
      *    ATTRIBUTES  = NOT REENTRANT OR REUSABLE                    *
      *                                                               *
      * ENTRY POINT =                                                 *
      *    PURPOSE = TO ILLUSTRATE 2 PHASE COMMIT                     *
      *    LINKAGE = INVOKE FROM DSN RUN                              *
      *    INPUT   = NONE                                             *
      *    OUTPUT  =                                                  *
      *              SYMBOLIC LABEL/NAME = SYSPRINT                   *
      *              DESCRIPTION = PRINT OUT THE DESCRIPTION OF EACH  *
      *                STEP AND THE RESULTANT SQLCA                   *
      *                                                               *
      * EXIT NORMAL = RETURN CODE 0 FROM NORMAL COMPLETION            *
      *                                                               *
      * EXIT ERROR = NONE                                             *
      *                                                               *
      * EXTERNAL REFERENCES =                                         *
      *    ROUTINE SERVICES = NONE                                    *
      *    DATA-AREAS       = NONE                                    *
      *    CONTROL-BLOCKS   =                                         *
      *       SQLCA   -   SQL COMMUNICATION AREA                      *
      *                                                               *
      * TABLES = NONE                                                 *
      *                                                               *
      * CHANGE-ACTIVITY = NONE                                        *
      *                                                               *
      *                                                               *
      *                                                               *
      * PSEUDOCODE                                                    *
      *                                                               *
      *    MAINLINE.                                                  *
      *      Perform CONNECT-TO-SITE-1 to establish                   *
      *        a connection to the local connection.                  *
      *      If the previous operation was successful Then            *
      *        Do.                                                    *
      *        | Perform PROCESS-CURSOR-SITE-1 to obtain the          *
      *        |   information about an employee that is              *
      *        |   transferring to another location.                  *
      *        | If the information about the employee was obtained   *
      *        |   successfully Then                                  *
      *        |   Do.                                                *
      *        |   | Perform UPDATE-ADDRESS to update the information *   
      *        |   |   to contain current information about the       *
      *        |   |   employee.                                      *
      *        |   | Perform CONNECT-TO-SITE-2 to establish           *
      *        |   |   a connection to the site where the employee is *
      *        |   |   transferring to.                               *
      *        |   |   If the connection is established successfully  *
      *        |   |   Then                                           *
      *        |   |     Do.                                          *
      *        |   |     | Perform PROCESS-SITE-2 to insert the       *
      *        |   |     |   employee information at the location     *
      *        |   |     |   where the employee is transferring to.   *
      *        |   |     End if the connection was established        *
      *        |   |       successfully.                              *
      *        |   End if the employee information was obtained       *
      *        |     successfully.                                    *
      *        End if the previous operation was successful.          *
      *      Perform COMMIT-WORK to COMMIT the changes made to STLEC1 *
      *        and STLEC2.                                            *
      *                                                               *
      *    PROG-END.                                                  *
      *      Close the printer.                                       *
      *      Return.                                                  *
      *                                                               *
      *    CONNECT-TO-SITE-1.                                         *
      *      Provide a text description of the following step.        *
      *      Establish a connection to the location where the         *
      *        employee is transferring from.                         *
      *      Print the SQLCA out.                                     *
      *                                                               *
      *    PROCESS-CURSOR-SITE-1.                                     *
      *      Provide a text description of the following step.        *
      *      Open a cursor that will be used to retrieve information  *
      *        about the transferring employee from this site.        *
      *      Print the SQLCA out.                                     *
      *      If the cursor was opened successfully Then               *
      *        Do.                                                    *
      *        | Perform FETCH-DELETE-SITE-1 to retrieve and          *
      *        |   delete the information about the transferring      *
      *        |   employee from this site.                           *
      *        | Perform CLOSE-CURSOR-SITE-1 to close the cursor.     *
      *        End if the cursor was opened successfully.             *
      *                                                               *  
      *    FETCH-DELETE-SITE-1.                                       *
      *      Provide a text description of the following step.        *
      *      Fetch information about the transferring employee.       *
      *      Print the SQLCA out.                                     *
      *      If the information was retrieved successfully Then       *
      *        Do.                                                    *
      *        | Perform DELETE-SITE-1 to delete the employee         *
      *        |   at this site.                                      *
      *        End if the information was retrieved successfully.     *
      *                                                               *
      *    DELETE-SITE-1.                                             *
      *      Provide a text description of the following step.        *
      *      Delete the information about the transferring employee   *
      *        from this site.                                        *
      *      Print the SQLCA out.                                     *
      *                                                               *
      *    CLOSE-CURSOR-SITE-1.                                       *
      *      Provide a text description of the following step.        *
      *      Close the cursor used to retrieve information about      *
      *        the transferring employee.                             *
      *      Print the SQLCA out.                                     *
      *                                                               *
      *    UPDATE-ADDRESS.                                            *
      *      Update the address of the employee.                      *
      *      Update the city of the employee.                         *
      *      Update the location of the employee.                     *
      *                                                               *
      *    CONNECT-TO-SITE-2.                                         *
      *      Provide a text description of the following step.        *
      *      Establish a connection to the location where the         *
      *        employee is transferring to.                           *
      *      Print the SQLCA out.                                     *
      *                                                               *
      *    PROCESS-SITE-2.                                            *
      *      Provide a text description of the following step.        *
      *      Insert the employee information at the location where    *
      *        the employee is being transferred to.                  *
      *      Print the SQLCA out.                                     *
      *                                                               *
      *    COMMIT-WORK.                                               *
      *      COMMIT all the changes made to STLEC1 and STLEC2.        *
      *                                                               *
      *****************************************************************
 
 
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PRINTER, ASSIGN TO S-OUT1.
 
       DATA DIVISION.
       FILE SECTION.
       FD  PRINTER
           RECORD CONTAINS 120 CHARACTERS
           DATA RECORD IS PRT-TC-RESULTS
           LABEL RECORD IS OMITTED.
       01  PRT-TC-RESULTS.
           03  PRT-BLANK              PIC X(120).
       WORKING-STORAGE SECTION.
 
      *****************************************************************
      * Variable declarations                                         *
      *****************************************************************
 
       01  H-EMPTBL.
           05  H-EMPNO   PIC X(6).
           05  H-NAME.
               49 H-NAME-LN   PIC S9(4) COMP-5.
               49 H-NAME-DA   PIC X(32).
           05  H-ADDRESS.
               49 H-ADDRESS-LN   PIC S9(4) COMP-5.
               49 H-ADDRESS-DA   PIC X(36).
           05  H-CITY.
               49 H-CITY-LN   PIC S9(4) COMP-5.
               49 H-CITY-DA   PIC X(36).
           05  H-EMPLOC   PIC X(4).
           05  H-SSNO     PIC X(11).
           05  H-BORN     PIC X(10).
           05  H-SEX      PIC X(1).
           05  H-HIRED    PIC X(10).
           05  H-DEPTNO   PIC X(3).
           05  H-JOBCODE  PIC S9(3)V COMP-3.
           05  H-SRATE    PIC S9(5) COMP.
           05  H-EDUC     PIC S9(5) COMP.
           05  H-SAL      PIC S9(6)V9(2) COMP-3.
           05  H-VALIDCHK PIC S9(6)V COMP-3.
 
       01  H-EMPTBL-IND-TABLE.
           02  H-EMPTBL-IND        PIC S9(4) COMP-5 OCCURS 15 TIMES.
 
      *****************************************************************
      * Includes for the variables used in the COBOL standard         *
      * language procedures and the SQLCA.                            *
      *****************************************************************
 
           EXEC SQL INCLUDE COBSVAR END-EXEC.
           EXEC SQL INCLUDE SQLCA END-EXEC.
 
      *****************************************************************
      * Declaration for the table that contains employee information  *
      *****************************************************************
 
           EXEC SQL DECLARE SYSADM.EMP TABLE
               (EMPNO   CHAR(6) NOT NULL,
                NAME    VARCHAR(32),
                ADDRESS VARCHAR(36) ,
                CITY    VARCHAR(36) ,
                EMPLOC  CHAR(4) NOT NULL,
                SSNO    CHAR(11),
                BORN    DATE,
                SEX     CHAR(1),
                HIRED   CHAR(10),
                DEPTNO  CHAR(3) NOT NULL,
                JOBCODE DECIMAL(3),
                SRATE   SMALLINT,
                EDUC    SMALLINT,
                SAL     DECIMAL(8,2) NOT NULL,
                VALCHK  DECIMAL(6))
           END-EXEC.
 
      *****************************************************************
      * Constants                                                     *
      *****************************************************************
 
       77  SITE-1                    PIC X(16) VALUE 'STLEC1'.
       77  SITE-2                    PIC X(16) VALUE 'STLEC2'.
       77  TEMP-EMPNO                PIC X(6)  VALUE '080000'.
       77  TEMP-ADDRESS-LN           PIC 99    VALUE 15.
       77  TEMP-CITY-LN              PIC 99    VALUE 18.
 
      *****************************************************************
      * Declaration of the cursor that will be used to retrieve       *
      * information about a transferring employee                     *
      *****************************************************************
 
           EXEC SQL DECLARE C1 CURSOR FOR
                SELECT EMPNO, NAME, ADDRESS, CITY, EMPLOC,
                       SSNO, BORN, SEX, HIRED, DEPTNO, JOBCODE,
                       SRATE, EDUC, SAL, VALCHK
                FROM   SYSADM.EMP
                WHERE EMPNO = :TEMP-EMPNO
           END-EXEC.
 
       PROCEDURE DIVISION.
       A101-HOUSE-KEEPING.
           OPEN OUTPUT PRINTER.
 
      *****************************************************************
      * An employee is transferring from location STLEC1 to STLEC2.   *
      * Retrieve information about the employee from STLEC1, delete   *
      * the employee from STLEC1 and insert the employee at STLEC2    *
      * using the information obtained from STLEC1.                   *
      *****************************************************************
 
       MAINLINE.
           PERFORM CONNECT-TO-SITE-1
           IF SQLCODE IS EQUAL TO 0
              PERFORM PROCESS-CURSOR-SITE-1
              IF SQLCODE IS EQUAL TO 0
                 PERFORM UPDATE-ADDRESS
                 PERFORM CONNECT-TO-SITE-2
                 IF SQLCODE IS EQUAL TO 0
                    PERFORM PROCESS-SITE-2.
           PERFORM COMMIT-WORK.
       PROG-END.
           CLOSE PRINTER.
           GOBACK.
 
      *****************************************************************
      * Establish a connection to STLEC1                              *
      *****************************************************************
 
       CONNECT-TO-SITE-1.
 
           MOVE 'CONNECT TO STLEC1   ' TO STNAME
           WRITE PRT-TC-RESULTS FROM STNAME
           EXEC SQL
              CONNECT TO :SITE-1
           END-EXEC.
           PERFORM PTSQLCA.
 
      *****************************************************************
      * When a connection has been established successfully at STLEC1,*
      * open the cursor that will be used to retrieve information     *
      * about the transferring employee.                              *
      *****************************************************************
 
       PROCESS-CURSOR-SITE-1.
 
           MOVE 'OPEN CURSOR C1      ' TO STNAME
           WRITE PRT-TC-RESULTS FROM STNAME
           EXEC SQL
              OPEN C1
           END-EXEC.
           PERFORM PTSQLCA.
           IF SQLCODE IS EQUAL TO ZERO
               PERFORM FETCH-DELETE-SITE-1
               PERFORM CLOSE-CURSOR-SITE-1.
 
      *****************************************************************
      * Retrieve information about the transferring employee.         *
      * Provided that the employee exists, perform DELETE-SITE-1 to   *
      * delete the employee from STLEC1.                              *
      *****************************************************************
 
       FETCH-DELETE-SITE-1.
 
           MOVE 'FETCH C1      ' TO STNAME
           WRITE PRT-TC-RESULTS FROM STNAME
           EXEC SQL
              FETCH C1 INTO :H-EMPTBL:H-EMPTBL-IND
           END-EXEC.
           PERFORM PTSQLCA.
           IF SQLCODE IS EQUAL TO ZERO
               PERFORM DELETE-SITE-1.
      *****************************************************************
      * Delete the employee from STLEC1.                              *
      *****************************************************************
 
       DELETE-SITE-1.
 
           MOVE 'DELETE EMPLOYEE ' TO STNAME
           WRITE PRT-TC-RESULTS FROM STNAME
           MOVE 'DELETE EMPLOYEE     ' TO STNAME
           EXEC SQL
              DELETE FROM SYSADM.EMP
                WHERE EMPNO = :TEMP-EMPNO
           END-EXEC.
           PERFORM PTSQLCA.
 
      *****************************************************************
      * Close the cursor used to retrieve information about the       *
      * transferring employee.                                        *
      *****************************************************************
 
       CLOSE-CURSOR-SITE-1.
 
           MOVE 'CLOSE CURSOR C1     ' TO STNAME
           WRITE PRT-TC-RESULTS FROM STNAME
           EXEC SQL
              CLOSE C1
           END-EXEC.
           PERFORM PTSQLCA.
 
      *****************************************************************
      * Update certain employee information in order to make it       *
      * current.                                                      *
      *****************************************************************
 
       UPDATE-ADDRESS.
           MOVE TEMP-ADDRESS-LN      TO H-ADDRESS-LN.
           MOVE '1500 NEW STREET'    TO H-ADDRESS-DA.
           MOVE TEMP-CITY-LN         TO H-CITY-LN.
           MOVE 'NEW CITY, CA 97804' TO H-CITY-DA.
           MOVE 'SJCA'               TO H-EMPLOC.
 
      *****************************************************************
      * Establish a connection to STLEC2                              *
      *****************************************************************
 
       CONNECT-TO-SITE-2.
 
           MOVE 'CONNECT TO STLEC2   ' TO STNAME
           WRITE PRT-TC-RESULTS FROM STNAME
           EXEC SQL
              CONNECT TO :SITE-2
           END-EXEC.
           PERFORM PTSQLCA.
      *****************************************************************
      * Using the employee information that was retrieved from STLEC1 *
      * and updated previously, insert the employee at STLEC2.             *
      *****************************************************************
 
       PROCESS-SITE-2.
 
           MOVE 'INSERT EMPLOYEE     ' TO STNAME
           WRITE PRT-TC-RESULTS FROM STNAME
           EXEC SQL
              INSERT INTO SYSADM.EMP VALUES
              (:H-EMPNO,
               :H-NAME,
               :H-ADDRESS,
               :H-CITY,
               :H-EMPLOC,
               :H-SSNO,
               :H-BORN,
               :H-SEX,
               :H-HIRED,
               :H-DEPTNO,
               :H-JOBCODE,
               :H-SRATE,
               :H-EDUC,
               :H-SAL,
               :H-VALIDCHK)
           END-EXEC.
           PERFORM PTSQLCA.
 
      *****************************************************************
      * COMMIT any changes that were made at STLEC1 and STLEC2.       *
      *****************************************************************
 
       COMMIT-WORK.
 
           MOVE 'COMMIT WORK         ' TO STNAME
           WRITE PRT-TC-RESULTS FROM STNAME
           EXEC SQL
              COMMIT
           END-EXEC.
           PERFORM PTSQLCA.
 
      *****************************************************************
      * Include COBOL standard language procedures                    *
      *****************************************************************
 
       INCLUDE-SUBS.
           EXEC SQL INCLUDE COBSSUB END-EXEC.