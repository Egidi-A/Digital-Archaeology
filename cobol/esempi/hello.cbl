       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.
       
       ENVIRONMENT DIVISION.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01 DBNAME PIC X(30) VALUE "testdb".
       01 USERNAME PIC X(30) VALUE "postgres".
       01 PASSWD PIC X(10) VALUE "postgres".
       EXEC SQL END DECLARE SECTION END-EXEC.
       
       EXEC SQL INCLUDE SQLCA END-EXEC.
       
       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           DISPLAY "HELLO STARTED".
           
           EXEC SQL
               CONNECT :USERNAME IDENTIFIED BY :PASSWD USING :DBNAME
           END-EXEC.
           
           IF SQLCODE NOT = 0
               DISPLAY "SQL ERROR SQLCODE: " SQLCODE
               DISPLAY "Undefined error"
               DISPLAY "ERRCODE: " SQLSTATE
           ELSE
               DISPLAY "CONNECTION SUCCESS".
               
               EXEC SQL
                   CREATE TABLE IF NOT EXISTS TESTTABLE (
                       ID INTEGER,
                       NAME VARCHAR(20)
                   )
               END-EXEC.
               
               IF SQLCODE NOT = 0
                   DISPLAY "Error creating table: " SQLCODE
               ELSE
                   DISPLAY "Table created successfully."
               END-IF.
               
               EXEC SQL DISCONNECT END-EXEC.
           END-IF.
           
           DISPLAY "HELLO ENDED".
           
           STOP RUN.
