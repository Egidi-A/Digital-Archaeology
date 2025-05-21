       IDENTIFICATION DIVISION.
       PROGRAM-ID. OPERAZIONI-DIPENDENTI.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO 'dipendentiExport.dat'
           ORGANIZATION IS LINE SEQUENTIAL.
           
           SELECT OUTPUT-FILE ASSIGN TO 'dipendentiElaborati.dat'
           ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 EMPLOYEE-INPUT-RECORD.
          05 EMP-ID-IN       PIC 9(5).
          05 EMP-NAME-IN     PIC X(30).
          05 EMP-POSITION-IN PIC X(20).
       
       FD OUTPUT-FILE.
       01 EMPLOYEE-OUTPUT-RECORD.
          05 EMP-ID-OUT       PIC 9(5).
          05 EMP-NAME-OUT     PIC X(30).
          05 EMP-POSITION-OUT PIC X(20).
          05 EMP-SALARY-OUT   PIC 9(6)V99.
          05 EMP-DEPT-OUT     PIC X(15).
       
       WORKING-STORAGE SECTION.
       01 WS-EOF             PIC A(1) VALUE 'N'.
       01 WS-COUNTER         PIC 9(3) VALUE 0.
       01 WS-SALARY          PIC 9(6)V99.
       01 WS-DEPT            PIC X(15).
       
       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           DISPLAY "Inizio Operazioni Dipendenti".
           
           OPEN INPUT INPUT-FILE.
           OPEN OUTPUT OUTPUT-FILE.
           
           PERFORM UNTIL WS-EOF = 'Y'
               READ INPUT-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       ADD 1 TO WS-COUNTER
                       
                       MOVE EMP-ID-IN TO EMP-ID-OUT
                       MOVE EMP-NAME-IN TO EMP-NAME-OUT
                       MOVE EMP-POSITION-IN TO EMP-POSITION-OUT
                       
                       EVALUATE EMP-POSITION-IN
                           WHEN "Sviluppatore"
                               MOVE 50000.00 TO WS-SALARY
                               MOVE "Tecnologia" TO WS-DEPT
                           WHEN "Direttrice"
                               MOVE 85000.00 TO WS-SALARY
                               MOVE "Management" TO WS-DEPT
                           WHEN "Analista"
                               MOVE 55000.00 TO WS-SALARY
                               MOVE "Analisi Dati" TO WS-DEPT
                           WHEN "Amministratrice"
                               MOVE 60000.00 TO WS-SALARY
                               MOVE "Amministrazione" TO WS-DEPT
                           WHEN "Tecnico"
                               MOVE 45000.00 TO WS-SALARY
                               MOVE "Supporto" TO WS-DEPT
                           WHEN "Responsabile HR"
                               MOVE 65000.00 TO WS-SALARY
                               MOVE "Risorse Umane" TO WS-DEPT
                           WHEN "Progettista"
                               MOVE 57000.00 TO WS-SALARY
                               MOVE "Progettazione" TO WS-DEPT
                           WHEN "Contabile"
                               MOVE 52000.00 TO WS-SALARY
                               MOVE "Finanza" TO WS-DEPT
                           WHEN "Consulente"
                               MOVE 70000.00 TO WS-SALARY
                               MOVE "Consulenza" TO WS-DEPT
                           WHEN "Coordinatrice"
                               MOVE 62000.00 TO WS-SALARY
                               MOVE "Coordinamento" TO WS-DEPT
                           WHEN OTHER
                               MOVE 40000.00 TO WS-SALARY
                               MOVE "Generale" TO WS-DEPT
                       END-EVALUATE
                       
                       MOVE WS-SALARY TO EMP-SALARY-OUT
                       MOVE WS-DEPT TO EMP-DEPT-OUT
                       
                       WRITE EMPLOYEE-OUTPUT-RECORD
                       
                       DISPLAY "Elaborato: " EMP-NAME-IN
                               " - Stipendio: " WS-SALARY
                               " - Dipartimento: " WS-DEPT
               END-READ
           END-PERFORM.
           
           CLOSE INPUT-FILE.
           CLOSE OUTPUT-FILE.
           
           DISPLAY "Elaborazione completata."
           DISPLAY "Record elaborati: " WS-COUNTER.
           DISPLAY "Dati salvati in dipendentiElaborati.dat".
           STOP RUN.