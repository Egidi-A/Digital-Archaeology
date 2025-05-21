       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCULATOR.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 NUM1 PIC 9(5) VALUE 10.
       01 NUM2 PIC 9(5) VALUE 20.
       01 RESULT PIC 9(6).
       
       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           DISPLAY "Calcoletti".
           DISPLAY "Numero 1: " NUM1.
           DISPLAY "Numero 2: " NUM2.
           
           ADD NUM1 TO NUM2 GIVING RESULT.
           DISPLAY "Somma: " RESULT.
           
           SUBTRACT NUM1 FROM NUM2 GIVING RESULT.
           DISPLAY "Sottrazione (NUM2-NUM1): " RESULT.
           
           MULTIPLY NUM1 BY NUM2 GIVING RESULT.
           DISPLAY "Moltiplicazione: " RESULT.
           
           DIVIDE NUM1 INTO NUM2 GIVING RESULT.
           DISPLAY "Divisione (NUM2/NUM1): " RESULT.
           
           DISPLAY "Fine calcoletti".
           STOP RUN.
