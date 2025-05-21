       IDENTIFICATION DIVISION.
       PROGRAM-ID. SIMPLE.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-MESSAGE PIC X(30) VALUE "Funzia valore assegnato".
       
       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           DISPLAY "Inizio display".
           DISPLAY WS-MESSAGE.
           DISPLAY "Fine display".
           STOP RUN.
