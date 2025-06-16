      ******************************************************************
      * SISTEMA DI GESTIONE CONTI CORRENTI BANCARI
      * Programma principale per la gestione di conti correnti
      * Include: apertura conti, depositi, prelievi, estratto conto
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANK-SYSTEM.
       
       ENVIRONMENT DIVISION.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-BALANCE    PIC 9(7)V99.
       
       PROCEDURE DIVISION.
           DISPLAY "Bank System Starting"
           MOVE 1000.00 TO WS-BALANCE
           DISPLAY "Current Balance: " WS-BALANCE
           STOP RUN.