      **** UNLDBCU1- DB2 SAMPLE BATCH COBOL UNLOAD PROGRAM  ***********
      *                                                               *
      *   MODULE NAME = UNLDBCU1                                      *
      *                                                               *
      *   DESCRIPTIVE NAME = DB2  SAMPLE APPLICATION                  *
      *                      UNLOAD PROGRAM                           *
      *                      BATCH                                    *
      *                      IBM ENTERPRISE COBOL FOR Z/OS            *
      *                                                               *
      *   COPYRIGHT = 5740-XYR (C) COPYRIGHT IBM CORP 1982, 1987      *
      *    REFER TO COPYRIGHT INSTRUCTIONS FORM NUMBER G120-2083      *
      *                                                               *
      *   STATUS = VERSION 1 RELEASE 3, LEVEL 0                       *
      *                                                               *
      *   FUNCTION = THIS MODULE PROVIDES THE STORAGE NEEDED BY       *
      *              UNLDBCU2 AND CALLS THAT PROGRAM.                 *
      *                                                               *
      *   NOTES =                                                     *
      *     DEPENDENCIES = ENTERPRISE COBOL FOR Z/OS IS REQUIRED.     *
      *                    SEVERAL NEW FACILITIES ARE USED.           *
      *                                                               *
      *     RESTRICTIONS =                                            *
      *               THE MAXIMUM NUMBER OF COLUMNS IS 750,           *
      *               WHICH IS THE SQL LIMIT.                         *
      *                                                               *
      *               DATA RECORDS ARE LIMITED TO 32700 BYTES,        *
      *               INCLUDING DATA, LENGTHS FOR VARCHAR DATA,       *
      *               AND SPACE FOR NULL INDICATORS.                  *
      *                                                               *
      *   MODULE TYPE = IBM ENTERPRISE COBOL PROGRAM                  *
      *      PROCESSOR   = ENTERPRISE COBOL FOR Z/OS                  *
      *      MODULE SIZE = SEE LINK EDIT                              *
      *      ATTRIBUTES  = REENTRANT                                  *
      *                                                               *
      *   ENTRY POINT = UNLDBCU1                                      *
      *      PURPOSE = SEE FUNCTION                                   *
      *      LINKAGE = INVOKED FROM DSN RUN                           *
      *      INPUT   = NONE                                           *
      *      OUTPUT  = NONE                                           *
      *                                                               *
      *   EXIT-NORMAL = RETURN CODE 0 NORMAL COMPLETION               *
      *                                                               *
      *   EXIT-ERROR =                                                *
      *      RETURN CODE = NONE                                       *
      *      ABEND CODES =  NONE                                      *
      *      ERROR-MESSAGES = NONE                                    *
      *                                                               *
      *   EXTERNAL REFERENCES =                                       *
      *      ROUTINES/SERVICES =                                      *
      *            UNLDBCU2 - ACTUAL UNLOAD PROGRAM                   *
      *                                                               *
      *      DATA-AREAS        =    NONE                              *
      *      CONTROL-BLOCKS    =    NONE                              *
      *                                                               *
      *   TABLES = NONE                                               *
      *   CHANGE-ACTIVITY = NONE                                      *
      *                                                               *
      *  *PSEUDOCODE*                                                 *
      *                                                               *
      *    PROCEDURE                                                  *
      *    CALL UNLDBCU2.                                             *
      *    END.                                                       *
      *---------------------------------------------------------------*
      /
       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID.    UNLDBCU1
      *
       ENVIRONMENT DIVISION.
      *
       CONFIGURATION SECTION.
       DATA DIVISION.
      *
       WORKING-STORAGE SECTION.
      *
       01  WORKAREA-IND.
               02  WORKIND PIC S9(4) COMP-5 OCCURS 750 TIMES.
       01  RECWORK.
               02  RECWORK-LEN PIC S9(8) COMP-5 VALUE 32700.
               02  RECWORK-CHAR PIC X(1) OCCURS 32700 TIMES.
      *
       PROCEDURE DIVISION.
      *
                CALL 'UNLDBCU2' USING WORKAREA-IND RECWORK.
                GOBACK.