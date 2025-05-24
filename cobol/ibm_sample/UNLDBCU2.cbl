      **** UNLDBCU2- DB2 SAMPLE BATCH COBOL UNLOAD PROGRAM  ***********
      *                                                               *
      *   MODULE NAME = UNLDBCU2                                      *
      *                                                               *
      *   DESCRIPTIVE NAME = DB2  SAMPLE APPLICATION                  *
      *                      UNLOAD PROGRAM                           *
      *                      BATCH                                    *
      *                      ENTERPRISE COBOL FOR Z/OS                *
      *                                                               *
      *   COPYRIGHT = 5740-XYR (C) COPYRIGHT IBM CORP 1982, 1987      *
      *    REFER TO COPYRIGHT INSTRUCTIONS FORM NUMBER G120-2083      *
      *                                                               *
      *   STATUS = VERSION 1 RELEASE 3, LEVEL 0                       *
      *                                                               *
      *   FUNCTION = THIS MODULE ACCEPTS A TABLE NAME OR VIEW NAME    *
      *              AND UNLOADS THE DATA IN THAT TABLE OR VIEW.      *
      *    READ IN A TABLE NAME FROM SYSIN.                           *
      *    PUT DATA FROM THE TABLE INTO DD SYSREC01.                  *
      *    WRITE RESULTS TO SYSPRINT.                                 *
      *                                                               *
      *   NOTES =                                                     *
      *     DEPENDENCIES = IBM ENTERPRISE COBOL FOR Z/OS              *
      *                    IS REQUIRED.                               *
      *                                                               *
      *     RESTRICTIONS =                                            *
      *               THE SQLDA IS LIMITED TO 33016 BYTES.            *
      *               THIS SIZE ALLOWS FOR THE DB2 MAXIMUM            *
      *               OF 750 COLUMNS.                                 *
      *                                                               *
      *               DATA RECORDS ARE LIMITED TO 32700 BYTES,        *
      *               INCLUDING DATA, LENGTHS FOR VARCHAR DATA,       *
      *               AND SPACE FOR NULL INDICATORS.                  *
      *                                                               *
      *               TABLE OR VIEW NAMES ARE ACCEPTED, AND ONLY      *
      *               ONE NAME IS ALLOWED PER RUN.                    *
      *                                                               *
      *   MODULE TYPE = ENTERPRISE COBOL FOR Z/OS                     *
      *      PROCESSOR   = DB2  PRECOMPILER, COBOL COMPILER           *
      *      MODULE SIZE = SEE LINK EDIT                              *
      *      ATTRIBUTES  = REENTRANT                                  *
      *                                                               *
      *   ENTRY POINT = UNLDBCU2                                      *
      *      PURPOSE = SEE FUNCTION                                   *
      *      LINKAGE =                                                *
      *         CALL 'UNLDBCU2' USING WORKAREA-IND RECWORK.           *
      *                                                               *
      *      INPUT   = SYMBOLIC LABEL/NAME = WORKAREA-IND             *
      *                DESCRIPTION = INDICATOR VARIABLE ARRAY         *
      *                01  WORKAREA-IND.                              *
      *                 02  WORKIND PIC S9(4) COMP-5 OCCURS 750 TIMES.*
      *                                                               *
      *                SYMBOLIC LABEL/NAME = RECWORK                  *
      *                DESCRIPTION = WORK AREA FOR OUTPUT RECORD      *
      *                01  RECWORK.                                   *
      *                  02  RECWORK-LEN PIC S9(8) COMP.              *
      *                  02  RECWORK-CHAR PIC X(1) OCCURS 32700 TIMES.*
      *                                                               *
      *                SYMBOLIC LABEL/NAME = SYSIN                    *
      *                DESCRIPTION = INPUT REQUESTS - TABLE OR VIEW   *
      *                                                               *
      *      OUTPUT  = SYMBOLIC LABEL/NAME = SYSPRINT                 *
      *                DESCRIPTION = PRINTED RESULTS                  *
      *                                                               *
      *                SYMBOLIC LABEL/NAME = SYSREC01                 *
      *                DESCRIPTION = UNLOADED TABLE DATA              *
      *                                                               *
      *   EXIT-NORMAL = RETURN CODE 0 NORMAL COMPLETION               *
      *   EXIT-ERROR =                                                *
      *      RETURN CODE = NONE                                       *
      *      ABEND CODES =  NONE                                      *
      *      ERROR-MESSAGES =                                         *
      *          DSNT490I SAMPLE COBOL DATA UNLOAD PROGRAM RELEASE 3.0*
      *                   -  THIS IS THE HEADER, INDICATING A NORMAL  *
      *                   -  START FOR THIS PROGRAM.                  *
      *          DSNT493I SQL ERROR, SQLCODE = NNNNNNNN               *
      *                   -  AN SQL ERROR OR WARNING WAS ENCOUNTERED  *
      *                   -  ADDITIONAL INFORMATION FROM DSNTIAR      *
      *                   -  FOLLOWS THIS MESSAGE.                    *
      *          DSNT495I SUCCESSFUL UNLOAD XXXXXXXX ROWS OF          *
      *                   TABLE TTTTTTTT                              *
      *                   -  THE UNLOAD WAS SUCCESSFUL.  XXXXXXXX IS  *
      *                   -  THE NUMBER OF ROWS UNLOADED.  TTTTTTTT   *
      *                   -  IS THE NAME OF THE TABLE OR VIEW FROM    *
      *                   -  WHICH IT WAS UNLOADED.                   *
      *          DSNT496I UNRECOGNIZED DATA TYPE CODE OF NNNNN        *
      *                   -  THE PREPARE RETURNED AN INVALID DATA     *
      *                   -  TYPE CODE.  NNNNN IS THE CODE, PRINTED   *
      *                   -  IN DECIMAL.  USUALLY AN ERROR IN         *
      *                   -  THIS ROUTINE OR A NEW DATA TYPE.         *
      *          DSNT497I RETURN CODE FROM MESSAGE ROUTINE DSNTIAR    *
      *                   -  THE MESSAGE FORMATTING ROUTINE DETECTED  *
      *                   -  AN ERROR.  SEE THAT ROUTINE FOR RETURN   *
      *                   -  CODE INFORMATION.  USUALLY AN ERROR IN   *
      *                   -  THIS ROUTINE.                            *
      *          DSNT498I ERROR, NO VALID COLUMNS FOUND               *
      *                   -  THE PREPARE RETURNED DATA WHICH DID NOT  *
      *                   -  PRODUCE A VALID OUTPUT RECORD.           *
      *                   -  USUALLY AN ERROR IN THIS ROUTINE.        *
      *          DSNT499I NO ROWS FOUND IN TABLE OR VIEW              *
      *                   -  THE CHOSEN TABLE OR VIEWS DID NOT        *
      *                   -  RETURN ANY ROWS.                         *
      *          ERROR MESSAGES FROM MODULE DSNTIAR                   *
      *                   -  WHEN AN ERROR OCCURS, THIS MODULE        *
      *                   -  PRODUCES CORRESPONDING MESSAGES.         *
      *          OTHER MESSAGES:                                      *
      *               THE TABLE COULD NOT BE UNLOADED. EXITING.       *
      *                                                               *
      *   EXTERNAL REFERENCES =                                       *
      *      ROUTINES/SERVICES =                                      *
      *            DSNTIAR  - TRANSLATE SQLCA INTO MESSAGES           *
      *      DATA-AREAS        =    NONE                              *
      *      CONTROL-BLOCKS    =                                      *
      *            SQLCA    - SQL COMMUNICATION AREA                  *
      *                                                               *
      *   TABLES = NONE                                               *
      *   CHANGE-ACTIVITY = NONE                                      *
      *                                                               *
      *  *PSEUDOCODE*                                                 *
      *    PROCEDURE                                                  *
      *    EXEC SQL DECLARE DT CURSOR FOR SEL END-EXEC.               *
      *    EXEC SQL DECLARE SEL STATEMENT END-EXEC.                   *
      *    INITIALIZE THE DATA, OPEN FILES.                           *
      *    OBTAIN STORAGE FOR THE SQLDA AND THE DATA RECORDS.         *
      *    READ A TABLE NAME.                                         *
      *    OPEN SYSREC01.                                             *
      *    BUILD THE SQL STATEMENT TO BE EXECUTED                     *
      *    EXEC SQL PREPARE SQL STATEMENT INTO SQLDA END-EXEC.        *
      *    SET UP ADDRESSES IN THE SQLDA FOR DATA.                    *
      *    INITIALIZE DATA RECORD COUNTER TO 0.                       *
      *    EXEC SQL OPEN DT END-EXEC.                                 *
      *    DO WHILE SQLCODE IS 0.                                     *
      *    EXEC SQL FETCH DT USING DESCRIPTOR SQLDA END-EXEC.         *
      *    ADD IN MARKERS TO DENOTE NULLS.                            *
      *    WRITE THE DATA TO SYSREC01.                                *
      *    INCREMENT DATA RECORD COUNTER.                             *
      *    END.                                                       *
      *    EXEC SQL CLOSE DT END-EXEC.                                *
      *    INDICATE THE RESULTS OF THE UNLOAD OPERATION.              *
      *    CLOSE THE SYSIN, SYSPRINT, AND SYSREC01 FILES.             *
      *    END.                                                       *
      *---------------------------------------------------------------*
      /
       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID.    UNLDBCU2
      *
       ENVIRONMENT DIVISION.
      *--------------------
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SYSIN
                  ASSIGN TO DA-S-SYSIN.
           SELECT SYSPRINT
                  ASSIGN TO UT-S-SYSPRINT.
           SELECT SYSREC01
                  ASSIGN TO DA-S-SYSREC01.
      *
       DATA DIVISION.
      *-------------
      *
       FILE SECTION.
       FD      SYSIN
               RECORD CONTAINS 80 CHARACTERS
               BLOCK CONTAINS 0 RECORDS
               LABEL RECORDS ARE OMITTED
               RECORDING MODE IS F.
       01  CARDREC                    PIC X(80).
      *
       FD  SYSPRINT
               RECORD CONTAINS 120 CHARACTERS
               LABEL RECORDS ARE OMITTED
               DATA RECORD IS MSGREC
               RECORDING MODE IS F.
       01  MSGREC                     PIC X(120).
      *
       FD  SYSREC01
               RECORD CONTAINS 5 TO 32704 CHARACTERS
               LABEL RECORDS ARE OMITTED
               DATA RECORD IS REC01
               RECORDING MODE IS V.
       01  REC01.
                02  REC01-LEN PIC S9(8) COMP.
                02  REC01-CHAR PIC X(1) OCCURS 1 TO 32700 TIMES
                                DEPENDING ON REC01-LEN.
      /
       WORKING-STORAGE SECTION.
      *
      *****************************************************
      * STRUCTURE FOR INPUT                               *
      *****************************************************
       01  IOAREA.
               02  TNAME         PIC X(72).
               02  FILLER        PIC X(08).
       01  STMTBUF.
               49  STMTLEN       PIC S9(4) COMP-5 VALUE 92.
               49  STMTCHAR      PIC X(92).
       01  STMTBLD.
               02  FILLER        PIC X(20) VALUE 'SELECT * FROM'.
               02  STMTTAB       PIC X(72).
      *
      *****************************************************
      * REPORT HEADER STRUCTURE                           *
      *****************************************************
       01  HEADER.
               02  FILLER PIC X(35)
                   VALUE ' DSNT490I SAMPLE COBOL DATA UNLOAD '.
               02  FILLER PIC X(85) VALUE 'PROGRAM RELEASE 3.0'.
       01  MSG-SQLERR.
               02  FILLER PIC X(31)
                   VALUE ' DSNT493I SQL ERROR, SQLCODE = '.
               02  MSG-MINUS       PIC X(1).
               02  MSG-PRINT-CODE  PIC 9(8).
               02  FILLER PIC X(81) VALUE '            '.
       01  MSG-OTHER-ERR.
               02  FILLER PIC X(42)
                   VALUE ' THE TABLE COULD NOT BE UNLOADED. EXITING.'.
               02  FILLER PIC X(78) VALUE '            '.
       01  UNLOADED.
               02  FILLER PIC X(28)
                   VALUE ' DSNT495I SUCCESSFUL UNLOAD '.
               02  ROWS   PIC 9(8).
               02  FILLER PIC X(15) VALUE ' ROWS OF TABLE '.
               02  TABLENAM PIC X(72) VALUE '        '.
       01  BADTYPE.
               02  FILLER PIC X(42)
                   VALUE ' DSNT496I UNRECOGNIZED DATA TYPE CODE OF  '.
               02  TYPCOD PIC 9(8).
               02  FILLER PIC X(71) VALUE '         '.
       01  MSGRETCD.
               02  FILLER PIC X(42)
                   VALUE ' DSNT497I RETURN CODE FROM MESSAGE ROUTINE'.
               02  FILLER PIC X(9) VALUE 'DSNTIAR '.
               02  RETCODE    PIC 9(8).
               02  FILLER PIC X(62) VALUE '          '.
       01  MSGNOCOL.
               02  FILLER PIC X(120)
                   VALUE ' DSNT498I ERROR, NO VALID COLUMNS FOUND'.
       01  MSG-NOROW.
               02  FILLER PIC X(120)
                   VALUE ' DSNT499I NO ROWS FOUND IN TABLE OR VIEW'.
      *****************************************************
      * WORKAREAS                                         *
      *****************************************************
       77  NOT-FOUND         PIC S9(8) COMP-5 VALUE  +100.
      *****************************************************
      * VARIABLES FOR ERROR-MESSAGE FORMATTING             *
      *****************************************************
       01  ERROR-MESSAGE.
               02  ERROR-LEN   PIC S9(4)  COMP-5 VALUE +960.
               02  ERROR-TEXT  PIC X(120) OCCURS 8 TIMES
                                          INDEXED BY ERROR-INDEX.
       77  ERROR-TEXT-LEN      PIC S9(8)  COMP-5 VALUE +120.
      *****************************************************
      * SQL DESCRIPTOR AREA                               *
      *****************************************************
       01  SQLDA.
               02  SQLDAID     PIC X(8)   VALUE 'SQLDA   '.
               02  SQLDABC     PIC S9(8) COMPUTATIONAL  VALUE 33016.
               02  SQLN        PIC S9(4) COMP-5  VALUE 750.
               02  SQLD        PIC S9(4) COMP-5  VALUE 0.
               02  SQLVAR      OCCURS 1 TO 750 TIMES
                                        DEPENDING ON SQLN.
                   03  SQLTYPE     PIC S9(4) COMP-5.
                   03  SQLLEN      PIC S9(4) COMP-5.
                   03  SQLDATA     POINTER.
                   03  SQLIND      POINTER.
                   03  SQLNAME.
                       49  SQLNAMEL    PIC S9(4) COMP-5.
                       49  SQLNAMEC    PIC X(30).
      *
      *  DATA TYPES FOUND IN SQLTYPE, AFTER REMOVING THE NULL BIT
      *
       77  VARCTYPE            PIC S9(4)  COMP-5 VALUE +448.
       77  CHARTYPE            PIC S9(4)  COMP-5 VALUE +452.
       77  VARLTYPE            PIC S9(4)  COMP-5 VALUE +456.
       77  VARGTYPE            PIC S9(4)  COMP-5 VALUE +464.
       77  GTYPE               PIC S9(4)  COMP-5 VALUE +468.
       77  LVARGTYP            PIC S9(4)  COMP-5 VALUE +472.
       77  FLOATYPE            PIC S9(4)  COMP-5 VALUE +480.
       77  DECTYPE             PIC S9(4)  COMP-5 VALUE +484.
       77  INTTYPE             PIC S9(4)  COMP-5 VALUE +496.
       77  HWTYPE              PIC S9(4)  COMP-5 VALUE +500.
       77  DATETYP             PIC S9(4)  COMP-5 VALUE +384.
       77  TIMETYP             PIC S9(4)  COMP-5 VALUE +388.
       77  TIMESTMP            PIC S9(4)  COMP-5 VALUE +392.
      *
       01  RECPTR POINTER.
       01  RECNUM REDEFINES RECPTR PICTURE S9(8) COMPUTATIONAL.
       01  IRECPTR POINTER.
       01  IRECNUM REDEFINES IRECPTR PICTURE S9(8) COMPUTATIONAL.
       01  I      PICTURE S9(4) COMPUTATIONAL.
       01  J      PICTURE S9(4) COMPUTATIONAL.
       01  DUMMY  PICTURE S9(4) COMPUTATIONAL.
       01  MYTYPE PICTURE S9(4) COMPUTATIONAL.
       01  COLUMN-IND PICTURE S9(4) COMPUTATIONAL.
       01  COLUMN-LEN PICTURE S9(4) COMPUTATIONAL.
       01  COLUMN-PREC PICTURE S9(4) COMPUTATIONAL.
       01  COLUMN-SCALE PICTURE S9(4) COMPUTATIONAL.
       01  INDCOUNT           PIC S9(4) COMPUTATIONAL.
       01  ROWCOUNT           PIC S9(4) COMPUTATIONAL.
       01  ERR-FOUND PICTURE X(1).
       01  WORKAREA2.
               02  WORKINDPTR  POINTER    OCCURS 750 TIMES.
      *****************************************************
      *   DECLARE CURSOR AND STATEMENT FOR DYNAMIC SQL
      *****************************************************
      *
                EXEC SQL DECLARE DT CURSOR FOR SEL  END-EXEC.
                EXEC SQL DECLARE SEL STATEMENT      END-EXEC.
      *
      *****************************************************
      * SQL INCLUDE FOR SQLCA                             *
      *****************************************************
                EXEC SQL INCLUDE SQLCA  END-EXEC.
      *
       77  ONE                 PIC S9(4)  COMP-5 VALUE +1.
       77  TWO                 PIC S9(4)  COMP-5 VALUE +2.
       77  FOUR                PIC S9(4)  COMP-5 VALUE +4.
       77  QMARK               PIC X(1)        VALUE '?'.
      *
       LINKAGE SECTION.
       01  LINKAREA-IND.
               02  IND   PIC   S9(4) COMP-5 OCCURS 750 TIMES.
       01  LINKAREA-REC.
                02  REC1-LEN PIC S9(8) COMP.
                02  REC1-CHAR PIC X(1) OCCURS 1 TO 32700 TIMES
                                 DEPENDING ON REC1-LEN.
       01  LINKAREA-QMARK.
               02  INDREC PIC   X(1).
      /
       PROCEDURE DIVISION USING LINKAREA-IND LINKAREA-REC.
      *
      *****************************************************
      * SQL RETURN CODE HANDLING                          *
      *****************************************************
           EXEC SQL WHENEVER SQLERROR   GOTO DBERROR END-EXEC.
           EXEC SQL WHENEVER SQLWARNING GOTO DBERROR END-EXEC.
           EXEC SQL WHENEVER NOT FOUND  CONTINUE     END-EXEC.
      *
      *****************************************************
      * MAIN PROGRAM ROUTINE                              *
      *****************************************************
                SET IRECPTR TO ADDRESS OF REC1-CHAR(1).
      *                                          **OPEN FILES
                MOVE 'N' TO ERR-FOUND.
      *                                          **INITIALIZE
      *                                          ** ERROR FLAG
                OPEN INPUT  SYSIN

                     OUTPUT SYSPRINT
                     OUTPUT SYSREC01.
      *                                          **WRITE HEADER
                WRITE MSGREC FROM HEADER
                      AFTER ADVANCING 2 LINES.
      *                                          **GET FIRST INPUT
                READ SYSIN  RECORD INTO IOAREA.
      *                                          **MAIN ROUTINE
                PERFORM PROCESS-INPUT THROUGH IND-RESULT.
      *
       PROG-END.
      *                                           **CLOSE FILES
                CLOSE SYSIN
                      SYSPRINT
                      SYSREC01.
                GOBACK.
      /
      ***************************************************************
      *                                                             *
      *    PERFORMED SECTION:                                       *
      *    PROCESSING FOR THE TABLE OR VIEW JUST READ               *
      *                                                             *
      ***************************************************************
       PROCESS-INPUT.
      *
           MOVE TNAME TO STMTTAB.
           MOVE STMTBLD TO STMTCHAR.
           MOVE +750 TO SQLN.
           EXEC SQL PREPARE SEL INTO :SQLDA FROM :STMTBUF  END-EXEC.
      ***************************************************************
      *                                                             *
      *    SET UP ADDRESSES IN THE SQLDA FOR DATA.                  *
      *                                                             *
      ***************************************************************
           IF SQLD = ZERO THEN
              WRITE MSGREC FROM MSGNOCOL
                      AFTER ADVANCING 2 LINES
              MOVE 'Y' TO ERR-FOUND
              GO TO IND-RESULT.
           MOVE ZERO TO ROWCOUNT.
           MOVE ZERO TO REC1-LEN.
           SET RECPTR TO IRECPTR.
           MOVE ONE TO I.
           PERFORM COLADDR UNTIL I > SQLD.
      ****************************************************************
      *                                                              *
      *    SET LENGTH OF OUTPUT RECORD.                              *
      *    EXEC  SQL OPEN DT END-EXEC.                               *
      *    DO WHILE SQLCODE IS 0.                                    *
      *       EXEC SQL FETCH DT USING DESCRIPTOR :SQLDA END-EXEC.    *
      *       ADD IN MARKERS TO DENOTE NULLS.                        *
      *       WRITE THE DATA TO SYSREC01.                            *
      *       INCREMENT DATA RECORD COUNTER.                         *
      *    END.                                                      *
      *                                                              *
      ****************************************************************
      *                                         **OPEN CURSOR
           EXEC SQL OPEN DT  END-EXEC.
           PERFORM BLANK-REC.
           EXEC SQL FETCH DT USING DESCRIPTOR :SQLDA END-EXEC.
      *                                          **NO ROWS FOUND
      *                                          **PRINT ERROR MESSAGE
                IF SQLCODE = NOT-FOUND
                   WRITE MSGREC FROM MSG-NOROW
                      AFTER ADVANCING 2 LINES
                   MOVE 'Y' TO ERR-FOUND
                ELSE
      *                                          **WRITE ROW AND
      *                                          **CONTINUE UNTIL
      *                                          **NO MORE ROWS
                   PERFORM WRITE-AND-FETCH
                      UNTIL SQLCODE IS NOT EQUAL TO ZERO.
      *
           EXEC SQL WHENEVER NOT FOUND  GOTO CLOSEDT    END-EXEC.
      *
       CLOSEDT.
           EXEC SQL CLOSE DT  END-EXEC.
      *
      ****************************************************************
      *                                                              *
      *    INDICATE THE RESULTS OF THE UNLOAD OPERATION.             *
      *                                                              *
      ****************************************************************
       IND-RESULT.
           IF ERR-FOUND = 'N' THEN
                MOVE TNAME TO TABLENAM
                MOVE ROWCOUNT TO ROWS
                WRITE MSGREC FROM UNLOADED
                      AFTER ADVANCING 2 LINES
           ELSE
                WRITE MSGREC FROM MSG-OTHER-ERR
                      AFTER ADVANCING 2 LINES
                MOVE +0012 TO RETURN-CODE
                GO TO PROG-END.
      *
       WRITE-AND-FETCH.
      *        ADD IN MARKERS TO DENOTE NULLS.
               MOVE ONE TO INDCOUNT.
               PERFORM NULLCHK UNTIL INDCOUNT = SQLD.
               MOVE REC1-LEN TO REC01-LEN.
               WRITE REC01 FROM LINKAREA-REC.
               ADD ONE TO ROWCOUNT.
               PERFORM BLANK-REC.
               EXEC SQL FETCH DT USING DESCRIPTOR :SQLDA END-EXEC.
      *
       NULLCHK.
               IF IND(INDCOUNT) < 0 THEN
                  SET ADDRESS OF LINKAREA-QMARK TO WORKINDPTR(INDCOUNT)
                  MOVE QMARK TO INDREC.
               ADD ONE TO INDCOUNT.
      *****************************************************
      *    BLANK OUT RECORD TEXT FIRST                    *
      *****************************************************
       BLANK-REC.
                MOVE ONE TO J.
                PERFORM BLANK-MORE UNTIL J > REC1-LEN.
       BLANK-MORE.
                MOVE ' ' TO REC1-CHAR(J).
                ADD  ONE TO J.
      *
       COLADDR.
            SET SQLDATA(I) TO RECPTR.
      ****************************************************************
      *
      *        DETERMINE THE LENGTH OF THIS COLUMN (COLUMN-LEN)
      *        THIS DEPENDS UPON THE DATA TYPE.  MOST DATA TYPES HAVE
      *        THE LENGTH SET, BUT VARCHAR, GRAPHIC, VARGRAPHIC, AND
      *        DECIMAL DATA NEED TO HAVE THE BYTES CALCULATED.
      *        THE NULL ATTRIBUTE MUST BE SEPARATED TO SIMPLIFY MATTERS.
      *
      ****************************************************************
            MOVE SQLLEN(I) TO COLUMN-LEN.
      *        COLUMN-IND IS 0 FOR NO NULLS AND 1 FOR NULLS
            DIVIDE SQLTYPE(I) BY TWO GIVING DUMMY REMAINDER COLUMN-IND.
      *        MYTYPE IS JUST THE SQLTYPE WITHOUT THE NULL BIT
            MOVE SQLTYPE(I) TO MYTYPE.
            SUBTRACT COLUMN-IND FROM MYTYPE.
      *        SET THE COLUMN LENGTH, DEPENDENT UPON DATA TYPE
            EVALUATE MYTYPE
               WHEN     CHARTYPE  CONTINUE,
               WHEN     DATETYP   CONTINUE,
               WHEN     TIMETYP   CONTINUE,
               WHEN     TIMESTMP  CONTINUE,
               WHEN     FLOATYPE  CONTINUE,
               WHEN     VARCTYPE
                     ADD TWO TO COLUMN-LEN,
               WHEN     VARLTYPE
                     ADD TWO TO COLUMN-LEN,
               WHEN     GTYPE
                     MULTIPLY COLUMN-LEN BY TWO GIVING COLUMN-LEN,
               WHEN     VARGTYPE
                     PERFORM CALC-VARG-LEN,
               WHEN     LVARGTYP
                     PERFORM CALC-VARG-LEN,
               WHEN     HWTYPE
                     MOVE TWO TO COLUMN-LEN,
               WHEN     INTTYPE
                     MOVE FOUR TO COLUMN-LEN,
               WHEN     DECTYPE
                     PERFORM CALC-DECIMAL-LEN,
               WHEN     OTHER
                     PERFORM UNRECOGNIZED-ERROR,
            END-EVALUATE.
            ADD COLUMN-LEN TO RECNUM.
            ADD COLUMN-LEN TO REC1-LEN.
      ****************************************************************
      *                                                              *
      *    IF THIS COLUMN CAN BE NULL, AN INDICATOR VARIABLE IS      *
      *    NEEDED.  WE ALSO RESERVE SPACE IN THE OUTPUT RECORD TO    *
      *    NOTE THAT THE VALUE IS NULL.                              *
      *                                                              *
      ****************************************************************
           MOVE ZERO TO IND(I).
           IF COLUMN-IND = ONE THEN
              SET SQLIND(I) TO ADDRESS OF IND(I)
              SET WORKINDPTR(I) TO RECPTR
              ADD ONE TO RECNUM
              ADD ONE TO REC1-LEN.
      *
           ADD ONE TO I.
      *        PERFORMED PARAGRAPH TO CALCULATE COLUMN LENGTH
      *        FOR A DECIMAL DATA TYPE COLUMN
       CALC-DECIMAL-LEN.
               DIVIDE COLUMN-LEN BY 256 GIVING COLUMN-PREC
                        REMAINDER COLUMN-SCALE.
               MOVE COLUMN-PREC TO COLUMN-LEN.
               ADD ONE TO COLUMN-LEN.
               DIVIDE COLUMN-LEN BY TWO GIVING COLUMN-LEN.
      *        PERFORMED PARAGRAPH TO CALCULATE COLUMN LENGTH
      *        FOR A VARGRAPHIC DATA TYPE COLUMN
       CALC-VARG-LEN.
               MULTIPLY COLUMN-LEN BY TWO GIVING COLUMN-LEN.
               ADD TWO TO COLUMN-LEN.
      *        PERFORMED PARAGRAPH TO NOTE AN UNRECOGNIZED
      *        DATA TYPE COLUMN
       UNRECOGNIZED-ERROR.
      *
      *        ERROR MESSAGE FOR UNRECOGNIZED DATA TYPE
      *
               MOVE  SQLTYPE(I) TO TYPCOD
               MOVE 'Y' TO ERR-FOUND
               WRITE MSGREC FROM BADTYPE
                      AFTER ADVANCING 2 LINES
               GO TO IND-RESULT.
      *
      *****************************************************
      * SQL ERROR OCCURRED - GET MESSAGE                  *
      *****************************************************
       DBERROR.
      *                                          **SQL ERROR
                MOVE 'Y' TO ERR-FOUND.
                MOVE SQLCODE TO MSG-PRINT-CODE.
                IF SQLCODE < 0 THEN MOVE '-' TO MSG-MINUS.
                WRITE MSGREC FROM MSG-SQLERR
                   AFTER ADVANCING 2 LINES.
                CALL 'DSNTIAR' USING SQLCA ERROR-MESSAGE ERROR-TEXT-LEN.
                IF RETURN-CODE = ZERO
                   PERFORM ERROR-PRINT VARYING ERROR-INDEX
                      FROM 1 BY 1 UNTIL ERROR-INDEX GREATER THAN 8
                ELSE
      *                                       **ERROR FOUND IN DSNTIAR
      *                                       **PRINT ERROR MESSAGE
                   MOVE RETURN-CODE TO RETCODE
                   WRITE MSGREC FROM MSGRETCD
                      AFTER ADVANCING 2 LINES.
                GO TO IND-RESULT.
      *
      *****************************************************
      *    PRINT MESSAGE TEXT                             *
      *****************************************************
       ERROR-PRINT.
                WRITE MSGREC FROM ERROR-TEXT (ERROR-INDEX)
                   AFTER ADVANCING 1 LINE.