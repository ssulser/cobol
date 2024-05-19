//SIMONCO JOB (1),'PAYRATE',CLASS=A,MSGCLASS=A
//COBOL  EXEC PROC=COB2UCG,SYSOUT='*'
//SYSIN   DD *
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAYRATE2.
       AUTHOR. SIMON SULSER.
       DATE-WRITTEN. MAY 18,1924.
       DATE-COMPILED.
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PAY-FILE ASSIGN TO UT-S-INPUT.
           SELECT PRINT-FILE ASSIGN TO UT-S-OUTPUT.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  PAY-FILE
           LABEL RECORD IS OMITTED.
       01  PAY-RECORD.
           05 EMP-POLICY-NR            PIC X(10).
           05 EMP-COMPANY              PIC X(30).
           05 EMP-AMOUNT               PIC 9(7).
           05 FILLER                   PIC XX.
           05 EMP-FACTOR1              PIC 9V99.
           05 FILLER                   PIC XX.
           05 EMP-FACTOR2              PIC 9V99.
           05 FILLER                   PIC XX.
           05 EMP-FACTOR3              PIC 9V99.
           05 FILLER                   PIC X(18).
      *
       FD  PRINT-FILE
           LABEL RECORD IS OMITTED.
       01  PRINT-LINE                  PIC X(132).
      *
       WORKING-STORAGE SECTION.
      *
       77  FLAG-EOF                    PIC X           VALUE "N".
           88 IS-EOF                                   VALUE "Y".
      *
       01  REPORT-HEADER.
           05 FILLER                   PIC X(51)       VALUE SPACES.
           05 FILLER                   PIC X(30)       VALUE
                 "ANALYSIS OF INSURANCE COVERAGE".
      *
       01  REPORT-TITLE1.
           05 FILLER                   PIC X(20)       VALUE SPACES.
           05 FILLER                   PIC X(06)       VALUE "POLICY".
           05 FILLER                   PIC X(50)       VALUE SPACES.
           05 FILLER                   PIC X(42)       VALUE
               "PAYMENT IF      PAYMENT IF      PAYMENT IF".
      *
       01  REPORT-TITLE2.
           05 FILLER                   PIC X(20)       VALUE SPACES.
           05 FILLER                   PIC X(06)       VALUE "NUMBER".
           05 FILLER                   PIC X(09)       VALUE SPACES.
           05 FILLER                   PIC X(17)       VALUE
               "INSURANCE COMPANY".
           05 FILLER                   PIC X(22)       VALUE SPACES.
           05 FILLER                   PIC X(43)       VALUE
               "NON-ACCIDENTAL     DEATH BY        DEATH IN".
      *
       01  REPORT-TITLE3.
           05 FILLER                   PIC X(78)       VALUE SPACES.
           05 FILLER                   PIC X(42)       VALUE
               "DEATH          ACCIDENT     COMMON CARRIER".
      *
       01  REPORT-LINE.
           05 FILLER                   PIC X(20)       VALUE SPACES.
           05 RPT-POLICY-NR            PIC X(10).
           05 FILLER                   PIC X(05)       VALUE SPACES.
           05 RPT-COMPANY              PIC X(30).
           05 FILLER                   PIC X(04)       VALUE SPACES.
           05 RPT-AMOUNT1              PIC Z,ZZZ,ZZ9.99.
           05 FILLER                   PIC X(06)       VALUE SPACES.
           05 RPT-AMOUNT2              PIC Z,ZZZ,ZZ9.99.
           05 FILLER                   PIC X(06)       VALUE SPACES.
           05 RPT-AMOUNT3              PIC Z,ZZZ,ZZ9.99.
      *
       01  REPORT-TOTAL.
           05  FILLER                  PIC X(55)       VALUE SPACES.
           05  FILLER                  PIC X(12)       VALUE
               "TOTAL PAID  ".
           05  RPT-TOTAL1              PIC $ZZ,Z(3),ZZ9.99BBBB.
           05  RPT-TOTAL2              PIC $ZZ,Z(3),ZZ9.99BBBB.
           05  RPT-TOTAL3              PIC $ZZ,Z(3),ZZ9.99.
      *
       01  INSURANCE-CALCULATIONS.
           05  TOTAL-AMOUNT1           PIC 9(7)V99     VALUE 0.0.
           05  TOTAL-AMOUNT2           PIC 9(7)V99     VALUE 0.0.
           05  TOTAL-AMOUNT3           PIC 9(7)V99     VALUE 0.0.
           05  ACTUAL-AMOUNT1          PIC 9(6)V99     VALUE 0.0.
           05  ACTUAL-AMOUNT2          PIC 9(6)V99     VALUE 0.0.
           05  ACTUAL-AMOUNT3          PIC 9(6)V99     VALUE 0.0.
      *
       PROCEDURE DIVISION.
       000-MAIN.
           PERFORM INITIALIZATION.
           PERFORM READ-AND-PRINT UNTIL IS-EOF.
           PERFORM PRINT-TOTAL.
           PERFORM CLOSING.
           STOP RUN.
      *
       INITIALIZATION.
           OPEN INPUT  PAY-FILE,
                OUTPUT PRINT-FILE.
           WRITE PRINT-LINE FROM REPORT-HEADER.
           WRITE PRINT-LINE FROM REPORT-TITLE1 AFTER 2 LINES.
           WRITE PRINT-LINE FROM REPORT-TITLE2.
           WRITE PRINT-LINE FROM REPORT-TITLE3.
           MOVE SPACE TO PRINT-LINE.
           WRITE PRINT-LINE AFTER 2 LINES.
           READ PAY-FILE AT END MOVE "Y" TO FLAG-EOF.
      *
       READ-AND-PRINT.
           MOVE EMP-POLICY-NR TO RPT-POLICY-NR.
           MOVE EMP-COMPANY TO RPT-COMPANY.
           MULTIPLY EMP-FACTOR1 BY EMP-AMOUNT GIVING ACTUAL-AMOUNT1.
           MULTIPLY EMP-FACTOR2 BY EMP-AMOUNT GIVING ACTUAL-AMOUNT2.
           MULTIPLY EMP-FACTOR3 BY EMP-AMOUNT GIVING ACTUAL-AMOUNT3.
           ADD ACTUAL-AMOUNT1 TO TOTAL-AMOUNT1.
           ADD ACTUAL-AMOUNT2 TO TOTAL-AMOUNT2.
           ADD ACTUAL-AMOUNT3 TO TOTAL-AMOUNT3.
           MOVE ACTUAL-AMOUNT1 TO RPT-AMOUNT1.
           MOVE ACTUAL-AMOUNT2 TO RPT-AMOUNT2.
           MOVE ACTUAL-AMOUNT3 TO RPT-AMOUNT3.
           WRITE PRINT-LINE FROM REPORT-LINE.
           READ PAY-FILE AT END MOVE "Y" TO FLAG-EOF.
      *
       PRINT-TOTAL.
           MOVE TOTAL-AMOUNT1 TO RPT-TOTAL1.
           MOVE TOTAL-AMOUNT2 TO RPT-TOTAL2.
           MOVE TOTAL-AMOUNT3 TO RPT-TOTAL3.
           WRITE PRINT-LINE FROM REPORT-TOTAL AFTER 2 LINES.
      *
       CLOSING.
           CLOSE PAY-FILE, PRINT-FILE.
/*
//GO.INPUT DD *
1975.21.21WORLD BEST INSURANCE INCORP.  0010000  000  120  220
2015.87.85ZURICH LIFE INSURANCE AG      0085000  000  160  199
1989.21.89AMSTERDAM SECURE LIFE         0060000  010  115  175
1999.78.65NEW YORK BEST LIFE INCORP.    0007250  015  125  201
/*
//GO.OUTPUT DD SYSOUT=*,
//             DCB=(RECFM=FBA,LRECL=132,BLKSIZE=13200)
//
