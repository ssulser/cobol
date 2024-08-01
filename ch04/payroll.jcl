//SIMONCO JOB (1),'PRAYROLL CH04',CLASS=A,MSGCLASS=A
//COBOL  EXEC PROC=COB2UCG,SYSOUT='*'
//SYSIN   DD *
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAYROLL.
       AUTHOR. SIMON SULSER.
       DATE-WRITTEN. MAY 25,1924.
       DATE-COMPILED.
      *
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE   ASSIGN TO UT-S-INPUT.
           SELECT PRINT-FILE   ASSIGN TO UT-S-OUTPUT.
      *
      *
       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE
           LABEL RECORD IS OMITTED.
       01  INPUT-RECORD.
           05 EMP-NAME                 PIC X(25).
           05 EMP-ID                   PIC X(10).
           05 EMP-DEPARTMENT           PIC X(15).
           05 EMP-PAYRATE              PIC 99V99.
           05 EMP-SALARY-CODE          PIC X.
           05 EMP-HOURS-WORKED         PIC 99.
           05 FILLER                   PIC X(23).
      *
       FD  PRINT-FILE
           LABEL RECORD IS OMITTED
           LINAGE IS 40
           LINES AT TOP 3
           LINES AT BOTTOM 3
           WITH FOOTING 38.
       01  PRINT-LINE                  PIC X(132).
      *
      *
       WORKING-STORAGE SECTION.
       77  END-OF-FILE                 PIC X           VALUE "N".
           88  IS-EOF                                  VALUE "Y".
      *
       77  PAGE-COUNT                  PIC 99          VALUE 0.
      *
       77  ACTUAL-DEPARTMENT           PIC X(15)       VALUE SPACES.
      *
       77  PAGE-STATUS                 PIC X           VALUE "Y".
           88 IS-FIRST-PAGE                            VALUE "Y".
           88 IS-NOT-FIRST-PAGE                        VALUE "N".
      *
       01  PAGE-TITLE-LINE.
           05 FILLER                   PIC X(42)       VALUE SPACES.
           05 FILLER                   PIC X(58)       VALUE
         "H A L  I N D U S T R I E S  --  P A Y R O L L  R E P O R T".
      *
       01  PAGE-NUMBER-LINE.
           05 FILLER                   PIC X(119)      VALUE SPACES.
           05 FILLER                   PIC X(06)       VALUE "PAGE: ".
           05 PAGE-NUMBER              PIC Z9.
      *
       01  DEPARTMENT-LINE.
           05 FILLER                   PIC X(04)       VALUE SPACES.
           05 FILLER                   PIC X(12)       VALUE
              "DEPARTMENT: ".
           05 EMP-DEPARTMENT           PIC X(15).
      *
       01  TITLE-LINE.
           05 FILLER                   PIC X(09)       VALUE SPACES.
           05 FILLER                   PIC X(12)       VALUE
              "EMPLOYEE ID:".
           05 FILLER                   PIC X(03)       VALUE SPACES.
           05 FILLER                   PIC X(14)       VALUE
              "EMPLOYEE NAME:".
           05 FILLER                   PIC X(15)       VALUE SPACES.
           05 FILLER                   PIC X(05)       VALUE
              "CODE:".
           05 FILLER                   PIC X(05)       VALUE SPACES.
           05 FILLER                   PIC X(06)       VALUE
              "HOURS:".
           05 FILLER                   PIC X(05)       VALUE SPACES.
           05 FILLER                   PIC X(08)       VALUE
              "PAYRATE:".
           05 FILLER                   PIC X(09)       VALUE SPACES.
           05 FILLER                   PIC X(06)       VALUE
              "TOTAL:".
      *
       01  OUTPUT-LINE.
           05 FILLER                   PIC X(09)       VALUE SPACES.
           05 EMP-ID                   PIC X(10).
           05 FILLER                   PIC X(05)       VALUE SPACES.
           05 EMP-NAME                 PIC X(25).
           05 FILLER                   PIC X(06)       VALUE SPACES.
           05 EMP-SALARY-CODE          PIC X(01).
           05 FILLER                   PIC X(08)       VALUE SPACES.
           05 EMP-HOURS-WORKED         PIC 99.
           05 FILLER                   PIC X(09)       VALUE SPACES.
           05 EMP-PAYRATE              PIC Z9.99.
           05 FILLER                   PIC X(11)       VALUE SPACES.
           05 EMP-LINE-AMOUNT          PIC ZZZ9.99.
      *
       01  END-OF-REPORT-LINE.
           05 FILLER                   PIC X(24)       VALUE
              "   *** END OF REPORT ***".
      *
      *
       PROCEDURE DIVISION.
       000-MAIN.
           PERFORM INITIALIZATION
           PERFORM READ-AND-PRINT UNTIL IS-EOF
           PERFORM CLOSING
           STOP RUN.
      *
       INITIALIZATION.
           OPEN INPUT  INPUT-FILE,
                OUTPUT PRINT-FILE
           PERFORM PRINT-NEW-PAGE
           READ INPUT-FILE AT END MOVE "Y" TO END-OF-FILE.
      *
       PRINT-NEW-PAGE.
           ADD 1 TO PAGE-COUNT
           IF IS-NOT-FIRST-PAGE THEN
               WRITE PRINT-LINE FROM PAGE-TITLE-LINE AFTER PAGE
           ELSE
               WRITE PRINT-LINE FROM PAGE-TITLE-LINE
               MOVE "Y" TO PAGE-STATUS.
      *    END-IF
           MOVE PAGE-COUNT TO PAGE-NUMBER
           WRITE PRINT-LINE FROM PAGE-NUMBER-LINE AFTER 2 LINES
           WRITE PRINT-LINE FROM TITLE-LINE AFTER 2 LINES.
      *
       READ-AND-PRINT.
           MOVE CORRESPONDING INPUT-RECORD TO OUTPUT-LINE
      *     IF ACTUAL-DEPARTMENT NOT EQUAL EMP-DEPARTMENT
      *       OF INPUT-RECORD THEN
      *         MOVE EMP-DEPARTMENT OF INPUT-RECORD
      *         TO ACTUAL-DEPARTMENT, EMP-DEPARTMENT OF DEPARTMENT-LINE
      *         WRITE PRINT-LINE FROM DEPARTMENT-LINE AFTER 2 LINES.
      *    END-IF
      *     IF EMP-SALARY-CODE
      *     MULTIPLY EMP-HOURS-WORKED OF INPUT-RECORD BY
      *         EMP-PAYRATE OF INPUT-RECORD GIVING EMP-LINE-AMOUNT
           WRITE PRINT-LINE FROM OUTPUT-LINE
               AT END-OF-PAGE PERFORM PRINT-NEW-PAGE.
           READ INPUT-FILE AT END MOVE "Y" TO END-OF-FILE.
      *
       CLOSING.
           WRITE PRINT-LINE FROM END-OF-REPORT-LINE AFTER 2 LINES.
           CLOSE INPUT-FILE, PRINT-FILE.
/*
//GO.INPUT DD *
SIMON SULSER MR.         123-456-88IT SERVICES    1000S12
GERHARD MEYER MR.        456-854-89IT SERVICES    1210S22
KARINA BECKER MS.        123-456-85IT SERVICES    0789U10
SIMON SULSER MR.         123-456-88IT SERVICES    1000S12
GERHARD MEYER MR.        456-854-89IT SERVICES    1210S22
KARINA BECKER MS.        123-456-85IT SERVICES    0789U10
SIMON SULSER MR.         123-456-88IT SERVICES    1000S12
GERHARD MEYER MR.        456-854-89IT SERVICES    1210S22
KARINA BECKER MS.        123-456-85IT SERVICES    0789U10
SIMON SULSER MR.         123-456-88IT SERVICES    1000S12
GERHARD MEYER MR.        456-854-89IT SERVICES    1210S22
KARINA BECKER MS.        123-456-85IT SERVICES    0789U10
HANS MUELLER MR.         456-852-23FINANCES       0800S17
PETER EHRENSBERGER MR.   436-232-23FINANCES       1120S25
ANNA BILBO MS.           753-159-98FINANCES       1324U14
HANS MUELLER MR.         456-852-23FINANCES       0800S17
PETER EHRENSBERGER MR.   436-232-23FINANCES       1120S25
ANNA BILBO MS.           753-159-98FINANCES       1324U14
HANS MUELLER MR.         456-852-23FINANCES       0800S17
PETER EHRENSBERGER MR.   436-232-23FINANCES       1120S25
ANNA BILBO MS.           753-159-98FINANCES       1324U14
HANS MUELLER MR.         456-852-23FINANCES       0800S17
PETER EHRENSBERGER MR.   436-232-23FINANCES       1120S25
ANNA BILBO MS.           753-159-98FINANCES       1324U14
CLAUDIA MILLER MS.       741-568-25HUMAN RESOURC. 1200S04
TAMARA VINZENS MS.       254-687-12HUMAN RESOURC. 1235S16
XAVER JONES MS.          778-168-25HUMAN RESOURC. 0754S30
CLAUDIA MILLER MS.       741-568-25HUMAN RESOURC. 1200S04
TAMARA VINZENS MS.       254-687-12HUMAN RESOURC. 1235S16
XAVER JONES MS.          778-168-25HUMAN RESOURC. 0754S30
CLAUDIA MILLER MS.       741-568-25HUMAN RESOURC. 1200S04
TAMARA VINZENS MS.       254-687-12HUMAN RESOURC. 1235S16
XAVER JONES MS.          778-168-25HUMAN RESOURC. 0754S30
CLAUDIA MILLER MS.       741-568-25HUMAN RESOURC. 1200S04
TAMARA VINZENS MS.       254-687-12HUMAN RESOURC. 1235S16
XAVER JONES MS.          778-168-25HUMAN RESOURC. 0754S30
PETER JAMES MR.          445-774-23GARDENING      0550S45
KEANU RIVER MR.          625-425-14GARDENING      0350U08
RALPH MOELLER MR.        658-111-07GARDENING      0731S27
PETER JAMES MR.          445-774-23GARDENING      0550S45
KEANU RIVER MR.          625-425-14GARDENING      0350U08
RALPH MOELLER MR.        658-111-07GARDENING      0731S27
PETER JAMES MR.          445-774-23GARDENING      0550S45
KEANU RIVER MR.          625-425-14GARDENING      0350U08
RALPH MOELLER MR.        658-111-07GARDENING      0731S27
PETER JAMES MR.          445-774-23GARDENING      0550S45
KEANU RIVER MR.          625-425-14GARDENING      0350U08
RALPH MOELLER MR.        658-111-07GARDENING      0731S27
/*
//GO.OUTPUT DD SYSOUT=*,
//             DCB=(RECFM=FBA,LRECL=132,BLKSIZE=13200)
//