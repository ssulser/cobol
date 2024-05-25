//SIMONCO JOB (1),'BIBLIOGRAPHY REPORT',CLASS=A,MSGCLASS=A
//COBOL  EXEC PROC=COB2UCG,SYSOUT='*'
//SYSIN   DD *
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BIBLIOGRAPHY-ORDER-LIST.
       AUTHOR. SIMON SULSER.
       DATE-WRITTEN. MAY 19,1924.
       DATE-COMPILED.
      *
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
      *
       SPECIAL-NAMES.
           C01 IS TOP-OF-PAGE.
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BIBLIOGRAPHY-CARD-FILE
              ASSIGN TO UT-S-INPUT.
           SELECT  BIBLIOGRAPHY-LIST-FILE
              ASSIGN TO UT-S-OUTPUT.
      *
      *
       DATA DIVISION.
      *
       FILE SECTION.
       FD  BIBLIOGRAPHY-CARD-FILE
           LABEL RECORD OMITTED.
       01  BIBLIOGRAPHY-CARD.
           05 BOOK-TITLE               PIC X(30).
           05 BOOK-AUTHOR-NAME         PIC X(24).
           05 BOOK-PUBLISHER           PIC X(20).
           05 BOOK-PRICE               PIC 99V99.
           05 ORDER-QUANTITY           PIC 99.
      *
       FD  BIBLIOGRAPHY-LIST-FILE
           LABEL RECORD OMITTED.
       01  PRINT-LINE                  PIC X(132).
      *
       WORKING-STORAGE SECTION.
      *
       77  MORE-CARDS                  PIC X(03)     VALUE "YES".
       77  LINE-COUNT                  PIC 99        VALUE ZEROS.
       77  SUM-PRICES-BY-PUBLISHER     PIC 9999V99   VALUE ZEROS.
       77  SUM-PRICES-OVER-ALL         PIC 9999V99   VALUE ZEROS.
       77  TOTAL-PRICE-PER-LINE        PIC 999V99    VALUE ZEROS.
      *
       01  REPORT-TITLE.
           05 FILLER                   PIC X(43)     VALUE SPACES.
           05 FILLER                   PIC X(45)     VALUE
              "B I B L I O G R A P H Y   O R D E R   L I S T".
           05 FILLER                   PIC X(44)     VALUE SPACES.
      *
       01  COLUMN-HEADING.
           05 FILLER                   PIC X(02)     VALUE SPACES.
           05 FILLER                   PIC X(09)     VALUE
              "PUBLISHER".
           05 FILLER                   PIC X(13)     VALUE SPACES.
           05 FILLER                   PIC X(06)     VALUE "AUTHOR".
           05 FILLER                   PIC X(22)     VALUE SPACES.
           05 FILLER                   PIC X(05)     VALUE "TITLE".
           05 FILLER                   PIC X(30)     VALUE SPACES.
           05 FILLER                   PIC X(05)     VALUE "PRICE".
           05 FILLER                   PIC X(04)     VALUE SPACES.
           05 FILLER                   PIC X(08)     VALUE "QUANTITY".
           05 FILLER                   PIC X(06)     VALUE SPACES.
           05 FILLER                   PIC X(05)     VALUE "TOTAL".
           05 FILLER                   PIC X(02)     VALUE SPACES.
           05 FILLER                   PIC X(05)     VALUE "ERROR".
           05 FILLER                   PIC X(10)     VALUE SPACES.
      *
       01  ORDER-DETAIL-LINE.
           05 FILLER                   PIC X(24)     VALUE SPACES.
           05 BOOK-AUTHOR-NAME         PIC X(24).
           05 FILLER                   PIC X(04)     VALUE SPACES.
           05 BOOK-TITLE               PIC X(30).
           05 FILLER                   PIC X(04)     VALUE SPACES.
           05 BOOK-PRICE               PIC $$9.99.
           05 FILLER                   PIC X(07)     VALUE SPACES.
           05 ORDER-QUANTITY           PIC Z9.
           05 FILLER                   PIC X(06)     VALUE SPACES.
           05 TOTAL-AMOUNT             PIC $$$$9.99.
           05 FILLER                   PIC X(02)     VALUE SPACES.
           05 ERROR-MESSAGE            PIC X(15).
      *
       01  PUBLISHER-NAME-LINE.
           05 FILLER                   PIC X(02)     VALUE SPACES.
           05 PUBLISHER-NAME           PIC X(20).
           05 FILLER                   PIC X(110)    VALUE SPACES.
      *
       01  ORDER-TOTAL-LINE.
           05 FILLER                   PIC X(81)     VALUE SPACES.
           05 OUTPUT-TOTAL-NAME        PIC X(20).
           05 FILLER                   PIC X(04)     VALUE SPACES.
           05 ORDER-TOTAL              PIC $$$,$$9.99.
           05 FILLER                   PIC X(17)     VALUE SPACES.
      *
      *
       PROCEDURE DIVISION.
       MAINLINE-CONTROL-ROUTINE.
           PERFORM INITIALIZATION.
           PERFORM PROCESS-AND-READ UNTIL MORE-CARDS = "NO".
           PERFORM PRINT-TOTALS-AND-CLOSE.
           STOP RUN.
      *
       INITIALIZATION.
           OPEN INPUT BIBLIOGRAPHY-CARD-FILE.
           OPEN OUTPUT BIBLIOGRAPHY-LIST-FILE.
           READ BIBLIOGRAPHY-CARD-FILE AT END
              MOVE "NO" TO MORE-CARDS.
           IF MORE-CARDS = "YES" THEN
              MOVE BOOK-PUBLISHER OF BIBLIOGRAPHY-CARD
                 TO PUBLISHER-NAME
              PERFORM NEW-PAGE-ROUTINE.
      *
       PROCESS-AND-READ.
           PERFORM INPUT-VALIDATION-ROUTINE.
           IF BOOK-PUBLISHER OF BIBLIOGRAPHY-CARD NOT = PUBLISHER-NAME
              THEN PERFORM NEW-PUBLISHER-ROUTINE.
           IF LINE-COUNT > 50 THEN
              PERFORM NEW-PAGE-ROUTINE.
           MOVE CORRESPONDING BIBLIOGRAPHY-CARD TO ORDER-DETAIL-LINE.
           IF ERROR-MESSAGE = SPACES THEN
              COMPUTE TOTAL-PRICE-PER-LINE = BOOK-PRICE OF
                 BIBLIOGRAPHY-CARD * ORDER-QUANTITY OF BIBLIOGRAPHY-CARD
              ADD TOTAL-PRICE-PER-LINE TO SUM-PRICES-BY-PUBLISHER,
                 SUM-PRICES-OVER-ALL
              MOVE TOTAL-PRICE-PER-LINE TO TOTAL-AMOUNT
           ELSE
              MOVE ZEROS TO TOTAL-PRICE-PER-LINE.
           MOVE TOTAL-PRICE-PER-LINE TO TOTAL-AMOUNT.
           WRITE PRINT-LINE FROM ORDER-DETAIL-LINE AFTER 1 LINE.
           ADD 1 TO LINE-COUNT.
           READ BIBLIOGRAPHY-CARD-FILE AT END
              MOVE "NO" TO MORE-CARDS.
      *
       INPUT-VALIDATION-ROUTINE.
           IF BOOK-PUBLISHER OF BIBLIOGRAPHY-CARD = SPACES THEN
              MOVE "NO PUBLISHER" TO ERROR-MESSAGE
           ELSE
              IF BOOK-TITLE OF BIBLIOGRAPHY-CARD = SPACES THEN
                 MOVE "NO TITLE" TO ERROR-MESSAGE
              ELSE
                 IF BOOK-AUTHOR-NAME OF BIBLIOGRAPHY-CARD = SPACES THEN
                    MOVE "NO AUTHOR" TO ERROR-MESSAGE
                 ELSE
                    IF BOOK-PRICE OF BIBLIOGRAPHY-CARD NOT NUMERIC THEN
                       MOVE ZEROS TO BOOK-PRICE OF BIBLIOGRAPHY-CARD
                       MOVE "INVALID PRICE" TO ERROR-MESSAGE
                    ELSE
                       IF BOOK-PRICE OF BIBLIOGRAPHY-CARD = ZEROS THEN
                          MOVE "INVALID PRICE" TO ERROR-MESSAGE
                       ELSE
                          IF ORDER-QUANTITY OF BIBLIOGRAPHY-CARD
                                NOT NUMERIC THEN
                             MOVE ZEROS TO ORDER-QUANTITY OF
                                BIBLIOGRAPHY-CARD
                             MOVE "INVALID QTY" TO ERROR-MESSAGE
                          ELSE
                             IF ORDER-QUANTITY OF BIBLIOGRAPHY-CARD
                                   = ZEROS THEN
                                MOVE "INVALID QTY" TO ERROR-MESSAGE
                             ELSE
                                MOVE SPACES TO ERROR-MESSAGE.
      *
       NEW-PUBLISHER-ROUTINE.
           MOVE " PUBLISHER SUBTOTAL:" TO OUTPUT-TOTAL-NAME.
           MOVE SUM-PRICES-BY-PUBLISHER TO ORDER-TOTAL.
           WRITE PRINT-LINE FROM ORDER-TOTAL-LINE AFTER 2 LINES.
           MOVE BOOK-PUBLISHER OF BIBLIOGRAPHY-CARD TO PUBLISHER-NAME.
           ADD 2 TO LINE-COUNT.
           MOVE ZEROS TO SUM-PRICES-BY-PUBLISHER.
           IF LINE-COUNT > 50 THEN
              PERFORM NEW-PAGE-ROUTINE
           ELSE
              WRITE PRINT-LINE FROM PUBLISHER-NAME-LINE AFTER 2 LINES
              ADD 2 TO LINE-COUNT.
      *
       NEW-PAGE-ROUTINE.
           WRITE PRINT-LINE FROM REPORT-TITLE AFTER TOP-OF-PAGE.
           WRITE PRINT-LINE FROM COLUMN-HEADING AFTER 2 LINES.
           WRITE PRINT-LINE FROM PUBLISHER-NAME-LINE AFTER 2 LINES.
           MOVE 5 TO LINE-COUNT.
      *
       PRINT-TOTALS-AND-CLOSE.
           MOVE " PUBLISHER SUBTOTAL:" TO OUTPUT-TOTAL-NAME.
           MOVE SUM-PRICES-BY-PUBLISHER TO ORDER-TOTAL.
           WRITE PRINT-LINE FROM ORDER-TOTAL-LINE AFTER 2 LINES.
           MOVE " OVERALL ORDER TOTAL:" TO OUTPUT-TOTAL-NAME.
           MOVE SUM-PRICES-OVER-ALL TO ORDER-TOTAL.
           WRITE PRINT-LINE FROM ORDER-TOTAL-LINE AFTER 2 LINES.
           CLOSE BIBLIOGRAPHY-CARD-FILE, BIBLIOGRAPHY-LIST-FILE.
/*
//GO.INPUT DD *
DO ANDROIDS DREAM OF SHEEP    PHILIP K. DICK          THIEME              123420
COOKNG FOR FUN                WENDY DOUGLAS           THIEME              548233
DON'T KNOW WHO WROTE THIS                             THIEME              100525
THE HITCHHIKER'S GUIDE        DOUGLAS ADAMS           ALEXA               456719
AND ANOTHER BOOK JUST SO      BENNY KING MEYERS       ALEXA               458756
EVERYTHING FOR FREE !!!       JAMES FREE KAHN         ALEXA
SOMETHING WICKED THIS WAY     RAY BRADBURY            REDERER             789522
WHAT SHOULD I DO NEXT...      JIMMY BRIXTON           REDERER             875521
                              ANNA WENDENER           REDERER             635241
NOONE WANTS TO PUBLISH MY BK  IRINA SAD                                   111111
/*
//GO.OUTPUT DD SYSOUT=*,
//             DCB=(RECFM=FBA,LRECL=132,BLKSIZE=13200)
//