//SIMONCO JOB (1),'BIBLIOGRAPHY',CLASS=A,MSGCLASS=A
//LEAPY  EXEC PROC=COB2UCG,SYSOUT='*'
//SYSIN   DD *
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BIBLIOGRAPHY-LIST.
       AUTHOR. SIMON SULSER.
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BIBLIO-FILE ASSIGN TO UT-S-INPUT.
           SELECT PRINT-FILE ASSIGN TO UT-S-OUTPUT.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  BIBLIO-FILE
           LABEL RECORD IS OMITTED.
       01  BIBLIO-RECORD.
           05  BOOK-TITLE              PIC X(30).
           05  BOOK-AUTHOR             PIC X(25).
           05  BOOK-PUBLISHER          PIC X(21).
           05  BOOK-PRICE              PIC 99V99.
      *
       FD  PRINT-FILE
           LABEL RECORD IS OMITTED.
       01  PRINT-LINE.
           05  FILLER                  PIC X(19).
           05  BOOK-AUTHOR             PIC X(25).
           05  FILLER                  PIC X(5).
           05  BOOK-TITLE              PIC X(30).
           05  FILLER                  PIC X(5).
           05  BOOK-PUBLISHER          PIC X(21).
           05  FILLER                  PIC X(4).
           05  BOOK-PRICE              PIC $99.99.
           05  FILLER                  PIC X(17).
      *
       WORKING-STORAGE SECTION.
      *
       PROCEDURE DIVISION.
       000-MAIN.
           PERFORM INITIALIZATION.
           PERFORM READ-PROCESS-PRINT 15 TIMES.
           PERFORM CLOSING.
           STOP RUN.
      *
       INITIALIZATION.
           OPEN INPUT BIBLIO-FILE.
           OPEN OUTPUT PRINT-FILE.
           MOVE SPACES TO PRINT-LINE.
      *
       READ-PROCESS-PRINT.
           READ BIBLIO-FILE
               AT END STOP RUN.
           MOVE BOOK-TITLE OF BIBLIO-RECORD TO
               BOOK-TITLE OF PRINT-LINE.
           MOVE BOOK-AUTHOR OF BIBLIO-RECORD TO
               BOOK-AUTHOR OF PRINT-LINE.
           MOVE BOOK-PUBLISHER OF BIBLIO-RECORD TO
               BOOK-PUBLISHER OF PRINT-LINE.
           MOVE BOOK-PRICE OF BIBLIO-RECORD TO
               BOOK-PRICE OF PRINT-LINE.
           WRITE PRINT-LINE.
      *
       CLOSING.
           CLOSE BIBLIO-FILE, PRINT-FILE.
/*
//GO.INPUT  DD *
MIS CONCEPTUAL FOUNDATIONS    MCGRAW-HILL BOOK CO      DAVIS, GORDON B.     1395
COMPUTERS IN SOCIETY          MCGRAW-HILL BOOK CO.     SANDERS, DONALD H.   1095
RPG FOR IBM SYSTEMS/360,370   PRENTICE-HALL, INC       LOSCHETTER, RICHARD P2050
INTRO DIGITAL COMPUTING       ADOISON-WESLEY, INC.     RDEN, BRUCE W.       1395
COMPUTER OATA PROCESSING      MCGRAW-HILL BOOK CO.     DAVIS, GORDON B.     1350
EOP FOR AUDITORS              JOHN WILEY SONS INC      BROWN, HARRY         0795
FUNDAMENTAL COBOL, IBM 360    PRENTICE-HALL, INC.      JONES, ROBERT L.     0950
ELEMENTS OF PROGRAMMING STYLE MCGRAW-HILL BOOK         KERNIGHAN/PLAUGER CO.0395
COMPUTER SORTING              PRENTICE-HALL, INC.      FLORES, IVAN         1495
INTRODUCTION TO FORTRAN IV    RINEHART PRESS           DICKSON/SMITH        0675
THE COMPUTERIZED SOCIETY      PRENTICE-HALL, INC.      MARTIN/NORMAN        1095
COMPUTERS IN BUSINESS         MCGRAW HILL BOOK CO.     SANDERS, DONALD      0895
GAME PLAYING WITH COMPUTERS   SPARTAN BOOKS            SPENCER, DONALD O.   1295
INTRO TO DECISION SCIENCE     PETROCELLI/CHARTER       LEE/MOORE            1450
AUDIT AND CONTROL OF COMP.SYS.PETROCELLI/CHARTER       JANCURA, ELISE G.    1395
/*
//GO.OUTPUT DD SYSOUT=*,
//             DCB=(RECFM=FBA,LRECL=132,BLKSIZE=13200)
//
