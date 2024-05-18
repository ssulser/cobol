//SIMONCO JOB (1),'BIBLIOGRAPHY II',CLASS=A,MSGCLASS=A
//COBOL  EXEC PROC=COB2UCG,SYSOUT='*'
//SYSIN   DD *
       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID. BIBLIO.                                         
       AUTHOR. SIMON SULSER.                                            
       DATE-WRITTEN. MAY 18,1924.                                          
       DATE-COMPILED.                                                   
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
       DATA DIVISION.                                                   
       FILE SECTION.                                                    
       FD  INPUT-FILE                                                    
           LABEL RECORD IS OMITTED.                                     
       01  INPUT-RECORD.
           05 BOOK-TITLE               PIC X(30).
           05 BOOK-AUTHOR-NAME         PIC X(25).
           05 BOOK-PUBLISHER           PIC X(21).
           05 BOOK-PRICE               PIC 99V99.
      *                                                                 
       FD  PRINT-FILE                                                   
           LABEL RECORD IS OMITTED.                                     
       01  PRINT-LINE                  PIC X(132).                      
      *                                                                 
       WORKING-STORAGE SECTION.                                         
      *
       77  MORE-CARDS                  PIC X(3)        VALUE "YES".
       77  SUM-OF-BOOK-PRICES          PIC 999V99      VALUE ZERO.
      *
       01  REPORT-TITLE.
           05 FILLER                   PIC X(47)       VALUE SPACES.
           05 FILLER                   PIC X(37)       VALUE
                "B I B L I O G R A P H Y   R E P O R T".
           05 FILLER                   PIC X(48)       VALUE SPACES.
      *
       01  COLUMN-HEADER.
           05 FILLER                   PIC X(19)       VALUE SPACES.
           05 FILLER                   PIC X(6)        VALUE "AUTHOR".
           05 FILLER                   PIC X(24)       VALUE SPACES.
           05 FILLER                   PIC X(5)        VALUE "TITLE".
           05 FILLER                   PIC X(30)       VALUE SPACES.
           05 FILLER                   PIC X(11)       VALUE
                "PUBLISHER".
           05 FILLER                   PIC X(14)       VALUE SPACES.
           05 FILLER                   PIC X(05)       VALUE "PRICE".
           05 FILLER                   PIC X(18)       VALUE SPACES.
      *
       01  BIBLIOGRAPHY-DETAIL-LINE.
           05 FILLER                   PIC X(19)       VALUE SPACES.
           05 BOOK-AUTHOR-NAME         PIC X(25).
           05 FILLER                   PIC X(05)       VALUE SPACES.
           05 BOOK-TITLE               PIC X(30).
           05 FILLER                   PIC X(05)       VALUE SPACES.
           05 BOOK-PUBLISHER           PIC X(21).
           05 FILLER                   PIC X(04)       VALUE SPACES.
           05 BOOK-PRICE               PIC $Z9.99.
           05 FILLER                   PIC X(17).
      *
       01  TOTAL-LINE.
           05 FILLER                   PIC X(07)       VALUE SPACES.
           05 FILLER                   PIC X(21)       VALUE
                "TOTAL PRICE OF BOOKS ".
           05 TOTAL-PRICE-OF-BOOKS     PIC $ZZ9.99.
           05 FILLER                   PIC X(17)       VALUE SPACES.
      *
       PROCEDURE DIVISION.                                              
       MAINLINE-CONTROL-ROUTINE.
           PERFORM INITIALIZATION.                                      
           PERFORM PROCESS-PRINT-READ UNTIL MORE-CARDS EQUAL TO "NO".
           PERFORM PRINT-TOTALS-AND-CLOSE.                              
           STOP RUN.                                                    
      *                                                                 
       INITIALIZATION.                                                  
           OPEN INPUT  INPUT-FILE,                                        
                OUTPUT PRINT-FILE.
           WRITE PRINT-LINE FROM REPORT-TITLE
                BEFORE ADVANCING 2 LINES.
           WRITE PRINT-LINE FROM COLUMN-HEADER
                BEFORE ADVANCING 3 LINES.
           READ INPUT-FILE AT END
                MOVE "NO" TO MORE-CARDS.
      *                                                                 
       PROCESS-PRINT-READ.                                                  
           MOVE BOOK-TITLE IN INPUT-RECORD TO
                BOOK-TITLE IN BIBLIOGRAPHY-DETAIL-LINE.
           MOVE BOOK-AUTHOR-NAME IN INPUT-RECORD  TO
                BOOK-AUTHOR-NAME IN BIBLIOGRAPHY-DETAIL-LINE.
           MOVE BOOK-PUBLISHER IN INPUT-RECORD TO
                BOOK-PUBLISHER IN BIBLIOGRAPHY-DETAIL-LINE.
           MOVE BOOK-PRICE IN INPUT-RECORD TO 
                BOOK-PRICE IN BIBLIOGRAPHY-DETAIL-LINE.
           WRITE PRINT-LINE FROM BIBLIOGRAPHY-DETAIL-LINE
                AFTER ADVANCING 1 LINE.
           ADD BOOK-PRICE IN INPUT-RECORD TO SUM-OF-BOOK-PRICES.
           READ INPUT-FILE AT END MOVE "NO" TO MORE-CARDS.
      *                                                                 
       PRINT-TOTALS-AND-CLOSE.
           MOVE SUM-OF-BOOK-PRICES TO TOTAL-PRICE-OF-BOOKS.
           WRITE PRINT-LINE FROM TOTAL-LINE AFTER ADVANCING 2 LINES.            
           CLOSE INPUT-FILE, PRINT-FILE.
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
