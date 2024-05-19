       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID. PAYRATE2.                                             
       AUTHOR. SIMON SULSER.                                            
       DATE-WRITTEN. MAY 19,1924.                                          
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
           05 EMP-COMPANY              PIC X(20).                       
           05 EMP-AMOUNT               PIC 9(7).
           05 FILLER                   PIC XX.                       
           05 EMP-FACTOR-1             PIC 9V99.
           05 FILLER                   PIC XX.                       
           05 EMP-FACTOR-2             PIC 9V99.
           05 FILLER                   PIC XX.                       
           05 EMP-FACTOR-3             PIC 9V99.
           05 FILLER                   PIC X(17).
      *                                                                 
       FD  PRINT-FILE                                                   
           LABEL RECORD IS OMITTED.                                     
       01  PRINT-LINE                  PIC X(132).                    
      *                                                                 
       WORKING-STORAGE SECTION.                                         
      *
       77  FLAG-EOF                    PIC X         VALUE "N".
           88 IS-EOF                                 VALUE "Y".
      *
       01  REPORT-HEADER.
           05 FILLER                   PIC X(52)     VALUE SPACES.
           05 FILLER                   PIC X(30)     VALUE
                 "ANALYSIS OF INSURANCE COVERAGE".
      *
       01  REPORT-TITLE1.
           05 FILLER                   PIC X(20)     VALUE SPACES.
           05 FILLER                   PIC X(58)     VALUE
           "POLICY                           PAYMENT IF     PAYMENT IF".
      *
       01  REPORT-TITLE2.
           05 FILLER                   PIC X(20)     VALUE SPACES.
           05 FILLER                   PIC X(57)     VALUE
           "NUMBER     INSURANCE COMPANY   NON-ACCIDENTAL    DEATH BY".
      *
       01  REPORT-TITLE3.
           05 FILLER                   PIC X(20)     VALUE SPACES.
           05 FILLER                   PIC X(57)     VALUE
           "                                  DEATH          ACCIDENT".
      *
       PROCEDURE DIVISION.                                              
       000-MAIN.                                                        
           PERFORM INITIALIZATION.                                      
           PERFORM READ-AND-PRINT.                             
           PERFORM CLOSING.                                             
           STOP RUN.                                                    
      *                                                                 
       INITIALIZATION.                                                  
           OPEN OUTPUT PRINT-FILE.                                      
      *                                                                 
       READ-AND-PRINT.                                                  
           WRITE PRINT-LINE FROM REPORT-HEADER.
           WRITE PRINT-LINE FROM REPORT-TITLE1 AFTER 2 LINES.
           WRITE PRINT-LINE FROM REPORT-TITLE2 AFTER 1 LINE.
           WRITE PRINT-LINE FROM REPORT-TITLE3 AFTER 1 LINE.                  
      *                                                                 
       CLOSING.                                                         
           CLOSE PRINT-FILE.