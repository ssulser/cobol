       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID. CARD-LISTER.                                         
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
       01  INPUT-RECORD                PIC X(80).                       
      *                                                                 
       FD  PRINT-FILE                                                   
           LABEL RECORD IS OMITTED.                                     
       01  PRINT-LINE                  PIC X(132).                      
      *                                                                 
       WORKING-STORAGE SECTION.                                         
      *                                                                 
       PROCEDURE DIVISION.                                              
       000-MAIN.                                                        
           PERFORM INITIALIZATION.                                      
           PERFORM READ-AND-PRINT 10 TIMES.                             
           PERFORM CLOSING.                                             
           STOP RUN.                                                    
      *                                                                 
       INITIALIZATION.                                                  
           OPEN INPUT  INPUT-FILE,                                        
                OUTPUT PRINT-FILE.                                      
      *                                                                 
       READ-AND-PRINT.                                                  
           READ INPUT-FILE  AT END STOP RUN.                              
           MOVE INPUT-RECORD  TO PRINT-LINE.                              
           WRITE PRINT-LINE.                                            
      *                                                                 
       CLOSING.                                                         
           CLOSE INPUT-FILE, PRINT-FILE.
