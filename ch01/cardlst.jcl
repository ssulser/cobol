//SIMONCO JOB (1),'CARD LISTER',CLASS=A,MSGCLASS=A                      
//COBOL  EXEC PROC=COB2UCG,SYSOUT='*'                                   
//SYSIN   DD *                                                          
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
           SELECT CARD-FILE ASSIGN TO UT-S-INPUT.                       
           SELECT PRINT-FILE ASSIGN TO UT-S-OUTPUT.                     
      *                                                                 
       DATA DIVISION.                                                   
       FILE SECTION.                                                    
       FD  CARD-FILE                                                    
           LABEL RECORD IS OMITTED.                                     
       01  CARD-RECORD                 PIC X(80).                       
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
           OPEN INPUT CARD-FILE,                                        
                OUTPUT PRINT-FILE.                                      
      *                                                                 
       READ-AND-PRINT.                                                  
           READ CARD-FILE AT END STOP RUN.                              
           MOVE CARD-RECORD TO PRINT-LINE.                              
           WRITE PRINT-LINE.                                            
      *                                                                 
       CLOSING.                                                         
           CLOSE CARD-FILE, PRINT-FILE.                                 
/*                                                                      
//GO.INPUT DD *                                                         
 *** PROGRAMMED BY SIMON SULSER ***                                     
 THIS IS A PROGRAM                                                      
 TO READ AN PRINT                                                       
 PUNCHED CARDS.                                                         
 IF IT WORKS,                                                           
 TEN CARDS                                                              
 WILL PRINT.                                                            
 THE LAST CARD                                                          
 IS MARKED,                                                             
 *** THIS IS THE LAST CARD. ***                                               
/*                                                                      
//GO.OUTPUT DD SYSOUT=*,                                                
//             DCB=(RECFM=FBA,LRECL=132,BLKSIZE=13200)                  
//