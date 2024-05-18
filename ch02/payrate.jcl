//SIMONCO JOB (1),'PAYRATE',CLASS=A,MSGCLASS=A                          
//COBOL  EXEC PROC=COB2UCG,SYSOUT='*'                                   
//SYSIN   DD *                                                          
       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID. PAYRATE.                                             
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
           SELECT GRADE-FILE ASSIGN TO UT-S-INPUT.                       
           SELECT PRINT-FILE ASSIGN TO UT-S-OUTPUT.                     
      *                                                                 
       DATA DIVISION.                                                   
       FILE SECTION.                                                    
       FD  GRADE-FILE                                                    
           LABEL RECORD IS OMITTED.                                     
       01  GRADE-RECORD.                                                 
           05  EMP-NAME-IN             PIC X(25).                       
           05  EMP-ID-IN               PIC X(10).                       
           05  EMP-DEPARTEMENT-IN      PIC X(15).                       
           05  EMP-RATE-OF-PAY-IN      PIC 99V99.                       
           05  FILLER                  PIC X(26).                       
      *                                                                 
       FD  PRINT-FILE                                                   
           LABEL RECORD IS OMITTED.                                     
       01  PRINT-LINE.                                                  
           05  FILLER                  PIC X(21).                       
           05  EMP-NAME-OUT            PIC X(25).                       
           05  FILLER                  PIC X(5).                        
           05  EMP-ID-OUT              PIC X(10).                       
           05  FILLER                  PIC X(10).                       
           05  EMP-DEPARTEMENT-OUT     PIC X(15).                       
           05  FILLER                  PIC X(5).                        
           05  EMP-RATE-OF-PAY-OUT     PIC $99.99.                      
      *                                                                 
       WORKING-STORAGE SECTION.                                         
      *                                                                 
       PROCEDURE DIVISION.                                              
       000-MAIN.                                                        
           PERFORM INITIALIZATION.                                      
           PERFORM READ-AND-PRINT 15 TIMES.                             
           PERFORM CLOSING.                                             
           STOP RUN.                                                    
      *                                                                 
       INITIALIZATION.                                                  
           OPEN INPUT GRADE-FILE,                                        
                OUTPUT PRINT-FILE.                                      
      *                                                                 
       READ-AND-PRINT.                                                  
           MOVE SPACES TO PRINT-LINE.                                   
           READ GRADE-FILE                                               
               AT END STOP RUN.                                         
           MOVE EMP-NAME-IN TO EMP-NAME-OUT.                            
           MOVE EMP-ID-IN TO EMP-ID-OUT.                                
           MOVE EMP-DEPARTEMENT-IN TO EMP-DEPARTEMENT-OUT.              
           MOVE EMP-RATE-OF-PAY-IN TO EMP-RATE-OF-PAY-OUT.              
           WRITE PRINT-LINE.                                            
      *                                                                 
       CLOSING.                                                         
           CLOSE GRADE-FILE, PRINT-FILE.                                 
/*                                                                      
//GO.INPUT DD *                                                         
JOSHUA WATSON            447-221-88BUDGET CONTROL 4856                  
LAUREN RODRIGUEZ         123-456-87MARKETING      5000                  
NATHAN SCOTT             654-852-85ACCOUNTING     7500                  
BENJAMIN PEREZ           159-753-56STRATEGIES     5600                  
ASHLEY WARD              612-782-12OPERATIONS     6250                  
EVELYN BELL              963-852-45PLANNING SPC   5875                  
GABRIEL ANDERSON         854-698-14RESEARCH       5687                  
TIMOTHY CARTER           654-852-99BRAND DIRECTOR 6352                  
OLIVER MOORE             112-254-56MARKETING      5345                  
OWEN WOOD                787-584-55IT SERVICES    6541                  
ROBERT ROSS              446-187-53RESEARCH       5988                  
HEATHER THOMAS           268-471-77IT SERVICES    5784                  
MEGAN SANDERS            784-885-69ACCOUNTING     6582                  
AUBREY COLLINS           456-753-12RESEARCH       5874                  
JAMES WASHINGTON         574-698-55FINANCE MANAGER8500                  
/*                                                                      
//GO.OUTPUT DD SYSOUT=*,                                                
//             DCB=(RECFM=FBA,LRECL=132,BLKSIZE=13200)                  
//                                                                      
