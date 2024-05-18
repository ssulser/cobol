//SIMONCO JOB (1),'LUGGAGE LABEL',CLASS=A,MSGCLASS=A                    
//COBOL  EXEC PROC=COB2UCG,SYSOUT='*'                                   
//SYSIN   DD *                                                          
      ***********************************************                   
      * THIS PROGRAM DOES NOT EXACTLY WHAT WAS ASK  *                   
      * FOR IN THE EXERCISE 2-3 BECAUSE IT IS USING *                   
      * A EOF DETECTION, 88 LEVEL AND IS PREPARING  *                   
      * THE OUTPUT IN THE WORKING STORAGE AND       *                   
      * PRINTING WITH WRITE FROM. WHICH WILL BE     *
      * INTRODUCED IN THE NEXT CHAPTER.             *      
      ***********************************************
      *                   
       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID. LUGGAGE-LABEL.                                       
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
           SELECT LUGGAGE-FILE ASSIGN TO UT-S-INPUT.                    
           SELECT PRINT-FILE   ASSIGN TO UT-S-OUTPUT.                   
      *                                                                 
       DATA DIVISION.                                                   
       FILE SECTION.                                                    
       FD LUGGAGE-FILE                                                  
           LABEL RECORDS OMITTED.                                       
       01  LUGGAGE-RECORD.                                              
           05  LUG-NAME-IN             PIC X(25).                       
           05  LUG-ADDRESS-IN          PIC X(40).                       
           05  FILLER                  PIC X(15).                       
      *                                                                 
       FD  PRINT-FILE                                                   
           LABEL RECORDS OMITTED.                                       
       01  PRINT-LINE                  PIC X(132).                      
      *                                                                 
       WORKING-STORAGE SECTION.                                         
       01  PRT-NAME-LINE.                                               
           05  FILLER                  PIC X(11) VALUE SPACES.          
           05  FILLER                  PIC X(10)                        
               VALUE "MY NAME IS".                                      
           05  FILLER                  PIC X(10) VALUE SPACES.          
           05  PRT-NAME                PIC X(25).                       
      *                                                                 
       01  PRT-ADDRESS-LINE.                                            
           05  FILLER                  PIC X(11) VALUE SPACES.          
           05  FILLER                  PIC X(13)                        
               VALUE "MY ADDRESS IS".                                   
           05  FILLER                  PIC X(07) VALUE SPACES.          
           05  PRT-ADDRESS             PIC X(40).                       
      *                                                                 
       77  FLAG-EOF                    PIC X VALUE "N".                 
           88  IS-EOF                  VALUE "Y".                       
      *                                                                 
       PROCEDURE DIVISION.                                              
       000-MAIN.                                                        
      *                                                                 
           PERFORM INITIALIZE.                                          
           PERFORM READ-AND-PROCESS UNTIL IS-EOF.                       
           PERFORM CLOSING.                                             
           STOP RUN.                                                    
      *                                                                 
       INITIALIZE.                                                      
           OPEN INPUT LUGGAGE-FILE,                                     
                OUTPUT PRINT-FILE.                                      
      *                                                                 
       READ-AND-PROCESS.                                                
           MOVE SPACES TO PRT-NAME, PRT-ADDRESS.                        
           READ LUGGAGE-FILE AT END MOVE "Y" TO FLAG-EOF.               
           MOVE LUG-NAME-IN TO PRT-NAME.                                
           WRITE PRINT-LINE FROM PRT-NAME-LINE AFTER 2 LINES.           
           MOVE LUG-ADDRESS-IN TO PRT-ADDRESS.                          
           WRITE PRINT-LINE FROM PRT-ADDRESS-LINE.                      
      *                                                                 
       CLOSING.                                                         
           CLOSE LUGGAGE-FILE, PRINT-FILE.                              
/*                                                                      
//*                                                                     
//GO.INPUT DD *                                                         
SAVANNAH CRAWLEY         COMET HOUSE  8264, MILANO - 1877               
LUKE AINSWORTH           APOSTLE  6364, SANTA ANA - 6802                
ROSALYN SMITH            HOWARD 2748, SAN ANTONIO - 2552                
DANIEL WILSON            QUEENSBERRY  4745, TOLEDO - 3237               
ENOCH PARKER             BACON  9145, ALBUQUERQUE - 4773                
MAYA MILLER              LAKE 5169, BELLEVUE - 2135                     
KARLA OSWALD             BEACONSFIELD  9025, FORT LAUDERDALE            
RUTH HARRIS              THORNDIKE   7670, LINCOLN - 4477               
JACOB MARTIN             PARKFIELDS 5938, JACKSONVILLE - 4124           
JULES ROGERS             COLLENT   3122, OKLAHOMA CITY - 3480           
JOY THATCHER             BLETCHLEY   1854, HAYWARD - 7581               
HAZEL ADDIS              BLAKE  5558, FORT LAUDERDALE - 3042            
PERCY EDDISON            BUTTONWOOD 3932, DENVER - 3288                 
LUCAS HARRISON           DUNSTANS  6786, PHOENIX - 3403                 
CLINT SHELDON            BERRY  6831, SAN FRANCISCO - 2211              
/*                                                                      
//GO.OUTPUT DD SYSOUT=*,                                                
//          DCB=(RECFM=FBA,LRECL=132,BLKSIZE=13200)                     
//                                                                      