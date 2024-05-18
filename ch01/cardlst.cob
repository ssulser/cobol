//SIMONCO JOB (1),'CARD LISTER',CLASS=A,MSGCLASS=A                      00000100
//COBOL  EXEC PROC=COB2UCG,SYSOUT='*'                                   00000200
//SYSIN   DD *                                                          00000300
       IDENTIFICATION DIVISION.                                         00000400
       PROGRAM-ID. CARD-LISTER.                                         00000500
       AUTHOR. SIMON SULSER.                                            00000600
       DATE-WRITTEN. 24/05/10.                                          00000700
       DATE-COMPILED.                                                   00000800
      *                                                                 00000900
       ENVIRONMENT DIVISION.                                            00001000
       CONFIGURATION SECTION.                                           00001100
       SOURCE-COMPUTER. IBM-370.                                        00001200
       OBJECT-COMPUTER. IBM-370.                                        00001300
       INPUT-OUTPUT SECTION.                                            00001400
       FILE-CONTROL.                                                    00001501
           SELECT CARD-FILE ASSIGN TO UT-S-INPUT.                       00001600
           SELECT PRINT-FILE ASSIGN TO UT-S-OUTPUT.                     00001700
      *                                                                 00001800
       DATA DIVISION.                                                   00001900
       FILE SECTION.                                                    00002000
       FD  CARD-FILE                                                    00002100
           LABEL RECORD IS OMITTED.                                     00002200
       01  CARD-RECORD                 PIC X(80).                       00002301
      *                                                                 00002400
       FD  PRINT-FILE                                                   00002500
           LABEL RECORD IS OMITTED.                                     00002600
       01  PRINT-LINE                  PIC X(132).                      00002701
      *                                                                 00002800
       WORKING-STORAGE SECTION.                                         00002900
      *                                                                 00003000
       PROCEDURE DIVISION.                                              00003100
       000-MAIN.                                                        00003200
           PERFORM INITIALIZATION.                                      00003300
           PERFORM READ-AND-PRINT 10 TIMES.                             00003400
           PERFORM CLOSING.                                             00003500
           STOP RUN.                                                    00003600
      *                                                                 00003700
       INITIALIZATION.                                                  00003800
           OPEN INPUT CARD-FILE,                                        00003900
                OUTPUT PRINT-FILE.                                      00004000
      *                                                                 00004100
       READ-AND-PRINT.                                                  00004200
           READ CARD-FILE AT END STOP RUN.                              00004300
           MOVE CARD-RECORD TO PRINT-LINE.                              00004400
           WRITE PRINT-LINE.                                            00004500
      *                                                                 00004600
       CLOSING.                                                         00004700
           CLOSE CARD-FILE, PRINT-FILE.                                 00004800
/*                                                                      00004900
//GO.INPUT DD *                                                         00005000
 *** PROGRAMMED BY SIMON SULSER ***                                     00005101
 THIS IS A PROGRAM                                                      00005201
 TO READ AN PRINT                                                       00005301
 PUNCHED CARDS.                                                         00005401
 IF IT WORKS,                                                           00005501
 TEN CARDS                                                              00005601
 WILL PRINT.                                                            00005701
 THE LAST CARD                                                          00005801
 IS MARKED,                                                             00005901
 'THIS IS THE LAST CARD.'                                               00006001
/*                                                                      00006100
//GO.OUTPUT DD SYSOUT=*,                                                00006200
//             DCB=(RECFM=FBA,LRECL=132,BLKSIZE=13200)                  00006301
//                                                                      00006400
