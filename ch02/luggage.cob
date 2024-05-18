//SIMONCO JOB (1),'LUGGAGE LABEL',CLASS=A,MSGCLASS=A                    00000100
//COBOL  EXEC PROC=COB2UCG,SYSOUT='*'                                   00000200
//SYSIN   DD *                                                          00000300
      ***********************************************                   00000401
      * THIS PROGRAM DOES NOT EXACTLY WHAT WAS ASK  *                   00000501
      * FOR IN THE EXERCISE 2-3 BECAUSE IT IS USING *                   00000601
      * A EOF DETECTION, 88 LEVEL AND IS PREPARING  *                   00000701
      * THE OUTPUT IN THE WORKING STORAGE AND       *                   00000801
      * PRINTING WITH WRITE FROM.                   *                   00000901
      ***********************************************                   00001001
       IDENTIFICATION DIVISION.                                         00001100
       PROGRAM-ID. LUGGAGE-LABEL.                                       00001200
       AUTHOR. SIMON SULSER.                                            00001300
       DATE-WRITTEN. MAY 18,1924.                                       00001400
       DATE-COMPILED.                                                   00001500
      *                                                                 00001600
       ENVIRONMENT DIVISION.                                            00001700
       CONFIGURATION SECTION.                                           00001800
       SOURCE-COMPUTER. IBM-370.                                        00001900
       OBJECT-COMPUTER. IBM-370.                                        00002000
       INPUT-OUTPUT SECTION.                                            00002100
       FILE-CONTROL.                                                    00002201
           SELECT LUGGAGE-FILE ASSIGN TO UT-S-INPUT.                    00002300
           SELECT PRINT-FILE   ASSIGN TO UT-S-OUTPUT.                   00002400
      *                                                                 00002500
       DATA DIVISION.                                                   00002600
       FILE SECTION.                                                    00002700
       FD LUGGAGE-FILE                                                  00002800
           LABEL RECORDS OMITTED.                                       00002901
       01  LUGGAGE-RECORD.                                              00003001
           05  LUG-NAME-IN             PIC X(25).                       00003101
           05  LUG-ADDRESS-IN          PIC X(40).                       00003201
           05  FILLER                  PIC X(15).                       00003301
      *                                                                 00003401
       FD  PRINT-FILE                                                   00003501
           LABEL RECORDS OMITTED.                                       00003601
       01  PRINT-LINE                  PIC X(132).                      00003701
      *                                                                 00003800
       WORKING-STORAGE SECTION.                                         00003900
       01  PRT-NAME-LINE.                                               00004001
           05  FILLER                  PIC X(11) VALUE SPACES.          00004101
           05  FILLER                  PIC X(10)                        00004201
               VALUE "MY NAME IS".                                      00004301
           05  FILLER                  PIC X(10) VALUE SPACES.          00004401
           05  PRT-NAME                PIC X(25).                       00004501
      *                                                                 00004601
       01  PRT-ADDRESS-LINE.                                            00004701
           05  FILLER                  PIC X(11) VALUE SPACES.          00004801
           05  FILLER                  PIC X(13)                        00004901
               VALUE "MY ADDRESS IS".                                   00005001
           05  FILLER                  PIC X(07) VALUE SPACES.          00005101
           05  PRT-ADDRESS             PIC X(40).                       00005201
      *                                                                 00005300
       77  FLAG-EOF                    PIC X VALUE "N".                 00005401
           88  IS-EOF                  VALUE "Y".                       00005501
      *                                                                 00005601
       PROCEDURE DIVISION.                                              00005700
       000-MAIN.                                                        00005800
      *                                                                 00005900
           PERFORM INITIALIZE.                                          00006001
           PERFORM READ-AND-PROCESS UNTIL IS-EOF.                       00006101
           PERFORM CLOSING.                                             00006201
           STOP RUN.                                                    00006300
      *                                                                 00006400
       INITIALIZE.                                                      00006501
           OPEN INPUT LUGGAGE-FILE,                                     00006601
                OUTPUT PRINT-FILE.                                      00006701
      *                                                                 00006801
       READ-AND-PROCESS.                                                00006901
           MOVE SPACES TO PRT-NAME, PRT-ADDRESS.                        00007001
           READ LUGGAGE-FILE AT END MOVE "Y" TO FLAG-EOF.               00007101
           MOVE LUG-NAME-IN TO PRT-NAME.                                00007201
           WRITE PRINT-LINE FROM PRT-NAME-LINE AFTER 2 LINES.           00007301
           MOVE LUG-ADDRESS-IN TO PRT-ADDRESS.                          00007401
           WRITE PRINT-LINE FROM PRT-ADDRESS-LINE.                      00007501
      *                                                                 00007601
       CLOSING.                                                         00007701
           CLOSE LUGGAGE-FILE, PRINT-FILE.                              00007801
/*                                                                      00007900
//*                                                                     00008001
//GO.INPUT DD *                                                         00008101
SAVANNAH CRAWLEY         COMET HOUSE  8264, MILANO - 1877               00008201
LUKE AINSWORTH           APOSTLE  6364, SANTA ANA - 6802                00008301
ROSALYN SMITH            HOWARD 2748, SAN ANTONIO - 2552                00008401
DANIEL WILSON            QUEENSBERRY  4745, TOLEDO - 3237               00008501
ENOCH PARKER             BACON  9145, ALBUQUERQUE - 4773                00008601
MAYA MILLER              LAKE 5169, BELLEVUE - 2135                     00008701
KARLA OSWALD             BEACONSFIELD  9025, FORT LAUDERDALE            00008801
RUTH HARRIS              THORNDIKE   7670, LINCOLN - 4477               00008901
JACOB MARTIN             PARKFIELDS 5938, JACKSONVILLE - 4124           00009001
JULES ROGERS             COLLENT   3122, OKLAHOMA CITY - 3480           00009101
JOY THATCHER             BLETCHLEY   1854, HAYWARD - 7581               00009201
HAZEL ADDIS              BLAKE  5558, FORT LAUDERDALE - 3042            00009301
PERCY EDDISON            BUTTONWOOD 3932, DENVER - 3288                 00009401
LUCAS HARRISON           DUNSTANS  6786, PHOENIX - 3403                 00009501
CLINT SHELDON            BERRY  6831, SAN FRANCISCO - 2211              00009601
/*                                                                      00009701
//GO.OUTPUT DD SYSOUT=*,                                                00009801
//          DCB=(RECFM=FBA,LRECL=132,BLKSIZE=13200)                     00009901
//                                                                      00010000
