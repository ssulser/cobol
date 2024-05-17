//SIMONCO JOB (1),'PAYRATE',CLASS=A,MSGCLASS=A                          00000100
//LEAPY  EXEC PROC=COB2UCG,SYSOUT='*'                                   00000200
//SYSIN   DD *                                                          00000300
       IDENTIFICATION DIVISION.                                         00000400
       PROGRAM-ID. PAYRATE.                                             00000500
       AUTHOR. SIMON SULSER.                                            00000600
       DATE-WRITTEN. 24/05/17.                                          00000700
       DATE-COMPILED.                                                   00000800
      *                                                                 00000900
       ENVIRONMENT DIVISION.                                            00001000
       CONFIGURATION SECTION.                                           00001100
       SOURCE-COMPUTER. IBM-370.                                        00001200
       OBJECT-COMPUTER. IBM-370.                                        00001300
      *                                                                 00001401
       INPUT-OUTPUT SECTION.                                            00001501
       FILE-CONTROL.                                                    00001601
           SELECT CARD-FILE ASSIGN TO UT-S-INPUT.                       00001701
           SELECT PRINT-FILE ASSIGN TO UT-S-OUTPUT.                     00001801
      *                                                                 00001901
       DATA DIVISION.                                                   00002001
       FILE SECTION.                                                    00002101
       FD  CARD-FILE                                                    00002201
           LABEL RECORD IS OMITTED.                                     00002301
       01  CARD-RECORD.                                                 00002401
           05  EMP-NAME-IN             PIC X(25).                       00002501
           05  EMP-ID-IN               PIC X(10).                       00002601
           05  EMP-DEPARTEMENT-IN      PIC X(15).                       00002701
           05  EMP-RATE-OF-PAY-IN      PIC 99V99.                       00002801
           05  FILLER                  PIC X(26).                       00002901
      *                                                                 00003001
       FD  PRINT-FILE                                                   00003101
           LABEL RECORD IS OMITTED.                                     00003201
       01  PRINT-LINE.                                                  00003301
           05  FILLER                  PIC X(21).                       00003401
           05  EMP-NAME-OUT            PIC X(25).                       00003501
           05  FILLER                  PIC X(5).                        00003601
           05  EMP-ID-OUT              PIC X(10).                       00003701
           05  FILLER                  PIC X(10).                       00003801
           05  EMP-DEPARTEMENT-OUT     PIC X(15).                       00003901
           05  FILLER                  PIC X(5).                        00004001
           05  EMP-RATE-OF-PAY-OUT     PIC $99.99.                      00004101
      *                                                                 00004201
       WORKING-STORAGE SECTION.                                         00004301
      *                                                                 00004401
       PROCEDURE DIVISION.                                              00004501
       000-MAIN.                                                        00004601
           PERFORM INITIALIZATION.                                      00004701
           PERFORM READ-AND-PRINT 15 TIMES.                             00004801
           PERFORM CLOSING.                                             00004901
           STOP RUN.                                                    00005001
      *                                                                 00005101
       INITIALIZATION.                                                  00005201
           OPEN INPUT CARD-FILE,                                        00005301
                OUTPUT PRINT-FILE.                                      00005401
      *                                                                 00005501
       READ-AND-PRINT.                                                  00005601
           MOVE SPACES TO PRINT-LINE.                                   00005701
           READ CARD-FILE                                               00005801
               AT END STOP RUN.                                         00005901
           MOVE EMP-NAME-IN TO EMP-NAME-OUT.                            00006001
           MOVE EMP-ID-IN TO EMP-ID-OUT.                                00006101
           MOVE EMP-DEPARTEMENT-IN TO EMP-DEPARTEMENT-OUT.              00006201
           MOVE EMP-RATE-OF-PAY-IN TO EMP-RATE-OF-PAY-OUT.              00006301
           WRITE PRINT-LINE.                                            00006401
      *                                                                 00006501
       CLOSING.                                                         00006601
           CLOSE CARD-FILE, PRINT-FILE.                                 00006701
/*                                                                      00006801
//GO.INPUT DD *                                                         00006901
JOSHUA WATSON            447-221-88BUDGET CONTROL 4856                  00007001
LAUREN RODRIGUEZ         123-456-87MARKETING      5000                  00007101
NATHAN SCOTT             654-852-85ACCOUNTING     7500                  00007201
BENJAMIN PEREZ           159-753-56STRATEGIES     5600                  00007301
ASHLEY WARD              612-782-12OPERATIONS     6250                  00007401
EVELYN BELL              963-852-45PLANNING SPC   5875                  00007501
GABRIEL ANDERSON         854-698-14RESEARCH       5687                  00007601
TIMOTHY CARTER           654-852-99BRAND DIRECTOR 6352                  00007701
OLIVER MOORE             112-254-56MARKETING      5345                  00007801
OWEN WOOD                787-584-55IT SERVICES    6541                  00007901
ROBERT ROSS              446-187-53RESEARCH       5988                  00008001
HEATHER THOMAS           268-471-77IT SERVICES    5784                  00008101
MEGAN SANDERS            784-885-69ACCOUNTING     6582                  00008201
AUBREY COLLINS           456-753-12RESEARCH       5874                  00008301
JAMES WASHINGTON         574-698-55FINANCE MANAGER8500                  00008401
/*                                                                      00008501
//GO.OUTPUT DD SYSOUT=*,                                                00008601
//             DCB=(RECFM=FBA,LRECL=132,BLKSIZE=13200)                  00008701
//                                                                      00008801
