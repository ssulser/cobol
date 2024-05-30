000001 IDENTIFICATION DIVISION.                                         00000100
000002 PROGRAM-ID. PAYROLL.                                             00000200
000003 AUTHOR. SIMON SULSER.                                            00000300
000004 DATE-WRITTEN. MAY 25,1924.                                       00000400
000005 DATE-COMPILED.                                                   00000500
000006*                                                                 00000600
000007*                                                                 00000700
000008 ENVIRONMENT DIVISION.                                            00000800
000009 CONFIGURATION SECTION.                                           00000900
000010 SOURCE-COMPUTER. IBM-370.                                        00001000
000011 OBJECT-COMPUTER. IBM-370.                                        00001100
000012 INPUT-OUTPUT SECTION.                                            00001200
000013 FILE-CONTROL.                                                    00001300
000014     SELECT INPUT-FILE   ASSIGN TO UT-S-INPUT.                    00001400
000015     SELECT PRINT-FILE   ASSIGN TO UT-S-OUTPUT.                   00001500
000016*                                                                 00001600
000017*                                                                 00001700
000018 DATA DIVISION.                                                   00001800
000019 FILE SECTION.                                                    00001900
000020 FD  INPUT-FILE                                                   00002000
000021     LABEL RECORD IS OMITTED.                                     00002100
000022 01  INPUT-RECORD.                                                00002200
000023     05 EMP-NAME                 PIC X(25).                       00002300
000024     05 EMP-ID                   PIC X(10).                       00002400
000025     05 EMP-DEPARTMENT           PIC X(15).                       00002500
000026     05 EMP-PAYRATE              PIC 99V99.                       00002600
000027     05 EMP-SALARY-CODE          PIC X.                           00002700
000028     05 EMP-HOURS-WORKED         PIC 99.                          00002800
000029     05 FILLER                   PIC X(23).                       00002900
000030*                                                                 00003000
000031 FD  PRINT-FILE                                                   00003100
000032     LABEL RECORD IS OMITTED                                      00003200
000033     LINAGE IS 40                                                 00003300
000034     LINES AT TOP 3                                               00003400
000035     LINES AT BOTTOM 3                                            00003500
000036     WITH FOOTING 38.                                             00003600
000037 01  PRINT-LINE                  PIC X(132).                      00003700
000038*                                                                 00003800
000039*                                                                 00003900
000040 WORKING-STORAGE SECTION.                                         00004000
000041 77  END-OF-FILE                 PIC X           VALUE "N".       00004100
000042     88  IS-EOF                                  VALUE "Y".       00004200
000043*                                                                 00004300
000044 77  PAGE-COUNT                  PIC 99          VALUE 0.         00004400
000045*                                                                 00004500
000046 77  ACTUAL-DEPARTMENT           PIC X(15)       VALUE SPACES.    00004600
000047*                                                                 00004700
000048 77  PAGE-STATUS                 PIC X           VALUE "Y".       00004800
000049     88 IS-FIRST-PAGE                            VALUE "Y".       00004900
000050     88 IS-NOT-FIRST-PAGE                        VALUE "N".       00005000
000051*                                                                 00005100
000052 01  PAGE-TITLE-LINE.                                             00005200
000053     05 FILLER                   PIC X(42)       VALUE SPACES.    00005300
000054     05 FILLER                   PIC X(58)       VALUE            00005400
000055   "H A L  I N D U S T R I E S  --  P A Y R O L L  R E P O R T".  00005500
000056*                                                                 00005600
000057 01  PAGE-NUMBER-LINE.                                            00005700
000058     05 FILLER                   PIC X(119)      VALUE SPACES.    00005800
000059     05 FILLER                   PIC X(06)       VALUE "PAGE: ".  00005900
000060     05 PAGE-NUMBER              PIC Z9.                          00006000
000061*                                                                 00006100
000062 01  DEPARTMENT-LINE.                                             00006200
000063     05 FILLER                   PIC X(04)       VALUE SPACES.    00006300
000064     05 FILLER                   PIC X(12)       VALUE            00006400
000065        "DEPARTMENT: ".                                           00006500
000066     05 EMP-DEPARTMENT           PIC X(15).                       00006600
000067*                                                                 00006700
000068 01  TITLE-LINE.                                                  00006800
000069     05 FILLER                   PIC X(09)       VALUE SPACES.    00006900
000070     05 FILLER                   PIC X(12)       VALUE            00007000
000071        "EMPLOYEE ID:".                                           00007100
000072     05 FILLER                   PIC X(03)       VALUE SPACES.    00007200
000073     05 FILLER                   PIC X(14)       VALUE            00007300
000074        "EMPLOYEE NAME:".                                         00007400
000075     05 FILLER                   PIC X(15)       VALUE SPACES.    00007500
000076     05 FILLER                   PIC X(05)       VALUE            00007600
000077        "CODE:".                                                  00007700
000078     05 FILLER                   PIC X(05)       VALUE SPACES.    00007800
000079     05 FILLER                   PIC X(06)       VALUE            00007900
000080        "HOURS:".                                                 00008000
000081     05 FILLER                   PIC X(05)       VALUE SPACES.    00008100
000082     05 FILLER                   PIC X(08)       VALUE            00008200
000083        "PAYRATE:".                                               00008300
000084     05 FILLER                   PIC X(09)       VALUE SPACES.    00008400
000085     05 FILLER                   PIC X(06)       VALUE            00008500
000086        "TOTAL:".                                                 00008600
000087*                                                                 00008700
000088 01  OUTPUT-LINE.                                                 00008800
000089     05 FILLER                   PIC X(09)       VALUE SPACES.    00008900
000090     05 EMP-ID                   PIC X(10).                       00009000
000091     05 FILLER                   PIC X(05)       VALUE SPACES.    00009100
000092     05 EMP-NAME                 PIC X(25).                       00009200
000093     05 FILLER                   PIC X(06)       VALUE SPACES.    00009300
000094     05 EMP-SALARY-CODE          PIC X(01).                       00009400
000095     05 FILLER                   PIC X(08)       VALUE SPACES.    00009500
000096     05 EMP-HOURS-WORKED         PIC 99.                          00009600
000097     05 FILLER                   PIC X(09)       VALUE SPACES.    00009700
000098     05 EMP-PAYRATE              PIC Z9.99.                       00009800
000099     05 FILLER                   PIC X(11)       VALUE SPACES.    00009900
000100     05 EMP-LINE-AMOUNT          PIC ZZZ9.99.                     00010000
000101*                                                                 00010100
000102 01  END-OF-REPORT-LINE.                                          00010200
000103     05 FILLER                   PIC X(24)       VALUE            00010300
000104        "   *** END OF REPORT ***".                               00010400
000105*                                                                 00010500
000106*                                                                 00010600
000107 PROCEDURE DIVISION.                                              00010700
000108 000-MAIN.                                                        00010800
000109     PERFORM INITIALIZATION                                       00010900
000110     PERFORM READ-AND-PRINT UNTIL IS-EOF                          00011000
000111     PERFORM CLOSING                                              00011100
000112     STOP RUN.                                                    00011200
000113*                                                                 00011300
000114 INITIALIZATION.                                                  00011400
000115     OPEN INPUT  INPUT-FILE,                                      00011500
000116          OUTPUT PRINT-FILE                                       00011600
000117     PERFORM PRINT-NEW-PAGE                                       00011700
000118     READ INPUT-FILE AT END MOVE "Y" TO END-OF-FILE.              00011800
000119*                                                                 00011900
000120 PRINT-NEW-PAGE.                                                  00012000
000121     ADD 1 TO PAGE-COUNT                                          00012100
000122     IF IS-NOT-FIRST-PAGE THEN                                    00012200
000123         WRITE PRINT-LINE FROM PAGE-TITLE-LINE AFTER PAGE         00012300
000124     ELSE                                                         00012400
000125         WRITE PRINT-LINE FROM PAGE-TITLE-LINE                    00012500
000126         MOVE "Y" TO PAGE-STATUS.                                 00012600
000127*    END-IF                                                       00012700
000128     MOVE PAGE-COUNT TO PAGE-NUMBER                               00012800
000129     WRITE PRINT-LINE FROM PAGE-NUMBER-LINE AFTER 2 LINES         00012900
000130     WRITE PRINT-LINE FROM TITLE-LINE AFTER 2 LINES.              00013000
000131*                                                                 00013100
000132 READ-AND-PRINT.                                                  00013200
000133     MOVE CORRESPONDING INPUT-RECORD TO OUTPUT-LINE               00013300
000134     IF ACTUAL-DEPARTMENT NOT EQUAL EMP-DEPARTMENT                00013400
000135       OF INPUT-RECORD THEN                                       00013500
000136         MOVE EMP-DEPARTMENT OF INPUT-RECORD                      00013600
000137         TO ACTUAL-DEPARTMENT, EMP-DEPARTMENT OF DEPARTMENT-LINE  00013700
000138         WRITE PRINT-LINE FROM DEPARTMENT-LINE AFTER 2 LINES.     00013800
000139*    END-IF                                                       00013900
000140     IF EMP-SALARY-CODE                                           00014000
000141     MULTIPLY EMP-HOURS-WORKED OF INPUT-RECORD BY                 00014100
000142         EMP-PAYRATE OF INPUT-RECORD GIVING EMP-LINE-AMOUNT       00014200
000143     WRITE PRINT-LINE FROM OUTPUT-LINE                            00014300
000144         AT END-OF-PAGE PERFORM PRINT-NEW-PAGE.                   00014400
000145     READ INPUT-FILE AT END MOVE "Y" TO END-OF-FILE.              00014500
000146*                                                                 00014600
000147 CLOSING.                                                         00014700
000148     WRITE PRINT-LINE FROM END-OF-REPORT-LINE AFTER 2 LINES.      00014800
000149     CLOSE INPUT-FILE, PRINT-FILE.                                00014900
