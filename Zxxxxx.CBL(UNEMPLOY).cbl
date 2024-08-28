      *-------------------------------------------------*
      *------Open Mainframe Project COBOL 3 / 9.3-------*
      *-------------------------------------------------*
      *-------------CREATE VSAM DATABASE----------------*
      *-------------------------------------------------*
      *---Read Missouri Unemployment statistic files ---*
      *---in 5 different layouts but with common key.---*
      *---Check table for key. If found, READ VSAM,  ---*
      *---add info to VSAM layout and REWRITE.       ---*
      *---Else WRITE VSAM & add key to table.        ---*
      *-------------------------------------------------*
       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID.    UNEMPLOY.
       AUTHOR.        Dave Driscoll.
      *--------------------
       ENVIRONMENT DIVISION.
      *--------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CSV-RACE      ASSIGN TO UNMPRACE.
           SELECT CSV-SEX       ASSIGN TO UNMPSEX.
           SELECT CSV-ETHNICITY ASSIGN TO UNMPETHN.
           SELECT CSV-AGE       ASSIGN TO UNMPAGE.
           SELECT CSV-INDUSTRY  ASSIGN TO UNMPIND.
           SELECT UE-VSAM            ASSIGN TO UNMPVSAM
           ORGANIZATION IS INDEXED
              ACCESS IS RANDOM
              RECORD KEY IS UE-VSAM-KEY
              FILE STATUS IS FILE-STATUS.
      *-------------
       DATA DIVISION.
      *-------------
       FILE SECTION.
       FD  CSV-RACE          RECORDING MODE F.
       01  CSV-RACE-REC      PIC X(80).
       FD  CSV-SEX           RECORDING MODE F.
       01  CSV-SEX-REC       PIC X(80).
       FD  CSV-ETHNICITY     RECORDING MODE F.
       01  CSV-ETHNICITY-REC PIC X(70).
       FD  CSV-AGE           RECORDING MODE F.
       01  CSV-AGE-REC       PIC X(70).
       FD  CSV-INDUSTRY      RECORDING MODE F.
       01  CSV-INDUSTRY-REC  PIC X(145).
       FD  UE-VSAM.
       01  UE-VSAM-REC.
           05 UE-VSAM-COMMON-FIELDS.
               10 UE-VSAM-KEY        PIC X(8).
               10 UE-VSAM-DATE       PIC X(10).
           05 UE-VSAM-RACE-CATEGORIES.
               10  UE-VSAM-RACE-WHITE        PIC 9(6).
               10  UE-VSAM-RACE-ASIAN        PIC 9(6).
               10  UE-VSAM-RACE-BLACK-AA     PIC 9(6).
               10  UE-VSAM-RACE-AMIND-ALNAT  PIC 9(6).
               10  UE-VSAM-RACE-HAWI-PACISL  PIC 9(6).
           05 UE-VSAM-SEX-CATEGORIES.
               10  UE-VSAM-SEX-FEMALE        PIC 9(6).
               10  UE-VSAM-SEX-MALE          PIC 9(6).
           05 UE-VSAM-ETHNICITY-CATEGORIES.
               10  UE-VSAM-ETHNICITY-HISP-LATINO   PIC 9(6).
               10  UE-VSAM-ETHNICITY-NOT-HL        PIC 9(6).
           05 UE-VSAM-AGE-CATEGORIES.
               10  UE-VSAM-AGE-LT-22         PIC 9(6).
               10  UE-VSAM-AGE-22-24         PIC 9(6).
               10  UE-VSAM-AGE-25-34         PIC 9(6).
               10  UE-VSAM-AGE-35-44         PIC 9(6).
               10  UE-VSAM-AGE-45-54         PIC 9(6).
               10  UE-VSAM-AGE-55-59         PIC 9(6).
               10  UE-VSAM-AGE-60-64         PIC 9(6).
               10  UE-VSAM-AGE-GTEQ-65       PIC 9(6).
           05 UE-VSAM-IND-CATEGORIES.
               10  UE-VSAM-IND-WH-TRADE          PIC 9(6).
               10  UE-VSAM-IND-TRANS-WAREH       PIC 9(6).
               10  UE-VSAM-IND-CONSTR            PIC 9(6).
               10  UE-VSAM-IND-FIN-INS           PIC 9(6).
               10  UE-VSAM-IND-MFTRG             PIC 9(6).
               10  UE-VSAM-IND-AG-FRST-FISH-HUNT PIC 9(6).
               10  UE-VSAM-IND-PUB-ADMIN         PIC 9(6).
               10  UE-VSAM-IND-UTIL              PIC 9(6).
               10  UE-VSAM-IND-ACCOM-FOOD        PIC 9(6).
               10  UE-VSAM-IND-INFO              PIC 9(6).
               10  UE-VSAM-IND-PROF-SCI-TECH     PIC 9(6).
               10  UE-VSAM-IND-RE-RENT-LEASE     PIC 9(6).
               10  UE-VSAM-IND-OTHER-SVCS        PIC 9(6).
               10  UE-VSAM-IND-MGMT-COMP-ENT     PIC 9(6).
               10  UE-VSAM-IND-EDU-SVCS          PIC 9(6).
               10  UE-VSAM-IND-MINING            PIC 9(6).
               10  UE-VSAM-IND-HLTHCARE-SOCASST  PIC 9(6).
               10  UE-VSAM-IND-ART-ENT-REC       PIC 9(6).
               10  UE-VSAM-IND-ADM-WASTE-REM     PIC 9(6).
               10  UE-VSAM-IND-RETAIL            PIC 9(6).

       WORKING-STORAGE SECTION.
       01  RACE-REC.
           05  RACE-COMMON-FIELDS.
               10  RACE-KEY          PIC 9(8).
               10  RACE-DATE         PIC X(10).
           05  RACE-INA              PIC 9(5).
           05  RACE-CATEGORIES.
               10  RACE-WHITE        PIC 9(6).
               10  RACE-ASIAN        PIC 9(6).
               10  RACE-BLACK-AA     PIC 9(6).
               10  RACE-AMIND-ALNAT  PIC 9(6).
               10  RACE-HAWI-PACISL  PIC 9(6).

       01  SEX-REC.
           05  SEX-COMMON-FIELDS.
               10  SEX-KEY           PIC 9(8).
               10  SEX-DATE          PIC X(10).
           05  SEX-INA               PIC 9(5).
           05  SEX-CATEGORIES.
               10  SEX-FEMALE        PIC 9(6).
               10  SEX-MALE          PIC 9(6).

       01  ETHNICITY-REC.
           05  ETHNICITY-COMMON-FIELDS.
               10  ETHNICITY-KEY           PIC 9(8).
               10  ETHNICITY-DATE          PIC X(10).
           05  ETHNICITY-INA               PIC 9(5).
           05  ETHNICITY-CATEGORIES.
               10  ETHNICITY-HISP-LATINO   PIC 9(6).
               10  ETHNICITY-NOT-HL        PIC 9(6).

       01  AGE-REC.
           05  AGE-COMMON-FIELDS.
               10  AGE-KEY           PIC 9(8).
               10  AGE-DATE          PIC X(10).
           05  AGE-INA               PIC 9(5).
           05  AGE-CATEGORIES.
               10  AGE-LT-22         PIC 9(6).
               10  AGE-22-24         PIC 9(6).
               10  AGE-25-34         PIC 9(6).
               10  AGE-35-44         PIC 9(6).
               10  AGE-45-54         PIC 9(6).
               10  AGE-55-59         PIC 9(6).
               10  AGE-60-64         PIC 9(6).
               10  AGE-GTEQ-65       PIC 9(6).

       01   INDUSTRY-REC.
           05  INDUSTRY-COMMON-FIELDS.
               10  INDUSTRY-KEY               PIC 9(8).
               10  INDUSTRY-DATE              PIC X(10).
           05  INDUSTRY-INA                   PIC 9(5).
           05  INDUSTRY-CATEGORIES.
               10  INDUSTRY-WH-TRADE          PIC 9(6).
               10  INDUSTRY-TRANS-WAREH       PIC 9(6).
               10  INDUSTRY-CONSTR            PIC 9(6).
               10  INDUSTRY-FIN-INS           PIC 9(6).
               10  INDUSTRY-MFTRG             PIC 9(6).
               10  INDUSTRY-AG-FRST-FISH-HUNT PIC 9(6).
               10  INDUSTRY-PUB-ADMIN         PIC 9(6).
               10  INDUSTRY-UTIL              PIC 9(6).
               10  INDUSTRY-ACCOM-FOOD        PIC 9(6).
               10  INDUSTRY-INFO              PIC 9(6).
               10  INDUSTRY-PROF-SCI-TECH     PIC 9(6).
               10  INDUSTRY-RE-RENT-LEASE     PIC 9(6).
               10  INDUSTRY-OTHER-SVCS        PIC 9(6).
               10  INDUSTRY-MGMT-COMP-ENT     PIC 9(6).
               10  INDUSTRY-EDU-SVCS          PIC 9(6).
               10  INDUSTRY-MINING            PIC 9(6).
               10  INDUSTRY-HLTHCARE-SOCASST  PIC 9(6).
               10  INDUSTRY-ART-ENT-REC       PIC 9(6).
               10  INDUSTRY-ADM-WASTE-REM     PIC 9(6).
               10  INDUSTRY-RETAIL            PIC 9(6).

       01 SAVE-ITEM-POSITION  PIC 9(3) VALUE 1.
       01  KEY-TABLE.
         05 KEY-ITEMS      PIC 9(8) OCCURS 300 TIMES
           INDEXED BY KEY-IDX.

       01 FLAGS.
         05 EOF-FLAG           PIC X VALUE SPACE.
           88 EOF               VALUE 'Y'.
           88 READ-IN-PROGRESS  VALUE 'N'.
         05 FILE-IN-PROGRESS   PIC X VALUE SPACE.
           88 RACE               VALUE 'R'.
           88 SEX                VALUE 'S'.
           88 AGE                VALUE 'A'.
           88 ETHNICITY          VALUE 'E'.
           88 INDUSTRY           VALUE 'I'.
       01 TARGET-KEY           PIC 9(8).
       01 FILE-STATUS          PIC X(02) VALUE SPACES.
       01 RACE-CTR           PIC 9(3) VALUE 0.
       01 SEX-CTR           PIC 9(3) VALUE 0.
       01 AGE-CTR           PIC 9(3) VALUE 0.
       01 ETH-CTR           PIC 9(3) VALUE 0.
       01 IND-CTR           PIC 9(3) VALUE 0.
       01 UPDATE-CTR          PIC 9(3) VALUE 0.
       01 WRITE-CTR           PIC 9(3) VALUE 0.
       01 WS-LAST-FIELD       PIC X(6).


      *------------------
       PROCEDURE DIVISION.
      *------------------
       0000-MAIN.
           PERFORM 0100-OPEN-FILES.
           SET READ-IN-PROGRESS TO TRUE.
           PERFORM 0200-CSV-RACE.
           SET READ-IN-PROGRESS TO TRUE.
           PERFORM 0300-CSV-SEX.
           SET READ-IN-PROGRESS TO TRUE.
           PERFORM 0400-CSV-ETHNICITY.
           SET READ-IN-PROGRESS TO TRUE.
           PERFORM 0500-CSV-AGE.
           SET READ-IN-PROGRESS TO TRUE.
           PERFORM 0600-CSV-INDUSTRY.
           PERFORM 1000-CLOSE-FILES.

           DISPLAY "Race counter: " RACE-CTR.
           DISPLAY "Sex counter: " SEX-CTR.
           DISPLAY "Age counter: " AGE-CTR.
           DISPLAY "Industry counter: " IND-CTR.
           DISPLAY "Ethnicity counter: " ETH-CTR.

           DISPLAY "Write counter: " WRITE-CTR.
           DISPLAY "Update counter: " UPDATE-CTR.

           GOBACK.

       0050-SEARCH-KEY-TABLE.
           SET KEY-IDX TO 1.
           SEARCH KEY-ITEMS VARYING KEY-IDX
             AT END PERFORM 0051-ADD-KEY-TO-TABLE
             WHEN KEY-ITEMS(KEY-IDX) = TARGET-KEY
                DISPLAY TARGET-KEY ' FOUND IN TABLE'
           END-SEARCH.

       0051-ADD-KEY-TO-TABLE.

           MOVE TARGET-KEY TO KEY-ITEMS(SAVE-ITEM-POSITION)
           DISPLAY TARGET-KEY ' ADDED TO TABLE'
           MOVE 0 TO TARGET-KEY
           ADD 1 TO SAVE-ITEM-POSITION.

       0100-OPEN-FILES.
           OPEN INPUT  CSV-RACE,
                       CSV-SEX,
                       CSV-ETHNICITY,
                       CSV-AGE,
                       CSV-INDUSTRY.
           OPEN I-O    UE-VSAM.
           IF FILE-STATUS NOT = "00"
              DISPLAY "OPEN FILE-STATUS IS " FILE-STATUS " ENDING..."
              STOP RUN
           END-IF.
.
      *
       0200-CSV-RACE.
           SET RACE TO TRUE.
           PERFORM 0210-READ-CSV-RACE.
           PERFORM UNTIL EOF
               PERFORM 0220-STRING-CSV-RACE
               MOVE RACE-KEY TO TARGET-KEY
               PERFORM 0050-SEARCH-KEY-TABLE
               IF TARGET-KEY > 0
                 MOVE TARGET-KEY  TO UE-VSAM-KEY
                 PERFORM 0700-UPDATE-UE-VASM
               ELSE
                 INITIALIZE UE-VSAM-REC
		         MOVE RACE-KEY  TO UE-VSAM-KEY
                 PERFORM 0750-NEW-UE-VSAM
               END-IF
               PERFORM 0210-READ-CSV-RACE
           END-PERFORM.
      *
       0210-READ-CSV-RACE.
           READ CSV-RACE
           AT END SET EOF TO TRUE
           END-READ
           IF NOT EOF
            ADD 1 TO RACE-CTR.

       0220-STRING-CSV-RACE.
           INITIALIZE RACE-REC.
      *     DISPLAY "CSV-RACE-REC: " CSV-RACE-REC.
           UNSTRING CSV-RACE-REC DELIMITED BY ","
             INTO RACE-KEY,
                  RACE-DATE,
                  RACE-INA,
                  RACE-WHITE,
                  RACE-ASIAN,
                  RACE-BLACK-AA,
                  RACE-AMIND-ALNAT,
                  WS-LAST-FIELD.
      ** Last field in UNSTRING can't be PIC 9 else data is garbage
      ** So WS-LAST-FIELD is PIC X followed by COMPUTE below.
           COMPUTE RACE-HAWI-PACISL = FUNCTION NUMVAL(WS-LAST-FIELD)
      *    DISPLAY "RACE-REC: " RACE-REC.

       0300-CSV-SEX.
           SET SEX TO TRUE.
           PERFORM 0310-READ-CSV-SEX.
           PERFORM UNTIL EOF
               PERFORM 0320-STRING-CSV-SEX
               MOVE SEX-KEY TO TARGET-KEY
               PERFORM 0050-SEARCH-KEY-TABLE
               IF TARGET-KEY > 0
                 MOVE TARGET-KEY  TO UE-VSAM-KEY
                 PERFORM 0700-UPDATE-UE-VASM
               ELSE
                 INITIALIZE UE-VSAM-REC
		         MOVE SEX-KEY  TO UE-VSAM-KEY
                 PERFORM 0750-NEW-UE-VSAM
               END-IF
               PERFORM 0310-READ-CSV-SEX
           END-PERFORM.

       0310-READ-CSV-SEX.
           READ CSV-SEX
           AT END SET EOF TO TRUE
           END-READ
           IF NOT EOF
            ADD 1 TO SEX-CTR.

       0320-STRING-CSV-SEX.
           INITIALIZE SEX-REC.
           UNSTRING CSV-SEX-REC DELIMITED BY ","
             INTO SEX-KEY,
                  SEX-DATE,
                  SEX-INA,
                  SEX-FEMALE,
                  WS-LAST-FIELD.
      ** Last field in UNSTRING can't be PIC 9 else data is garbage
      ** So WS-LAST-FIELD is PIC X followed by COMPUTE below.
           COMPUTE SEX-MALE = FUNCTION NUMVAL(WS-LAST-FIELD).

       0400-CSV-ETHNICITY.
           SET ETHNICITY TO TRUE.
           PERFORM 0410-READ-CSV-ETHNICITY.
           PERFORM UNTIL EOF
               PERFORM 0420-STRING-CSV-ETHNICITY
               MOVE ETHNICITY-KEY TO TARGET-KEY
               PERFORM 0050-SEARCH-KEY-TABLE
               IF TARGET-KEY > 0
                 MOVE TARGET-KEY  TO UE-VSAM-KEY
                 PERFORM 0700-UPDATE-UE-VASM
               ELSE
                 INITIALIZE UE-VSAM-REC
		         MOVE ETHNICITY-KEY  TO UE-VSAM-KEY
                 PERFORM 0750-NEW-UE-VSAM
               END-IF
               PERFORM 0410-READ-CSV-ETHNICITY
           END-PERFORM.

       0410-READ-CSV-ETHNICITY.
           READ CSV-ETHNICITY
           AT END SET EOF TO TRUE
           END-READ
           IF NOT EOF
            ADD 1 TO ETH-CTR.

       0420-STRING-CSV-ETHNICITY.
           INITIALIZE ETHNICITY-REC.
           UNSTRING CSV-ETHNICITY-REC DELIMITED BY ","
             INTO ETHNICITY-KEY,
                  ETHNICITY-DATE,
                  ETHNICITY-INA,
                  ETHNICITY-HISP-LATINO,
                  WS-LAST-FIELD.
      ** Last field in UNSTRING can't be PIC 9 else data is garbage
      ** So WS-LAST-FIELD is PIC X followed by COMPUTE below.
           COMPUTE ETHNICITY-NOT-HL = FUNCTION NUMVAL(WS-LAST-FIELD).

       0500-CSV-AGE.
           SET AGE TO TRUE.
           PERFORM 0510-READ-CSV-AGE.
           PERFORM UNTIL EOF
               PERFORM 0520-STRING-CSV-AGE
               MOVE AGE-KEY TO TARGET-KEY
               PERFORM 0050-SEARCH-KEY-TABLE
               IF TARGET-KEY > 0
                 MOVE TARGET-KEY  TO UE-VSAM-KEY
                 PERFORM 0700-UPDATE-UE-VASM
               ELSE
                 INITIALIZE UE-VSAM-REC
		         MOVE AGE-KEY  TO UE-VSAM-KEY
                 PERFORM 0750-NEW-UE-VSAM
               END-IF
               PERFORM 0510-READ-CSV-AGE
           END-PERFORM.

       0510-READ-CSV-AGE.
           READ CSV-AGE
           AT END SET EOF TO TRUE
           END-READ
           IF NOT EOF
            ADD 1 TO AGE-CTR.

       0520-STRING-CSV-AGE.
           INITIALIZE AGE-REC.
           UNSTRING CSV-AGE-REC DELIMITED BY ","
             INTO AGE-KEY,
                  AGE-DATE,
                  AGE-INA,
                  AGE-LT-22,
                  AGE-22-24,
                  AGE-25-34,
                  AGE-35-44,
                  AGE-45-54,
                  AGE-55-59,
                  AGE-60-64,
                  WS-LAST-FIELD.
      ** Last field in UNSTRING can't be PIC 9 else data is garbage
      ** So WS-LAST-FIELD is PIC X followed by COMPUTE below.
           COMPUTE AGE-GTEQ-65 = FUNCTION NUMVAL(WS-LAST-FIELD).


       0600-CSV-INDUSTRY.
           SET   INDUSTRY TO TRUE.
           PERFORM 0610-READ-CSV-INDUSTRY.
           PERFORM UNTIL EOF
               PERFORM 0620-STRING-CSV-INDUSTRY
               MOVE INDUSTRY-KEY TO TARGET-KEY
               PERFORM 0050-SEARCH-KEY-TABLE
               IF TARGET-KEY > 0
                 MOVE TARGET-KEY  TO UE-VSAM-KEY
                 PERFORM 0700-UPDATE-UE-VASM
               ELSE
                 INITIALIZE UE-VSAM-REC
		         MOVE INDUSTRY-KEY  TO UE-VSAM-KEY
                 PERFORM 0750-NEW-UE-VSAM
               END-IF
               PERFORM 0610-READ-CSV-INDUSTRY
           END-PERFORM.

       0610-READ-CSV-INDUSTRY.
           READ CSV-INDUSTRY
           AT END SET EOF TO TRUE
           END-READ
           IF NOT EOF
            ADD 1 TO IND-CTR.

       0620-STRING-CSV-INDUSTRY.
           INITIALIZE INDUSTRY-REC.
           UNSTRING CSV-INDUSTRY-REC DELIMITED BY ","
             INTO INDUSTRY-KEY,
                  INDUSTRY-DATE,
                  INDUSTRY-INA,
                  INDUSTRY-WH-TRADE,
                  INDUSTRY-TRANS-WAREH,
                  INDUSTRY-CONSTR,
                  INDUSTRY-FIN-INS,
                  INDUSTRY-MFTRG,
                  INDUSTRY-AG-FRST-FISH-HUNT,
                  INDUSTRY-PUB-ADMIN,
                  INDUSTRY-UTIL,
                  INDUSTRY-ACCOM-FOOD,
                  INDUSTRY-INFO,
                  INDUSTRY-PROF-SCI-TECH,
                  INDUSTRY-RE-RENT-LEASE,
                  INDUSTRY-OTHER-SVCS,
                  INDUSTRY-MGMT-COMP-ENT,
                  INDUSTRY-EDU-SVCS,
                  INDUSTRY-MINING,
                  INDUSTRY-HLTHCARE-SOCASST,
                  INDUSTRY-ART-ENT-REC,
                  INDUSTRY-ADM-WASTE-REM,
                  WS-LAST-FIELD.
      ** Last field in UNSTRING can't be PIC 9 else data is garbage
      ** So WS-LAST-FIELD is PIC X followed by COMPUTE below.
           COMPUTE INDUSTRY-RETAIL = FUNCTION NUMVAL(WS-LAST-FIELD).

       0700-UPDATE-UE-VASM.
           READ UE-VSAM KEY IS UE-VSAM-KEY
           IF FILE-STATUS = "00"
              PERFORM 0705-MOVE-FIELDS
              REWRITE UE-VSAM-REC
              ADD 1 TO UPDATE-CTR
              IF FILE-STATUS GREATER THAN "00"
                    DISPLAY "REWRITE FILE-STATUS IS " FILE-STATUS
                            " FOR " UE-VSAM-KEY
              END-IF
           ELSE
              DISPLAY "READ FILE-STATUS IS " FILE-STATUS
                            " FOR " UE-VSAM-KEY
           END-IF.

       0705-MOVE-FIELDS.
           EVALUATE TRUE
              WHEN RACE
                 MOVE RACE-DATE TO UE-VSAM-DATE
                 MOVE RACE-CATEGORIES TO UE-VSAM-RACE-CATEGORIES
              WHEN SEX
                 MOVE SEX-DATE TO UE-VSAM-DATE
                 MOVE SEX-CATEGORIES TO UE-VSAM-SEX-CATEGORIES
              WHEN ETHNICITY
                 MOVE ETHNICITY-DATE TO UE-VSAM-DATE
                 MOVE ETHNICITY-CATEGORIES
                   TO UE-VSAM-ETHNICITY-CATEGORIES
              WHEN AGE
                 MOVE AGE-DATE TO UE-VSAM-DATE
                 MOVE AGE-CATEGORIES TO UE-VSAM-AGE-CATEGORIES
              WHEN INDUSTRY
                 MOVE INDUSTRY-DATE TO UE-VSAM-DATE
                 MOVE INDUSTRY-CATEGORIES
                   TO UE-VSAM-IND-CATEGORIES
           END-EVALUATE.

       0750-NEW-UE-VSAM.
           PERFORM 0705-MOVE-FIELDS.
           WRITE UE-VSAM-REC.
           IF FILE-STATUS NOT = "00"
              DISPLAY "WRITE FILE-STATUS IS " FILE-STATUS
                            " FOR " UE-VSAM-KEY
           ELSE
               ADD 1 TO WRITE-CTR
           END-IF.

       1000-CLOSE-FILES.
           CLOSE CSV-RACE,
                 CSV-SEX,
                 CSV-ETHNICITY,
                 CSV-AGE,
                 CSV-INDUSTRY,
                 UE-VSAM.
      *

