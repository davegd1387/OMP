      *-----------------------
       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID.    HACKER.
       AUTHOR.        Dave Driscoll.
      *--------------------
       ENVIRONMENT DIVISION.
      *--------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT HACKER-IN ASSIGN TO HACKIN.
           SELECT HACKER-OUT  ASSIGN TO HACKOUT.
      *-------------
       DATA DIVISION.
      *-------------
       FILE SECTION.
       FD  HACKER-IN RECORDING MODE F.
       01  HACKER-REC PIC X(143).

       FD  HACKER-OUT RECORDING MODE F
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F
           RECORD CONTAINS 138 CHARACTERS
           LABEL RECORD IS STANDARD.
      * 01  HACKER-OUT-REC              PIC X(138).
        01  HACKER-OUT-REC.
           05 HACK-OUT-MAIN.
              10 HACK-OUT-ID PIC X(8).
              10 HACK-OUT-TITLE PIC X(96).
              10 HACK-OUT-POINTS PIC 9(4).
              10 HACK-OUT-COMMENTS PIC 9(4).
              10 HACK-OUT-AUTHOR PIC X(15).
           05 HACK-OUT-TIME PIC X(05).
           05 HACK-OUT-RANKING-SCORE PIC S9999V999999 COMP-3.


       WORKING-STORAGE SECTION.
       01 FLAGS.
         05 LASTREC           PIC X VALUE SPACE.
       01 WS-TITLE PIC X(96).
       01 DT PIC X(10).
       01 HOURS PIC 9(2).
       01 MINUTES PIC 9(2).
       01 DEC-TIME PIC 99V999.
       01 DEC-TIME-DISP PIC ZZ.999.
       01 ALT-RANKING-SCORE PIC S9999V999999 COMP-3.
       01 DISP-RANKING-SCORE PIC Z,ZZZ.999999.
       01 OVERFLOW-CH PIC X(5).
       01 COUNTER PIC 9(4) COMP.

       01  HACKER-IN-REC.
           05 HACK-IN-MAIN.
              10 HACK-IN-ID PIC X(8).
              10 HACK-IN-TITLE PIC X(96).
              10 HACK-IN-POINTS PIC 9(5).
              10 HACK-IN-COMMENTS PIC 9(5).
              10 HACK-IN-AUTHOR PIC X(15).
           05 HACK-IN-CREATE-DATE PIC X(16).
      *------------------
       PROCEDURE DIVISION.
      *------------------
       OPEN-FILES.
           OPEN INPUT  HACKER-IN.
           OPEN OUTPUT HACKER-OUT.
      *
       READ-NEXT-RECORD.
           PERFORM READ-NEW-RECORD.
           PERFORM UNTIL LASTREC = 'Y'
               PERFORM STRING-CSV
               PERFORM FORMAT-OUT-RECORD

               PERFORM READ-NEW-RECORD
            END-PERFORM.

       STRING-CSV.
           INITIALIZE HACKER-IN-REC
           UNSTRING HACKER-REC DELIMITED BY ","
             INTO HACK-IN-ID, HACK-IN-TITLE, HACK-IN-POINTS,
             HACK-IN-COMMENTS, HACK-IN-AUTHOR, HACK-IN-CREATE-DATE.

       CLOSE-STOP.
           CLOSE HACKER-IN.
           CLOSE HACKER-OUT.
           GOBACK.

       READ-NEW-RECORD.
           READ HACKER-IN
           AT END MOVE 'Y' TO LASTREC
           END-READ.

       FORMAT-OUT-RECORD.

           MOVE HACK-IN-TITLE TO WS-TITLE.

           INSPECT WS-TITLE CONVERTING
            "abcdefghijklmnopqrstuvwxyz" TO
            "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
           MOVE 0 to COUNTER.
           INSPECT WS-TITLE TALLYING COUNTER
              FOR ALL "MAINFRAME".
           INSPECT WS-TITLE TALLYING COUNTER
              FOR ALL "COBOL ".
           IF COUNTER > 0
      *       DISPLAY "in POINTS " HACK-IN-POINTS  " COMMENTS "
      *        HACK-IN-COMMENTS
      *        DISPLAY "HACK-IN-CREATE-DATE " HACK-IN-CREATE-DATE
              INITIALIZE HACKER-OUT-REC
              MOVE HACK-IN-MAIN TO HACK-OUT-MAIN
              UNSTRING HACK-IN-CREATE-DATE DELIMITED BY SPACES
                  INTO DT, HACK-OUT-TIME, OVERFLOW-CH
                    ON OVERFLOW
                    CONTINUE
      *                DISPLAY "OVERFLOW" HACK-IN-CREATE-DATE
              END-UNSTRING
              PERFORM CALCULATE-SCORE
              PERFORM WRITE-RECORD
           END-IF.

       CALCULATE-SCORE.
           UNSTRING FUNCTION TRIM(HACK-OUT-TIME) DELIMITED BY ":"
              INTO HOURS, MINUTES

           COMPUTE DEC-TIME = HOURS + (MINUTES / 60).
           MOVE DEC-TIME TO DEC-TIME-DISP.
      *    DISPLAY "DECIMAL TIME: " DEC-TIME-DISP.

           COMPUTE HACK-OUT-RANKING-SCORE =
              (HACK-IN-POINTS - 1) ** 0.8 /
              (DEC-TIME + 2) ** 1.8.
           MOVE HACK-OUT-RANKING-SCORE TO DISP-RANKING-SCORE.
      *     DISPLAY "SCORE: " DISP-RANKING-SCORE.

       WRITE-RECORD.

           WRITE HACKER-OUT-REC.
