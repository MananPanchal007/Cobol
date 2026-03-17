       IDENTIFICATION DIVISION.
       PROGRAM-ID. 20_TableExample.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       * Define a table (array)
       01  NAME-TABLE.
           05 NAME-ENTRY OCCURS 3 TIMES.
              10 FIRSTNAME PIC X(10).
              10 LASTNAME  PIC X(10).

       01  FULLNAME PIC X(25).
       01  I        PIC 9 VALUE 1.

       * For INSPECT
       01  WS-COUNT PIC 9(2) VALUE 0.

       PROCEDURE DIVISION.

       * -------------------------------
       * Populate Table
       * -------------------------------
           MOVE "MANAN"   TO FIRSTNAME(1)
           MOVE "PANCHAL" TO LASTNAME(1)

           MOVE "JOHN"    TO FIRSTNAME(2)
           MOVE "DOE"     TO LASTNAME(2)

           MOVE "ALICE"   TO FIRSTNAME(3)
           MOVE "SMITH"   TO LASTNAME(3)

       * -------------------------------
       * Loop through table
       * -------------------------------
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3

               * STRING for each entry
               STRING FIRSTNAME(I) DELIMITED BY SPACE
                      " " DELIMITED BY SIZE
                      LASTNAME(I) DELIMITED BY SPACE
                      INTO FULLNAME
               END-STRING

               DISPLAY "Full Name: " FULLNAME

               * INSPECT TALLYING (count 'A')
               MOVE 0 TO WS-COUNT
               INSPECT FULLNAME
                   TALLYING WS-COUNT FOR ALL 'A'

               DISPLAY "Count of A: " WS-COUNT

           END-PERFORM

           STOP RUN.
