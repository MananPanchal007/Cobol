      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
              IDENTIFICATION DIVISION.
       PROGRAM-ID. 10_StringStatement.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  FIRSTNAME PIC X(6) VALUE "MANAN".
       01  LASTNAME  PIC X(7) VALUE "PANCHAL".
       01  FULLNAME  PIC X(20) VALUE SPACES.

       * For UNSTRING
       01  U-FIRST    PIC X(10).
       01  U-LAST     PIC X(10).

       * For INSPECT
       01  WS-COUNT   PIC 9(2) VALUE 0.

       PROCEDURE DIVISION.

       * -------------------------------
       * STRING
       * -------------------------------
           STRING FIRSTNAME DELIMITED BY SPACE
                  " " DELIMITED BY SIZE
                  LASTNAME DELIMITED BY SPACE
                  INTO FULLNAME
           END-STRING.

           DISPLAY "After STRING: " FULLNAME.

       * -------------------------------
       * UNSTRING
       * -------------------------------
           UNSTRING FULLNAME
               DELIMITED BY SPACE
               INTO U-FIRST
                    U-LAST
           END-UNSTRING.

           DISPLAY "After UNSTRING:"
           DISPLAY "First Name: " U-FIRST
           DISPLAY "Last Name : " U-LAST

       * -------------------------------
       * INSPECT (TALLYING)
       * Count occurrences of 'A'
       * -------------------------------
           INSPECT FULLNAME
               TALLYING WS-COUNT FOR ALL 'A'.

           DISPLAY "Count of A: " WS-COUNT

       * -------------------------------
       * INSPECT with REPLACING
       * Replace A with *
       * -------------------------------
           INSPECT FULLNAME
               REPLACING ALL 'A' BY '*'.

           DISPLAY "After REPLACING A with *: " FULLNAME

       * -------------------------------
       * INSPECT with CONVERTING
       * Convert lowercase to uppercase
       * -------------------------------
           MOVE "manan panchal" TO FULLNAME

           INSPECT FULLNAME
               CONVERTING 'abcdefghijklmnopqrstuvwxyz'
                           TO 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.

           DISPLAY "After CONVERTING to uppercase: " FULLNAME

           STOP RUN.
