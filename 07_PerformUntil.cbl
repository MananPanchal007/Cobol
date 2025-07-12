      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 07_PerformUntil.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  I PIC 9 VALUE 1.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

      *>  000-MAIN-PROCEDURE.
      *>      PERFORM 100-DISPLAY-HELLO UNTIL I > 5.
      *>      STOP RUN.

      *>  100-DISPLAY-HELLO.
      *>      DISPLAY "HELLO WORLD!".
      *>      COMPUTE I = I + 1.

      *>      ***********************************************

           PERFORM WITH TEST AFTER
               VARYING I FROM 1 BY 1
               UNTIL I = 5
               DISPLAY "HELLO WORLD! AFTER"
           END-PERFORM.


           PERFORM WITH TEST BEFORE
               VARYING I FROM 1 BY 1
               UNTIL I = 5
               DISPLAY "HELLO WORLD! BEFORE"
           END-PERFORM.
           STOP RUN.
