      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 08_Evaluate.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  AGE PIC 99 VALUE ZEROES.

       PROCEDURE DIVISION.
           DISPLAY "PLEASE ENTER YOUR AGE".
           ACCEPT AGE.

           EVALUATE AGE
               WHEN 15
                   DISPLAY "YOU ARE 15"
               WHEN 16
                   DISPLAY "YOU ARE 16"
               WHEN 17
                   DISPLAY "YOU ARE 17"
               WHEN OTHER
                   DISPLAY "YOU ARE NOT 15, 16 OR 17"
           END-EVALUATE.

           EVALUATE TRUE
               WHEN AGE < 18
                   DISPLAY "YOU ARE A MINOR"
               WHEN AGE >= 18
                   DISPLAY "YOU ARE AN ADULT"
           END-EVALUATE.

           STOP RUN.
