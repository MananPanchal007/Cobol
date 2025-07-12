      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 09_RefModifi.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  FULL-DATE PIC 9(8) VALUE 31102002.

       01  THIS-DAY PIC 99 VALUE ZEROES.
       01  THIS-MONTH PIC 99 VALUE ZEROES.
       01  THIS-YEAR PIC 9999 VALUE ZEROES.

       PROCEDURE DIVISION.
           MOVE FULL-DATE(1:2) TO THIS-DAY.
           MOVE FULL-DATE(3:2) TO THIS-MONTH.
           MOVE FULL-DATE(5:4) TO THIS-YEAR.
           DISPLAY "DAY: " THIS-DAY.
           DISPLAY "MONTH: " THIS-MONTH.
           DISPLAY "YEAR: " THIS-YEAR.
            STOP RUN.
