      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 05_StatisticalFunc.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "MEAN: " FUNCTION MEAN(1 2 3 4 5 6 7 8 9 10).
           DISPLAY "MEDIAN: " FUNCTION MEDIAN(1 2 3 4 5 6 7 8 9 10).
           DISPLAY "STANDARD-DEVIATION: "
           FUNCTION STANDARD-DEVIATION(1 2 3 4 5 6 7 8 9 10).
           DISPLAY "VARIANCE: "
           FUNCTION VARIANCE(1 2 3 4 5 6 7 8 9 10).
           DISPLAY "MIN: " FUNCTION MIN(1 2 3 4 5 6 7 8 9 10).
           DISPLAY "MAX: " FUNCTION MAX(1 2 3 4 5 6 7 8 9 10).
           STOP RUN.
