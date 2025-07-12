      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 04_MathematicalFunctions.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.
           DISPLAY "COBOL MATHEMATICAL FUNCTIONS DEMO".
           DISPLAY FUNCTION SUM(1 2).
           DISPLAY FUNCTION SQRT(1).
           DISPLAY FUNCTION REM(1 2).
           DISPLAY FUNCTION FACTORIAL(2).
           DISPLAY FUNCTION LOG(1).
           DISPLAY FUNCTION LOG10(32).
           DISPLAY FUNCTION INTEGER(4.8).
           DISPLAY FUNCTION NUMVAL("    12").
           DISPLAY FUNCTION NUMVAL-C("$    12").
           DISPLAY FUNCTION RANDOM(9).

           STOP RUN.
