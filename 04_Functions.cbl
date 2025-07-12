      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 04_Functions.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  ANSWER PIC 999 VALUES ZEROES.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
      *>      CHARACTER FUNCTIONS

           DISPLAY FUNCTION LENGTH("ASDFGH").
           DISPLAY FUNCTION REVERSE("ASDFGH").
           DISPLAY FUNCTION UPPER-CASE("ASDFGH").
           DISPLAY FUNCTION LOWER-CASE("ASDFGH").

      *>      TRIGONOMATRIC FUNCTIONS

           DISPLAY FUNCTION SIN(0).
           DISPLAY FUNCTION COS(0).
            STOP RUN.
