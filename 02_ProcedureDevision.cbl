      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 02_ProcedureDivision.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  Phone PIC s9(10) VALUE ZEROES.
       01  MyPhone PIC 9(10).
       01  Addr PIC x(10).

       PROCEDURE DIVISION.

       000-MAIN-PROCEDURE.

           PERFORM 100-ACCEPT-VALUES.
           PERFORM 200-DISPLAY-VALUES.
           STOP RUN.

       100-ACCEPT-VALUES.
           ACCEPT Phone.
           ACCEPT Addr.

       200-DISPLAY-VALUES.
           DISPLAY Phone.
           DISPLAY Addr.
