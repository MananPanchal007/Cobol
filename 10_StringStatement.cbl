      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 10_StringStatement.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  FIRSTNAME PIC X(6) VALUE "MANAN".
       01  LASTNAME PIC X(7) VALUE "PANCHAL".
       01  FULLNAME PIC X(13) VALUE SPACES.
       PROCEDURE DIVISION.

           STRING FIRSTNAME DELIMITED BY SIZE
                  LASTNAME DELIMITED BY SIZE
                  INTO FULLNAME
           END-STRING.

           DISPLAY FULLNAME.
           STOP RUN.
