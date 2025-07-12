      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 03_Arithmatics.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
      *>  For adding
       01  A PIC 9(1)V99 VALUE 2.25.
       01  B PIC 9(1)V99 VALUE 1.10.
       01  C PIC 9(1) VALUE ZEROES.

      *>  For subtracting
       01  D PIC 999 VALUE 20.
       01  E PIC 999 VALUE 10.
       01  ANSWER PIC 999 VALUE ZEROES.

      *>      For dividing
       01  F PIC 999 VALUE 24.
       01  G PIC 999 VALUE ZEROES.
       01  VARIABLEC PIC 999 VALUE ZEROES.



       PROCEDURE DIVISION.

      *>      ********************ADDING NUMBERS********************
           ADD A TO B.
           COMPUTE B = A + B.
           ADD A TO B GIVING C.
           DISPLAY "B: " B.
           DISPLAY "C: " C.
           COMPUTE C ROUNDED= A + B.

           ADD A TO B GIVING C ROUNDED.
           DISPLAY "C(rounded): " C.
           DISPLAY"******************************************"

      *>      ******************SUBTRACTING NUMBERS*******************
           SUBTRACT 1 FROM D.
           COMPUTE ANSWER = D - E.

           DISPLAY "D: " D.
           DISPLAY "Answer: " ANSWER.
           DISPLAY"******************************************"

      *>      *************MULTIPLICATION OF NUMBERS****************
           MULTIPLY D BY E.
           COMPUTE ANSWER = D * E.

           DISPLAY "D(MULTIPLY): " D.
           DISPLAY "Answer: " ANSWER.
           DISPLAY"******************************************"

      *>      *************DIVISION OF NUMBERS****************
      *>      DIVIDE 4 INTO F.         *> OR
      *>      DIVIDE F BY 4 GIVING G.  *> OR
      *>      COMPUTE G = F / 4.

           DIVIDE F BY 5 GIVING G REMAINDER VARIABLEC.

           DISPLAY "F: " F.
           DISPLAY "G: " G.
           DISPLAY "VARIABLEC: " VARIABLEC.

           STOP RUN.
