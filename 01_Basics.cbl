      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.      *>Mandatory
       PROGRAM-ID. Basics.           *>Mandatory
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.


       01  MyName PICTURE X(5).      *>AlphaNumeric
       01  Phone  PICTURE 9(10).     *>Numeric
       01  Marks  PICTURE S999.      *>Sign Numeric
       01  Income PICTURE 9(3)v9(2). *>Decimal point numeric
       01  Tax    PICTURE Z99.99.    *>z suppress 0es before any value
       01  My-Add PIC X(40) VALUE "10 Rajpath, Delhi, India".
       01  Height PIC 99 VALUE ZEROES.
       01  Distnc PIC Z(4) VALUE SPACES.

       01  My-Score.
           05 Maths PIC 9(3).
           05 Phy   Pic 9(3).
           05 Che   PIC 9(3).

       PROCEDURE DIVISION.

           MOVE "Manan" TO MyName.
           MOVE 1234567899 TO Phone.
           Move -100 TO Marks.
           Move 980.10 TO Income.
           MOVE 030.10 TO Tax.

           MOVE "111222333" TO My-Score.


           DISPLAY "MyName: " MyName.
           DISPLAY "Phone: " Phone.
           DISPLAY "Marks: " Marks.
           DISPLAY "Income: " Income.
           DISPLAY "Tax: " Tax.
           DISPLAY "Address: " My-Add.
           DISPLAY "Height: " Height.
           DISPLAY "Distnc: " Distnc.

           DISPLAY "My-Score: " My-Score.
           DISPLAY "Maths: " Maths.
           DISPLAY "Physics: " Phy.
           DISPLAY "Chemistry: " Che.
            STOP RUN.


       END PROGRAM Basics.
