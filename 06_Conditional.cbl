      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 06_Conditional.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  USER-AGE PIC 99 VALUE ZEROES.
       01  STUDENT-MARKS       PIC 9(3).
       01  I PIC 99 VALUES ZEROES.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

      *>      DISPLAY "PLEASE ENTER YOUR AGE".
      *>      ACCEPT USER-AGE.

      *>      IF USER-AGE > 18
      *>          DISPLAY "YOU ARE ADULT"
      *>      ELSE
      *>          DISPLAY "YOU ARE NOT ADULT"
      *>      END-IF.

      *>      DISPLAY "*******************************".

      *> *>          NESTED STATEMENTS
      *>      DISPLAY "Enter Student Marks: ".
      *>      ACCEPT STUDENT-MARKS.

      *>      IF STUDENT-MARKS >= 90 THEN
      *>          DISPLAY "YOUR GRADE IS A+"
      *>      ELSE
      *>          IF STUDENT-MARKS >= 80 THEN
      *>              DISPLAY "YOUR GRADE IS A"
      *>          ELSE
      *>              IF STUDENT-MARKS >= 70 THEN
      *>                  DISPLAY "YOUR GRADE IS B"
      *>              ELSE
      *>                  IF STUDENT-MARKS >= 60 THEN
      *>                      DISPLAY "YOUR GRADE IS C"
      *>                  ELSE
      *>                      DISPLAY "YOUR GRADE IS D"
      *>                  END-IF
      *>              END-IF
      *>          END-IF
      *>      END-IF.
      *>      DISPLAY "*******************************".

      *>      PERFORM UNTIL STATAEMENT
       000-MAIN-PROCEDURE.
           PERFORM 100-DISPLAY-HELLO UNTIL I > 10.
           STOP RUN.

       100-DISPLAY-HELLO.
           DISPLAY "HELLO WORLD!".
           COMPUTE I = I + 1.
