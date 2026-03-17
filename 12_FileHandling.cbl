       IDENTIFICATION DIVISION.
       PROGRAM-ID. 30_FileHandling.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT EMP-FILE ASSIGN TO 'employee.dat'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD EMP-FILE.
       01 EMP-RECORD.
          05 EMP-ID     PIC 9(3).
          05 EMP-NAME   PIC X(20).

       WORKING-STORAGE SECTION.
       01 WS-EOF        PIC X VALUE 'N'.
       01 WS-CHOICE     PIC X.

       PROCEDURE DIVISION.

       * -------------------------------
       * Ask user: Write or Read
       * -------------------------------
           DISPLAY "Enter W to WRITE, R to READ: "
           ACCEPT WS-CHOICE

           IF WS-CHOICE = 'W'
               PERFORM WRITE-FILE
           ELSE
               PERFORM READ-FILE
           END-IF

           STOP RUN.

       * -------------------------------
       * WRITE to file
       * -------------------------------
       WRITE-FILE.

           OPEN OUTPUT EMP-FILE

           MOVE 101 TO EMP-ID
           MOVE "MANAN PANCHAL" TO EMP-NAME
           WRITE EMP-RECORD

           MOVE 102 TO EMP-ID
           MOVE "JOHN DOE" TO EMP-NAME
           WRITE EMP-RECORD

           MOVE 103 TO EMP-ID
           MOVE "ALICE SMITH" TO EMP-NAME
           WRITE EMP-RECORD

           CLOSE EMP-FILE

           DISPLAY "Data written to file."

           EXIT.

       * -------------------------------
       * READ from file
       * -------------------------------
       READ-FILE.

           OPEN INPUT EMP-FILE

           PERFORM UNTIL WS-EOF = 'Y'

               READ EMP-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       DISPLAY "ID: " EMP-ID
                       DISPLAY "NAME: " EMP-NAME
               END-READ

           END-PERFORM

           CLOSE EMP-FILE

           EXIT.
