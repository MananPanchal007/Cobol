IDENTIFICATION DIVISION.
       PROGRAM-ID. FUN.
       AUTHOR. CLAUDE AI.
       DATE-WRITTEN. 2025-07-11.
       DATE-COMPILED.
       
       ENVIRONMENT DIVISION.
       
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLOYEE-FILE ASSIGN TO "EMPLOYEE.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT REPORT-FILE ASSIGN TO "REPORT.TXT"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TEMP-FILE ASSIGN TO "TEMP.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       
       FILE SECTION.
       FD  EMPLOYEE-FILE.
       01  EMPLOYEE-RECORD.
           05  EMP-ID              PIC 9(5).
           05  EMP-NAME            PIC X(30).
           05  EMP-DEPARTMENT      PIC X(20).
           05  EMP-SALARY          PIC 9(7)V99.
           05  EMP-HIRE-DATE       PIC 9(8).
           05  EMP-STATUS          PIC X(1).
           05  FILLER              PIC X(14).
       
       FD  REPORT-FILE.
       01  REPORT-RECORD           PIC X(132).
       
       FD  TEMP-FILE.
       01  TEMP-RECORD             PIC X(100).
       
       WORKING-STORAGE SECTION.
       
       01  WS-CONSTANTS.
           05  WS-PROGRAM-NAME     PIC X(20) VALUE "FUN COBOL PROGRAM".
           05  WS-VERSION          PIC X(10) VALUE "V1.0".
           05  WS-MAX-EMPLOYEES    PIC 9(4) VALUE 1000.
           05  WS-MIN-SALARY       PIC 9(7)V99 VALUE 25000.00.
           05  WS-MAX-SALARY       PIC 9(7)V99 VALUE 150000.00.
       
       01  WS-SWITCHES.
           05  WS-EOF-SWITCH       PIC X(1) VALUE 'N'.
               88  WS-EOF          VALUE 'Y'.
           05  WS-FIRST-RECORD     PIC X(1) VALUE 'Y'.
               88  WS-FIRST        VALUE 'Y'.
           05  WS-ERROR-SWITCH     PIC X(1) VALUE 'N'.
               88  WS-ERROR-FOUND  VALUE 'Y'.
           05  WS-VALID-SWITCH     PIC X(1) VALUE 'Y'.
               88  WS-VALID-DATA   VALUE 'Y'.
           05  WS-MENU-SWITCH      PIC X(1) VALUE 'Y'.
               88  WS-CONTINUE     VALUE 'Y'.
       
       01  WS-COUNTERS.
           05  WS-RECORD-COUNT     PIC 9(5) VALUE ZERO.
           05  WS-VALID-COUNT      PIC 9(5) VALUE ZERO.
           05  WS-ERROR-COUNT      PIC 9(5) VALUE ZERO.
           05  WS-TOTAL-COUNT      PIC 9(5) VALUE ZERO.
           05  WS-PAGE-COUNT       PIC 9(3) VALUE ZERO.
           05  WS-LINE-COUNT       PIC 9(3) VALUE ZERO.
           05  WS-DEPT-COUNT       PIC 9(3) VALUE ZERO.
       
       01  WS-ACCUMULATORS.
           05  WS-TOTAL-SALARY     PIC 9(10)V99 VALUE ZERO.
           05  WS-AVG-SALARY       PIC 9(7)V99 VALUE ZERO.
           05  WS-HIGH-SALARY      PIC 9(7)V99 VALUE ZERO.
           05  WS-LOW-SALARY       PIC 9(7)V99 VALUE 999999.99.
       
       01  WS-WORK-AREAS.
           05  WS-CURRENT-DATE     PIC 9(8).
           05  WS-CURRENT-TIME     PIC 9(6).
           05  WS-EDIT-DATE        PIC 99/99/9999.
           05  WS-EDIT-TIME        PIC 99:99:99.
           05  WS-EDIT-SALARY      PIC $ZZZ,ZZ9.99.
           05  WS-WORK-FIELD       PIC X(100).
           05  WS-TEMP-FIELD       PIC X(50).
           05  WS-CALCULATE-FIELD  PIC 9(10)V99.
       
       01  WS-MENU-CHOICE          PIC X(1).
           88  WS-CHOICE-1         VALUE '1'.
           88  WS-CHOICE-2         VALUE '2'.
           88  WS-CHOICE-3         VALUE '3'.
           88  WS-CHOICE-4         VALUE '4'.
           88  WS-CHOICE-5         VALUE '5'.
           88  WS-CHOICE-EXIT      VALUE 'X'.
       
       01  WS-DEPARTMENT-TABLE.
           05  WS-DEPT-ENTRY OCCURS 10 TIMES.
               10  WS-DEPT-NAME    PIC X(20).
               10  WS-DEPT-TOTAL   PIC 9(10)V99.
               10  WS-DEPT-COUNT-EMP PIC 9(4).
               10  WS-DEPT-AVG     PIC 9(7)V99.
       
       01  WS-EMPLOYEE-ARRAY.
           05  WS-EMP-ENTRY OCCURS 100 TIMES.
               10  WS-EMP-ID-ARR   PIC 9(5).
               10  WS-EMP-NAME-ARR PIC X(30).
               10  WS-EMP-SAL-ARR  PIC 9(7)V99.
       
       01  WS-REPORT-HEADERS.
           05  WS-HEADER-1         PIC X(132) VALUE ALL '='.
           05  WS-HEADER-2.
               10  FILLER          PIC X(40) VALUE SPACES.
               10  FILLER          PIC X(52) VALUE
                   'EMPLOYEE MANAGEMENT SYSTEM REPORT'.
               10  FILLER          PIC X(40) VALUE SPACES.
           05  WS-HEADER-3         PIC X(132) VALUE ALL '='.
           05  WS-HEADER-4.
               10  FILLER          PIC X(5) VALUE 'PAGE:'.
               10  WS-PAGE-NO      PIC ZZ9.
               10  FILLER          PIC X(20) VALUE SPACES.
               10  FILLER          PIC X(5) VALUE 'DATE:'.
               10  WS-RPT-DATE     PIC 99/99/9999.
               10  FILLER          PIC X(20) VALUE SPACES.
               10  FILLER          PIC X(5) VALUE 'TIME:'.
               10  WS-RPT-TIME     PIC 99:99:99.
               10  FILLER          PIC X(61) VALUE SPACES.
       
       01  WS-COLUMN-HEADERS.
           05  WS-COL-HEADER-1.
               10  FILLER          PIC X(5) VALUE 'EMP'.
               10  FILLER          PIC X(3) VALUE SPACES.
               10  FILLER          PIC X(30) VALUE 'EMPLOYEE NAME'.
               10  FILLER          PIC X(3) VALUE SPACES.
               10  FILLER          PIC X(20) VALUE 'DEPARTMENT'.
               10  FILLER          PIC X(3) VALUE SPACES.
               10  FILLER          PIC X(12) VALUE 'SALARY'.
               10  FILLER          PIC X(3) VALUE SPACES.
               10  FILLER          PIC X(10) VALUE 'HIRE DATE'.
               10  FILLER          PIC X(3) VALUE SPACES.
               10  FILLER          PIC X(6) VALUE 'STATUS'.
               10  FILLER          PIC X(34) VALUE SPACES.
           05  WS-COL-HEADER-2.
               10  FILLER          PIC X(2) VALUE 'ID'.
               10  FILLER          PIC X(6) VALUE SPACES.
               10  FILLER          PIC X(30) VALUE ALL '-'.
               10  FILLER          PIC X(3) VALUE SPACES.
               10  FILLER          PIC X(20) VALUE ALL '-'.
               10  FILLER          PIC X(3) VALUE SPACES.
               10  FILLER          PIC X(12) VALUE ALL '-'.
               10  FILLER          PIC X(3) VALUE SPACES.
               10  FILLER          PIC X(10) VALUE ALL '-'.
               10  FILLER          PIC X(3) VALUE SPACES.
               10  FILLER          PIC X(6) VALUE ALL '-'.
               10  FILLER          PIC X(34) VALUE SPACES.
       
       01  WS-DETAIL-LINE.
           05  WS-DTL-EMP-ID       PIC Z(4)9.
           05  FILLER              PIC X(3) VALUE SPACES.
           05  WS-DTL-EMP-NAME     PIC X(30).
           05  FILLER              PIC X(3) VALUE SPACES.
           05  WS-DTL-DEPT         PIC X(20).
           05  FILLER              PIC X(3) VALUE SPACES.
           05  WS-DTL-SALARY       PIC $ZZZ,ZZ9.99.
           05  FILLER              PIC X(3) VALUE SPACES.
           05  WS-DTL-HIRE-DATE    PIC 99/99/9999.
           05  FILLER              PIC X(3) VALUE SPACES.
           05  WS-DTL-STATUS       PIC X(6).
           05  FILLER              PIC X(28) VALUE SPACES.
       
       01  WS-SUMMARY-LINES.
           05  WS-TOTAL-LINE.
               10  FILLER          PIC X(20) VALUE 'TOTAL EMPLOYEES: '.
               10  WS-TOTAL-EMP    PIC ZZZ,ZZ9.
               10  FILLER          PIC X(106) VALUE SPACES.
           05  WS-AVG-LINE.
               10  FILLER          PIC X(20) VALUE 'AVERAGE SALARY:  '.
               10  WS-AVG-SAL      PIC $ZZZ,ZZ9.99.
               10  FILLER          PIC X(101) VALUE SPACES.
           05  WS-HIGH-LINE.
               10  FILLER          PIC X(20) VALUE 'HIGHEST SALARY:  '.
               10  WS-HIGH-SAL     PIC $ZZZ,ZZ9.99.
               10  FILLER          PIC X(101) VALUE SPACES.
           05  WS-LOW-LINE.
               10  FILLER          PIC X(20) VALUE 'LOWEST SALARY:   '.
               10  WS-LOW-SAL      PIC $ZZZ,ZZ9.99.
               10  FILLER          PIC X(101) VALUE SPACES.
       
       01  WS-ERROR-MESSAGES.
           05  WS-ERR-MSG-1        PIC X(50) VALUE
               'ERROR: INVALID EMPLOYEE ID'.
           05  WS-ERR-MSG-2        PIC X(50) VALUE
               'ERROR: INVALID SALARY AMOUNT'.
           05  WS-ERR-MSG-3        PIC X(50) VALUE
               'ERROR: INVALID HIRE DATE'.
           05  WS-ERR-MSG-4        PIC X(50) VALUE
               'ERROR: INVALID STATUS CODE'.
           05  WS-ERR-MSG-5        PIC X(50) VALUE
               'ERROR: MISSING EMPLOYEE NAME'.
       
       01  WS-VALIDATION-FIELDS.
           05  WS-VAL-YEAR         PIC 9(4).
           05  WS-VAL-MONTH        PIC 9(2).
           05  WS-VAL-DAY          PIC 9(2).
           05  WS-CURRENT-YEAR     PIC 9(4).
           05  WS-DAYS-IN-MONTH    PIC 9(2).
       
       01  WS-MENU-DISPLAY.
           05  WS-MENU-TITLE       PIC X(50) VALUE
               'EMPLOYEE MANAGEMENT SYSTEM - MAIN MENU'.
           05  WS-MENU-LINE-1      PIC X(40) VALUE
               '1. PROCESS EMPLOYEE FILE'.
           05  WS-MENU-LINE-2      PIC X(40) VALUE
               '2. GENERATE DEPARTMENT REPORT'.
           05  WS-MENU-LINE-3      PIC X(40) VALUE
               '3. SALARY ANALYSIS'.
           05  WS-MENU-LINE-4      PIC X(40) VALUE
               '4. EMPLOYEE SEARCH'.
           05  WS-MENU-LINE-5      PIC X(40) VALUE
               '5. DATA VALIDATION'.
           05  WS-MENU-EXIT        PIC X(40) VALUE
               'X. EXIT PROGRAM'.
           05  WS-MENU-PROMPT      PIC X(40) VALUE
               'ENTER YOUR CHOICE: '.
       
       01  WS-SEARCH-CRITERIA.
           05  WS-SEARCH-ID        PIC 9(5).
           05  WS-SEARCH-NAME      PIC X(30).
           05  WS-SEARCH-DEPT      PIC X(20).
           05  WS-FOUND-FLAG       PIC X(1).
               88  WS-FOUND        VALUE 'Y'.
       
       01  WS-CALCULATION-WORK.
           05  WS-TEMP-CALC        PIC 9(10)V99.
           05  WS-PERCENTAGE       PIC 9(3)V99.
           05  WS-BONUS-CALC       PIC 9(7)V99.
           05  WS-TAX-CALC         PIC 9(7)V99.
           05  WS-NET-CALC         PIC 9(7)V99.
       
       01  WS-DATE-WORK.
           05  WS-YEARS-SERVICE    PIC 9(2).
           05  WS-MONTHS-SERVICE   PIC 9(2).
           05  WS-DAYS-SERVICE     PIC 9(4).
           05  WS-ANNIVERSARY      PIC X(1).
               88  WS-ANNIVERSARY-YR VALUE 'Y'.
       
       01  WS-SORT-WORK.
           05  WS-SORT-KEY         PIC X(35).
           05  WS-SORT-INDEX       PIC 9(3).
           05  WS-SORT-TEMP        PIC 9(3).
           05  WS-SWAP-FLAG        PIC X(1).
               88  WS-SWAP-MADE    VALUE 'Y'.
       
       01  WS-NUMERIC-WORK.
           05  WS-NUM-FIELD        PIC 9(10).
           05  WS-REMAINDER        PIC 9(10).
           05  WS-QUOTIENT         PIC 9(10).
           05  WS-RANDOM-SEED      PIC 9(8).
           05  WS-RANDOM-NUM       PIC 9(5).
       
       01  WS-STRING-WORK.
           05  WS-STRING-FIELD     PIC X(100).
           05  WS-STRING-LENGTH    PIC 9(3).
           05  WS-STRING-POS       PIC 9(3).
           05  WS-CHAR-COUNT       PIC 9(3).
           05  WS-WORD-COUNT       PIC 9(3).
       
       01  WS-FORMATTING.
           05  WS-SPACES           PIC X(80) VALUE ALL SPACES.
           05  WS-DASHES           PIC X(80) VALUE ALL '-'.
           05  WS-STARS            PIC X(80) VALUE ALL '*'.
           05  WS-EQUALS           PIC X(80) VALUE ALL '='.
       
       01  WS-STATUS-CODES.
           05  WS-ACTIVE           PIC X(1) VALUE 'A'.
           05  WS-INACTIVE         PIC X(1) VALUE 'I'.
           05  WS-TERMINATED       PIC X(1) VALUE 'T'.
           05  WS-RETIRED          PIC X(1) VALUE 'R'.
           05  WS-LEAVE            PIC X(1) VALUE 'L'.
       
       01  WS-DEPARTMENT-CODES.
           05  WS-DEPT-IT          PIC X(20) VALUE 'INFORMATION TECH'.
           05  WS-DEPT-HR          PIC X(20) VALUE 'HUMAN RESOURCES'.
           05  WS-DEPT-FIN         PIC X(20) VALUE 'FINANCE'.
           05  WS-DEPT-MKT         PIC X(20) VALUE 'MARKETING'.
           05  WS-DEPT-OPS         PIC X(20) VALUE 'OPERATIONS'.
           05  WS-DEPT-SALES       PIC X(20) VALUE 'SALES'.
           05  WS-DEPT-ADMIN       PIC X(20) VALUE 'ADMINISTRATION'.
           05  WS-DEPT-LEGAL       PIC X(20) VALUE 'LEGAL'.
           05  WS-DEPT-SECURITY    PIC X(20) VALUE 'SECURITY'.
           05  WS-DEPT-MAINT       PIC X(20) VALUE 'MAINTENANCE'.
       
       01  WS-SALARY-RANGES.
           05  WS-RANGE-1          PIC X(20) VALUE 'ENTRY LEVEL'.
           05  WS-RANGE-2          PIC X(20) VALUE 'JUNIOR'.
           05  WS-RANGE-3          PIC X(20) VALUE 'SENIOR'.
           05  WS-RANGE-4          PIC X(20) VALUE 'MANAGER'.
           05  WS-RANGE-5          PIC X(20) VALUE 'DIRECTOR'.
           05  WS-RANGE-6          PIC X(20) VALUE 'EXECUTIVE'.
       
       01  WS-MESSAGES.
           05  WS-MSG-WELCOME      PIC X(60) VALUE
               'WELCOME TO THE EMPLOYEE MANAGEMENT SYSTEM'.
           05  WS-MSG-PROCESSING   PIC X(60) VALUE
               'PROCESSING EMPLOYEE DATA...'.
           05  WS-MSG-COMPLETE     PIC X(60) VALUE
               'PROCESSING COMPLETED SUCCESSFULLY'.
           05  WS-MSG-ERROR        PIC X(60) VALUE
               'ERROR OCCURRED DURING PROCESSING'.
           05  WS-MSG-GOODBYE      PIC X(60) VALUE
               'THANK YOU FOR USING THE SYSTEM'.
       
       01  WS-STATISTICS.
           05  WS-STAT-PROCESSED   PIC 9(5).
           05  WS-STAT-ERRORS      PIC 9(5).
           05  WS-STAT-WARNINGS    PIC 9(5).
           05  WS-STAT-DUPLICATES  PIC 9(5).
           05  WS-STAT-UPDATES     PIC 9(5).
           05  WS-STAT-DELETIONS   PIC 9(5).
           05  WS-STAT-INSERTIONS  PIC 9(5).
       
       01  WS-PERFORMANCE.
           05  WS-START-TIME       PIC 9(8).
           05  WS-END-TIME         PIC 9(8).
           05  WS-ELAPSED-TIME     PIC 9(8).
           05  WS-RECORDS-PER-SEC  PIC 9(5).
       
       01  WS-FILE-STATUS.
           05  WS-EMP-STATUS       PIC X(2).
           05  WS-RPT-STATUS       PIC X(2).
           05  WS-TEMP-STATUS      PIC X(2).
       
       01  WS-BACKUP-AREA.
           05  WS-BACKUP-EMP       PIC X(100).
           05  WS-BACKUP-COUNT     PIC 9(5).
           05  WS-BACKUP-DATE      PIC 9(8).
       
       01  WS-CONFIG-SETTINGS.
           05  WS-PAGE-SIZE        PIC 9(2) VALUE 55.
           05  WS-LINE-SIZE        PIC 9(3) VALUE 132.
           05  WS-TAB-SIZE         PIC 9(1) VALUE 8.
           05  WS-DECIMAL-PLACES   PIC 9(1) VALUE 2.
       
       01  WS-UTILITY-FIELDS.
           05  WS-UTIL-COUNTER     PIC 9(5).
           05  WS-UTIL-INDEX       PIC 9(3).
           05  WS-UTIL-FLAG        PIC X(1).
           05  WS-UTIL-TEMP        PIC X(50).
           05  WS-UTIL-RESULT      PIC X(100).
       
       01  WS-FINAL-TOTALS.
           05  WS-GRAND-TOTAL      PIC 9(12)V99.
           05  WS-FINAL-COUNT      PIC 9(6).
           05  WS-FINAL-AVERAGE    PIC 9(8)V99.
       
       PROCEDURE DIVISION.
       
       0000-MAIN-PROGRAM.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-MAIN-PROCESS
           PERFORM 9000-TERMINATE
           STOP RUN.
       
       1000-INITIALIZE.
           DISPLAY WS-MSG-WELCOME
           ACCEPT WS-CURRENT-DATE FROM DATE
           ACCEPT WS-CURRENT-TIME FROM TIME
           MOVE WS-CURRENT-DATE TO WS-EDIT-DATE
           MOVE WS-CURRENT-TIME TO WS-EDIT-TIME
           MOVE ZERO TO WS-RECORD-COUNT
           MOVE ZERO TO WS-ERROR-COUNT
           MOVE ZERO TO WS-TOTAL-SALARY
           MOVE 'N' TO WS-EOF-SWITCH
           MOVE 'Y' TO WS-FIRST-RECORD
           PERFORM 1100-INITIALIZE-TABLES
           PERFORM 1200-INITIALIZE-COUNTERS
           PERFORM 1300-SETUP-REPORT-HEADERS.
       
       1100-INITIALIZE-TABLES.
           PERFORM VARYING WS-UTIL-INDEX FROM 1 BY 1
               UNTIL WS-UTIL-INDEX > 10
               MOVE SPACES TO WS-DEPT-NAME(WS-UTIL-INDEX)
               MOVE ZERO TO WS-DEPT-TOTAL(WS-UTIL-INDEX)
               MOVE ZERO TO WS-DEPT-COUNT-EMP(WS-UTIL-INDEX)
               MOVE ZERO TO WS-DEPT-AVG(WS-UTIL-INDEX)
           END-PERFORM
           PERFORM VARYING WS-UTIL-INDEX FROM 1 BY 1
               UNTIL WS-UTIL-INDEX > 100
               MOVE ZERO TO WS-EMP-ID-ARR(WS-UTIL-INDEX)
               MOVE SPACES TO WS-EMP-NAME-ARR(WS-UTIL-INDEX)
               MOVE ZERO TO WS-EMP-SAL-ARR(WS-UTIL-INDEX)
           END-PERFORM.
       
       1200-INITIALIZE-COUNTERS.
           MOVE ZERO TO WS-VALID-COUNT
           MOVE ZERO TO WS-TOTAL-COUNT
           MOVE ZERO TO WS-PAGE-COUNT
           MOVE ZERO TO WS-LINE-COUNT
           MOVE ZERO TO WS-DEPT-COUNT
           MOVE ZERO TO WS-STAT-PROCESSED
           MOVE ZERO TO WS-STAT-ERRORS
           MOVE ZERO TO WS-STAT-WARNINGS
           MOVE ZERO TO WS-STAT-DUPLICATES.
       
       1300-SETUP-REPORT-HEADERS.
           MOVE WS-CURRENT-DATE TO WS-RPT-DATE
           MOVE WS-CURRENT-TIME TO WS-RPT-TIME
           ADD 1 TO WS-PAGE-COUNT
           MOVE WS-PAGE-COUNT TO WS-PAGE-NO.
       
       2000-MAIN-PROCESS.
           PERFORM 2100-DISPLAY-MENU
           PERFORM 2200-PROCESS-CHOICE
           PERFORM UNTIL NOT WS-CONTINUE
               PERFORM 2100-DISPLAY-MENU
               PERFORM 2200-PROCESS-CHOICE
           END-PERFORM.
       
       2100-DISPLAY-MENU.
           DISPLAY ' '
           DISPLAY WS-STARS(1:50)
           DISPLAY WS-MENU-TITLE
           DISPLAY WS-STARS(1:50)
           DISPLAY WS-MENU-LINE-1
           DISPLAY WS-MENU-LINE-2
           DISPLAY WS-MENU-LINE-3
           DISPLAY WS-MENU-LINE-4
           DISPLAY WS-MENU-LINE-5
           DISPLAY WS-MENU-EXIT
           DISPLAY WS-STARS(1:50)
           DISPLAY WS-MENU-PROMPT WITH NO ADVANCING
           ACCEPT WS-MENU-CHOICE.
       
       2200-PROCESS-CHOICE.
           EVALUATE WS-MENU-CHOICE
               WHEN '1'
                   PERFORM 3000-PROCESS-EMPLOYEE-FILE
               WHEN '2'
                   PERFORM 4000-DEPARTMENT-REPORT
               WHEN '3'
                   PERFORM 5000-SALARY-ANALYSIS
               WHEN '4'
                   PERFORM 6000-EMPLOYEE-SEARCH
               WHEN '5'
                   PERFORM 7000-DATA-VALIDATION
               WHEN 'X' OR 'x'
                   MOVE 'N' TO WS-MENU-SWITCH
               WHEN OTHER
                   DISPLAY 'INVALID CHOICE. PLEASE TRY AGAIN.'
           END-EVALUATE.
       
       3000-PROCESS-EMPLOYEE-FILE.
           DISPLAY WS-MSG-PROCESSING
           PERFORM 3100-OPEN-FILES
           PERFORM 3200-READ-EMPLOYEE-FILE
           PERFORM 3300-PROCESS-RECORDS
           PERFORM 3400-CLOSE-FILES
           PERFORM 3500-DISPLAY-STATISTICS
           DISPLAY WS-MSG-COMPLETE.
       
       3100-OPEN-FILES.
           OPEN INPUT EMPLOYEE-FILE
           OPEN OUTPUT REPORT-FILE.
       
       3200-READ-EMPLOYEE-FILE.
           READ EMPLOYEE-FILE
               AT END
                   MOVE 'Y' TO WS-EOF-SWITCH
               NOT AT END
                   ADD 1 TO WS-RECORD-COUNT
           END-READ.
       
       3300-PROCESS-RECORDS.
           PERFORM UNTIL WS-EOF
               PERFORM 3310-VALIDATE-RECORD
               IF WS-VALID-DATA
                   PERFORM 3320-PROCESS-VALID-RECORD
               ELSE
                   PERFORM 3330-PROCESS-ERROR-RECORD
               END-IF
               PERFORM 3200-READ-EMPLOYEE-FILE
           END-PERFORM.
       
       3310-VALIDATE-RECORD.
           MOVE 'Y' TO WS-VALID-SWITCH
           IF EMP-ID = ZERO OR EMP-ID > 99999
               MOVE 'N' TO WS-VALID-SWITCH
           END-IF
           IF EMP-NAME = SPACES
               MOVE 'N' TO WS-VALID-SWITCH
           END-IF
           IF EMP-SALARY < WS-MIN-SALARY OR EMP-SALARY > WS-MAX-SALARY
               MOVE 'N' TO WS-VALID-SWITCH
           END-IF
           IF EMP-STATUS NOT = 'A' AND EMP-STATUS NOT = 'I'
               AND EMP-STATUS NOT = 'T' AND EMP-STATUS NOT = 'R'
               MOVE 'N' TO WS-VALID-SWITCH
           END-IF.
       
       3320-PROCESS-VALID-RECORD.
           ADD 1 TO WS-VALID-COUNT
           ADD EMP-SALARY TO WS-TOTAL-SALARY
           PERFORM 3321-UPDATE-SALARY-RANGES
           PERFORM 3322-UPDATE-DEPARTMENT-TOTALS
           PERFORM 3323-WRITE-DETAIL-LINE.
       
       3321-UPDATE-SALARY-RANGES.
           IF EMP-SALARY > WS-HIGH-SALARY
               MOVE EMP-SALARY TO WS-HIGH-SALARY
           END-IF
           IF EMP-SALARY < WS-LOW-SALARY
               MOVE EMP-SALARY TO WS-LOW-SALARY
           END-IF.
       
       3322-UPDATE-DEPARTMENT-TOTALS.
           PERFORM VARYING WS-UTIL-INDEX FROM 1 BY 1
               UNTIL WS-UTIL-INDEX > 10
               IF WS-DEPT-NAME(WS-UTIL-INDEX) = EMP-DEPARTMENT
                   ADD EMP-SALARY TO WS-DEPT-TOTAL(WS-UTIL-INDEX)
                   ADD 1 TO WS-DEPT-COUNT-EMP(WS-UTIL-INDEX)
                   EXIT PERFORM
               END-IF
               IF WS-DEPT-NAME(WS-UTIL-INDEX) = SPACES
                   MOVE EMP-DEPARTMENT TO WS-DEPT-NAME(WS-UTIL-INDEX)
                   MOVE EMP-SALARY TO WS-DEPT-TOTAL(WS-UTIL-INDEX)
                   MOVE 1 TO WS-DEPT-COUNT-EMP(WS-UTIL-INDEX)
                   EXIT PERFORM
               END-IF
           END-PERFORM.
       
       3323-WRITE-DETAIL-LINE.
           MOVE EMP-ID TO WS-DTL-EMP-ID
           MOVE EMP-NAME TO WS-DTL-EMP-NAME
           MOVE EMP-DEPARTMENT TO WS-DTL-DEPT
           MOVE EMP-SALARY TO WS-DTL-SALARY
           MOVE EMP-HIRE-DATE TO WS-DTL-HIRE-DATE
           EVALUATE EMP-STATUS
               WHEN 'A'
                   MOVE 'ACTIVE' TO WS-DTL-STATUS
               WHEN '
