(defun c:xx ()
  ; Trigger
  (defun *error* ( errorMessage ) (princ (strcat "n-------- ERROR: " errorMessage " --------n")) (vl-bt) (DT:ReportError))
  (DT:AutoLoadFileFromCivilTemp "ErrorTracing.lsp")
  (DT:AutoLoadFileFromCivilTemp "DateTime.lsp")
  (princ (strcat "\nTemp file loaded (" (DT:Now) ")\n"))(princ)
  (c:dateTimeTest)

  ; v0.0 - 2017.08.23 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.23
)
; (defun c:dateTimeTest ( / testCases testResults i )
;   ; test function for date time
;   (setq testCases
;     (list
;       '("2017.08.23" "YYYY.MM.DD")
;       '("2017.08.23" "YYYY.MM.DD")
;       '("2017.08.23" 1)
;       '("2017.08.23" "YYYY.MM.DD")
;     );END list
;   );END setq
;
;   (setq testResults
;     (mapcar
;       '(lambda (testCase)
;         (list
;           testCase
;           (DT:ParseDateTime (nth 0 testCase) (nth 1 testCase))
;         );END if
;       );END lambda
;       testCases
;     );END mapcar
;   );END setq
;   (setq i 0)
;   (foreach testResult testResults
;     (setq i (1+ i))
;     (princ "\nTest ")(princ i)(princ ": ")
;     (princ (nth 1 testResult))
;     (if (nth 1 testResult)(princ "    ")(princ "  "))
;     (princ (nth 0 testResult))
;     (princ "\n")
;   );END foreach
;   (princ)
;
;   ; v0.0 - 2017.08.23 - First issue
;   ; Author: David Torralba
;   ; Last revision: 2017.08.23
; )
(defun c:dateTimeTest ( / testCases testResults i )
  ; test function for date time
  (setq testCases
    (list
      ; "YYYY"
      ; "YY"
      ; "MMMM"
      ; "MMM"
      ; "MM"
      ; "M"
      ; "DD"
      ; "D"
      ; "hh"
      ; "h"
      ; "mm"
      ; "m"
      ; "SS"
      ; "S"
      ; "ss"
      ; "s"
      ; "YYYY.MM.DD"
      "YYYY.MM.DD hh:mm:SS.ss"
    );END list
  );END setq

  (setq testResults
    (mapcar
      '(lambda (testCase) (princ testCase)(princ "\n")(list testCase (DT:ParseDatePattern testCase)) )
      testCases
    );END mapcar
  );END setq

  (setq i 0)
  (foreach testResult testResults
    (setq i (1+ i))
    (princ "\nTest ")(princ i)(princ ": ")
    (princ (nth 1 testResult))
    (if (nth 1 testResult)(princ "    ")(princ "  "))
    (princ (nth 0 testResult))
    (princ "\n")
  );END foreach
  (princ)

  ; v0.0 - 2017.08.23 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.23
)
(defun DT:DateTime ( in )
  ; Parte input
  (if (DT:Arg 'DT:DateTime '((in 'str)))
    (progn
      (DT:PrintVar 'in)
      (cond
        ( (and ; yyyy/mm/dd
            (= 10 (strlen in))
            (= "/" (substr in 5 1) )
            (= "/" (substr in 8 1) )
          );END and
          T
        );END subcond
        ( (and ; yyyy/mm/dd
            ; (= 10 (strlen in))
            (= "." (substr in 5 1) )
            (= "." (substr in 8 1) )
          );END and
          T
        );END subcond
        (t
          (DT:Error 'DT:DateTime "\"in\" no understood mate...")
        );END subcond
      );END cond
    );END progn
  );END if

  ; v0.0 - 2017.08.23 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.23
)
(defun DT:ParseDatePattern ( pattern / remainingPattern result return currentCharPosition )
  (if (and pattern (= 'str (type pattern)) )
    (progn
      (setq remainingPattern pattern)
      (setq currentCharPosition 1) ; 1-index based
      (while (> (strlen remainingPattern) 0)
        (setq result (DT:GetFirstLeftDatePattern remainingPattern))
        (setq remainingPattern (substr remainingPattern (+ 1 (nth 1 result) (nth 2 result))))
        (setq return (append return (list (list
          (nth 0 result) (+ currentCharPosition (nth 1 result)) (nth 2 result)
        ))))
        (setq currentCharPosition (+ currentCharPosition (nth 1 result) (nth 2 result)))
      );END while
      return
    );END progn
  );END if

  ; v0.0 - 2017.10.27 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.10.27
)
(defun DT:StringSearch ( pattern string / result )
  ; Return the pattern position in the string and the length of the pattern,
  ; if pattern is found. Otherwise, return nil
  ; Example: (DT:StringSearch "YYYY" "012YYYY") --> ("YYYY" 3 4)
  ;  - "YYYY": pattern found
  ;  - 3: index (0 based) where the pattern started
  ;  - 4: pattern length
  (if (and pattern string (= 'str (type pattern)) (= 'str (type string)))
    (if (setq result (vl-string-search pattern string))
      (list pattern result (strlen pattern))
      nil
    );END if
  );END if

  ; v0.0 - 2017.10.27 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.10.27
)
(defun DT:GetFirstLeftDatePattern ( string / patternList i result )
  ; Return a list with:
  ;  - Pattern found
  ;  - The position of the first pattern found in string (0-index based)
  ;  - Length of the pattern
  ; Example: (DT:GetFirstLeftDatePattern "012345DD") --> ("DD" 6 2)
  (if (and string (= 'str (type string)))
    (progn
      (setq patternList (list "YYYY" "YY" "MMMM" "MMM" "MM" "M" "DD" "D" "hh" "h" "mm" "m" "SS" "S" "ss" "s"))
      (setq i 0)
      (while (and (not result) (<= i (length patternList)))
        (setq result (DT:StringSearch (nth i patternList) string))
        (setq i (1+ i))
      );END while
      result
    );END progn
  );END if

  ; v0.0 - 2017.10.27 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.10.27
)
(defun DT:GetDate ( pattern / now logic )
  ; Return current time as per passed pattern
  ; Years:
  ;   YYYY - Full year (2017)
  ;   YY   - Year last 2 digits (17)
  ; Months:
  ;   MMMM - Month as text (August)
  ;   MMM  - Month as text short (Aug)
  ;   MM   - Month as number. 2 digits (08)
  ;   M    - Month as number. 1 digit if possible (8)
  ; Days:
  ;   DDDD - Day of the week (Wednesday)
  ;   DDD  - Day of the week short (Wed)
  ;   DD   - Day of the month. 2 digits (06)
  ;   D    - Day of the month. 1 digit if possible (6)
  ; Hours:
  ;   hh   - Hours (24h). 2 digits (07)
  ;   h    - Hours (24h). 1 digit if possible (7)
  ; Minutes:
  ;   mm   - Minutes. 2 digits (04)
  ;   m    - Minutes. 1 digit if possible (4)
  ; Seconds:
  ;   SS   - Seconds. 2 digits (03)
  ;   S    - Seconds. 1 digit if possible (3)
  ; Miliseconds:
  ;   ss   - Miliseconds. 2 digits (03)
  ;   s    - Miliseconds. 1 digit if possible (3)
  (if (and pattern (= 'str (type pattern)))
    (progn
      (setq now (LM:rtos (getvar 'CDATE) 2 6))
      (setq logic (DT:ParseDatePattern pattern))
    );END progn
  );END if


  ; v0.0 - 2017.10.27 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.10.27
)
