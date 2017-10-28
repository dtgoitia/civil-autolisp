(defun c:xx ()
  ; Trigger
  (defun *error* ( errorMessage ) (princ (strcat "n-------- ERROR: " errorMessage " --------n")) (vl-bt) (DT:ReportError))
  (DT:AutoLoadFileFromCivilTemp "ErrorTracing.lsp")
  (DT:AutoLoadFileFromCivilTemp "Testing.lsp")
  (c:Testing)

  ; v0.0 - 2017.08.24 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.24
)
(defun c:Testing ( / kk testList )
  ; Testing trigger
  (setq
    testList
    (list
      '(= (ar 2 4) 8)
      '(= (ar 2 3) 7)
      '(= (ar 2 3) 6)
      '(= (ar 2 3) 6)
      '(= (ar 3 3) 9)
      '(= (ar 2 3) 7)
      '(= (ar 2 4) 8)
      '(= (ar 2 30) 60)
    );END list
    kk testList
  );END setq

  (repeat 1000
    (setq kk (append kk testList))
  );END repeat

  (DT:ReportTest (DT:TestList kk) 2)

  ; v0.0 - 2017.07.03 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.07.03
)
(defun ar (x y)
  (* x y)
)
(defun DT:Test ( expression )
  ; Tests the expression passed
  (if expression
    (if (eval expression)
      (list expression T)
      (list expression nil)
    );END if
  );END if

  ; v0.0 - 2017.07.03 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.07.03
)
(defun DT:TestList ( testList / testReport )
  ; Loop through all test cases
  (if (DT:Arg 'DT:TestList '((testList 'list)))
    (foreach test testList
      (setq testReport (append testReport (list (DT:Test test))))
    );END foreach
  );END if

  ; v0.0 - 2017.07.03 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.07.03
)
(defun DT:ReportTest ( testReportList mode / return iPassed iFailed testAmount )
  ; Report test
  ; testReportList [list] = ((test testResult), report2, report3, ..., reportN)
  ; mode:
  ;  - 0 = report all
  ;  - 1 = report only failed tests
  ;  - 2 = report only summary

  (if (DT:Arg 'DT:ReportTest '((testReportList 'list)(mode 'int)))
    (progn
      (setq return "" iPassed 0 iFailed 0)
      (setq testAmount (length testReportList) )
      (cond
        ((= 0 mode)
          (foreach report testReportList
            (if (nth 1 report)
              (progn
                ; Test passed
                (setq return (strcat return "\n ok  ... " (vl-princ-to-string (nth 0 report)) ))
                (setq iPassed (+ iPassed 1))
              );END progn
              (progn
                ; Test failed
                (setq return (strcat return "\nFAIL ... " (vl-princ-to-string (nth 0 report)) ))
                (setq iFailed (+ iFailed 1))
              );END progn
            );END if
          );END foreach
        );END subcond
        ((= 1 mode)
          (foreach report testReportList
            (if (= nil (nth 1 report))
              ; Test passed
              (setq iPassed (+ iPassed 1))
              (progn
                ; Test failed
                (setq return (strcat return "\n"(vl-princ-to-string (nth 0 report)) ))
                (setq iFailed (+ iFailed 1))
              );END progn
            );END if
          );END foreach
        );END subcond
        ((= 2 mode)
          (foreach report testReportList
            (if (= nil (nth 1 report))
              (setq iPassed (+ iPassed 1)) ; Test passed
              (setq iFailed (+ iFailed 1)) ; Test failed
            );END if
          );END foreach
        );END subcond
      );END cond

      ; Print report
      (if (and return iPassed iFailed)
        (progn
          (princ (strcat return "\n>> " (itoa iPassed) " pass, " (itoa iFailed) " fail. Total number of tests: " (itoa testAmount) "\n") )
          (princ)
        );END progn
      );END if
    );END progn
  );END if

  ; v0.0 - 2017.07.03 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.07.03
)
