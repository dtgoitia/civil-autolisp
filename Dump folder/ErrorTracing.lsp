; (defun c:xx ( / *error* )
;   ; Trigger
;   (defun *error* ( errorMessage ) (princ (strcat "\n-------- ERROR: " errorMessage " --------\n")) (vl-bt))
;   (DT:AutoLoadFileFromCivilTemp "ErrorTracing.lsp")
;   (DT:ReportError '(kk pointList testString))
;
;   ; v0.0 - 2017.08.22 - First issue
;   ; Author: David Torralba
;   ; Last revision: 2017.08.22
; )
(defun c:errorProneFunction ( / testFunction testFunctionReturn )
  (defun testFunction ( arg1 )
    (float arg1)
  )
  (princ "\n-------- ERROR DOWN HERE --------")
  (itoa 1)

  ; (setq testFunctionReturn (testFunction "1"))
  (setq testFunctionReturn (vl-catch-all-apply 'testFunction (list "1")))
  (DT:PrintVar 'testFunctionReturn)
  (princ (strcat "\nERROR message:\n" (vl-catch-all-error-message testFunctionReturn) "\n"))
  nil

  ; Wrong function to trigger error:
  ; (+ 12 "asd")
)
(defun c:GetLastError ()
  ; Command version of DT:GetLastError
  (DT:GetLastError)

  ; v0.0 - 2017.08.22 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.22
)
(defun DT:GetLastError ()
  ; Return error message of last happened error
  (if (not errorCodesReference) (DT:AutoLoadFileFromCivilTemp "ErrorCodesReference.lsp"))
  (cdr (assoc (getvar "errno") errorCodesReference))

  ; v0.0 - 2017.08.22 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.22
)
(defun DT:ReportError ( globalVariableList / newFilePath newFileHandler )
  ; Create a report file with the global variables passed
  (if (DT:Arg 'DT:ReportError '((globalVariableList 'list)))
    (progn
      ; Report path: C:/Users/%username%/Desktop/errorReport.txt
      (setq newFilePath (strcat "C:/Users/" (getenv "username") "/Desktop/dtgoitiaAcadErrorReport.txt"))
      (princ (strcat "\nError report: " newFilePath))
      (if (setq newFileHandler (open newFilePath "w"))
        (progn
          (write-line "-----------------------------\n-----------------------------\n\n    ERROR REPORT\n\n-----------------------------\n-----------------------------\n" newFileHandler)

          (foreach globalVariable globalVariableList
            (write-line
              (strcat
                (vl-prin1-to-string globalVariable) ":\n"         ; write variable identifier
                (vl-prin1-to-string (eval globalVariable)) "\n"   ; write variable value
              );END strcat
              newFileHandler
            );END write-line
          );END foreach
          (close newFileHandler)
        );END progn
      );END if
    );END progn
  );END if
  (princ)

  ; v0.0 - 2017.08.23 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.23
)
(defun c:1 ()
  (repeat 5
    (setq
      pointList
      (append
        pointList
        (list (getpoint) )
      );END append
    );END setq
  );END repeat
)
