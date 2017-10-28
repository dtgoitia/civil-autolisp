(defun c:xx ()
  ; Trigger
  (defun *error* ( errorMessage ) (princ (strcat "n-------- ERROR: " errorMessage " --------n")) (vl-bt) (DT:ReportError))
  (DT:AutoLoadFileFromCivilTemp "ErrorTracing.lsp")
  (DT:AutoLoadFileFromCivilTemp "KtfBreaklinesReport.lsp")
  (c:KtfBreaklinesReport)

  ; v0.0 - 2017.09.20 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.09.20
)
(defun c:KtfBreaklinesReport ( / filePath fileHandle line tmp i x y breaklineCoordinates )
  ; Command version of DT:KtfBreaklinesReport
  (setq filePath (strcat "C:\\Users\\" (getenv "username") "\\Desktop\\kk.txt"))
  (if (setq fileHandle (open filePath "r"))
    (progn
      (while (setq line (read-line fileHandle))
        (setq tmp (substr line 3))
        (setq i (vl-string-search "  " tmp))
        (setq x (atof (substr tmp 1 i)))
        (setq tmp (substr tmp (+ i 3)))
        (setq i (vl-string-search "  " tmp))
        (setq y (atof (substr tmp 1 i)))
        (setq breaklineCoordinates (append breaklineCoordinates (list (list x y))))
      )
      (if breaklineCoordinates
        (mapcar
          '(lambda (p) (entmakex (list (cons 0 "CIRCLE") (cons 10 p) (cons 40 2))) )
          breaklineCoordinates
        );END mapcar
      );END if
      (close fileHandle)
    );END progn
    (DT:Error 'c:KtfBreaklinesReport (strcat "impossible to open " filePath " file"))
  );END if

  ; v0.0 - 2017.09.20 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.09.20
)
