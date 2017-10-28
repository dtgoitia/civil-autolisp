; (defun c:xx ()
;   ; Trigger
;   (defun *error* ( errorMessage ) (princ (strcat "n-------- ERROR: " errorMessage " --------n")) (vl-bt) (DT:ReportError))
;   (DT:AutoLoadFileFromCivilTemp "ErrorTracing.lsp")
;   (DT:AutoLoadFileFromCivilTemp "GridLabels.lsp")
;   (c:entryFunction)
;
;   ; v0.0 - 2017.09.18 - First issue
;   ; Author: David Torralba
;   ; Last revision: 2017.09.18
; )
(defun c:entryFunction ( / coordType coordValue newValue ename )
  (if (not lastValue) (setq lastValue (DT:GetText (car (entsel "\nSelect label to extract text: "))) ))

  (setq coordType  (substr lastValue 1 1))
  (setq coordValue (substr lastValue 2  ))
  (setq newValue (strcat coordType (itoa (+ (atoi coordValue) 100))))
  (if (setq ename (car (entsel (strcat "\nSelect label to update to \"" newValue "\""))))
    (progn
      (DT:SetText ename newValue)
      (vlax-put-property (vlax-ename->vla-object ename) 'Color 7)
      (setq lastValue newValue)
    );END progn
  );END if
  (DT:PrintVar 'newValue)

  ; v0.0 - 2017.09.18 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.09.18
)
