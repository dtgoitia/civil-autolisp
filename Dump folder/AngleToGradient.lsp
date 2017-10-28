; (defun c:xx ()
;   ; Trigger
;   (defun *error* ( errorMessage ) (princ (strcat "n-------- ERROR: " errorMessage " --------n")) (vl-bt) (DT:ReportError))
;   (DT:AutoLoadFileFromCivilTemp "ErrorTracing.lsp")
;   (DT:AutoLoadFileFromCivilTemp "AngleToGradient.lsp")
;   (c:test)
;
;   ; v0.0 - 2017.09.28 - First issue
;   ; Author: David Torralba
;   ; Last revision: 2017.09.28
; )
(defun c:test ( / p1 p2 )
  ; Command version of DT:AngleToGradient
  (DT:AngleToGradient 45.0)

  ; v0.0 - 2017.09.28 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.09.28
)
(defun DT:AngleToGradient ( a / rad )
  ; Return the angle as a gradient (1:...)
  (if (DT:Arg 'DT:AngleToGradient '((a 'real)))
    (progn
      (setq rad (DT:DegToRad a))
      (/ 1 (DT:Tan rad))
    );END progn
  );END if


  ; v0.0 - 2017.09.28 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.09.28
)
(defun DT:Tan ( a )
  ; Tangent function
  (/ (sin a) (cos a) )

  ; v0.0 - 2017.09.28 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.09.28
)
