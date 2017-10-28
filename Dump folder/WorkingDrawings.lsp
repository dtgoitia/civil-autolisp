(defun c:xx ()
  ; Trigger
  (defun *error* ( errorMessage ) (princ (strcat "n-------- ERROR: " errorMessage " --------n")) (vl-bt) (DT:ReportError))
  (DT:AutoLoadFileFromCivilTemp "ErrorTracing.lsp")
  (DT:AutoLoadFileFromCivilTemp "WorkingDrawings.lsp")
  (princ (strcat "\nTemp file loaded (" (DT:Now) ")\n"))(princ)
  (c:BlockWorkingDrawing)

  ; v0.0 - 2017.10.26 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.10.26
)
