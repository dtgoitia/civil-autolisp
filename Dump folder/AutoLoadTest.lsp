(defun c:xx ()
  ; Trigger
  (DT:AutoLoadFileFromCivilTemp "AutoLoadTest.lsp")
  (c:1)

  ; v0.0 - 2017.07.25 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.07.25
)
(defun c:1 () (princ "\nI'm alive!")(princ))
