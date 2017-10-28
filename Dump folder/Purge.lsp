(defun c:xx ()
  ; Clean data links
  (dictremove (namedobjdict) "ACAD_DATALINK")
  (alert
" ----- IMPORTANT -----

Data links removed. In order to see effects, please:

1. Save,
2. Close and
3. Reopen the drawing

Thanks! :D
")
  (princ)

  ; v0.0 - 2017.06.26 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.06.26
)
