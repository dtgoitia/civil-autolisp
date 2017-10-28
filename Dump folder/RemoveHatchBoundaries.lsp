(defun c:xx ()
  ; Trigger
  (defun *error* ( errorMessage ) (princ (strcat "n-------- ERROR: " errorMessage " --------n")) (vl-bt) (DT:ReportError))
  (DT:AutoLoadFileFromCivilTemp "ErrorTracing.lsp")
  (DT:AutoLoadFileFromCivilTemp "RemoveHatchBoundaries.lsp")
  (c:RemoveHatchBoundaries)

  ; v0.0 - 2017.08.24 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.24
)
(defun c:RemoveHatchBoundaries ()
  ; Command version of DT:RemoveHatchBoundaries
  (DT:RemoveHatchBoundaries (car (entsel)))

  ; v0.0 - 2017.08.24 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.24
)
(defun DT:RemoveHatchBoundaries ( ename )
  ; DT:RemoveHatchBoundaries function description
  (if (DT:Arg 'DT:RemoveHatchBoundaries '((ename 'ename)))
    (progn
      (command
        "_.-hatchedit"
        ename
        "R"               ; Remove boundaries
      )
    );END progn
  );END if

  ; v0.0 - 2017.08.24 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.24
)
