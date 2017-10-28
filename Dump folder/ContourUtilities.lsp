(defun c:xx ()
  ; Trigger
  (defun *error* ( errorMessage ) (princ (strcat "n-------- ERROR: " errorMessage " --------n")) (vl-bt) (DT:ReportError))
  (DT:AutoLoadFileFromCivilTemp "ErrorTracing.lsp")
  (DT:AutoLoadFileFromCivilTemp "ContourUtilities.lsp")
  (c:entryFunction)

  ; v0.0 - 2017.09.29 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.09.29
)
(defun c:entryFunction ()
  (if (setq ss (ssget '((-4 . "<OR")(0 . "TEXT")(0 . "MTEXT")(-4 . "OR>"))))
    (foreach a (ssnamex ss)
      (if (= 'ename (type (cadr a)))
        (DT:TextToReadableAngle (cadr a))
      );END if
    );END foreach
  );END if

  ; v0.0 - 2017.09.29 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.09.29
)
(defun DT:TextToReadableAngle ( ename / object )
  ; Update text (ename) rotation to be a readable angle
  (if (DT:Arg 'DT:TextToReadableAngle '((ename 'ename)))
    (progn
      (setq object (vlax-ename->vla-object ename))
      (vlax-put-property object 'Rotation (DT:ReadableTextAngle (vlax-get-property object 'Rotation)))
    );END progn
  );END if

  ; v0.0 - 2017.09.29 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.09.29
)
