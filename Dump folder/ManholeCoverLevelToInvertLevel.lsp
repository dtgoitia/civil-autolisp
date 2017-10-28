(defun c:xx ()
  ; Trigger
  (DT:AutoLoadFileFromCivilTemp "ManholeCoverLevelToInvertLevel.lsp")
  (c:ManholeCoverLevelToInvertLevel)

  ; v0.0 - 2017.08.15 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.15
)
(defun c:ManholeCoverLevelToInvertLevel ( / successfullyUpdated notUpdate ss totalManholes )
  ; Command version of DT:ManholeCoverLevelToInvertLevel
  (setq successfullyUpdated 0)
  (setq notUpdate 0)
  (if (setq ss (ssget '(( 0 . "INSERT" ))))
    (foreach a (ssnamex ss)
      (if (= 'ename (type (cadr a)))
        (if (DT:ManholeCoverLevelToInvertLevel (cadr a))
          (setq successfullyUpdated (1+ successfullyUpdated))
          (setq notUpdate (1+ successfullyUpdated))
        );END if
      );END if
    );END foreach
  );END if

  ; Print message on screen: number updated
  (if (setq totalManholes (+ successfullyUpdated notUpdate))
    (princ
      (strcat
        "\n" (itoa successfullyUpdated) " manholes out of " (itoa totalManholes) " successfully updated"
        (if (= 0 notUpdate) "" (strcat "\n" (itoa notUpdate) " manholes out of " (itoa totalManholes) " not updated\n") )
      );END strcat
    );END princ
  );END if
  (princ)

  ; v0.0 - 2017.08.15 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.15
)
(defun DT:ManholeCoverLevelToInvertLevel ( ename )
  ; Set IL on manhole (ename) doing CL-1.200m
  ; WARNING! pipe to be substracted!
  (if (DT:Arg 'DT:ManholeCoverLevelToInvertLevel '((ename 'ename)))
    (progn
      (setq object (vlax-ename->vla-object ename))
      (if (setq stringCL (LM:vl-getattributevalue object "CL"))
        (progn
          (setq realCL (atof stringCL))
          (setq realIL (- realCL 1.200))
          (if (setq stringIL (LM:rtos realIL 2 3))
            (if (= stringIL (LM:vl-setattributevalue object "IL1" stringIL))
              T   ; successfully updated IL1 value
              nil ; IL1 value not updated
            );END if
          );END if
        );END progn
        (DT:Error 'DT:ManholeCoverLevelToInvertLevel "no CL found")
      );END if
    );END progn
  );END if

  ; v0.0 - 2017.08.15 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.15
)
