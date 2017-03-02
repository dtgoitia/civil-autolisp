; DO NOT REMOVE THIS LINE. It's a checking.
(defun c:RT ()
  (defun *error* ( msg )
    (if (not (member msg '("Function cancelled" "quit / exit abort")))
      (princ (strcat "\nError: " msg))
    )
    ; RESET to original "osmode" and "cmdecho".
    (setvar "osmode" oldosmode)
    (setvar "cmdecho" oldcmdecho)
    (princ)
  )

  ; SAVE "osmode" and "cmdecho"
  (setq oldosmode (getvar "osmode"))
  (setq oldcmdecho (getvar "cmdecho"))

  ; CHANGE "osmode" and "cmdecho"
  (setvar "osmode" 512)
  (setvar "cmdecho" 0)

  (setq
    ss (ssget)
    ang (angle (getpoint "\nSelect point 1: ") (getpoint "\nSelect point 2: "))
  )

  (foreach a (ssnamex ss)
    (if
      (and
        (if DT:ReadableTextAngle T nil)
        (or
          (= "TEXT" (cdr (assoc 0 (entget (cadr a))) ) )
          (= "MTEXT" (cdr (assoc 0 (entget (cadr a))) ) )
        );END or
      );END and
      (setq ang (DT:ReadableTextAngle ang ) )
    );END if
    (vlax-put-property (vlax-ename->vla-object (cadr a)) 'Rotation ang )
  );END foreach
  ; RESET to original "osmode" and "cmdecho".
  (setvar "osmode" oldosmode)
  (setvar "cmdecho" oldcmdecho)
  (princ)

  ; v0.1 - 2017.03.02 - Loop simplified.
  ;                   - Readability angle included for TEXT and MTEXTs
  ; v0.0 - 2016.03.15 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.03.02
)
