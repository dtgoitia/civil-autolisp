; DO NOT REMOVE THIS LINE. It's a checking.
(defun C:RT ()
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
    p1 (getpoint "\nSelect point 1: ")
    p2 (getpoint "\nSelect point 2: ")
    ang (angle p1 p2)
    i 0
  )

  (while (< i (sslength ss))
    (setq
      ent (ssname ss i)
      entList (entget ent)
      entList (subst (cons 50 ang)(assoc 50 entList) entList)
    )
    (entmod entList)
    (setq i (+ i 1))
  ); END while

  ; RESET to original "osmode" and "cmdecho".
  (setvar "osmode" oldosmode)
  (setvar "cmdecho" oldcmdecho)
  (princ)

  ; v0.0 - 2016.03.15 - First issue
  ; Author: David Torralba
  ; Last revision: 2016.03.15
)
