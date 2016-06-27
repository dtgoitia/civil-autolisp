(defun c:int3d (/
                *error*
                oldosmode oldcmdecho oldclayer
                )
  (defun *error* ( msg )
    (if (not (member msg '("Function cancelled" "quit / exit abort")))
      (princ (strcat "\nError: " msg))
    )
    ; OPERATION - Delete auxiliary data, if any
    (if (/= set_line12 nil) (vla-delete (vlax-ename->vla-object set_line12)))
    (if (/= set_line13 nil) (vla-delete (vlax-ename->vla-object set_line13)))
    (if (/= set_line23 nil) (vla-delete (vlax-ename->vla-object set_line23)))
    (if (/= reference_circle1 nil) (vla-delete (vlax-ename->vla-object reference_circle1)))
    (if (/= reference_circle2 nil) (vla-delete (vlax-ename->vla-object reference_circle2)))
    (if (/= reference_circle3 nil) (vla-delete (vlax-ename->vla-object reference_circle3)))

    ; Restore previous settings
    (setvar "osmode" oldosmode)
    (setvar "cmdecho" oldcmdecho)
    (setvar "clayer" oldclayer)
    (princ)
  )

  ; SAVE SETTINGS
  (setq
    oldosmode (getvar "osmode")
    oldcmdecho (getvar "cmdecho")
    oldclayer (getvar "clayer")
  )

  ; OPERATION - Check if function library is loaded. If not, exit.
  (princ "\nLooking for function library... ")
  (if (and
        (= (eval DT:input_string_or_point) nil)
        (= (eval DT:clic_or_type_level) nil)
      );END and
    (progn
      (princ "required library not found.\nPlease, load function library and run command again.")
      (exit)
    )
    (princ "loaded.")
  )

  ; OPERATION - Turn off the system echo
  (setvar "cmdecho" 0)

  ; INPUT - Point 1
  (setq p1 (getpoint "\nSelect point A: "))

  ; OPERATION - Create auxiliary data and objects
  (setq
    real_radius 8
    correction_factor 0.001
    radius (* real_radius (* correction_factor (getvar "viewsize")))  ; Calculate circle size at curent zoom

    reference_circle1 ( _Reference_Circle p1 radius)
  )

  ;INPUT - Point 1 level
  (setq z1 (DT:clic_or_type_level))
  (princ "\nLevel A = ")(princ z1)(princ "m")

  ; INPUT - Point 2
  (setq p2 (getpoint "\nSelect point B: "))

  ; OPERATION - Create auxiliary data and objects
  (setq
    set_line12 ( _Set_Line p1 p2)
    reference_circle2 ( _Reference_Circle p2 radius)
  )

  ; INPUT - Point 2 level
  (setq z2 (DT:clic_or_type_level))
  (princ "\nLevel B = ")(princ z2)(princ "m")

  ; INPUT - Point 3
  (setq p3 (getpoint "\nSelect point C: "))

  ; OPERATION - Create auxiliary data and objects
  (setq
    set_line23 ( _Set_Line p2 p3)
    set_line13 ( _Set_Line p1 p3)
    reference_circle3 ( _Reference_Circle p3 radius)
  )

  ;INPUT - Point 3 level
  (setq z3 (DT:clic_or_type_level))
  (princ "\nLevel C = ")(princ z3)(princ "m")

  ; OPERATION - Print plane configuration:
  (princ "\n---")
  (princ "\nPoint") (princ "\tLevel")
  (princ "\nA")     (princ "\t")
  (princ "\nB")     (princ "\t")
  (princ "\nC")     (princ "\t")
  (princ
    (strcat
      "\n---"
      "\nMaster plane defined by"
      "\nA (" (rtos (car p1) 2 3) " " (rtos (cadr p1) 2 3) " " (rtos z1 2 3) ")"
      "\nB (" (rtos (car p2) 2 3) " " (rtos (cadr p2) 2 3) " " (rtos z2 2 3) ")"
      "\nC (" (rtos (car p3) 2 3) " " (rtos (cadr p3) 2 3) " " (rtos z3 2 3) ")"
    )
  )

  (setq p0 nil)
  (while (not p0)
    (setq
      p0 (getpoint "\nSelect point to calculate level: ")
      x0 (car p0)
      y0 (cadr p0)

      z (+
          z1
          (/
            (+
              (* (- (cadr p3) (cadr p1)) (- z2 z1) (- (car p0) (car p1)))
              (* (- (car p2) (car p1)) (- (cadr p0) (cadr p1)) (- z3 z1))
              (-
                0
                (+
                  (* (- (car p0) (car p1)) (- (cadr p2) (cadr p1)) (- z3 z1))
                  (* (- (car p3) (car p1)) (- (cadr p0) (cadr p1)) (- z2 z1))
                )
              )
            )
            (-
              (* (- (car p2) (car p1)) (- (cadr p3) (cadr p1)))
              (* (- (car p3) (car p1)) (- (cadr p2) (cadr p1)))
            )
          )
        )
    )
    (princ
      (strcat
        "z = " (rtos z 2 3) "m (" (rtos (car p0) 2 3) " " (rtos (cadr p0) 2 3) ")"
      )
    )

    ; OPERATION - Introduce point 4
    (setvar "osmode" 0)
    (command "._insert" "PI_DT" p0 "0.5" "0.5" "" (rtos z 2 3))

    (setq p0 nil)
  )

  ; OPERATION - Delete auxiliary data, if any
  (if (/= set_line12        nil) (vla-delete (vlax-ename->vla-object set_line12)))
  (if (/= set_line13        nil) (vla-delete (vlax-ename->vla-object set_line13)))
  (if (/= set_line23        nil) (vla-delete (vlax-ename->vla-object set_line23)))
  (if (/= reference_circle1 nil) (vla-delete (vlax-ename->vla-object reference_circle1)))
  (if (/= reference_circle2 nil) (vla-delete (vlax-ename->vla-object reference_circle2)))
  (if (/= reference_circle3 nil) (vla-delete (vlax-ename->vla-object reference_circle3)))

  ; Restore previous settings
  (setvar "osmode" oldosmode)
  (setvar "cmdecho" oldcmdecho)
  (setvar "clayer" oldclayer)
  (princ)
  ; v0.1 - 2015.05.25 - System variable management bug fixed.
  ; v0.0 - 2015.04.06 - First issue.
  ; Author: David Torralba
  ; Last revision: 2016.05.25
)
