(defun c:DIP (/
              reference_circle1 cursor_text_L1 cursor_text_L2 cursor_line
              p0 p1
              z0 z1
              oldlayer oldosmode oldcmdecho)
  ; Dynamic Interpolation

  ; SET - Error handling function
  (defun *error* ( msg )
    (if (not (member msg '("Function cancelled" "quit / exit abort")))
      (princ (strcat "\nError: " msg))
    )
    ; PRINC LAST INTERPOLATION DATA
    (if (and (/= z0 nil) (/= z1 nil) (/= txt_dist nil) (/= txt_slope nil))
      (princ (strcat "\nz0 = " (rtos z0 2 3) "   zf = " (rtos z1 2 3) "   -->   " txt_dist "m @ 1/" txt_slope ))
    ) ; END if

    ; CLEAN VARIABLES
    (if (/= reference_circle1 nil) (vla-delete (vlax-ename->vla-object reference_circle1)))
    (if (/= cursor_text_L1 nil) (vla-delete (vlax-ename->vla-object cursor_text_L1)))
    (if (/= cursor_text_L2 nil) (vla-delete (vlax-ename->vla-object cursor_text_L2)))
    (if (/= cursor_line nil) (vla-delete (vlax-ename->vla-object cursor_line)))

    ; RESTORE PREVIOUS SETTINGS
    (setvar "clayer" oldlayer)
    (setvar "osmode" oldosmode)
    (setvar "cmdecho" oldcmdecho)

    ; End without double messages
    (princ)
  )

  ; SAVE CURRENT SETTINGS - Current layer, OSMODE and CMDECHO
  (setq oldlayer (getvar "clayer")
        oldosmode (getvar "osmode")
        oldcmdecho (getvar "cmdecho")
  )

  ; OPERATION - Check if function library is loaded. If not, exit.
  (princ "\nLooking for function library... ")
  (if (and
        (= (eval DT:destripar_txt) nil)
        (= (eval DT:level_detection) nil)
        (= (evak DT:clic_or_type_level) nil)
      );END and
    (progn
      (princ "required library not found.\nPlease, load function library and run command again.")
      (exit)
    )
    (princ "loaded.")
  )

  ; CHANGE INITIAL SETTINGS - "osmode" and "cmdecho"
  (setvar "cmdecho" 0)

  ; INPUT - Ask starting level
  ;(setq z0 (DT:level_detection))
  (setq z0 (DT:clic_or_type_level))

  ; INPUT - Ask starting point
  (setq p0 (getpoint "\nIntroduce starting point: ") )

  ; OPERATION - Create auxiliary objects
  (setq
    real_radius 8
    correction_factor 0.001
    radius (* real_radius (* correction_factor (getvar "viewsize")))  ; Calculate circle size at current zoom

    reference_circle1 ( _Reference_Circle p0 radius)
  )

  ; OPERATION - Show selected level
  (princ (strcat " level = " (rtos z0 2 3) "m"))

  ; INPUT - Ask to fix slope or IL
  (initget 1 "Slope Level")
  (setq answer (getkword "\nChoose to fix [Slope/Level]: "))

  ; SET - Real text height
  (setq real_text_height 0.03)

  (cond
    ((= answer "Slope")
      ; ASK - Slope
      (setq slope (getreal "\nIntroduce slope = 1/"))

      ; OPERATION - Calculate and print level
      (while (= 5 (car (setq gr (grread 't 13 0))))
        (setq p1 (cadr gr)
              dist (distance p0 p1)
              txt_dist (rtos dist 2 1)
              dif (/ dist slope)
              z1 (+ z0 dif)
              txt_z1 (rtos z1 2 3)
              txt_slope (itoa (LM:Round slope))
        )
        ; OPERATION - Delete auxiliary data, if any
        (if (/= reference_circle1 nil) (vla-delete (vlax-ename->vla-object reference_circle1)))
        (if (/= cursor_text_L1 nil) (vla-delete (vlax-ename->vla-object cursor_text_L1)))
        (if (/= cursor_text_L2 nil) (vla-delete (vlax-ename->vla-object cursor_text_L2)))
        (if (/= cursor_line nil) (vla-delete (vlax-ename->vla-object cursor_line)))

        ; OPERATION - Calculate
        (setq text_height (* real_text_height (getvar "viewsize")))

        ; OPERATION - Create auxiliary data and objects
        (setq
          reference_circle1 ( _Reference_Circle p0 radius)
          cursor_text_L1 ( _CursorText_L1 p1 (strcat "z = " txt_z1) text_height)
          cursor_text_L2 ( _CursorText_L2 p1 (strcat "L = " txt_dist "m") text_height)
          cursor_line ( _Set_Line p0 p1 )
        ); END setq
      ); END while
    ); END cond "Slope"
    ((= answer "Level")
      ; ASK - level
      (setq z1 (DT:level_detection))

      ; OPERATION - Calculate and print slope
      (while (= 5 (car (setq gr (grread 't 13 0))))
        (setq p1 (cadr gr)
              dist (distance p0 p1)
              txt_dist (rtos dist 2 1)
              dif (- z1 z0)
              slope (/ dist dif)
              txt_slope (itoa (LM:Round slope))
        )

        ; OPERATION - Delete auxiliary data, if any
        (if (/= reference_circle1 nil) (vla-delete (vlax-ename->vla-object reference_circle1)))
        (if (/= cursor_text_L1 nil) (vla-delete (vlax-ename->vla-object cursor_text_L1)))
        (if (/= cursor_text_L2 nil) (vla-delete (vlax-ename->vla-object cursor_text_L2)))
        (if (/= cursor_line nil) (vla-delete (vlax-ename->vla-object cursor_line)))

        ; OPERATION - Calculate
        (setq real_text_height 0.02)
        (setq text_height (* real_text_height (getvar "viewsize")))

        ; OPERATION - Create auxiliary data
        (setq
          reference_circle1 ( _Reference_Circle p0 radius)
          cursor_text_L1 ( _CursorText_L1 p1 (strcat "p = 1/" txt_slope) text_height)
          cursor_text_L2 ( _CursorText_L2 p1 (strcat "L = " txt_dist "m") text_height)
          cursor_line ( _Set_Line p0 p1 )
        ); END setq
      )
    )
  )

  ; PRINC LAST INTERPOLATION DATA
  (princ (strcat "\nz0 = " (rtos z0 2 3) "   zf = " (rtos z1 2 3) "   -->   " txt_dist "m @ 1/" txt_slope ))

  ; OPERATION - Delete auxiliary data, if any
  (if (/= reference_circle1 nil) (vla-delete (vlax-ename->vla-object reference_circle1)))
  (if (/= cursor_text_L1 nil) (vla-delete (vlax-ename->vla-object cursor_text_L1)))
  (if (/= cursor_text_L2 nil) (vla-delete (vlax-ename->vla-object cursor_text_L2)))
  (if (/= cursor_line nil) (vla-delete (vlax-ename->vla-object cursor_line)))

  ; RESTORE PREVIOUS SETTINGS
  (setvar "clayer" oldlayer)
  (setvar "osmode" oldosmode)
  (setvar "cmdecho" oldcmdecho)

  ; End without double messages
  (princ)
  ; v0.6 - 2016.03.22 - Level prompt when first point selected
  ;                   - Minor bugs fixed
  ; v0.5 - 2016.03.21 - Font size increased
  ;                   - Auxiliary functions moved into library
  ;                   - Auxiliary circle added to mark star point (p0)
  ;                   - Code optimized and comments translated into English.
  ; v0.4 - 2016.03.17 - Auxliary dashed line added from the initial point to the cursor position
  ;                   - Level extraction functions moved to library
  ; v0.3 - 2016.03.08 - Final level, slope and distance printed at prompt when clic
  ; v0.2 - 2016.03.02 - Text size updated proportionaly with the zoom ("viewsize" system variable)
  ; v0.1 - 2016.02.29 - Data prompt removed from command-line, and put next to the mouse cross.
  ; v0.0 - 2016.02.26
  ; Author: David Torralba
  ; Last revision: 2016.03.22
)
