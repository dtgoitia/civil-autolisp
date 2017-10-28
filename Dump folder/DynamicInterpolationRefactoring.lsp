(defun DT:SetVars ( varList )
  ; Set the system variables

  (if (DT:Arg 'DT:SetVars '((varList 'list)))
    (mapcar
      '(lambda (x) (setvar (nth 0 x) (nth 1 x)))
      varList
    );END mapcar
  );END if
  (princ )
  ; v0.0 - 2017.07.05 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.07.05
)
(defun DT:GetVars ( varList )
  ; Set the system variables

  (if (DT:Arg 'DT:GetVars '((varList 'list)))
    (mapcar
      '(lambda (x)
        (setq return (append (list x (getvar x))))
      );END lambda
      varList
    );END mapcar
  );END if

  ; v0.0 - 2017.07.05 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.07.05
)
﻿(defun c:DIP ( / *error* p0 p1 z0 z1 oldVars
              oldlayer oldosmode oldcmdecho
              reference_circle1 cursor_text_L1 cursor_text_L2 cursor_line
              )
  ; Dynamic Interpolation

  ; SET - Error handling function
  (defun *error* ( msg )
    (if (not (member msg '("Function cancelled" "quit / exit abort")))
      (princ (strcat "\nError: " msg))
    )
    ; PRINC LAST INTERPOLATION DATA
    (if (and (/= z0 nil) (/= z1 nil) (/= txt_dist nil) (/= txt_grad nil))
      (princ (strcat "\nz0 = " (rtos z0 2 3) "   zf = " (rtos z1 2 3) "   -->   " txt_dist "m @ 1/" txt_grad ))
    ) ; END if

    (mapcar 'DT:Delete (list reference_circle1 cursor_text_L1 cursor_text_L2 cursor_line) ) ; CLEAN VARIABLES
    (DT:SetVars oldVars) ; RESTORE PREVIOUS SETTINGS
    (princ)
  )

  ; SAVE CURRENT SETTINGS - Current layer, OSMODE and CMDECHO
  (setq oldVars (DT:GetVars (list "clayer" "osmode" "cmdecho") ))

  ; CHANGE INITIAL SETTINGS
  (setvar "cmdecho" 0)

  ; INPUT - Ask starting level
  (if (setq z0 (DT:clic_or_type_level))
    (progn
      ; OPERATION - Show selected level
      (princ (strcat "\nz0 = " (rtos z0 2 3) "m"))

      ; INPUT - Ask starting point
      (if (setq p0 (getpoint "\nIntroduce starting point: ") )
        (progn
          ; OPERATION - Create auxiliary objects
          (setq
            correction_factor 1
            radius (* 0.008 (* correction_factor (getvar "viewsize")))  ; Calculate circle size at current zoom
            reference_circle1 ( _Reference_Circle p0 radius)
          )

          ; INPUT - Ask to fix Gradient or IL
          (initget "Slope Gradient Level")
          (setq answer (getkword "\nChoose to fix [Gradient/Level]: "))
          (if (not answer) (setq answer "Gradient"))

          ; SET - Real text height
          (setq real_text_height 0.03)

          (cond
            ( (or (= answer "Slope") (= answer "Gradient"))
              ; ASK - Gradient
              (if (setq grad (getreal "\nIntroduce gradient = 1/"))
                (progn
                  ; OPERATION - Calculate and print level
                  (while (= 5 (car (setq gr (grread 't 13 0))))
                    (setq p1 (cadr gr)
                          dist (distance p0 p1)
                          txt_dist (rtos dist 2 1)
                          dif (/ dist grad)
                          z1 (+ z0 dif)
                          txt_z1 (rtos z1 2 3)
                          txt_grad (itoa (LM:Round grad))
                    )
                    ; OPERATION - Delete auxiliary data, if any
                    (mapcar 'DT:Delete (list reference_circle1 cursor_text_L1 cursor_text_L2 cursor_line) )

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
                );END progn
              );END if
            );END subcond
            ((= answer "Level")
              ; ASK - level
              (setq z1 (DT:clic_or_type_level))
              (princ (strcat "\nz0 = " (rtos z0 2 3) "   zf = " (rtos z1 2 3)))
              ; OPERATION - Calculate and print gradient
              (while (= 5 (car (setq gr (grread 't 13 0))))
                (setq p1 (cadr gr)
                      dist (distance p0 p1)
                      txt_dist (rtos dist 2 1)
                      dif (- z1 z0)
                      grad (/ dist dif)
                      txt_grad (itoa (LM:Round grad))
                )

                ; OPERATION - Delete auxiliary data, if any
                (mapcar 'DT:Delete (list reference_circle1 cursor_text_L1 cursor_text_L2 cursor_line) )

                ; OPERATION - Calculate
                (setq real_text_height 0.02)
                (setq text_height (* real_text_height (getvar "viewsize")))

                ; OPERATION - Create auxiliary data
                (setq
                  reference_circle1 ( _Reference_Circle p0 radius)
                  cursor_text_L1 ( _CursorText_L1 p1 (strcat "p = 1/" txt_grad) text_height)
                  cursor_text_L2 ( _CursorText_L2 p1 (strcat "L = " txt_dist "m") text_height)
                  cursor_line ( _Set_Line p0 p1 )
                ); END setq
              )
            );END subcond
          );END cond

          ; PRINC LAST INTERPOLATION DATA
          (princ (strcat "\nz0 = " (rtos z0 2 3) "   zf = " (rtos z1 2 3) "   -->   " txt_dist "m @ 1/" txt_grad ))

          ; OPERATION - Delete auxiliary data, if any
          (mapcar 'DT:Delete (list reference_circle1 cursor_text_L1 cursor_text_L2 cursor_line) )

        );END progn
      );END if
    );END progn
  );END if

  ; RESTORE PREVIOUS SETTINGS
  (DT:SetVars oldVars)

  (princ)

  ; v1.0 - 2017.07.05 - Refactoring
  ; v0.9 - 2016.05.18 - Gradient/Level selection updated
  ; v0.8 - 2016.05.17 - Auxiliary object deletion bug fixed
  ; v0.7 - 2016.05.10 - Auxiliary object deletion bug fixed
  ; v0.6 - 2016.03.22 - Level prompt when first point selected
  ;                   - Minor bugs fixed
  ; v0.5 - 2016.03.21 - Font size increased
  ;                   - Auxiliary functions moved into library
  ;                   - Auxiliary circle added to mark star point (p0)
  ;                   - Code optimized and comments translated into English.
  ; v0.4 - 2016.03.17 - Auxliary dashed line added from the initial point to the cursor position
  ;                   - Level extraction functions moved to library
  ; v0.3 - 2016.03.08 - Final level, gradient and distance printed at prompt when clic
  ; v0.2 - 2016.03.02 - Text size updated proportionaly with the zoom ("viewsize" system variable)
  ; v0.1 - 2016.02.29 - Data prompt removed from command-line, and put next to the mouse cross.
  ; v0.0 - 2016.02.26
  ; Author: David Torralba
  ; Last revision: 2016.05.18
)
