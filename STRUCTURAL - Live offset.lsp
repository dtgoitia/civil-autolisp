(defun c:00 (
            /
            olderror
            p0 p1 p2
            alpha0 alpha1
            beta0 beta1
            omega1 d
            p0_off p1_off p2_off
            offset off_angle
            pt_list pt_off_list
            pline_aux_ename pline_ename
            )

  ; Press ENTER, Esc, Space or Right-Clic-Button to exit the routine.

  ; CHECK - UCS world
  (if (= 0 (getvar "WORLDUCS"))
    (progn
      (alert "------------------  WARNING!  ------------------\n\nCurrent UCS is not \"World\".\n\nPlease, set UCS \"World\" and try again.")
      (exit)
    );END progn
  );END if

  ; SAVE SETTINGS
  (setq
    olderror *error*
  )


  ; AUXILIARY FUNCTIONS
  (defun *error* (msg)
    ; REMOVE unnecessary entities
    (vla-delete (vlax-ename->vla-object pline_aux_ename))
    ; RESTORE SETTINGS
    (setq *error* olderror)
    (princ)
  )
  (defun _Set_PLine ( 3Dpt_List )
    (entmakex
      (append
        (list
          (cons 0 "LWPOLYLINE")                                   ; Element type: line
          (cons 100 "AcDbEntity")                                 ;
          (cons 100 "AcDbPolyline")                               ;
          (cons 38 0)                                             ; Elevation
          (cons 43 0)                                             ; Polyline width
          (cons 62 256)                                           ; Polyline color (256 ByLayer)
          (cons 70 0)                                             ; Polyline opened (0), closed (1)
          (cons 90 (length 3Dpt_list))                            ; Number of vertex
        )
        (mapcar
          (function
            (lambda (x)
              (cons 10 (list (car x) (cadr x)))
            );END lambda
          );END function
          3Dpt_List
        );END mapcar
      );END append
    );END entmakex
  );END defun _Set_PLine

  (setq
    offset (getreal "\nIntroduce offset distance: ")
    off_angle (* 0.5 pi)

    ; INPUT - Ask user start point
    p0 (getpoint "\nSelect start point: ")


    ; INPUT - Ask user next point
    p1 (getpoint "\nSelect next point: ")

    pt_list (append (list p1) (list p0))      ; store point at clicked point list

    alpha0 (angle p0 p1)                      ; get angle between current and previous point
    beta0 (+ alpha0 off_angle)                ; add 90º to previous angle

    p0_off (polar p0 beta0 offset)            ; get p0 offseted point coordinates
    p1_off (polar p1 beta0 offset)            ; get p1 offseted point coordinates

    pt_off_list (list p1_off p0_off)          ; store offseted point at clicked offseted point list

    pline_ename (_Set_PLine pt_off_list)      ; draw offseted polyline and store flag
    pline_aux_ename (_Set_PLine pt_list)      ; draw offseted polyline and store flag
  )

  ; OPERATION - Change auxiliar polyline color and style
  (vlax-put-property (vlax-ename->vla-object pline_aux_ename) 'Color 251)
  (if (/= (tblsearch "LTYPE" "DASHED") nil)
    (progn
      (vlax-put-property (vlax-ename->vla-object pline_aux_ename) 'Linetype "DASHED")
      (vlax-put-property (vlax-ename->vla-object pline_aux_ename) 'LinetypeScale (* 0.015 (getvar "VIEWSIZE")))
    );END progn
  );END if

  ; OPERATION - Create 3rd and following points.
  (while (not kkkk)
    ; INPUT - Ask user next point
    (setq p2 (getpoint "\nSelect next point: ") )

    ; OPERATION - Check if new point has been added, if not, exit routine.
    (if (not p2) (exit) )

    (setq
      pt_list (append (list p2) pt_list)

      alpha1 (angle p1 p2)
      beta1 (+ alpha1 off_angle)

      omega1 (* 0.5 (+ beta0 beta1))
      d (/ offset (cos (* 0.5 (abs (- beta0 beta1) ) ) ) )

      p1_off (polar p1 omega1 d)
      p2_off (polar p2 beta1 offset)

      pt_off_list (append (list p2_off) (list p1_off) (cdr pt_off_list))
    )
    ; OPERATION - Erase last polylines
    (vla-delete (vlax-ename->vla-object pline_ename))
    (vla-delete (vlax-ename->vla-object pline_aux_ename))

    ; OPERATION - Draw new polylines
    (setq
      pline_ename (_Set_PLine pt_off_list)
      pline_aux_ename (_Set_PLine pt_list)

    ; OPERATION - Shift values one step back
      p1 p2
      alpha0 alpha1
      beta0 beta1
    )

    ; OPERATION - Change auxiliar polyline color and style
    (vlax-put-property (vlax-ename->vla-object pline_aux_ename) 'Color 251)
    (if (/= (tblsearch "LTYPE" "DASHED") nil)
      (progn
        (vlax-put-property (vlax-ename->vla-object pline_aux_ename) 'Linetype "DASHED")
        (vlax-put-property (vlax-ename->vla-object pline_aux_ename) 'LinetypeScale (* 0.015 (getvar "VIEWSIZE")))
      );END progn
    );END if

  );END while

  ; RESTORE SETTINGS
  (setq *error* olderror)
  (princ)

  ; v0.1 - 2016.08.26 - Add UCS checking
  ; v0.0 - 2016.08.26 - First issue
  ; Author: David Torralba
  ; Last revision: 2016.08.26
)
