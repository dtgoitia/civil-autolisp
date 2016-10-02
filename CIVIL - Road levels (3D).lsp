(defun c:1() (c:road_levels) )
(defun c:road_levels (
                      /
                      oldosmode
                      ent_name interval offset side
                      exit_variable
                      p p1 p2 ang
                    )
  ; INSTRUCTIONS
  ; Select 3D polyline with the levels of the channel, but with centreline plan.
  ; Select interval: every how many meters (along the previously selected 3Dpolyline, but IN PLAN (flat centreline)) is a plot levels going to be inserted
  ; Select offset distance form the target channel to the centreline
  ; Select start and end chainages.
  ; Select side to offset to, according to centreline advance direction.

  ; OTHER SETTING TO KEEP IN MIND
  ; Plot levels are created in the current layer.

  ; TO-DO:
  ; Improve start and end chainages input: type or click, or enter to get start and end of polyline.
  ;

  ; SAVE SETTINGS
  (setq
    oldosmode (getvar "osmode")
    oldangdir (getvar "angdir")
    oldangbase (getvar "angbase")
  )

  ; AUXILIARY FUNCTIONS
  (defun *error* (msg)
  ;  (princ msg)

    ; RESTORE SETTINGS
    (setvar "osmode" oldosmode)
    (setvar "angdir" oldangdir)
    (setvar "angbase" oldangbase)
    (princ)
  )
  (defun DT:RoadLevel (pt ang z)
    (entmakex
      (list
        (cons 0 "TEXT")
        (cons 10 '(0.0 0.0 0.0))
        (cons 40 0.35)
        (cons 1  (strcat "%%U" (LM:rtos z 2 3)))
        (cons 50 ang)
        (cons 11  pt)
        (cons 72 1)
        (cons 73 2)
      );END list
    );END entmakex
  )

  ; CHANGE SETTINGS
  (setvar "osmode" 0)
  (setvar "angdir" 0)   ; same direction as system native angle reference system
  (setvar "angbase" 0)  ; same direction as system native angle reference system

  ; INPUT - Ask user 3D polyline of reference, chainage interval, offset
  (setq
    ent_name (car (entsel "\nSelect centreline"))
    interval (getreal "\nSelect interval: ")
    offset (getreal "\nSelect offset: ")
  )
  ; INPUT - Start and end chainages
  (princ "\nSelect start chainage:")
  (setq ch_0 (DT:input_chainage (vlax-ename->vla-object ent_name) 0.0 ))
  (princ (strcat "\nStart chainage: " (LM:rtos ch_0 2 3) "m"))

  (princ "\nSelect end chainage:")
  (cond
    ((= 1 (car (setq length (DT:get_3Dpoly_2Dlength (vlax-ename->vla-object ent_name)) )))
      (setq ch_f (DT:input_chainage (vlax-ename->vla-object ent_name) (cdr (DT:get_3Dpoly_2Dlength (vlax-ename->vla-object ent_name))) ))
    );END subcond
    ((= 2 (car (setq length (DT:get_3Dpoly_2Dlength (vlax-ename->vla-object ent_name)) )))
      (setq ch_f (getreal "\nImposible to detected selected polyline end chainage. Please, type it: "))
    );END subcond

  );END if
  (princ (strcat "\nEnd chainage: " (LM:rtos ch_f 2 3) "m"))

  ; INPUT - Ask user centreline side where to insert the levels
  (while (not side)
    (initget "Left Right")
    (setq side (getkword "\nSelect side where to insert the levels [Left/Right]: ") )
  );END while

  (princ "\n   >> KeyPoint: Setting finished.")

  ; OPERATION - Start a loop inserting texts
  (setq ch ch_0)
  (while (not exit_variable)
    (if (setq info (DT:get_chainage_data ent_name ch))
      (cond
        ((= 1 (car info))
          (setq
            p (list (car (cdr (cdr info))) (cadr (cdr (cdr info))) 0.0 )
            z (caddr (cdr (cdr info)))
            ang (car (cdr info))
            ang_offset  (+ ang
                          (cond
                            ((= side "Left")  (*  0.5 pi) )
                            ((= side "Right") (* -0.5 pi) )
                          )
                        )
            d_ins (+ offset -0.4)
            p_ins (polar p ang_offset d_ins)
          )
          (if (and (< (* 0.5 pi) ang) (< ang (* 1.5 pi)) )
            ; Text is not readable, needs a flip
            (setq ang_text (+ ang pi))
            ; Text is readable, doesn't need a flip
            (setq ang_text ang)
          );END if
          (DT:RoadLevel p_ins ang_text z)
        );END subcond
        ((= 2 (car info))
          (princ "\nERROR: ")(princ (cdr info))
        );END subcond
      );END cond
    );END if

    (setq ch (+ ch interval) )
    ; OPERATION - Are we inside the limit?
    (if (> ch ch_f)
      (setq
        exit_variable "CORTA EL LOOP"
      )
    );END if
  );END while

  (princ "\n   >> KeyPoint: Drawing finished.")

  ; RESTORE SETTINGS
  (setvar "osmode" oldosmode)
  (setvar "angdir" oldangdir)
  (setvar "angbase" oldangbase)
  (princ)

  (princ)

  ; v0.0 - _DATE_ - First issue
  ; Author: David Torralban
  ; Last revision: _DATE_
);END defun
(defun DT:get_chainage_data ( ent_name ch
                        /
                        aux_ent_name
                        aux_VL_ent_name
                        arr larr i p
                        )
  ; FUNCIÓN AUXILIAR PARA DEVOLVER LAS COORDENADAS DEL PUNTO CON EL CHAINAGE
  ; FUNCIONA DISTINTO PARA 3DPOLYLINES Y LWPOLYLINES
  ; Devuelve una lista con:
  ;   (cons 1 (cons ang_tang '(coordenadas)))
  ;   (cons 2 "error_message")
  ; Possible "error_message":
  ;   selected entity is not a 3Dpolyline or a LWpolyline

  ; PENDIENTE encontrar el ángulo: ------------------------------------------------------------------- PENDIENTE
  ; hecho: en la 3d polyline no hay arcos, así que con el auxiliar es facil: usa los parameters y listo
  ; en la 2d polyline tienes que mirar si hay bulge factor y jugar...
  ; PENDIENTE encontrar el ángulo: ------------------------------------------------------------------- PENDIENTE

  (vl-load-com)

  ; OPERATION - Check if DT:get_chainage_data function parametres are correct
  (cond
    ((or (not ent_name) (not ch) ) ; if not entity or not chainage introduced
      (cond
        ((and (not ent_name) (not ch) )
          (cons 2 "no centreline nor chainage selected")
        );END subcond
        ((not ent_name)
          (cons 2 "no centreline selected")
        );END subcond
        ((not ch)
          (cons 2 "no chainage introduced")
        );END subcond
      );END cond
    );END subcond
    ((> 0 ch)
      (cons 2 "requested chainage out of range") ; if negative chainage has been introduced
    );END subcond
    ((and (/= ent_name nil) (/= ch nil) )
      ; OPERATION - Check if ent_name is a 3d or 2D polyline
      (cond
        ((= "LWPOLYLINE" (cdr (assoc 0 (entget ent_name))) )
          ; OPERATION - Check if requested chainage is not bigger than the length of the selected polyline
          (cond
            ((> ch (vlax-curve-getDistAtParam (vlax-ename->vla-object ent_name) (vlax-curve-getEndParam (vlax-ename->vla-object ent_name))) )
            ; ERROR: Chainage is bigger than centreline total length
            (cons 2 "requested chainage out of range")
            );END subcond
            (t
              ; OK: Chainage is equal to or smaller than centreline total length
              (cons 1 (vlax-curve-getPointAtDist (vlax-ename->vla-object ent_name) ch))
            );END subcond
          );END cond
        );END subcond
        ((= "POLYLINE" (cdr (assoc 0 (entget ent_name))) )
          ; OPERATION - Create an auxiliary 3D polyline
          (setq
            aux_VL_ent_name (vla-copy (vlax-ename->vla-object ent_name) )
            aux_ent_name (vlax-vla-object->ename aux_VL_ent_name)
            ; OPERATION - Save coordinates array and convert it to a list
            arr (vlax-variant-value (vla-get-Coordinates aux_VL_ent_name))
            larr (vlax-safearray->list arr)
            i 1
          )
          ; OPERATION - Set every Z value of the auxiliary 3D polyline to 0 (zero)
          (foreach a larr
              (if (= (* 3 (fix (/ (float i) 3))) i) (vlax-safearray-put-element arr (- i 1) 0.0))
              (setq i (+ i 1))
          );END foreach
          (vlax-put-property aux_VL_ent_name 'Coordinates arr)

          ; OPERATION - Check if requested chainage is not bigger than the length of the selected polyline (auxiliary flat 3D polyline)
          (cond
            ((> ch (vlax-curve-getDistAtParam (vlax-ename->vla-object aux_ent_name) (vlax-curve-getEndParam (vlax-ename->vla-object aux_ent_name))) ) ; ERROR: Chainage is bigger than centreline total length
              ; OPERATION - Remove the auxiliary polyline (3D flat polyline)
              (vla-delete aux_VL_ent_name)
              ; OPERATION - Return error message
              (cons 2 "requested chainage out of range")
            );END subcond
            (t ; OK: Chainage is equal to or smaller than centreline total length
              ; OPERATION - Get current chainage's coordinates, parameter and Z value
              (setq
                p2D   (vlax-curve-getPointAtDist  aux_VL_ent_name ch )
                Param (vlax-curve-getParamAtPoint aux_VL_ent_name p2D)
                z (caddr (vlax-curve-getPointAtParam (vlax-ename->vla-object ent_name) Param))
                p3D (list (car p2D) (cadr p2D) z )
              )

              ; OPERATION - Find previous and next vertexes
              (cond
                ((= Param 0) ; Case 1: first vertex
                  (setq
                    p1 (vlax-curve-getPointAtParam (vlax-ename->vla-object aux_ent_name) 0 )
                    p2 (vlax-curve-getPointAtParam (vlax-ename->vla-object aux_ent_name) 1 )
                  )
                );END subcond
                ((= Param (fix Param))  ; Case 2: any vertex
                  (setq
                    p1 (vlax-curve-getPointAtParam (vlax-ename->vla-object aux_ent_name) (- Param 1) )
                    p2 (vlax-curve-getPointAtParam (vlax-ename->vla-object aux_ent_name) Param )
                  )
                );END subcond
                (t ; Case 3: any non-vertex point
                  (setq
                    p1 (vlax-curve-getPointAtParam (vlax-ename->vla-object aux_ent_name) (fix Param) )
                    p2 (vlax-curve-getPointAtParam (vlax-ename->vla-object aux_ent_name) (fix (+ Param 1)) )
                  )
                );END subcond
              );END cond

              ; OPERATION - Calculate tangent angle (ang_tang) at current point (p2D)
              (setq ang_tang (angle p1 p2))

              ; OPERATION - Remove the auxiliary polyline (3D flat polyline)
              (vla-delete aux_VL_ent_name)

              ; OPERATION - Return angle and coordinates
              (cons 1 (cons ang_tang p3D) )
            );END subcond
          );END cond
        );END subcond
      );END cond
    );END subcond
  );END cond

  ; PENDIENTE
  ; Actualiza la función PK con este algoritmo para que funcione con 3Dpolys que ahora mismo sólo funciona con las 2D---------------------------------

  ; v0.0 - 2016.08.26 - First issue
  ; Author: David Torralban
  ; Last revision: 2016.08.26
);END defun
(defun DT:input_chainage ( VL_ent_name default_ch / ans)
  ; Get Chainage: typing or clikcing (type click)
  (while (not ans)
    (setq ans (getstring (strcat "\nIntroduce chainage (or press P to select point) <" (LM:rtos default_ch 2 3) ">: ")) )
    (cond
      ((= ans "")
        (setq ans default_ch)
      );END subcond
      ((or (= ans "P") (= ans "p"))
        (setq ans (DT:PK VL_ent_name (getpoint "\nSelect a point: ")))
        (if (not ans)
          (princ "\nNo point introduced.")
          (setq ans ans)
        );END if
      );END subcond
      ((and ; real positive number
        (= (type (atof ans)) 'REAL)  ; real number
        (or ; or start with (.) or with a number (0-9), this implies to be a positive number
          (and
            (= (ascii (substr ans 1 1)) 46) ; (ascii ".") = 46
            (and
              (>= (ascii (substr ans 2 1)) 48) ; (ascii "0") = 48
              (<= (ascii (substr ans 2 1)) 57) ; (ascii "9") = 57
            );END and
          );END and
          (and
            (>= (ascii (substr ans 1 1)) 48) ; (ascii "0") = 48
            (<= (ascii (substr ans 1 1)) 57) ; (ascii "9") = 57
          );END and
        );END or
       );END and
        (setq ans (atof ans))
      );END subcond
      (t
        (princ "\nPlease, introduce a positive real number.")
        (setq ans nil)
      );END subcond
    );END cond
  );END while

  ; v0.0 - 2016.09.29 - First issue
  ; Author: David Torralban
  ; Last revision: 2016.09.29
)
(defun DT:get_3Dpoly_2Dlength ( VL_ent_name )
  ; PENDIENTE -------------------------------------------------------------------------------------------------------- PENDIENTE
  ; Return the 2D length of a 3d polyline
  (cond
    ((= "LWPOLYLINE" (cdr (assoc 0 (entget (vlax-vla-object->ename VL_ent_name) ))) )
      (cons 1 (vla-get-length VL_ent_name))
    );END subcond
    ((= "POLYLINE" (cdr (assoc 0 (entget (vlax-vla-object->ename VL_ent_name) ))) )
      ; OPERATION - Create an auxiliary 3D polyline
      (setq
        aux_VL_ent_name (vla-copy VL_ent_name)
        ; OPERATION - Save coordinates array and convert it to a list
        arr (vlax-variant-value (vla-get-Coordinates aux_VL_ent_name))
        larr (vlax-safearray->list arr)
        i 1
      )

      ; OPERATION - Set every Z value of the auxiliary 3D polyline to 0 (zero)
      (foreach a larr
          (if (= (* 3 (fix (/ (float i) 3))) i) (vlax-safearray-put-element arr (- i 1) 0.0))
          (setq i (+ i 1))
      );END foreach
      (vlax-put-property aux_VL_ent_name 'Coordinates arr)

      ; OPERATION - Find last point and save its chainage
      (setq ch (vlax-curve-getDistAtParam aux_VL_ent_name (vlax-curve-getEndParam aux_VL_ent_name)) )

      ; OPERATION - Remove the auxiliary polyline (3D flat polyline)
      (vla-delete aux_VL_ent_name)

      ; OPERATION - Return saved chainage
      (cons 1 ch)
    );END subcond
    (t
      (cons 2 "\nSelected object is not a polyline.")
    );END subcond
  );END cond
  ; PENDIENTE -------------------------------------------------------------------------------------------------------- PENDIENTE
  ; v0.0 - 2016.09.29 - First issue
  ; Author: David Torralban
  ; Last revision: 2016.09.29
)
(defun c:mch ( / oldosmode )
  ; Mark CHainage

  ; SAVE SETTINGS
  (setq oldosmode (getvar "osmode") )

  ; AUXILIARY FUNCTIONS
  (defun *error* (msg)
  ;  (princ msg)

    ; RESTORE SETTINGS
    (setvar "osmode" oldosmode)
    (princ)
  )

  ; CHANGE SETTINGS
  (setvar "osmode" 0)

  ; INPUT - Ask user 3D polyline of reference
  (setq
    ent_name (car (entsel "\nSelect centreline"))
    ch (getreal "\nSelect chainage to mark: ")
  )

  ; OPERATION - Mark chainage
  (if (setq p (DT:get_chainage_data ent_name ch))
    (cond
      ((= 1 (car p))
        (command "circle" (cdr p) "1")
      );END subcond
      ((= 2 (car p))
        (princ "\nERROR: ")(princ (cdr p))
      );END subcond
    );END cond
  );END if

  ; RESTORE SETTINGS
  (setvar "osmode" oldosmode)

  (princ)

  ; v0.0 - 2016.08.26 - First issue
  ; Author: David Torralban
  ; Last revision: 2016.08.26
);END defun
