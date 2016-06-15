(vl-load-com)
(defun c:iso() (command "isolateobjects")(princ))
(defun c:uiso() (command "unisolateobjects")(princ))
(defun c:c() (command "copy")(princ))
(defun c:ci()	(command "circle")(princ))
(defun c:n() (command "NCOPY" pause "" "" ""))
(defun c:xu() (command "-xref" "u" "*")(alert "Xref Unload finished!")(princ)) ;Unload all Xrefs
(defun c:xr() (command "-xref" "r" "*")(alert "Xref Reload finished!")(princ)) ;Reload all Xrefs
(defun c:nt() (command "-text" "S" "ARIAL" "J" "MC" pause 3 90)(princ)); Add note
(defun c:las() (command "layerstate")(princ))
(defun c:RTM ()
	; RT and move together
	(C:RT)
	(command "move" "P" "" (cadr (grread 't)) pause)
	(princ)
  ; v0.1 - 2016.04.07 - Move reference clic sustituted for current mouse position.
	; v0.0 - 2016.03.29 - First issue
  ; Author: David Torralba
  ; Last revision: 2016.04.07
)
(defun c:ha45 ()
	(garden_block_paving "0.45")
  ; v0.0 - 2016.03.29 - First issue
  ; Author: David Torralba
  ; Last revision: 2016.03.29
)
(defun c:ha60 ()
	(garden_block_paving "0.60")
  ; v0.0 - 2016.03.29 - First issue
  ; Author: David Torralba
  ; Last revision: 2016.03.29
)
(defun garden_block_paving ( slab_size / *error* ss i ent ent_name VL_ent_name)
  ; automatic gardn block pave hatching
  ; This function selects the polylines within the selection set,
  ; closes them and creates an individual associative hatch for each polyline
  ;
	; SET - Error handling function
  (defun *error* ( msg )
    (if (not (member msg '("Function cancelled" "quit / exit abort")))
      (princ (strcat "\nError: " msg))
    )
    ; RESET system variables
    (setvar "osmode" oldosmode)
    (princ)
  )

  ; SAVE and CHANGE system variables
  (setq oldosmode (getvar "osmode"))
  (setvar "osmode" 33)

  ; Load VLA
  (vl-load-com)

  ; INPUT - Select objects and keep just polylines
  (setq ss (ssget '((-4 . "<OR") (0 . "LWPOLYLINE") (0 . "POLYLINE") (-4 . "OR>"))))

  ; OPERATION - Loop to close
  (setq i 0)
  (while (< i (sslength ss) )
    (setq
      ent_name (ssname ss i)
      VL_ent_name (vlax-ename->vla-object ent_name)
			ang (* -180 (/ (getangle "\nSelect slabs orientation: ") 3.141592653))
    )
		(if (= :vlax-false (vla-get-closed VL_ent_name)) (vla-put-closed VL_ent_name :vlax-true)) ; close if opened
    ;(command "-hatch" "P" "SLABS" slab_size "" "A" "A" "Y" "" "S" ent_name "" "")             ; add hatch
		(command "-hatch" "P" "SLABS" slab_size "" "A" "A" "Y" "" "S" ent_name "" "")
		(command "-hatchedit" "L" "" "" "" ang)
		(command "-hatchedit" "L" "O" "S" pause "N" "")
		;(command "O" "S" pause "N" "")             ; add hatch

    (setq i (+ i 1))                                                                          ; continue to next polyline
  ); END while

	; RESET system variables
	(setvar "osmode" oldosmode)

	(princ)

	; v0.0 - 2016.03.29 - First issue
  ; Author: David Torralba
  ; Last revision: 2016.03.29
)
(defun c:gate ()
  (command "-insert" "Access Gate" pause 0.001 0.001 pause)
)
(defun c:has()
	(command "-hatch" "S" (ssget) "" "")
	(princ)
)
(defun c:d()
	(command "measuregeom" "D")
	(princ)
)
(defun c:ha ( / ss i ent ent_name VL_ent_name)
  ; automatic HAtching
  ; This function selects the polylines within the selection set,
  ; closes them and creates an individual associative hatch for each polyline
  ;
  ; Load VLA
  (vl-load-com)

  ; INPUT - Select objects and keep just polylines
  (setq ss (ssget '((-4 . "<OR") (0 . "LWPOLYLINE") (0 . "POLYLINE") (-4 . "OR>"))))

  ; OPERATION - Loop to close
  (setq i 0)
  (while (< i (sslength ss) )
    (setq
      ent_name (ssname ss i)
      VL_ent_name (vlax-ename->vla-object ent_name)
    )
    (if (= :vlax-false (vla-get-closed VL_ent_name)) (vla-put-closed VL_ent_name :vlax-true)) ; close if opened
    (command "-hatch" "LA" "." "P" "ANSI31" "0.1" "" "A" "A" "Y" "" "S" ent_name "" "")       ; add hatch
    (setq i (+ i 1))                                                                          ; continue to next polyline
  ); END while

	; v0.1 - 2016.06.15 - Force hatch to be in current layer.
  ; v0.0 - 2016.03.16 - First issue
  ; Author: David Torralba
  ; Last revision: 2016.06.15
)
(defun c:BY ( / ss)
  ; Set by layer
  (setq ss (ssget))
  (command "._SetByLayer" ss "" "y" "y")

  ; v0.0 - 2016.03.18 - First issue
  ; Author: David Torralba
  ; Last revision: 2016.03.18
)
(defun c:au ()
  ; Fast audit and safe
  (command "audit" "Yes")
  (command "qsave")
	(princ "\nDrawing audited and saved.")
  (princ)

  ; v0.0 - 2016.03.18 - First issue
  ; Author: David Torralba
  ; Last revision: 2016.03.18
)
(defun c:cl ( / ss i ent ent_name VL_ent_name)
  ; Load VLA
  (vl-load-com)
  ; INPUT - Select entity
  (setq ss (ssget '((-4 . "<OR")
                    (0 . "LWPOLYLINE")
                    (0 . "POLYLINE")
                    (-4 . "OR>")
                   )
           )
  )
  ; OPERATION - Loop to close
  (setq i 0)
  (while (< i (sslength ss) )
    (setq
      ent_name (ssname ss i)
      VL_ent_name (vlax-ename->vla-object ent_name)
    )
    (if (= :vlax-false (vla-get-closed VL_ent_name)) (vla-put-closed VL_ent_name :vlax-true))
    (setq i (+ i 1))
  ); END while

  ; v0.0 - 2016.03.16 - First issue
  ; Author: David Torralba
  ; Last revision: 2016.03.16
)
(defun c:nested_name ( / e obj blkName)
  (while T
    (while (/=(type(setq e (car(last(nentsel "\nSelect any BLOCK to now its name: "))))) 'ENAME))
    (setq obj (vlax-ename->vla-object e))
      (if (= (vlax-get-property obj 'ObjectName) "AcDbBlockReference")
        (setq
          blkName (vlax-get-property obj (if (vlax-property-available-p obj 'effectivename)
                                              'effectivename
                                              'name
                                         ); END if
                  ); END vlax-get-property
        ); END setq
      ); END if
      (princ (strcat "\nBlock Name is: " BlkName))
  )
)
(defun c:info (/ opt obj)
  ;; VLA & DXF Info of selected Primary or Nested object
  ;; Alan J. Thompson
  (initget 0 "Nested Primary")
  (setq opt (cond ((getkword "\nSpecify selection mode [Nested/Primary] <Primary>: "))
                  ("Primary")
            )
  )
  (if (setq obj (car ((if (eq opt "Primary")
                        entsel
                        nentsel
                      )
                       (strcat "\nSelect " opt " object for VLA & DXF info: ")
                     )
                )
      )
    (progn
      (textscr)
      (princ "\nVLA Info:\n\n")
      (vlax-dump-object (vlax-ename->vla-object obj) T)
      (princ "\nDXF Info:\n")
      (mapcar 'print (entget obj '("*")))
    ); END progn
  ); END if
  (princ)
)
(defun c:re ( / ss i ent ent_name VL_ent_name)
  ; Load VLA
  (vl-load-com)
  ; INPUT - Select entity
  (setq ss (ssget '((-4 . "<OR")
                    (0 . "LWPOLYLINE")
                    (0 . "POLYLINE")
                    (-4 . "OR>")
                   )
           )
  )
  ; OPERATION - Loop to close
  (setq i 0)
  (while (< i (sslength ss) )
    (setq ent_name (ssname ss i))
    (command "PEDIT" ent_name "R" "")
    (setq i (+ i 1))
  ); END while

  ; v0.0 - 2016.03.22 - First issue
  ; Author: David Torralba
  ; Last revision: 2016.03.22
)
(defun c:get_CG ( /
									ent obj centroid
								)
	; It finds the gravity center of a closed polyline
	(setq ent (car (entsel "\nSelect closed polyline: ")))
	(command "region" ent "")
	(setq ent (entlast))
	(setq obj (vlax-ename->vla-object ent))
	(setq centroid (vlax-safearray->list (vlax-variant-value (vla-get-centroid obj))))
	(command "undo" "")
	(princ centroid)
	;(command "circle" centroid "0.3")
	(princ)
  ; v0.0 - 2016.04.05 - First issue
  ; Author: David Torralba
  ; Last revision: 2016.04.05
)
(defun CopyToClipboard (str / html result)
  (if (= 'STR (type str))
    (progn
      (setq
        html (vlax-create-object "htmlfile")
        result (vlax-invoke (vlax-get (vlax-get html 'ParentWindow) 'ClipBoardData) 'setData "Text" str)
      ); END setq
      (vlax-release-object html)
      str
    ); END progn
   );END if
)
(defun c:ct()
  ; Copy any nested string into the clipboard
  (CopyToClipboard (DT:destripar_txt))
)
(defun DT:PK( VL_ent_name pt )
	; Returns chainage of a polyline given polylines VLA name and any point
	; pt doesn't need to be a point within the centerline
  (vlax-curve-getDistAtPoint VL_ent_name (vlax-curve-getClosestPointTo VL_ent_name pt))
)
(defun c:PK( / ent VL_ent_name pt ch)
  (while (not ent)
    (setq ent (entsel "\nSelect centerline: "))
    (if (not ent)
      (princ "nothing selected.")
      (setq centerline_VL_ent_name (vlax-ename->vla-object (car ent)))
    ); END if
  ); END while centerline selection

  (while (not kkkk)
    (setq ch (DT:PK centerline_VL_ent_name (getpoint "\nSelect a point: ")))
    (princ (strcat "\nChainage = " (LM:rtos ch 2 3)))
  );END while
  (princ)
)
(defun DT:BlockPerpendicularToPolyline ( VL_ent_name blk / *error* VL_ent_name gr p0 p1 )
  ; Insert block perpendicular to selected polyline
  ; Inserta blocque perpendicular a la polilinea seleccionada
	(defun *error* ( msg )
    (if (not (member msg '("Function cancelled" "quit / exit abort")))
      (princ (strcat "\nError: " msg))
    )
		; OPERATION - Delete auxiliary data, if any
		(if (/= reference_circle1 nil) (if (/= (vlax-ename->vla-object reference_circle1) nil) (vla-delete (vlax-ename->vla-object reference_circle1))))
		(if (/= cursor_line nil) (if (/= (vlax-ename->vla-object cursor_line) nil) (vla-delete (vlax-ename->vla-object cursor_line))))

    (princ)
  )
  (while (= 5 (car (setq gr (grread 't 13 0))))
    ; OPERATION - Delete auxiliary data, if any
		(if (/= reference_circle1 nil) (if (/= (vlax-ename->vla-object reference_circle1) nil) (vla-delete (vlax-ename->vla-object reference_circle1))))
		(if (/= cursor_line nil) (if (/= (vlax-ename->vla-object cursor_line) nil) (vla-delete (vlax-ename->vla-object cursor_line))))

    ; OPERATION - Create auxiliary data and objects
    (setq
      p0 (cadr gr)
      p1 (vlax-curve-getClosestPointTo VL_ent_name (cadr gr))
      reference_circle1 ( _Reference_Circle p1 0.2)
      cursor_line ( _Set_Line p0 p1 )
    ); END setq
  ); END while

  ; OPERATION - Insert block
  (command "-insert" blk p1 "1" "1" p0)

  ; OPERATION - Delete auxiliary data, if any
	(if (/= reference_circle1 nil) (if (/= (vlax-ename->vla-object reference_circle1) nil) (vla-delete (vlax-ename->vla-object reference_circle1))))
	(if (/= cursor_line nil) (if (/= (vlax-ename->vla-object cursor_line) nil) (vla-delete (vlax-ename->vla-object cursor_line))))
	; v0.1 - 2016.05.11 - Auxiliary object detection bug fix
	; v0.0 - 2016.05.10 - First issue
  ; Author: David Torralba
  ; Last revision: 2016.05.11
)
(defun c:nla (/ obj)
  ; Find nested object real layer
  (if (setq obj (car (nentsel "\nSelect object to know layer: ")))
    (mapcar '(lambda (x) (if (= (car x) 8) (progn (princ "\nDXF Layer = ")(princ (cdr x)) )) ) (entget obj '("*")))
  ); END if
  (princ)
)
(defun c:3DPT ( / *error* VL_ent_name arr narr larr nz z oldosmode)
	; EDIT 3D polyline vertex levels
  (vl-load-com)
  (defun *error* ( msg )
    (if (not (member msg '("Function cancelled" "quit / exit abort")))
      (princ (strcat "\nError: " msg))
    )
    ; RESET system variables
    (setvar "osmode" oldosmode)
    (princ)
  )
  ; SAVE system variables
  (setq oldosmode (getvar "osmode"))

  ; CHANGE system variables
  (setvar "osmode" 1)

  ; INPUT - Select polyline
  (setq VL_ent_name (vlax-ename->vla-object (car (entsel "\nSelect 3D polyline: "))))
  (if (= "AcDb3dPolyline" (vla-get-ObjectName VL_ent_name))
    (progn
      ; El Param daba cero porque se vuelve loco con el OSMODE = 0, pon OSMODE = 1 y funciona perfecto. Pon la cabecera de copiar y salvar el OSMODE y blablabla
      (while (not exit_variable)
        (setq
          ; OPERATION - Save coordinates array and convert it to a list
          arr (vlax-variant-value (vla-get-Coordinates VL_ent_name))
          larr (vlax-safearray->list arr)
          ; INPUT - Select point to return parameter
          p (getpoint "\nSelect vertex to edit <Esc to exit>: ")
          ; OPERATION - Get data of point
          Param (atoi (LM:rtos (vlax-curve-getParamAtPoint VL_ent_name (vlax-curve-getClosestPointTo VL_ent_name p)) 2 0))
          z (nth (+ 2 (* 3 Param)) larr)
          ; INPUT - Ask for new Z value por the selected point
          nz (getreal (strcat "\nZ value <" (LM:rtos z 2 3) ">: "))
        )
        (if (or (not nz) (= nz z))
          (princ "\nZ not changed.")
          (progn
            (vlax-safearray-put-element arr (+ 2 (* 3 Param)) nz)
            (vlax-put-property VL_ent_name 'Coordinates arr)
          ); END progn
        ); END if
      );END while
    ); END progn
    (alert "Sorry, that is not a 3D polyline.")
  ); END if
  (princ)
)
(defun DT:SDIP( / p1 z0 dist grad)
  ; Light version for DIP
  (setq
    z0 (DT:clic_or_type_level)
    dist (distance (getpoint "\npoint 1: ") (setq p1 (getpoint "\npoint 2: ")))
    grad (getreal "\nGradient= 1/")
  )
	(list (+ z0 (/ dist grad)) p1)
)
(defun c:sDIP_near50( / p1)
	; For plot or private drainage levels
	; Light version for DIP rounding the result to the nearest 0.05, inserting a level block and copying it to the ClipBoard
	(setq p1 (DT:SDIP))
	(princ "\nLevel = ")(princ (car p1))
	(if (/= nil (tblsearch "block" "PI_DT"))
		(command "._insert" "PI_DT" (cadr p1) "0.25" "0.25" "" (LM:rtos (* 0.050 (atof (LM:rtos (/ (car p1) 0.050) 2 0))) 2 2))
	);END if
  (princ "\nto clipboard: ")
  (CopyToClipboard (LM:rtos (* 0.050 (atof (LM:rtos (/ (car p1) 0.050) 2 0))) 2 2))
)
(defun c:SDIP( / p1)
	; For plot or private drainage levels
	; Light version for DIP rounding the result to the 3rd decimal, inserting a level block and copying it to the ClipBoard
	(setq p1 (DT:SDIP))
	(princ "\nLevel = ")(princ (car p1))
	(if (/= nil (tblsearch "block" "PI_DT"))
		(command "._insert" "PI_DT" (cadr p1) "0.25" "0.25" "" (LM:rtos (car p1) 2 3))
	);END if
  (princ "\nto clipboard: ")
  (CopyToClipboard (LM:rtos (car p1) 2 3))
)
(defun c:garden_gradient (
                          /
                          p0 p1 z0 z1 ang ang_txt dif oldangdir oldosmode
                          )
; Calculate slope and draw slope line
  (defun *error* ( msg )
    (if (not (member msg '("Function cancelled" "quit / exit abort")))
      (princ (strcat "\nError: " msg))
    )
    ; RESET system variables
    (setvar "angdir" oldangdir)
    (setvar "osmode" oldosmode)
    (princ)
  )
  (setq oldangdir (getvar "angdir"))
  (setq oldosmode (getvar "osmode"))
  (setvar "angdir" 0)

  (setq
    p0 (getpoint "\nSelect point A: ")
    z0 (DT:clic_or_type_level)
    p1(getpoint "\nSelect point B: ")
    z1 (DT:clic_or_type_level)
    dif (- z1 z0)
    ang (- (angle p0 p1) (* 0.5 pi))
    pm (polar p0 (+ ang (* 0.5 pi)) (* 0.5 (distance p0 p1)))
  )
  (setvar "osmode" 0)
  (cond
    ( (= dif 0)
      (alert "Flat gradient. Try again.")
      (exit)
    );END cond flat
    ( (< dif 0)
      (command ".-insert" "grad-arrow" pm 1 1 (* (/ 180 pi) (+ ang (* 1.5 pi))))
      (command "mirror" "L" "" p0 p1 "Y")
      (setq
        ent_name (entlast)
        grad (strcat "1/" (LM:rtos (/ (distance p0 p1) (- 0 dif)) 2 0))
      )
    );END cond z0 high
    ( (> dif 0)
      (command ".-insert" "grad-arrow" pm 1 1 (* (/ 180 pi) (+ ang (* 0.5 pi))))
      (setq
        ent_name (entlast)
        grad (strcat "1/" (LM:rtos (/ (distance p0 p1) dif) 2 0))
      )
    );END cond z1 high
  )
  (if (and (> ang 0) (< ang pi))
    (progn
      (setq ang_txt (+ ang pi))
      ; falta hacer mirror del bloque arrow
      (command "mirror" "L" "" p0 p1 "Y")
    );END progn
    (setq ang_txt ang)
  )
  (command "-text" "S" "ROMANS" "J" "BC" pm "0.350" (* (/ 180 pi) ang_txt) grad)

  ;(command "circle" p0 "0.1")
  ;(command "circle" p1 "0.1")
  ;(command "circle" pm "0.1")

  (setvar "angdir" oldangdir)
  (setvar "osmode" oldosmode)
  (princ)
)
(defun c:DB() (command "draworder" (ssget) "" "Back" ""))
(defun c:DF() (command "draworder" (ssget) "" "Front" ""))
(defun c:DT:move_KTF_SettingOutLabel( /
                            ent_name VL_ent_name
                            label_X label_Y p0 p_ins gr
                            )
  (if (= "INSERT" (cdr (assoc 0 (entget (setq ent_name (car (entsel "\nSelect setting out block: ")) )))))
    (progn
      (princ "\"XY_advanced\" setting out block selected.")
      (setq VL_ent_name (vlax-ename->vla-object ent_name))
      (cond
        ((= "XY_advanced" (LM:effectivename VL_ent_name))
          (setq
            label_X (LM:getdynpropvalue VL_ent_name "Position1 X")  ;label initial position, to block insertion point
            label_Y (LM:getdynpropvalue VL_ent_name "Position1 Y")  ;label initial position, to block insertion point
            p0 (cadr (grread 't))                                   ;pointer position
            p_ins (cdr (assoc 10 (entget ent_name)))                ;block insertion point
          )
          (while (= 5 (car (setq gr (grread 't 13 0))))
            (LM:setdynpropvalue VL_ent_name "Position1 X" (+ (- (car (cadr gr)) (car p0)) label_X) )
            (LM:setdynpropvalue VL_ent_name "Position1 Y" (+ (- (cadr (cadr gr)) (cadr p0)) label_Y) )
          );END while
          (princ
            (strcat
              "\nLabel moved from ("
              (LM:rtos label_X 2 3)
              " "
              (LM:rtos label_Y 2 3)
              " 0.000) to ("
              (LM:rtos (+ (- (car (cadr gr)) (car p0)) label_X) 2 3)
              " "
              (LM:rtos (+ (- (cadr (cadr gr)) (cadr p0)) label_Y) 2 3)
              " 0.000)"
            );EMD strcat
          );END princ
        )
        (t
          (princ "\nSelected object is not a \"XY_advanced\" setting out block.")
        )
      );END cond
    )
    (princ "\nSelected object is not a \"XY_advanced\" setting out block.")
  );END if
  (princ)
  ; v0.0 - 2016.06.10
  ; Author: David Torralba
  ; Last revision: 2016.06.10
)
