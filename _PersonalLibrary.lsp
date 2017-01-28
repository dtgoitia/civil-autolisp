; DO NOT REMOVE THIS LINE. It's a checking.
(vl-load-com)
(defun loadRT( / filePath)
  ; Descarga mi libreria personal
  (setq filePath "C:/RT.lsp")
  (if (= T (download "https://raw.githubusercontent.com/dtgoitia/civil-autolisp/master/RT.lsp" "C:/"))
    (progn
      ; Comprobar que el archivo es correcto
      (if (= T (CheckPersonalLibraryFirstLine filePath))
        (progn
          (LoadWithoutSecureload filePath "\nEl archivo se ha descargado, contiene lo esperado, pero como no lo puedo cargar se ha eliminado.")
          (vl-file-delete filePath)                                 ; Borra el archivo
        );END progn
        (progn
          (princ "\nEl archivo descargado no contiene lo esperado y se ha eliminado. Comprueba la URL de GitHub.")
          (vl-file-delete filePath) ; Borra el archivo
        );END progn
      );END if
    );END progn
    (princ "\nNo he podido descargar el archivo.")
  );END if
  ;(CleanCommandLine)
  (princ)
)
(loadRT)
; UNDEFINE
(command
  "_.undefine" "intersect"
  "_.undefine" "interfere"
  "_.undefine" "redraw"
  "_.undefine" "redrawall"
  "_.undefine" "recombine"
  "_.undefine" "fill"
  "_.undefine" "filletedge"
  "_.undefine" "surffillet"
  "_.undefine" "donut"
  "_.undefine" "donutid"
  "_.undefine" "donutod"
  "_.undefine" "targetpoint"
)
(defun c:iso() (command "_.isolateobjects")(princ))
(defun c:uiso() (command "_.unisolateobjects")(princ))
(defun c:c() (command "_.copy" pause "" (cadr (grread 't)) pause) )
(defun c:cc() (command "_.copy") )
(defun c:m() (command "_.move" pause "" (cadr (grread 't)) pause) )
(defun c:mo() (command "_.move"))
(defun c:mm() (command "_.move"))
(defun c:p00()
  (command
    "_.pasteblock" "0,0"
    "_.scale" "L" "" "0,0" "1000"
    "_.explode" "L" ""
    "_.zoom" "O" "L" ""
  )
  (princ)
)
(defun c:ci()	(command "_.circle")(princ))
(defun c:n() (command "_.NCOPY" pause "" "" ""))
(defun c:xu() (command "_-xref" "u" "*")(alert "Xref Unload finished!")(princ)) ;Unload all Xrefs
(defun c:xr() (command "_-xref" "r" "*")(alert "Xref Reload finished!")(princ)) ;Reload all Xrefs
(defun c:nt( / oldtextstyle olderror pt)
  (setq
    oldtextstyle (getvar "textstyle")
    oldcmdecho (getvar "cmdecho")
    olderror *error*
  )
  (setvar "cmdecho" 0)
  (setq
    pt (getpoint "\nSelect text insertion point: ")
  )
  (defun *error* ( msg )
    (setvar "textstyle" oldtextstyle)
    (setvar "cmdecho" oldcmdecho)
    (setq *error* olderror)
    (princ)
  )
  (command
    "-text" "S"
    (if (tblsearch "STYLE" "ARIAL") "ARIAL" "Standard")                   ; Text style management
    "J" "MC" pt 3
    (* (if (= 1 (getvar "angdir")) 1 -1) (getvar "angbase") (/ 180 pi) )  ; Text rotation management
    (getstring t "\nEnter text: ")
    "scale" "L" "" pt pause
  )
  (setvar "textstyle" oldtextstyle)
  (setvar "cmdecho" oldcmdecho)
  (setq *error* olderror)
  (princ)
); Add note
(defun c:pp()(command "_.publish"))
(defun c:las() (command "_.layerstate")(princ))
(defun c:r() (princ "\nRegenerating...")(command "_.regen") (princ " done.")(princ))
(defun c:os( / x ) (if (setq x (getint "\nObject Snap Mode: ")) (setvar "osmode" x) (princ "*Cancel*"))(princ))
(defun c:RTM ()
	; RT and move together
	(c:RT)
	(command "_.move" "P" "" (cadr (grread 't)) pause)
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
(defun c:ha ( )
  ; automatic HAtching
  ; This function selects the polylines within the selection set,
  ; closes them and creates an individual associative hatch for each polyline
  ;
  (vl-load-com)
  (foreach a (ssnamex (ssget '((-4 . "<OR") (0 . "LWPOLYLINE") (0 . "POLYLINE") (-4 . "OR>"))) )
    (if (= 'ename (type (cadr a)))
      (DT:ha (cadr a) "ANSI31" "0.1")
    );END if
  );END foreach

  ; v0.2 - 2016.12.23 - DT:implemented
	; v0.1 - 2016.06.15 - Force hatch to be in current layer.
  ; v0.0 - 2016.03.16 - First issue
  ; Author: David Torralba
  ; Last revision: 2016.12.23
)
(defun DT:ha ( ent_name pattern scale )
  ; automatic HAtching
  ; This function selects the polylines within the selection set,
  ; closes them and creates an individual associative hatch for each polyline

  (if (or (= "LWPOLYLINE" (cdr (assoc 0 (entget ent_name)))) (= "POLYLINE" (cdr (assoc 0 (entget ent_name)))))
    (progn
      (if (= :vlax-false (vla-get-closed (vlax-ename->vla-object ent_name))) (vla-put-closed (vlax-ename->vla-object ent_name) :vlax-true)) ; close if opened
      (cond
        ((= pattern "SOLID")
          (command "-hatch" "LA" "." "P" pattern "A" "A" "Y" "" "S" ent_name "" "" )
        );END subcond
        (t
          (command "-hatch" "LA" "." "P" pattern scale "" "A" "A" "Y" "" "S" ent_name "" "")
        );END subcond
      );END cond
    );END progn
  );END if

  ; v0.1 - 2016.12.23 - Pattern management added
	; v0.0 - 2016.12.23 - First issue
  ; Author: David Torralba
  ; Last revision: 2016.12.23
)
(defun c:BY ( / ss)
  ; Set by layer
  (setq ss (ssget))
  (command "._SetByLayer" ss "" "y" "y")

  ; v0.0 - 2016.03.18 - First issue
  ; Author: David Torralba
  ; Last revision: 2016.03.18
)
(defun c:BYC ()
  (princ "\nSelet objects to set ByLayer:")
  (foreach a (ssnamex (ssget))
    (if (= 'ename (type (cadr a)))
      (if (= "INSERT" (cdr (assoc 0 (entget (cadr a)))))
        (if (= 0 (cdr (assoc 70 (tblsearch "BLOCK" (cdr (assoc 2 (entget (cadr a)))) ))))
          (vlax-put-property (vlax-ename->vla-object (cadr a)) 'Color 256)
        );END if
        (vlax-put-property (vlax-ename->vla-object (cadr a)) 'Color 256)
      );END if
    );END if1
  );END foreach
  (princ)

  ; v0.0 - 2016.11.14 - Xref's filtered and excluded to speed up command
  ; v0.0 - 2016.11.11 - First issue
  ; Author: David Torralba
  ; Last revision: 2016.11.14
)
(defun c:c0()
  ; Shortcut for copybase with origin (0 0 0) as reference coordinates
  (command "copybase" (list 0 0 0) (ssget) "")(princ)
)
(defun c:p0()
  ; Shortcut to PASTECLIP at origin (0 0 0)
  (command "_pasteclip" "0,0")(princ)
)
(defun c:au ()
  ; Fast audit and save
  (command "audit" "Yes")
  (command "_qsave")
	(princ "\nDrawing audited and saved.")
  (alert "Drawing audited and saved.")
  (princ)

  ; v0.0 - 2016.03.18 - First issue
  ; Author: David Torralba
  ; Last revision: 2016.03.18
)
(defun c:s()
  ; Save and prompt message
  (command "_qsave")
  (princ "\nDrawing saved.")
  (alert "Drawing saved.")
  (princ)
  ; v0.0 - 2016.11.17 - First issue
  ; Author: David Torralba
  ; Last revision: 2016.11.17
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
(defun DT:PK( VL_ent_name pt / aux_VL_ent_name arr larr i a ch)
	; Returns chainage of a polyline given polyline VLA entity name and any point
	; pt doesn't need to be a point within the centerline
  (cond
    ((= "LWPOLYLINE" (cdr (assoc 0 (entget (vlax-vla-object->ename VL_ent_name) ))) )
      (vlax-curve-getDistAtPoint VL_ent_name (vlax-curve-getClosestPointTo VL_ent_name pt))
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

      ; OPERATION - Find closest point and save its chainage
      (setq ch (vlax-curve-getDistAtPoint aux_VL_ent_name (vlax-curve-getClosestPointTo aux_VL_ent_name pt)) )

      ; OPERATION - Remove the auxiliary polyline (3D flat polyline)
      (vla-delete aux_VL_ent_name)

      ; OPERATION - Return saved chainage
      (setq ch ch)
    );END subcond
  );END cond

  ; v0.1 - 2016.09.28 - Added 3D polyline management
  ; v0.0 - 2019.??.?? - First issue
  ; Author: David Torralban
  ; Last revision: 2016.09.28
)
(defun c:PK( / ent centreline_VL_ent_name ans i pt ch)
  ; INPUT - Select centreline
  (while (not ent)
    (setq ent (entsel "\nSelect centreline: "))
    (if (not ent)
      (princ "nothing selected.")
      (setq centreline_VL_ent_name (vlax-ename->vla-object (car ent)))
    ); END if
  ); END while centreline selection

  ; INPUT - Ask user what to do: type a chainage and mark it or clic and get the chainage
  (initget "Type Pick")
  (setq
    ans (getkword "\nSelect input mode [Type/Pick] <Pick>:")
    i 0
  )
  (if (not ans) (setq ans "Pick") )

  ; OPERATION - Start infinite loop with selected option:
  (while (not kkkk)
    (cond
      ((= ans "Type")
        (if (setq dist (getreal "\nIntroduce distance to mark: ") )
          (if (setq pt (vlax-curve-getPointAtDist centreline_VL_ent_name dist) )
            (entmakex (list (cons 0 "CIRCLE") (cons 10 pt) (cons 40 0.1) ))
            (princ (strcat "Point no marked. "(LM:rtos dist 2 3) "m falls out of selected centreline length."))
          );END if
          (exit)
        );END if
      );END subcond
      ((= ans "Pick")
        (if (setq pt (getpoint "\nSelect a point: ") )
          (progn
            (setq
              ch (DT:PK centreline_VL_ent_name pt)
              i (+ i 1)
            )
            (princ (strcat (itoa i) " chainage = " (LM:rtos ch 2 3)))
          );END progn
          (exit)
        );END if
    );END subcond
    );END cond
  );END while
  (princ)
)
(defun DT:BlockPerpendicularToPolyline ( VL_ent_name blk / *error* VL_ent_name gr p0 p1 )
  ; Insert block perpendicular to selected polyline
  ; Inserta bloque perpendicular a la polilinea seleccionada
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
(defun DT:nla (ent_name / lay)
  ; Returns nested object's layer name
  (if (/= ent_name nil)
    (progn
      (mapcar
        '(lambda (x)
            (if (= (car x) 8) (setq lay (cdr x)) )
          ); END lambda
        (entget ent_name '("*"))
      );END mapcar)
      (setq lay lay)
    );END progn
  );END if
);END defun
(defun c:nla( / ent_name lay)
  ; Print nested object real layer
  (if (setq ent_name (car (nentsel "\nSelect object to know layer: ")))
    (progn
      (setq lay (DT:nla ent_name))
      (princ
        (strcat
          "\nDXF Layer = " lay
          " (colour " (itoa (cdr (assoc 62 (tblsearch "layer" lay)))) ")"
        )
      )
    ); END progn
  ); END if
  (princ)
);END defun
(defun c:cnla( / ent_name lay)
  ; Print nested object real layer and copy its name to clipboard
  (if (setq ent_name (car (nentsel "\nSelect object to know layer: ")))
    (progn
      (setq lay (DT:nla ent_name))
      (princ
        (strcat
          "\nDXF Layer = " lay
          " (colour " (itoa (cdr (assoc 62 (tblsearch "layer" lay)))) ")"
        )
      )
      (CopyToClipboard lay)
    ); END progn
  ); END if
  (princ)
);END defun
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
(defun DT:SDIP ( p1 p2 gradient / z1 p1_2D p2_2D distance2D z2 )
  ; Return a point with the level updated
  ; p1 [pt] - start point (3D)
  ; p2 [pt] - end point (Z coordinate not relevant)
  ; grad [real] - gradient (1/grad) from p1 to p2
  ;   0 < grad = running up, p2 is above p1
  ;   0 > grad = running down, p2 is below p1
  (setq
    z1 (nth 2 p1)
    p1_2D (list (nth 0 p1) (nth 1 p1) 0.0)
    p2_2D (list (nth 0 p2) (nth 1 p2) 0.0)
    distance2D (distance p1_2D p2_2D)
    z2 (+ z1 (/ distance2D gradient))
    p2 (list (nth 0 p2) (nth 1 p2) z2)
  )
  p2
)
(defun c:SDIP( / z1 p1_2D p1 p2_2D p2 dist ans)
  ; Get level with fixed gradient between 2 points, and
  ; insert a level block and copy level to ClipBoard
	(setq
    z1 (DT:clic_or_type_level)
    p1_2D (getpoint (strcat "\nStart level = " (LM:rtos z1 2 3) "m\npoint 1: "))
    p1 (list (nth 0 p1_2D) (nth 1 p1_2D) z1)
    p2_2D (getpoint (strcat "\nStart level = " (LM:rtos z1 2 3) "m\npoint 1: OK\npoint 2: "))
  )
  (if SDIPgradient
    (progn
      (setq ans (getreal (strcat "\nStart level = " (LM:rtos z1 2 3) "m\npoint 1: OK\npoint 2: OK" "\nGradient= 1/<" (LM:rtos SDIPgradient 2 0) ">: ") ) )
      (if ans (setq SDIPgradient ans));END if
    );END progn
    (setq SDIPgradient (getreal (strcat "\nStart level = " (LM:rtos z1 2 3) "m\npoint 1: OK\npoint 2: OK\nGradient= 1/")))
  );END if
  (setq
    p2 (DT:SDIP p1 p2_2D SDIPgradient)
  )
	(if (tblsearch "block" "PI_DT")
		(command "._insert" "PI_DT" (list (nth 0 p2) (nth 1 p2) 0.0) "0.25" "0.25" "" (LM:rtos (nth 2 p2) 2 3))
	);END if
  (princ "\nto clipboard: ")
  (CopyToClipboard (LM:rtos (nth 2 p2) 2 3))
)
(defun c:sDIP_near50( / z1 p1_2D p1 p2_2D p2 z2 gradient)
  ; Get level with fixed gradient between 2 points, and
  ; insert a level block and copy level to ClipBoard.
  ; It rounds the result to the nearest 0.05m
  (setq z1 (DT:clic_or_type_level))
	(princ (strcat "\nStart level = " (LM:rtos z1 2 3) "m"))
	(setq
    p1_2D (getpoint "\npoint 1: ")
    p1 (list (nth 0 p1_2D) (nth 1 p1_2D) z1)
    p2_2D (getpoint "\npoint 2: ")
    gradient (getreal "\nGradient= 1/")
    p2 (DT:SDIP p1 p2_2D gradient)
    z2 (LM:rtos (* 0.050 (atof (LM:rtos (/ (nth 2 p2) 0.050) 2 0))) 2 2)
  )
	(if (tblsearch "block" "PI_DT")
		(command "._insert" "PI_DT" (list (nth 0 p2) (nth 1 p2) 0.0) "0.25" "0.25" "" z2)
	);END if
  (princ "\nto clipboard: ")
  (CopyToClipboard z2)
)
(defun DT:flatPoint( pt )
; Return the provided 3D point with Z=0
  (list (nth 0 pt) (nth 1 pt) 0.0)
)
(defun c:SDIPP( /
  ; Recursive DT:SDIP along polyline vertexes
                ent_name insertionMode
                startZ startPoint startParam endPoint endParam gradient
                p1 p2 param
                )
  ; AUXILIARY FUNCTIONS
  (defun DT:AskBlockInAllVertex( / ans )
  ; Returns T if the user wants blocks in every vertex,
  ; or nil if the user want just one block in the last vertex
    (initget "All Last")
    (if (not (setq ans (getkword "\nSelect block vertexes to insert the blocks [All/Last] <Last>? ") ))
      (setq ans Last)
    );END if
    (cond
      ((= ans "All")
        T
      );END subcond
      ((= ans "Last")
        nil
      );END subcond
    );END cond
  )
  (defun DT:InsertFlatLevelBlock ( pt )
    (if (tblsearch "block" "PI_DT")
      (command "._insert" "PI_DT" (DT:flatPoint pt) "0.25" "0.25" "" (LM:rtos (nth 2 pt) 2 3))
    );END if
  )

  (if (setq ent_name (car (entsel "\nSelect a 2D polyline: ")) )
    (if (= "LWPOLYLINE" (cdr (assoc 0 (entget ent_name))) )
      (progn
        (setq
          insertionMode (DT:AskBlockInAllVertex)
          startZ (DT:clic_or_type_level)
        )
      	(princ (strcat "\nStart level = " (LM:rtos startZ 2 3) "m"))
        (setq
          startPoint (getpoint "\nSelect start point: ")
          startParam (vlax-curve-getParamAtPoint (vlax-ename->vla-object ent_name) startPoint)
          endPoint (getpoint "\nSelect end point: ")
          endParam (vlax-curve-getParamAtPoint (vlax-ename->vla-object ent_name) endPoint)
          gradient (getreal "\nGradient= 1/")
          p1 (list (nth 0 startPoint) (nth 1 startPoint) startZ)
          param startParam
        )
        (cond
          ((= startParam endParam)
            (princ "\nStart and end point are the same.")
          );END subcond
          ((< startParam endParam) ; normal direction
            (while (< param endParam)
              (setq
                param (+ param 1)
                p2 (vlax-curve-getPointAtParam (vlax-ename->vla-object ent_name) param)
                p2 (DT:SDIP p1 p2 gradient)
              )
              (if insertionMode (DT:InsertFlatLevelBlock p2))
              (setq p1 p2)
            );END while
            (DT:InsertFlatLevelBlock p2)
          );END subcond
          ((> startParam endParam) ; reverse direction
            (while (> param endParam)
              (setq
                param (- param 1)
                p2 (vlax-curve-getPointAtParam (vlax-ename->vla-object ent_name) param)
                p2 (DT:SDIP p1 p2 gradient)
              )
              (if insertionMode (DT:InsertFlatLevelBlock p2))
              (setq p1 p2)
            );END while
            (DT:InsertFlatLevelBlock p2)
          );END subcond
        );END cond
        (princ "\nto clipboard: ")
        (CopyToClipboard (LM:rtos (nth 2 p2) 2 3))
      );END progn
      (princ "\nSelected object is not a 2D polyline.")
    );END if
    (princ "\nNothing selected.")
  );END if
  (princ)
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
(defun c:PI_block_to_Z_level()
  ; Move blocks PI to the coordinate Z indicated by their attribute "LEVEL"
  (foreach a (ssnamex (ssget '((0 . "INSERT"))))
    (if (= 'ename (type (cadr a)))
      (vla-move
        (vlax-ename->vla-object (cadr a))
        (vlax-3d-point (list 0 0 0))
        (vlax-3d-point
          (list
            0
            0
            (-
              (atof
                (vl-some
                  '(lambda
                    ( att )
                    (if (= "LEVEL" (strcase (vla-get-tagstring att))) (vla-get-textstring att))
                  );END lambda
                  (vlax-invoke (vlax-ename->vla-object (cadr a)) 'getattributes)
                );END vl-some
              );END atof
              (caddr (cdr (assoc 10 (entget (cadr a)))))
            );END - (substraction)
          );END list
        );END vlax-3d-point
      );END vla-move
    );END if
  );END foreach
  (princ)
  ; v0.0 - 2016.06.16
  ; Author: David Torralba
  ; Last revision: 2016.06.16
)
(defun c:nlayf (/ obj lay)
  ; Freeze nested object real layer
  (if (setq obj (car (nentsel "\nSelect object to freeze layer: ")))
    (mapcar
      '(lambda (x)
        (if (= (car x) 8)
          (progn
            (princ "\nDXF Layer = ")
            (princ (cdr x))
            (setq lay (cdr x))
          ); END progn
        )
      )
      (entget obj '("*"))
    )
  ); END if
  (if (not lay)
    (princ "\nLayer name has not been saved at lay variable. Take a look to the code.")
    (command "-layer" "f" lay "")
  );END if
  (princ)
  ; v0.0 - 2016.06.17
  ; Author: David Torralba
  ; Last revision: 2016.06.17
)
(defun c:TOTAL_AREA( / a ar art i)
  ; Select all hatches and iterate adding their areas.
  (setq
    i 0
    art 0
  )
  (foreach a (ssnamex (ssget '((-4 . "<or") (0 . "HATCH") (0 . "LWPOLYLINE") (-4 . "or>"))))
    (if (= 'ename (type (cadr a)))
      (if (setq ar (vla-get-area (vlax-ename->vla-object (cadr a))))
        (progn
          (setq
            i (+ i 1)
            art (+ art ar)
          )
          (princ (strcat "\nArea " (itoa i) " = " (LM:rtos ar 2 3) "m2"))
        );END progn
      );END if2
    );END if
  );END foreach
  (if (/= nil art)
    (princ (strcat "\nCUMULATIVE AREA = " (LM:rtos art 2 3) "m2"))
  );END if
  (princ)
  ; v0.0 - 2016.06.23
  ; Author: David Torralba
  ; Last revision: 2016.06.23
)
(defun c:TOTAL_LENGTH( / a l lt i)
  ; Select all lines and polylnes and iterate adding their length.
  (setq
    i 0
    lt 0
  )
  (foreach a (ssnamex (ssget '((-4 . "<or") (0 . "LINE") (0 . "LWPOLYLINE") (-4 . "or>"))))
    (if (= 'ename (type (cadr a)))
      (if (setq l (vla-get-length (vlax-ename->vla-object (cadr a))))
        (progn
          (setq
            i (+ i 1)
            lt (+ lt l)
          )
          (princ (strcat "\nLength " (itoa i) " = " (LM:rtos l 2 3) "m"))
        );END progn
      );END if2
    );END if
  );END foreach
  (if (/= nil lt)
    (princ (strcat "\nCUMULATIVE LENGTH = " (LM:rtos lt 2 3) "m"))
  );END if
  (princ)
  ; v0.0 - 2016.06.23
  ; Author: David Torralba
  ; Last revision: 2016.06.23
)
(defun c:ltp ( / ss i p1 p2)
	; Convert lines to polylines (lwpolylines)
  (princ "\nSelect lines to convert to polylines:")
  (setq
    ss (ssget '(( 0 . "LINE")) )
    i 0
  )
  (foreach a (ssnamex ss)
    (if (= 'ename (type (cadr a)))
      (progn
        (setq
          p1 (vlax-curve-getStartPoint (vlax-ename->vla-object (cadr a)))
          p2 (vlax-curve-getEndPoint (vlax-ename->vla-object (cadr a)))
        )
        (entmake
          (append
            (list
              '(000 . "LWPOLYLINE")         ; Object type
              '(100 . "AcDbEntity")
              '(100 . "AcDbPolyline")
              '(070 . 0)                  ; Open(0)/Closed(1)
              '(090 . 2)                  ; Number of vertices
            );END list
            (list
              (cons 10 p1)
              (cons 10 p2)
            );END list
          );END append
        );END entmake
        (vla-delete (vlax-ename->vla-object (cadr a)))
        (setq i (+ i 1) )
      ); END progn
    );END if1
  );END foreach
  (princ (strcat "\n" (itoa i) " lines converted to polylines."))
  (princ)

  ; v0.0 - 2016.07.29 - First issue
  ; Author: David Torralba
  ; Last revision: 2016.07.29
)
(defun c:ewl()
  ; create E-Work Layers
  (command "-layer" "m" "e-work-services" "c" "9" "" "")
  (command "-layer" "m" "e-work-hse" "c" "9" "" "")
  (princ)
	; v0.0 - 2016.08.12 - First issue
	; Author: David Torralba
	; Last revision: 2016.08.12
)
(defun c:ewb( / *error* p_ins p0 ss blk_name)
  ; creates E-Work Block

  ; SAVE CURRENT SETTINGS
  (setq
    oldclayer   (getvar "clayer")
    oldosmode   (getvar "osmode")
    oldcmdecho  (getvar "cmdecho")
    olderror    *error*
  )

  ; CHANGE SETTINGS
  (setvar "cmdecho" 1)
  (setvar "osmode" 1)

  ; AUXILIARY FUNCTION
  (defun *error* (errmes)
    ;(princ (strcat "\nProcess halted by the following error: \n    >>> " ERRMES))
    ; NOTE: to turn error message on, erase the semicolon in the line above.

    (setvar "cmdecho" OLDCE)
    (setq *error* OLDERR)
    (setvar "OSMODE" oldSnap)  ; Reset "OSMODE" to previous value.
    (setvar "cmdecho" OLDCE)   ; Reset "cmdecho" to previous value.
    (setvar "CLAYER" oldCla)   ; Reset "CLAYER" to previous value.
    (prin1)
  )

  ; OPERATION - Check if "e-work-hse" layer exists, and if not, create it
  (if (not (tblsearch "layer" "e-work-hse")) (command "-layer" "m" "e-work-hse" "c" "9" "" ""))

  ; OPERATION - Check if "e-work-hse" layer is current, and if not, put as current layer
  (if (/= "e-work-hse" (getvar "clayer")) (setvar "clayer" "e-work-hse") )

  ; INPUT - Ask user to specify block basepoint
  (setq p_ins (getpoint "\nSpecify block base point: "))

  ; OPERATION - Switch off the snap mode
  (setvar "OSMODE" 0)

  ; INPUT - Ask user to specify the point in where to copy scaled block
  (setq p0 (getpoint "\nSpecify point to copy and scale the block: "))

  ; OPERATION - Draw a circle of 2m around the selected point to label the block
  (command "_.circle" p0 "2000")

  ; INPUT - Ask user to select the objects to copy, scale and block
  (setq ss (ssget))

  ; INPUT - Ask user to introduce block name
  (setq blk_name (getstring 't "\nSpecify block name (press Enter to finish): "))

  ;; OPERATION - Copy, scale and block the objects.
  (command "_.move" ss "" p_ins p_ins)
  (command "_.scale" ss "" p_ins "0.001")
  (command "_.-Block" blk_name p_ins ss "")
  (command "_.-insert" blk_name p0 "" "" "")
  (command "_.-insert" blk_name p_ins 1000 1000 "")
  (command "_.explode" "L")

  ; RESTORE MODIFIED SYSTEM VARIABLES
  (setvar "cmdecho" oldcmdecho)
  (setvar "clayer" oldclayer)
  (setvar "osmode" oldosmode)
  (setq *error* olderror)

  (princ)

	; v0.0 - 2016.08.12 - First issue
	; Author: David Torralba
	; Last revision: 2016.08.12
)
(defun c:CS(
            /
            s s_str
            )
; Choose Scale
  (if (setq s (getint "\nSelect scale: 1:"))
    (progn
      (setq s_str (strcat (LM:rtos (/ 1000 (float s) ) 2 2) "xp") )
      (command "zoom" "S" s_str)
      (princ (strcat "\nViewport scale updated to 1:" (itoa s) "."))
    );END progn
    (princ "\nNothing introduced. Routine finished with no viewport scale change.")
  );END if
  (princ)
	; v0.0 - 2016.08.15 - First issue
	; Author: David Torralba
	; Last revision: 2016.08.15
)
(defun c:erase_bylayer (
    /
    oldclayer oldcmdecho olderror
    ent_name ss
    lay
    )
  ; SAVE SETTINGS
  (setq
    oldclayer (getvar "clayer")
    oldcmdecho (getvar "cmdecho")
    olderror *error*
  )

  ; CHANGE SETTINGS
  (setvar "cmdecho" 1)

  (defun *error* (errmes)
    ; RESTORE SETTINGS
    (setvar "cmdecho" oldcmdecho)
    (setvar "clayer" oldclayer)
    (setq *error* olderror)
    (princ)
  )

  ; INPUT - Ask the user to select an object to identify the layer to clean
  (setq
    ent_name (car (entsel "\nSelect an object in the layer to clean: "))
    lay (cdr (assoc 8 (entget ent_name)))
  )
  (princ (strcat "\nLayer where to erase objetcs: " lay))

  ; INPUT - Ask user to select objects to clean
  (setq ss (ssget (list (cons 8 lay))) )

  ; OPERATION - Erase the objects
  (command "_.erase" ss "")

  ; RESTORE SETTINGS
  (setvar "cmdecho" oldcmdecho)
  (setvar "clayer" oldclayer)
  (setq *error* olderror)
  (princ)

  ; v0.0 - 2016.08.23 - First issue
  ; Author: David Torralba
  ; Last revision: 2016.08.23
)
(defun c:ug() (command "_ungroup") )
(defun c:ge() (command "_groupedit") )
(defun c:gg()
  ; Selection Group ON/OFF shortcut
  (cond
    ((= 0 (getvar "pickstyle"))
      (setvar "pickstyle" 1)
      (princ "\nGroup selection ON.")
    );END subcond
    ((= 1 (getvar "pickstyle"))
      (setvar "pickstyle" 0)
      (princ "\nGroup selection OFF.")
    );END subcond
  );END cond
  (princ)

  ; v0.0 - 2016.08.30 - First issue
  ; Author: David Torralban
  ; Last revision: 2016.08.30
);END defun
(defun c:lb( / ans)
  ; Layout Background
  (initget "White Black")
  (setq ans (getkword "\nWhat background color would you like [White/Black]? ") )
  (cond
    ((= ans "White" )
      (vla-put-GraphicsWinLayoutBackgrndColor (vla-get-display (vla-get-preferences (vlax-get-acad-object))) 16777215)
    );END subcond
    ((= ans "Black" )
      (vla-put-GraphicsWinLayoutBackgrndColor (vla-get-display (vla-get-preferences (vlax-get-acad-object))) 0)
    );END subcond
    (t
      (princ "\nWrong answer :P")
    )
  );END cond
  (princ)
  ; v0.0 - 2016.09.08 - First issue
  ; Author: David Torralban
  ; Last revision: 2016.09.08
);END defun
(defun PrintDateTime( / d)
  ; Returns a string with the date formated as YYYY.MM.DD hh:mm:ss
  (setq d (rtos (getvar "CDATE") 2 6))
  (strcat (substr d 1 4) "." (substr d 5 2) "." (substr d 7 2) " " (substr d 10 2) ":" (substr d 12 2) ":" (substr d 14 2))
)
(defun PrintDate( / d)
  ; Returns a string with the time formated as YYYY.MM.DD
  (setq d (rtos (getvar "CDATE") 2 6))
  (strcat (substr d 1 4) "." (substr d 5 2) "." (substr d 7 2))
)
(defun PrintTime( / d)
  ; Returns a string with the date formated as hh:mm:ss
  (setq d (rtos (getvar "CDATE") 2 6))
  (strcat (substr d 10 2) ":" (substr d 12 2) ":" (substr d 14 2))
)
(defun PrintSupersedDate( / d)
  ; Returns a string with the date formated as DD.MM.YY
  (setq d (rtos (getvar "CDATE") 2 6))
  (strcat (substr d 7 2) "." (substr d 5 2) "." (substr d 3 2))
)
(defun ReloadXref (file)
  (command "-xref" "R" file)
)
(defun DT:GetText(ent_name)
  ; Returns a string with the text of the selected object, if any
  (if (= 'ename (type ent_name))
    (if (vlax-property-available-p (vlax-ename->vla-object ent_name) 'TextString)
      (vlax-get-property (vlax-ename->vla-object ent_name) 'TextString)
    );END if2
  );END if
)
(defun DT:SetText(ent_name txt)
  ; Sets the text of the selected object, if possible
  (if (= 'ename (type ent_name))
    (if (vlax-property-available-p (vlax-ename->vla-object ent_name) 'TextString)
      (vlax-put-property (vlax-ename->vla-object ent_name) 'TextString txt)
    );END if2
  );END if
)
(defun asin (sine) (atan sine (sqrt (- 1 (* sine sine)))))
(defun acos (cosine) (atan (sqrt (- 1 (* cosine cosine))) cosine))
(defun c:cfile( / filePath )
  ; Print and copy current file path
  (setq filePath (strcat (getvar "dwgprefix") (getvar "dwgname") ) )
  (CopyToClipboard filePath)
  (princ (strcat "\n" filePath "   \(copied to ClipBoard\)") )
  (princ)
)
(defun c:cpath( / filePath )
  ; Print and copy current file path
  (setq filePath (getvar "dwgprefix") )
  (CopyToClipboard filePath)
  (princ (strcat "\n" filePath "   \(copied to ClipBoard\)") )
  (princ)
)
(defun c:cfolder()
  ; Open current file containing folder in Windows Explorer and select the file
  (startapp
    (strcat "explorer /select, "
      (getvar "dwgprefix")
      (getvar "dwgname")
      ", /e"
    )
  )
  (princ)
)
(defun c:f( / url)
  ; Open the default browser and search in Google for the desired term adding "Autodesk Help " at the beginning
  (setq
    url
    (strcat
      "https://www.google.co.uk/search?q="
      "AutoCAD+Help+"
      (DT:ReplaceSpaceForPlus (getstring t "\nWhat do you want to know about: ") )
    ); END strcat
  );END setq
  (command "._browser" url)
  (princ)
);END defun
(defun DT:ReplaceSpaceForPlus ( txt / i)
  ; Return the provided string with "+" instead of spaces, if not, returns the provided string
  ; txt [str] - String where to replace " " for "+"
  (if (not (vl-string-search " " txt))
    txt
    (while (setq i (vl-string-search " " txt))
      (setq txt (vl-string-subst "+" " " txt) )
    );END while
  );END if
);END defun
(defun c:LastCustomShortcuts( / filePath)
  ; Descarga mi libreria personal
  (setq filePath "C:/_LastCustomShortcuts.lsp")
  (if (= T (download "https://raw.githubusercontent.com/dtgoitia/civil-autolisp/master/_LastCustomShortcuts.lsp" "C:/"))
    (progn
      ; Comprobar que el archivo es correcto
      (if (= T (CheckPersonalLibraryFirstLine filePath))
        (progn
          (LoadWithoutSecureload filePath "\nEl archivo se ha descargado, contiene lo esperado, pero como no lo puedo cargar se ha eliminado.")
          (vl-file-delete filePath)                                 ; Borra el archivo
        );END progn
        (progn
          (princ "\nEl archivo descargado no contiene lo esperado y se ha eliminado. Comprueba la URL de GitHub.")
          (vl-file-delete filePath) ; Borra el archivo
        );END progn
      );END if
    );END progn
    (princ "\nNo he podido descargar el archivo.")
  );END if
  ;(CleanCommandLine)
  (princ)
)
(defun DT:mid3dPoint ( p1 p2 )
  ; Return the middle point (in 3D) between points p1 and p2
  ; p1 [pt] - Point 1
  ; p2 [pt] - Point 2
  (list
    (* 0.5 (+ (car   p1) (car   p2) ))
    (* 0.5 (+ (cadr  p1) (cadr  p2) ))
    (* 0.5 (+ (caddr p1) (caddr p2) ))
  );END list
)
(defun DT:ReduceAngle ( ang )
  ; Return the provided angle equivalent between 0 and 2π
  ; ang [rad] - Angle to reduce
  (setq ang (- ang (* (float (fix (/ ang (* 2 pi)) )) 2 pi) ) )
)
(defun DT:ReadableTextAngle ( ang / a )
  ; Return text angle in radians corrected for readability
  ; ang [rad] - Angle to correct for readability, from 0 to 2π
  (setq a (DT:ReduceAngle ang)) ; reduce angle
  (if (and (> a (* 0.5 pi) ) (< a (* 1.5 pi) ) )
    (if (< a pi )
      (+ a pi)  ; unreadable angle, add 180º
      (- a pi)  ; unreadable angle, substract 180º
    );END if
    a           ; readable angle, return as is
  );END if
)
(defun c:ReplaceBlocks( / newBlockName)
  ; Replaces all selected blocks for an existing block
  (setq newBlockName (getstring "\nType new block name: ") )
  (princ "\nCheking if selected block exists... ")
  (if (DT:CheckIfBlockExists newBlockName)
    (progn
      (princ "block found!")
      (command "._UNDO" "_Begin")
      (princ "\nTell me which blocks you want me to replace.")
      (foreach a (ssnamex (ssget))
        (if (= 'ename (type (cadr a)))
          (if (= "INSERT" (cdr (assoc 0 (entget (cadr a)) )) )
            (DT:replaceBlock (cadr a) newBlockName)
          );END if
        );END if1
      );END foreach
      (command "._UNDO" "_End")
    );END progn
    (princ "block not found.")
  );END if
  (princ)
)
(defun DT:GetStringLastNChar ( string n )
  ; Return string last n characters
  (substr string (- (strlen string) (- n 1)) )
)
(defun DT:RemovePolylineVertex ( ent_name param / VL_ent_name array i paramCounter newListArray)
  ; Removes the selected vertex of the polyline
  ; ent_name [ename] - Polyline entity name
  ; param [int] - Parameter of the vertex to remove on the polyline
  (setq
    VL_ent_name (vlax-ename->vla-object ent_name)
    array (vlax-variant-value (vla-get-Coordinates VL_ent_name))
    i 0
    paramCounter 0
  )
  (foreach a (vlax-safearray->list array)
    (if (= param paramCounter)
      (princ "\nSelected vertex found!")
      (setq newListArray (append newListArray (list a)))
    );END if
    (setq
      i (+ i 1)
      paramCounter (fix (/ i 2))
    )
  );END foreach
  (setq newArray (vlax-make-safearray vlax-vbdouble (cons 0 (+ -1 (length newListArray)) )))
  (vlax-safearray-fill newArray newListArray)
  (vlax-put-property VL_ent_name 'Coordinates newArray )
)
(defun GetEntityTransparency ( ent )
  ; Get entity transparency
  ; ent [ename/vla-object] - Entity name/VLA oject name
  ; Returns transparency value as integer, or "ByLayer" as string
  (cond
    ((= 'vla-object (type ent))
      (vla-get-entitytransparency ent)
    );END subcond
    ((= 'ename (type ent))
      (vla-get-entitytransparency (vlax-ename->vla-object ent))
    );END subcond
  );END cond
)
(defun SetEntityTransparency (ent transparency / entType )
  ; Set entity transparency
  ; ent [ename/vla-object] - Entity name/VLA oject name
  ; transparency [int/str] - Transparecy value/"ByLayer"
  ; Returns T if successful, nil if not
  (if (or
        (and (= 'str (setq entType (type transparency)))
             (= "BYLAYER" (strcase transparency))
        );END and
        (and (= 'int entType) (<= 0 transparency 90))
      );END or
      (cond
        ((= 'vla-object (setq entType (type ent)))
          (not (vla-put-entitytransparency ent transparency))
        );END subcond
        ((= 'ename entType)
          (SetEntityTransparency (vlax-ename->vla-object ent) transparency )
        );END subcond
      );END cond
  );END if
)
(defun c:DNLA( / info infoString infoStringSize txt)
  ; Print deep object nested block name and layer
  (setq
    info (NestedEntityNames (getpoint "\nSelect object to see its nested layers:"))
    infoString (ChangeEnameForLevel info)
    infoStringSize (GetColumnLength infoString)
    txt (NL_GetTextFormatted infoString infoStringSize)
  )
  (princ txt)
  (princ)
)
(defun EntityInfo (ent_name / blockName blockType )
  ; Return a list with useful information of the nested object
  ; (ename (Block/Xref Name Layer))
  (if (= 'ename (type ent_name))
    (if (= "INSERT" (cdr (assoc 0 (entget ent_name))))
      (progn
        (if (setq blockName (LM:effectivename (vlax-ename->vla-object ent_name)) )
          (if (assoc 1 (tblsearch "block" blockName))
            (setq blockType "XREF")
            (setq blockType "BLOCK")
          );END if
        );END if
        (list ent_name blockType blockName (cdr (assoc 8 (entget ent_name))) )
      );END progn
      (list ent_name (cdr (assoc 0 (entget ent_name))) "-" (cdr (assoc 8 (entget ent_name))) )
    );END if
    nil
  );END if
)
(defun NestedEntityNames ( pt / ent_name i info )
  ; Returns a list with nested entity names, being the parent first one and deepest child last one
  (if (= 'list (type pt)) ; if point provided, continue
    (if (setq ent_name (nentselp pt)) ; if entity selected, continue
      (progn
        (setq
          i 0
          info (list (list "Level" "Type" "Block name" "Object layer") (EntityInfo (car ent_name)) )
        )
        (if (nth 3 ent_name) ; if any nested entity
          (progn
            (setq nestedLevels (length (nth 3 ent_name)) )
            (foreach x (nth 3 ent_name)
              (setq
                i (+ i 1)
                info (append info (list (EntityInfo x)))
              )
            );END foreach
          );END progn
        );END if
        info
      );END progn
    );END if
  );END if
)
(defun ChangeEnameForLevel ( info / i x )
  ; Return info replacing ename for level
  (if (= 'list (type info))
    (progn
      (setq i -1)
      (foreach a info
        (setq
          x (append x (list (if (< i 0) a (LM:SubstNth (itoa i) 0 a)) ))
          i (+ i 1)
        )
      );END foreach
      x
    );END progn
  );END if
)
(defun GetColumnLength ( infoString / q col )
  ; Return list with the biggest length needed on each column
  (if (= 'list (type infoString))
    (progn
      ; initialise an zero list to contain each column length
      (repeat (length (car infoString))
        (setq col (append col (list 0)) )
      )

      ; populate the list col with the biggest lengths found
      (mapcar
        '(lambda ( x )
          (setq q nil)
          (mapcar '(lambda ( xx yy ) (setq q (append q (list (max (strlen xx) yy)) )) ) x col )
          (setq col q)
        );END lambda
        infoString
      );END mapcar
      col
    );END progn
  );END if
)
(defun NL_GetTextFormatted ( infoString infoStringSize / txt s )
  ; Format text correctly and concatenate it
  (setq txt "\n")
  (mapcar
    '(lambda (x)
      (setq txt (strcat txt "\n"))
      (mapcar
        '(lambda (xx yy)
          (setq s " ")
          (if (< (strlen xx) yy)
            (setq txt (strcat txt xx (repeat (+ 1 (- yy (strlen xx))) (setq s (strcat s " "))) ))
            (setq txt (strcat txt xx "  "))
          );END if
        );END lambda
        x infoStringSize
      );END mapcar
    );END lambda
    infoString
  );END mapcar
  txt
)
(defun c:Convert3DpolyTo2Dpoly ( / old_error old_sysvars iSuccess iFail )
  ; Convert multiple 3Dpolylines to 2Dpolylines
  (setq
    iSuccess  0
    iFail     0
  )
  (princ "\nCONVERT 3D POLYLINES TO 2D POLYLINES\nSelect 3D polyilines to convert:")
  (if (setq ss (ssget '((0 . "POLYLINE"))) )
    (foreach a (ssnamex ss)
      (if (= 'ename (type (cadr a)))
        (if (Convert3DpolyTo2Dpoly (cadr a))
          (setq iSuccess (+ iSuccess 1))
          (setq iFail (+ iFail 1))
        );END if
      );END if
    );END foreach
  );END if

  ; Summary comment
  (princ
    (strcat
      "\n" (itoa (+ iSuccess iFail) ) " objects processed. "
      (cond
        ((and (> iSuccess 0) (> iFail 0))
          (strcat
            (itoa iSuccess) " successful, "
            (itoa iFail) " not successful."
          );END strcat
        );END subcond
        ((= iFail 0)
          "All successful."
        );END subcond
        ((= iSuccess 0)
          "None successful."
        );END subcond
      );END cond
    )
  )
  (princ)

  ; v0.0 - 2017.01.19 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.01.19
);END defun
(defun Convert3DpolyTo2Dpoly ( ent_name / coordinateList )
  ; Extract 3D coordinates
  (if (setq
        coordinateList (Get3DpolylineCoordinateList ent_name)
        3DpolyOpenClose
          (cond
            ((= :vlax-false (vla-get-closed (vlax-ename->vla-object ent_name))) 0 )
            ((= :vlax-true (vla-get-closed (vlax-ename->vla-object ent_name)))  1 )
          );END cond
      );END setq
    ; Create 2Dpoly
    (if (CreateLWPOLYINE coordinateList 3DpolyOpenClose)
      (progn
        (vla-delete (vlax-ename->vla-object ent_name))
        T
      );END progn
      (princ "\nERROR @ Convert3DpolyTo2Dpoly: (CreateLWPOLYINE) = nil")
    );END if
    (princ "\nERROR @ Convert3DpolyTo2Dpoly: (Get3DpolylineCoordinateList) = nil")
  );END if
)
(defun Get3DpolylineCoordinateList ( ent_name / l m)
  ; Return 3D polyline coordinate list
  (mapcar
    '(lambda ( x )
      (if (or (< (length l) 3) (not l)) ; si hay menos de 3 elementos en la lista
        (setq l (append l (list x)) )
        (setq
          m (append m (list l)) ; main list
          l (list x)
        )
      );END if
    );END lambda
    (vlax-safearray->list (vlax-variant-value
      (vla-get-Coordinates (vlax-ename->vla-object ent_name)) ))
  );END mapcar
  (append m (list l))
)
(defun CreateLWPOLYINE (l cl)
  (entmakex
    (append
      (list
        (cons 0 "LWPOLYLINE")
        (cons 100 "AcDbEntity")
        (cons 100 "AcDbPolyline")
        (cons 90 (length l))
        (cons 70 cl)
      );END list
      (mapcar '(lambda (p) (cons 10 p)) l)
    );END append
  );END entmakex
)
(defun DT:Gradient ( pA pB )
  ; Return gradient [real] between pA and pB, being the slope expressed as 1/gradient
  ; pA [pt] - 3D point of reference
  ; pB [pt] - 3D point of reference
  (if (and pA pB)
    (progn ; True: 2 no-nil arguments passed
      (if (and (= 'LIST (type pA)) (= 'LIST (type pB)) )
        (progn ; True: 2 list-type arguments passed
          (if (and (= 3 (length pA)) (= 3 (length pB)) )
            ; True: 2 point-list type arguments passed
            ; Return gradient value as real
            (/ (distance (DT:flatPoint pA) (DT:flatPoint pB)) (- (nth 2 pB) (nth 2 pA)))
            ; False: not 2 point-list-type arguments passed
            (cond
              ((/= 3 (length pA)) (princ "\nERROR @ DT:int : pA is not a point")(princ) )
              ((/= 3 (length pB)) (princ "\nERROR @ DT:int : pB is not a point")(princ) )
            );END cond
          );END if
        );END progn
        (cond
          ((/= 'LIST (type pA)) (princ "\nERROR @ DT:int : pA is not a list")(princ) )
          ((/= 'LIST (type pB)) (princ "\nERROR @ DT:int : pB is not a list")(princ) )
        );END cond
      );END if
    );END progn
    (cond
      ((not pA) (princ "\nERROR @ DT:int : pA=nil")(princ) )
      ((not pB) (princ "\nERROR @ DT:int : pB=nil")(princ) )
    );END cond
  );END if

  ; v0.0 - 2017-01-28 - First issue
  ; Author: David Torralba
  ; Last revision: 2017-01-28
)
