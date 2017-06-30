(defun GetFromClipboard(/ html result)
  (setq html (vlax-create-object "htmlfile")
	result (vlax-invoke (vlax-get (vlax-get html 'ParentWindow) 'ClipBoardData) 'GetData "Text")
  )
  (vlax-release-object html)
  result
)
;Get serial number of the current product
(getvar "_pkser")
(defun c:xx ( / ss totalArea pt )
  ; Insert text whith total area of selected objects
  (if (setq ss (ssget))
    (if (setq totalArea (DT:TotalArea ss))
      (if (setq ent_name (DT:DrawText (setq pt (cadr (grread 't))) (getvar "clayer") (strcat (LM:rtos totalArea 2 2) "m2") 1 0 ))
        (command "_.move" ent_name "" "_non" pt "_non" pause)
      );END if
    );END if
  );END if

  ; Return total area
  (if totalArea totalArea)

  ; v0.0 - 2017.04.18 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.04.18
)
(defun c:kk()
  ; Sustituye la primera letra de
  ; todos aquellos textos que esten en la capa "e-pfd" o "e-pfd-adoptable-lateral"
  ; que tengan 6 caracteres de longitud
  ; y que empiecen por la letra S.
  (setq
    ss (ssget "x" '(
                    (-4 . "<AND")
                      ( 0 . "TEXT")
                      (-4 . "<OR")
                        ( 8 . "e-pfd")
                        ( 8 . "e-pfd-adoptable-lateral")
                      (-4 . "OR>")
                    (-4 . "AND>")
                   )
       )
    ssl (sslength ss)
    i 0
  ); END setq

  (while (< i ssl)
    (setq ent (ssname ss i))
    (setq txt (cdr (assoc 1 (entget ent)) ) )
    (if (and (= "S" (substr txt 1 1)) (= 6 (strlen txt)))
      (progn
        (setq
          entList (entget ent)
          new_txt (strcat "F" (substr txt 2))
          entList (subst (cons 1 new_txt)(assoc 1 entList) entList)
        )
        (entmod entList)
      )
    );END if
    (setq i (+ 1 i))
  ); END while
  (princ)
)
(defun c:1 ( /
									ent p0 p1 pm12 d level
								)
  ;Calculate cross section average level given AREA, and base line.
	(setvar "OSMODE" 1)
  (setq
    ent_name (car (entsel "\nSelect closed polyline: "))
    p0 (getpoint "\nSelect section bottom-left point: ")
    p1 (getpoint "\nSelect section bottom-right point: ")
  )
  (setvar "OSMODE" 512)
  (setq
    d (distance p0 p1)
    pm12 (polar p0 (angle p0 p1) (* 0.5 d))
  )
	(if (/= ent_name nil)
    (progn
      (command "area" "Object" ent_name)
      (setq level (+ 58 (/ (getvar "Area") d)))
      (command "-text" "S" "ARIAL" "J" "BC" pm12 "0.5" "" (rtos level 2 5))
    ); END progn
  ); END if

  (CopyToClipboard (rtos level 2 5))
	(princ "\nlevel = ")(princ level)(princ "  \(copied to clipboard\)")
  (setvar "OSMODE" 33)
	(princ)
)
(defun c:1( / ent VL_ent_name pt ch)
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
    (CopyToClipboard (LM:rtos ch 2 5))
  );END while
  (princ)
)
; Mark level in long section. selecciona texto con cota, y pica punto en el datum para que dibuje polilinea polyline vertical
; Check: datum
(defun c:3( / datum exageration dist p k)
  (setq
    datum 70
    exageration 10
    espacio_caja 42 ; midelo y picalo aqui
    dist (+ espacio_caja (* (- (atof (cdr (assoc 1 (entget (car (entsel)))))) datum) exageration))
    p (getpoint "\nPick a point on datum: ")
  )
  (command "PLINE" p (polar p (* 0.5 pi) dist) "")
  (princ)
)
;EngBase S38 hatches
(defun c:1() (princ "\nRoad")     (command "-hatch" "LA" "e-sec38-road"     "CO" "." "." "P" "SOLID" "1" "" (getpoint) "")(princ))
(defun c:2() (princ "\nFootpath") (command "-hatch" "LA" "e-sec38-footpath" "CO" "." "." "P" "SOLID" "1" "" (getpoint) "")(princ))
(defun c:3() (princ "\nVerge")    (command "-hatch" "LA" "e-sec38-verge"    "CO" "." "." "P" "SOLID" "1" "" (getpoint) "")(princ))
(defun c:1()
; Draw kerbing polyline with 0.2 width and 150 color
  (princ "\nPOLYLINE. Width 0.2 and color 150")
  (setvar "PLINEWID" 0.2)
  (command "-color" 150 "_pline" )
  (princ)
)
(defun c:2()
; Draw kerbing polyline with 0.2 width and 211 color
  (princ "\nPOLYLINE. Width 0.2 and color 211")
  (setvar "PLINEWID" 0.2)
  (command "-color" 211 "_pline")
  (princ)
)
(defun c:3()
; Draw kerbing polyline with 0.2 width and 51 color
  (princ "\nPOLYLINE. Width 0.2 and color 51")
  (setvar "PLINEWID" 0.2)
  (command "-color" 51 "_pline")
  (princ)
)
(defun c:1()
  (command "_pline" pause pause "")
  (command "offset" 0.125 (entlast) pause "")
  (princ)
)
(defun c:1( / VL_ent_name )
; Insert DK block perpendicular to selected polyline, infinite loop
  (setq VL_ent_name (vlax-ename->vla-object (car (entsel "\nSelect centerline: "))))
  (while (not kkkk)
    (DT:BlockPerpendicularToPolyline VL_ent_name "MarkerBlock")
  )
)
(defun c:2( / VL_ent_name )
; Insert DK block perpendicular to selected polyline, infinite loop
  (setq VL_ent_name (vlax-ename->vla-object (car (entsel "\nSelect centerline: "))))
  (DT:BlockPerpendicularToPolyline VL_ent_name "MarkerBlock")
)
(defun c:1() (c:3DPT)); Edit 3d polyline vertex levels

(defun c:1() (princ "\nRTM: ")(c:RTM) (c:00))
(defun DT:VLA_ent_name ( msg ) (vlax-ename->vla-object (car (entsel msg))))
(defun c:2( / z VL_ent_name)
; SimpleDIP + result to ClipBoard + overwrite text (underlined) + text color ByLayer
  (princ "\nSDIP: ")
  (setq
    z (princ (CopyToClipboard (LM:rtos (DT:SDIP) 2 3)))
    VL_ent_name (DT:VLA_ent_name "\nSelect text to copy result: ")
  )
  (vlax-put-property VL_ent_name 'Color 256)
  (vla-put-textstring VL_ent_name (strcat "%%U" z))
  (princ)
)
(defun c:3 ()
  (princ "\nHeigh at long section (vertical exageration = x10): ")
  (CopyToClipboard (LM:rtos (+ 70 (* 0.1 (distance (getpoint "\nPunto inicial: ") (getpoint "\nPunto final: ")))) 2 5))
)
(defun c:4() (princ "\nBY: ") (c:BY))
(defun c:5() (princ "\n+ 50 + 50") (CopyToClipboard (LM:rtos (+ 0.05 (+ 0.05 (DT:clic_or_type_level))) 2 3)))
(defun c:6() (princ "\n+ 50 + 25") (CopyToClipboard (LM:rtos (+ 0.025 (+ 0.05 (DT:clic_or_type_level))) 2 3)))
(defun c:9()
; Set blue color to selected object
(vlax-put-property (vlax-ename->vla-object (car (entsel "\nSelect object to be blue: "))) 'Color 5))
(defun c:00(/ ss ) (setq ss (ssget "p")) (command "._SetByLayer" ss "" "y" "y"))
(defun c:9( / VL_ent_name )
  (setq VL_ent_name (vlax-ename->vla-object (car (entsel "\nSelect centerline: "))))
    ;(vlax-property-available-p VL_ent_name 'explode)
    ;(vlax-get-property VL_ent_name 'Color)
    ;(vla-put-layer VL_ent_name "e-set-out-house")
    ;(vlax-put-property (vlax-ename->vla-object (car (entsel))) 'Color 256)
    ;(vlax-put-property (vlax-ename->vla-object (car (entsel))) 'ConstantWidth 1.5)
    (vlax-put-property VL_ent_name 'Color 5); Set color by layer
  )
  (vlax-put-property VL_ent_name 'Color 256); Set color by layer
  (vlax-get-property VL_ent_name 'Color)

(defun c:8( / z0 zdiff VL_ent_name)
  ; Toma el valor del primer objeto seleccionado, opera y sustituye el del segundo objeto seleccionado (y pone su color ByLayer)
  (setq zdiff 0.0625)
  (princ "\n+")(princ zdiff)
  (setq
    z0 (DT:clic_or_type_level)
    VL_ent_name (vlax-ename->vla-object (car (entsel "\nSelect target object: ")))
  )
  (vlax-put-property VL_ent_name 'Color 256); Set color by layer
  (vla-put-textstring VL_ent_name (strcat "%%U" (LM:rtos (+ z0 zdiff) 2 3)))
  (princ)
)
(defun c:7( / z0 zdiff VL_ent_name)
  ; Toma el valor del primer objeto seleccionado, opera y sustituye el del segundo objeto seleccionado (y pone su color ByLayer)
  (setq zdiff (+ 0.050 0.050))
  (princ "\n+")(princ zdiff)
  (setq
    z0 (DT:clic_or_type_level)
    VL_ent_name (vlax-ename->vla-object (car (entsel "\nSelect target object: ")))
  )
  (vlax-put-property VL_ent_name 'Color 256); Set color by layer
  (vla-put-textstring VL_ent_name (strcat "%%U" (LM:rtos (+ z0 zdiff) 2 3)))
  (princ)
)
;
;
;

; PENDIENTE! Sin acabar ------------------------------------------------------------------------------------------------------- PENDIENTE!
(defun c:TextTo3DPoint()
(defun c:1()
  (setq
    p (getpoint "\nSelect point: ")
    z (DT:clic_or_type_level)
  )
  (command "point" (list (car p) (cadr p) z))
)
(defun c:1()
; No tengo claro por qué no funciona...creo que es la instrucción (command)
  (setq
    p (reverse (append (list 1 (DT:clic_or_type_level)) (cdr (reverse (getpoint "\nSelect point: ")))))
  )
  (command "point" p)
)
; PENDIENTE! Sin acabar ------------------------------------------------------------------------------------------------------- PENDIENTE!
;
;
;(vlax-put-property (vlax-ename->vla-object (car (entsel))) 'ConstantWidth 0.3)
;(vlax-get-property (vlax-ename->vla-object (car (entsel))) 'Rotation)
;(vlax-get-property (vlax-ename->vla-object (car (entsel))) 'InsertionPoint)
;(vlax-safearray->list (vlax-variant-value (vlax-get-property (vlax-ename->vla-object (car (entsel))) 'InsertionPoint)))
;vlax-get B_TEMP 'InsertionPoint
(defun c:7( / z0 zdiff VL_ent_name)
  ; Toma el valor del primer objeto seleccionado, opera y sustituye el del segundo objeto seleccionado (y pone su color ByLayer)
  (setq zdiff (+ 0.050 0.050))
  (princ "\n+")(princ zdiff)
  (setq
    z0 (DT:clic_or_type_level)
    VL_ent_name (vlax-ename->vla-object (car (entsel "\nSelect target object: ")))
  )
  (vlax-put-property VL_ent_name 'Color 256); Set color by layer
  (vla-put-textstring VL_ent_name (strcat "%%U" (LM:rtos (+ z0 zdiff) 2 3)))
  (princ)
)
(defun c:1()
  ; Move part-m individually
  (DT:OffsetPartM (car (entsel "\nSelect part-m to move: ")))
)
(defun c:111()
  ; Move part-m in group
  (princ "\nPART-M BLOCKS WILL BE FILTERED\nSelect part-m to move: ")
  (command "._UNDO" "_Begin")
  (foreach a (ssnamex (ssget))
    (if (= 'ename (type (cadr a)))
      (if (= "INSERT" (cdr (assoc 0 (entget (cadr a)))) )
        (DT:OffsetPartM (cadr a))
      );END if
    );END if
  );END foreach
  (command "._UNDO" "_End")
)
(defun c:7( / VL_ent_name LastParam x1 xL arr larr newlarr)
  ; Eliminar vertices duplicados (solo mira primer y ultimo vertice)
  (setq
    VL_ent_name (vlax-ename->vla-object (car (entsel "\nSelect object: ")))
    LastParam (- (vlax-curve-getEndParam VL_ent_name) 1)
    x1 (LM:rtos (car (vlax-curve-getPointAtParam VL_ent_name 0)) 2 3)
    xL (LM:rtos (car (vlax-curve-getPointAtParam VL_ent_name LastParam)) 2 3)
  )
  (princ "\nLastParam = ")(princ LastParam)
  (princ "\nx1 = ")(princ x1)
  (princ "\nxL = ")(princ xL)
  (if (= x1 xL)
    (progn
      ;(alert "COINCIDEN")
      (setq
        ; OPERATION - Save coordinates array and convert it to a list
        arr (vlax-variant-value (vla-get-Coordinates VL_ent_name))
        larr (vlax-safearray->list arr)
      )
      (princ "\nlarr = ")(princ larr)(princ "\n")

      (vlax-safearray-put-element arr (fix (+ 0 (* 3 LastParam))) (- (car (vlax-curve-getPointAtParam VL_ent_name LastParam)) 5))
      ;(vlax-safearray-put-element arr (fix (+ 1 (* 3 LastParam))) nil)
      ;(vlax-safearray-put-element arr (fix (+ 2 (* 3 LastParam))) nil)

      (setq larr (vlax-safearray->list arr))
      (princ "\nnewlarr = ")(princ larr)

      (vlax-put-property VL_ent_name 'Coordinates arr)
    )
    (vlax-put-property VL_ent_name 'Color 256)
  )
  (princ)
)
(defun c:8 ( / *error* VL_ent_name arr narr larr nz z oldosmode)
	; EDIT 3D polyline vertex levels and input clicking text or typing (error when stopping yet)
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
        )
        (princ (strcat "\nZ value <" (LM:rtos z 2 3) ">: "))
        (setq
          ;nz (getreal (strcat "\nZ value <" (LM:rtos z 2 3) ">: "))
          nz (DT:clic_or_type_level)
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

;LEVELS shorcut set 2016.05.23
(defun c:1() (princ "\nRTM: ")(c:RTM))
(defun c:2() (princ "\nINT: ") (c:INT))
(defun c:3() (princ "\nSDIP: ") (c:sDIP))
(defun c:4() (princ "\nDIP: ") (c:DIP))
(defun c:5() (princ "\ngarden_gradient: ") (c:garden_gradient))
(defun c:6() (command "-insert" "PI_DT" pause "0.25" "0.25" "0" (getstring) "") (vla-put-layer (vlax-ename->vla-object (entlast)) "TEMP garden 3m"))
(defun c:7() (fbi "3x2_Slab45"))
(defun c:8() (while (not kkkk) (vla-put-textstring (vlax-ename->vla-object (car (entsel "\nSelect destiny: "))) (strcat "%%U" (LM:rtos (DT:clic_or_type_level) 2 2)))))
(defun c:9() (vlax-put-property (vlax-ename->vla-object (car (entsel "\nSelect object to be magenta: "))) 'Color 6))
(defun c:0() (fbi "TEMP01"))
(defun c:00(/ ss ) (setq ss (ssget "p")) (command "._SetByLayer" ss "" "y" "y"))
;(defun c:0() (vla-put-layer (vlax-ename->vla-object (car (entsel "\nSelect object to move: "))) "e-pfd-adoptable-lateral"))


;DRAINAGE shorcut set 2016.05.24
(defun c:1() (princ "\nRTM: ")(c:RTM))
(defun c:2() (princ "\nINT: ") (c:INT))
(defun c:3() (princ "\nSDIP: ") (c:sDIP))
(defun c:4() (princ "\nDIP: ") (c:DIP))
(defun c:5( / txt) (princ "\nPermeable subbase: ")
  (princ (CopyToClipboard (setq txt (strcat "SB" (LM:rtos (- (DT:clic_or_type_level) 0.392) 2 2)))))
  (vla-put-textstring (vlax-ename->vla-object (car (entsel "\nSelect destiny: "))) txt)
)
(defun c:6() (command "-insert" "PI_DT" pause "0.25" "0.25" "0" (getstring) "") (vla-put-layer (vlax-ename->vla-object (entlast)) "TEMP garden 3m"))
(defun c:7() (fbi "GULLY-DT"))
(defun c:8() (while (not kkkk) (vla-put-textstring (vlax-ename->vla-object (car (entsel "\nSelect destiny: "))) (strcat "%%U" (LM:rtos (DT:clic_or_type_level) 2 2)))))
(defun c:9() (vlax-put-property (vlax-ename->vla-object (car (entsel "\nSelect object to be blue: "))) 'Color 2))
(defun c:00(/ ss ) (setq ss (ssget "p")) (command "._SetByLayer" ss "" "y" "y"))
(defun c:xx() (vla-put-layer (vlax-ename->vla-object (car (entsel "\nSelect object to move to to layer \"e-asd-Phase2\": "))) "e-asd-Phase2"))

;3D MODEL BUILDING shorcut set 2016.06.01
(defun c:1() (princ "\n3D polyline: ")(command "3DPOLY"))
(defun c:2() (princ "\nINT: ") (c:INT))
(defun c:3() (command "-view" "O" "T"))
(defun c:4() (command "3dorbit"))
(defun c:5() (princ "\nKTF - 3D Offset: ")(c:KTF_3DOFFSET) )
(defun c:6() (princ "\nKTF - Add vertices to polyline: ")(c:KTF_3DPLADVX) )
(defun c:7() (princ "\nInsert PI block: ") (command "-insert" "PI_DT" pause "0.25" "0.25" "0" (LM:rtos (DT:clic_or_type_level) 2 3) "") (vla-put-layer (vlax-ename->vla-object (entlast)) "e-proposed-3d"))
(defun c:9() (vlax-put-property (vlax-ename->vla-object (car (entsel "\nSelect object to be yellow: "))) 'Color 5))
(defun c:1() (princ "\nRTM: ")(c:RTM))
(defun c:7( / p z)
  (setq
    p (cdr (assoc 10 (entget (car (entsel)))))
    z (LM:rtos (- (DT:clic_or_type_level) 0.5) 2 3)
  )
  (command "-insert" "PI" p "0.25" "0.25" "0" z "")
)
(defun c:8( / ent_name)
  (princ "\nReduce elevation 150mm")
  (setq ent_name (car (entsel "\nSelect object to reduce elevation: ")) )
  (command "move" ent_name "" (list 0 0 0) (list 0 0 -0.150) )
  (vlax-put-property (vlax-ename->vla-object ent_name) 'Color 2)
  (princ)
)
(defun c:5( / VL_ent_name)
  (while (not kkkk)
    (setq VL_ent_name (vlax-ename->vla-object (car (entsel "\nSelect object to rise: "))))
    (vla-put-elevation VL_ent_name (DT:clic_or_type_level))
    (vlax-put-property VL_ent_name 'Color 256)
    (setq VL_ent_name nil)
  )
)


; Setting out shortcut set 2016.06.10
(defun c:1( / ss)
  (princ "\nMark in yellow no-closed polylines:")
  (setq ss (ssget) )
  (foreach a (ssnamex ss)
    (if (= 'ename (type (cadr a)))
      (if (= :vlax-false (vla-get-closed (vlax-ename->vla-object (cadr a))))
        (vlax-put-property (vlax-ename->vla-object (cadr a)) 'Color 2)
        (princ "\nClosed polyline.")
      ); END if2
    );END if1
  );END foreach
  (princ)
)
(defun c:2() (princ "\nIndividual setting out: ") (c:coord) )
(defun c:3() (princ "\nPolyline setting out: ") (c:KTF_SET) )
(defun c:4() (princ "\nMove setting out label: ") (c:DT:move_KTF_SettingOutLabel) )
(defun c:5() (princ "\nAligned dimension: ") (command "_dimaligned") )
(defun c:6() (princ "\nRadius dimension: ") (command "_dimradius" pause "") )
(defun c:7( / ss ) (princ "\nReset dimension position:") (setq ss (ssget '((0 . "DIMENSION")))) (command "_DIM1" "HOME" ss "") )

(defun c:1( / xy z xyz)
  (setq
    z (DT:clic_or_type_level)
    xy (cdr (assoc 10 (entget (car (entsel "\nSelect object to extract coordinates: ")))))
    xyz (list (car xy) (cadr xy) z)
  )
  (princ "\nCoordinates = ")(princ xyz)
  (command "_point" xyz)
  (princ)
)
(defun c:1( / ss xy z xyz)
  (princ "\nConvert texts to 3d points:")
  (setq ss (ssget '((-4 . "<and") (0 . "TEXT") (50 . 0.0) (-4 . "and>") )) )
  (foreach a (ssnamex ss)
    (if (= 'ename (type (cadr a)))
      (progn
        ;(setq
        ;  xy (cdr (assoc 10 (entget (cadr a))))
        ;  z (vla-get-textstring (vla-get-closed (vlax-ename->vla-object (cadr a))))
        ;  xyz (list (car xy) (cadr xy) z)
        ;)
        ;(princ "\nCoordinates = ")(princ xyz)
        (princ "\n.")(princ "\n.")(princ a)
      )
    );END if1
  );END foreach
  (princ)
)

; Manchole Schedule shorcut set 2016.06.15
(defun c:1() (c:MSCS))
(defun c:2() (vla-put-layer (vlax-ename->vla-object (car (entsel "\nSelect object to move to e-manhole-schedule layer: "))) "e-manhole-schedule") (princ))
(defun c:3() (vla-put-layer (vlax-ename->vla-object (car (entsel "\nSelect object to move to e-manhole-schedule-TEMP layer: "))) "e-manhole-surround") (princ))
(defun c:4( / IL DN x)
  (setq
    IL (DT:clic_or_type_level)
    DN (* 0.001 (getreal "Introduce pipe DN:"))
    x (- (+ IL DN) 0.15 )
  )
  (princ "\nIL = ")(princ IL)
  (princ "\nDN = ")(princ DN)
  (princ "\n x = ")(princ x)(princ "\n >>> ")
  (CopyToClipboard (LM:rtos x 2 3))
)

; SEC38 layout shorcut set 2016.06.15
(defun c:1() (princ "\nSec38 road:")
  (setvar "hpname" "SOLID")
  (command "-hatch" "A" "A" "N" "" "LA" "." "CO" "." (getpoint) "")
  (vla-put-layer (vlax-ename->vla-object (entlast)) "e-sec38-road")
  (princ)
)
(defun c:2() (princ "\nSec38 footpath:")
  (setvar "hpname" "SOLID")
  (command "-hatch" "A" "A" "N" "" "LA" "." "CO" "." (getpoint) "")
  (vla-put-layer (vlax-ename->vla-object (entlast)) "e-sec38-footpath")
  (princ)
)
(defun c:3() (princ "\nSec38 verge:")
  (setvar "hpname" "SOLID")
  (command "-hatch" "A" "A" "N" "" "LA" "." "CO" "." (getpoint) "")
  (vla-put-layer (vlax-ename->vla-object (entlast)) "e-sec38-verge")
  (princ)
)
(defun c:1() (command "move" pause "" (cadr (grread 't)) pause) )
(defun c:3() (vla-put-layer (vlax-ename->vla-object (car (entsel "\nSelect object to move to e-3D-proposed: "))) "e-3D-proposed") (princ))
(defun c:4() (while (not kkkk) (vla-put-layer (vlax-ename->vla-object (car (entsel "\nSelect object to move to e-3D-proposed: "))) "e-3D-proposed-TEMP")))
; 3D MODEL (GARDENS) shortcut 2016.06.21
(defun c:1() (command "3DPOLY"))
(defun c:2() (princ "\nInsert PI block: ") (command "-insert" "PI_DT" pause "0.25" "0.25" "0" (LM:rtos (DT:clic_or_type_level) 2 3) "") (vla-put-layer (vlax-ename->vla-object (entlast)) "e-proposed-3d"))
(defun c:3() (c:3DPT))
(defun c:4() (c:KTF_3DPLADVX))
(defun c:5( / pt0)
  ; Mete un circulo en el punto clicado y recorta lo que haya en él.
  (setq pt0 (getpoint))
  (command "circle" pt0 "0.1")
  (command "trim" "L" "" "C" (polar pt0 -0.785 0.05) (polar pt0 2.355 0.05) "")
  (command "zoom" "S" "0.6")
)
; Localizar puntos encontrar puntos find points marcándolos con un circulo
(setq ptl
  (list
    '(451047.897 189279.906)
    '(451052.742 189284.074)
  )
)
(foreach a ptl
  (command "circle" a "0.1")
)
(defun c:PI_block_to_POINT( / mspace)
  ; Creates a point where PI blocks where, using as Z coordinate the "LEVEL" indicated in their attribute
  (vl-load-com)
  (setq mspace (vla-get-modelspace (vla-get-activedocument (vlax-get-acad-object))))
  (foreach a (ssnamex (ssget '((0 . "INSERT"))))
    (if (= 'ename (type (cadr a)))
      (vla-AddPoint
        mspace
        (vlax-3d-point
          (list
            (car (cdr (assoc 10 (entget (cadr a)))))
            (cadr (cdr (assoc 10 (entget (cadr a)))))
            (atof
              (vl-some
                '(lambda ( att ) (if (= "LEVEL" (strcase (vla-get-tagstring att))) (vla-get-textstring att)))
                (vlax-invoke (vlax-ename->vla-object (cadr a)) 'getattributes)
              );END vl-some
            );END atof
          );END list
        );END vlax-3d-point

      );END vla-move     vla-AddPoint
    );END if
  );END foreach
  (princ)
  ; v0.0 - 2016.06.21
  ; Author: David Torralba
  ; Last revision: 2016.06.21
)
(defun c:1() (princ "\nFast hatch (no SOLID):")
  (setvar "hpname" "ANGLE")
  (command "-hatch" "A" "A" "N" "" "LA" "." "." "CO" "." (getpoint) "")
  (princ)
)
(defun c:block_to_POINT( / mspace)
  ; Creates a point where PI blocks are, and removes the block after
  (vl-load-com)
  (setq mspace (vla-get-modelspace (vla-get-activedocument (vlax-get-acad-object))))
  (foreach a (ssnamex (ssget '((0 . "INSERT"))))
    (if (= 'ename (type (cadr a)))
      (progn
        (vla-AddPoint
          mspace
          (vlax-3d-point
            (list
              (car (cdr (assoc 10 (entget (cadr a)))))
              (cadr (cdr (assoc 10 (entget (cadr a)))))
              (caddr (cdr (assoc 10 (entget (cadr a)))))
            );END list
          );END vlax-3d-point
        );END vla-AddPoint
        (vla-delete (vlax-ename->vla-object (cadr a)))
      );END progn
    );END if
  );END foreach
  (princ)
  ; v0.0 - 2016.06.24
  ; Author: David Torralba
  ; Last revision: 2016.06.24
)
(defun c:POINT_where_Object( / ent_name z )
  ;Takes seleted object coordinates and text coordinates, and it creates a point, in blue color with those coordinates. Deletes de initial object.
  (vl-load-com)
  (setq
    mspace (vla-get-modelspace (vla-get-activedocument (vlax-get-acad-object)))
    ent_name (car (entsel "\nSelect object: "))
  )
  (if (= 0 (caddr (cdr (assoc 10 (entget ent_name)))))
    (setq z (DT:clic_or_type_level))
    (setq z (caddr (cdr (assoc 10 (entget ent_name)))))
  );END if
  (vla-AddPoint mspace
    (vlax-3d-point
      (list
        (car (cdr (assoc 10 (entget ent_name))))
        (cadr (cdr (assoc 10 (entget ent_name))))
        z
      );END list
    );END vlax-3d-point
  );END vla-AddPoint
  (vla-delete (vlax-ename->vla-object ent_name))
  (vlax-put-property (vlax-ename->vla-object (entlast)) 'Color 5)
)
(defun c:1() (while (not kkkk) (c:POINT_where_Object)))
(defun c:1( / layer_VL_ent_name)
  ;Set desired layer style
  (setq
    layer_name "e-psd"
    layer_color 210
    layer_linetype "DASHED"
    style_file "acad.lin"
    layer_description "Private storm drainage"
  )
  ;OPERATION - Check if layer exists
  (if (setq lay_flag (tblsearch "layer" layer_name))
    (progn
      (setq layer_VL_ent_name (vla-item (vla-get-layers (vla-get-ActiveDocument (vlax-get-acad-object))) layer_name))
      (if (/= layer_color (cdr (assoc 62 lay_flag))) (vla-put-color layer_VL_ent_name layer_color))
      (if (/= layer_linetype (cdr (assoc 6 lay_flag)))
        (progn
          (if (not (tblobjname "ltype" layer_linetype)) (vla-load (vla-Get-Linetypes (vla-get-ActiveDocument (vlax-get-acad-object))) "DASHED" "acad.lin"))
          (vla-put-linetype layer_VL_ent_name layer_linetype)
        )
      );END if
    ); END progn
    ; FALSE -
    (progn
      (setq layer_VL_ent_name (vla-add (vla-get-layers (vla-get-ActiveDocument (vlax-get-acad-object))) layer_name))
      (vla-put-color layer_VL_ent_name layer_color)
      (if (not (tblobjname "ltype" layer_linetype)) (vla-load (vla-Get-Linetypes (vla-get-ActiveDocument (vlax-get-acad-object))) "DASHED" "acad.lin"))
      (vla-put-linetype layer_VL_ent_name layer_linetype)
      (vla-put-Description layer_VL_ent_name layer_description)
    ); END progn
  );END if
  (princ)
  ; v0.0 - 2016.06.28 - First issue
  ; Author: David Torralba
  ; Last revision: 2016.06.28
)
(defun c:2( / mspace ent_name a z txt_p)
  ; This loop creates a blue point where the original point was, gives it text's z value, and removes the original point after
  (vl-load-com)
  (setq
    mspace (vla-get-modelspace (vla-get-activedocument (vlax-get-acad-object)))
    ;ent_name (car (entsel "\nSelect object: "))
  )
  (foreach a (ssnamex (ssget '((0 . "POINT"))))
    (if (= 'ename (type (cadr a)))
      (progn
        (princ "\nObject found")
        (setq
          ent_name (cadr a) ; BORRAR SI NO ES NECESARIO
          pt_p (cdr (assoc 10 (entget ent_name)))
          txt_p (list (+ (car pt_p) -135) (+ (cadr pt_p) 315) 0.0)
        )
        (princ "OK1 ")
        (if (= 0 (caddr (cdr (assoc 10 (entget ent_name)))))
          (setq z (atof (cdr (assoc 1 (entget (car (nentselp txt_p)))))) )
          (setq z (caddr (cdr (assoc 10 (entget ent_name)))))
        );END if
        (princ "OK2 ")
        (princ "\ntype z = ")(princ (type z))
        (if (/= z 0)
          (progn
            (vla-AddPoint mspace
              (vlax-3d-point
                (list
                  (car (cdr (assoc 10 (entget ent_name))))
                  (cadr (cdr (assoc 10 (entget ent_name))))
                  z
                );END list
              );END vlax-3d-point
            );END vla-AddPoint
            (vla-delete (vlax-ename->vla-object ent_name))
            (vlax-put-property (vlax-ename->vla-object (entlast)) 'Color 5)
          );END progn2
        );END if2
      );END progn
    );END if1
  );END foreach
  (princ)
  ; v0.0 - 2016.06.28 - First issue
  ; Author: David Torralba
  ; Last revision: 2016.06.28
)
(defun c:1( / ent_list a txt)
  ; Loop throuth selected texts rounding from 3 to 2 decimals.
  (foreach a (ssnamex (ssget '((0 . "TEXT"))))
    (if (= 'ename (type (cadr a)))
      (progn
        (setq
          ent_list (entget (cadr a))
          txt (strcat "%%U" (LM:rtos (atof (substr (cdr (assoc 1 (entget (cadr a)))) 4)) 2 2))
          ent_list (subst (cons 1 txt) (assoc 1 ent_list) ent_list)
        )
        (princ "\n")(princ txt)
        (entmod ent_list)
      );END progn
    );END if1
  );END foreach
  (princ)
)
(defun c:1( / tablist)
  (setq tablist
    '(
      "P01 Engineering Layout"
      "C10 Longitudinal Sections"
      "C11 Cross Sections"
      "C20 Construction Details"
      "C21 Drainage Details"
      "C22 Private Soakaway Details"
      "C30 House Setting Out"
      "C31 Road Setting Out"
      "C70 Offsite FWS"
      "C71 Highway Soakaway Detail"
      "C80 Street Lighting Layout"
      "C90 Swept Path Analysis"
      "C510 Section 104 Layout"
      "C511 Manhole Schedule"
      "512 SW Manhole Schedule"
    )
  )
  (foreach tab tablist
    (princ "\n")(princ tab)
    (setvar "CTAB" tab)
    (alert)
    (command
    "-plot"
    "No"  ; Don't plot detailed configuration
    ""    ; Accept layout default name
    ""    ; Accept default page setup name
    "Bluebeam PDF" ; Printer name
    "No"  ; Write the plot into a file
    "No"  ; Don't safe changes on page setup
    "Yes" ; Proceed with the plot
    )
  )
  (princ)
)
(defun c:3() (while (not kkkk) (bsus "Part-m-primary-0")))
(defun bsus( b2 / b1 pb1 rot1 )
  ; Block Substitution: select a block and substitute it (same position and rotation) by block with "b2" block_name
  (setq
    b1 (car (entsel))
    pb1 (cdr (assoc 10 (entget b1)))
    rot1 (vlax-get-property (vlax-ename->vla-object b1) 'Rotation)
  )
  ; OPERATION - Correct angle
  (if (= 1 (getvar "angdir")) (setq rot1 (- 0 rot1)))
  (setq rot1 (/ (* 180 rot1) pi))
  (command "-insert" b2 pb1 "1" "1" rot1)
  (vla-delete (vlax-ename->vla-object b1))
  (princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Cross section functions
(defun c:1() (c:distance_to_centerline))
(defun c:2() (c:heigh_minus_datum))
(defun c:distance_to_centerline()
  (CopyToClipboard (LM:rtos (distance (getpoint "\nSelect point 1: ") (getpoint "\nSelect point 2: \n")) 2 3))
)
(defun c:heigh_minus_datum( / p d )
  (setq
    oldosmode (getvar "osmode")
    d (- (atof (DT:destripar_txt)) 12)
    p (getpoint "\nSelect point at datum")
  )
  (setvar "osmode" 0)
  (command "line" p (polar p (/ pi 2) d) "")
  (setvar "osmode" oldosmode)
)
(defun c:1() (while (not kkkk) (vla-put-layer (vlax-ename->vla-object (car (entsel "\nSelect object to move to to layer \"e-sewerbox-foul\": "))) "e-sewerbox-foul")))
(defun c:2() (while (not kkkk) (vla-put-layer (vlax-ename->vla-object (car (entsel "\nSelect object to move to to layer \"e-sewerbox-storm\": "))) "e-sewerbox-storm")))(
(defun c:ltp ( / ss i p1 p2)
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
(defun c:1() (while (not kkkk) (vla-put-layer (vlax-ename->vla-object (car (entsel "\nSelect object to move to to layer \"e-cf-EG3-TIN-bund-bottom\": "))) "e-cf-TEMP")))
(defun DT:clic_or_type_level (/ in x nent txt VL_ent_name)
  ; Clic on any atribute or text with a level and return its text, or type it in
  (setq in (DT:input_string_or_point))
  (cond
    ((= 'LIST (type in)) ; it's a point
      (setq nent (nentselp in))
      (if (not nent)
        (setq x "nothing_selected")
        (progn
          (setq VL_ent_name (vlax-ename->vla-object (car nent)))
          (if (vlax-property-available-p VL_ent_name 'TextString)
            (setq txt (vlax-get-property VL_ent_name 'TextString))
            (progn
              (setq
                x "nothing_selected"
                nent nil
              )
            ); END progn2
          ); END if2
        ); END progn1
      ); END if1
    )
    ((= 'STR (type in)) ; it's a string
      (setq txt in)
    )
    (t
      (setq x "nothing_selected")
    )
  )
  ; Analize the input text if x = "something_selected"
  (if (= x "nothing_selected")
    (setq x nil)
    (cond
      ; Normal number: adoptable manholes, PI_DAVID block
      ( (and (< (strlen txt) 8) (> (strlen txt) 4) (/= "S" (substr txt 1 1)) (/= "F" (substr txt 1 1)))
        ;(alert "case 1")
        (atof txt)
      )
      ; FFL
      ( (and (= "FFL " (substr txt 1 4)) (= 4 (- (strlen txt) (vl-string-search "." txt))) )
        ;(alert "case 2")
        (atof (substr txt 5))
      )
      ; Road level
      ( (and (or (= "%%U" (substr txt 1 3)) (= "%%u" (substr txt 1 3))) (= 3 (- (strlen txt) (vl-string-search "." txt))))
        ;(alert "case 3")
        (atof (substr txt 4 10))
      )
      ; Plot level
      ( (and (or (= "%%U" (substr txt 1 3)) (= "%%u" (substr txt 1 3))) (= 4 (- (strlen txt) (vl-string-search "." txt))))
        ;(alert "case 4")
        (atof (substr txt 4 9))
      )
      ; Private mahole
      ( (and
          (or (= "S" (substr txt 1 1)) (= "F" (substr txt 1 1)))
          (and (>= (ascii (substr txt 2 1)) 48) (<= (ascii (substr txt 2 1)) 57))
          (> (strlen txt) 5)
        )
        ;(setq cota (atof (substr txt 4 9)))
        ;(alert "case 5")
        (atof (substr txt 2))
      )
      ; Non number
      ( (and
          (= (atof txt) 0)
          (or (< (ascii (substr txt 2 1)) 97) (> (ascii (substr txt 2 1)) 122))
          (/= (ascii (substr txt 1 1)) 48)
        )
        ;(alert "case 6")
        (getreal "\nNumber format not understood. Please, introduce level: ")
      )
      ; Other
      (t
        ;(alert "case 7")
        (initget "Yes No")
        (setq ans (getkword (strcat "\nNo standard format. Verify " txt "m level. [Yes/No] <Yes>:")))
        (if (or (not ans) (= ans "Yes"))
          (atof txt)
          (exit)
        )
      );END cond4
    ); END cond
  );END if
  ; Updated to aboid error on 2016.08.03
)
(defun DT:input_string_or_point ( / in number ch pt)
  (princ "\nSelect a level or type it: ")
  (setq
    in (grread)
    number ""
  )
  (cond
    ((= 3 (car in)) ; Point input
      (cadr in)
    )
    ((= 2 (car in)) ; String input
      (while (and (= 2 (car in))  (and (/= 13 (cadr in)) (/= 32 (cadr in))))
        (if (/= 8 (cadr in))
          (progn ; if the key is not "delete"
            (setq
              ch (chr (cadr in))        ; convert input key (chr) in to a character (ch)
              number (strcat number ch) ; join it with the previous string and store it
            )
            (princ ch)
          )
          (progn ; if the key is "delete"
            (setq
              ch (chr (cadr in))                              ; convert input key (chr) in to a character (ch)
              number (substr number 1 (- (strlen number) 1 )) ; remove last character from the string
            )
            (princ ch)
          )
        )
        (setq in (grread))
      )
      (setq number number)
    )
    ((= 25 (car in))
      (setq number nil)
    )
    (t (alert "IMPORTANT!\nNew case detected. Please, report to David inmediatly what did you do before this pop up appeared to improve the routine.")(princ))
  )
  ; v0.2 - 2016.08.03 - Bug fixed
  ; v0.1 - 2016.03.31 - Add possibily to remove typed characters.
  ; v0.0 - 2016.03.30 - First issue
  ; Author: David Torralba
  ; Last revision: 2016.08.03
)
(defun c:3DPT ( / *error* VL_ent_name arr narr larr nz z oldosmode)
	; EDIT 3D polyline vertex levels, INPUT: typing or clicking

  ; PENDIENTE! - Refrescar tras cada edición la entidad y mostrar el nuevo listado
  ; de puntos. Si no, al reseleccionar un vértice no te muestra la cota cmabiada
  ; hasta que terminas el comando y reseleccionas.
  (vl-load-com)
  (defun *error* ( msg )
;    (if (not (member msg '("Function cancelled" "quit / exit abort")))
;      (princ (strcat "\nError: " msg))
;    )
    ; RESET system variables
    (setvar "osmode" oldosmode)
    (princ)
  )
  ; SAVE system variables
  (setq oldosmode (getvar "osmode"))

  ; CHANGE system variables
  (setvar "osmode" 1)
  ; El Param daba cero porque se vuelve loco con el OSMODE = 0, pon OSMODE = 1 y funciona perfecto. Pon la cabecera de copiar y salvar el OSMODE y blablabla

  ; INPUT - Select polyline
  (if (setq ent (entsel "\nSelect 3D polyline: "))
    (progn
      (setq VL_ent_name (vlax-ename->vla-object (car ent)))
      (if (= "AcDb3dPolyline" (vla-get-ObjectName VL_ent_name))
        (progn
          (while (not exit_variable)
            (setq
              ; OPERATION - Save coordinates array and convert it to a list
              arr (vlax-variant-value (vla-get-Coordinates VL_ent_name))
              larr (vlax-safearray->list arr)
            )
            ; INPUT - Select point to return parameter
            (if (setq p (getpoint "\nSelect vertex to edit <Esc to exit>: "))
              (progn
                (setq
                  ; OPERATION - Get data of point
                  Param (atoi (LM:rtos (vlax-curve-getParamAtPoint VL_ent_name (vlax-curve-getClosestPointTo VL_ent_name p)) 2 0))
                  z (nth (+ 2 (* 3 Param)) larr)
                )
                (princ (strcat "\nZ value <" (LM:rtos z 2 3) ">: "))
                (setq
                  ; INPUT - Ask for new Z value por the selected point
                  nz (DT:clic_or_type_level)
                )
                (cond
                  ((not nz)
                    (princ "...nothing selected. Z coordinate not changed.")
                  );END cond not nz
                  ((= nz z)
                    (princ (strcat (LM:rtos nz 2 3) "m. Same level selected. Z coordinate not changed."))
                  );END cond nz = z (new level = old level)
                  (t
                    (vlax-safearray-put-element arr (+ 2 (* 3 Param)) nz)
                    (vlax-put-property VL_ent_name 'Coordinates arr)
                    (princ (strcat "\nVertex level updated to " (LM:rtos nz 2 3) "m."))
                  );END cond new nz
                );END cond
              );END progn p point selected
              (progn
                (princ "\nNo vertex selected.")
                (*error*)
                (exit)
              )
            );END if
          );END while
        ); END progn
        (alert "Sorry, that is not a 3D polyline.")
      ); END if
    );END progn
    (princ "\nNothing selected.")
  );END if
  (princ)
)
(defun c:1 ( / lay_list)
; NOT FINISHED - Purpose: list layers with XDATA
  (defun OrganiseLayerData (lay_list)
    ;(if (/= (cdr (assoc -3 lay_list)) nil)
    (if (not KK)
      (progn
        (princ "\n")
        (princ (cdr (assoc 2 lay_list)))
        (princ "\n    >> ")
        ;(princ (cdr (assoc -3 lay_list)))
        (princ lay_list)
      );END progn
    );END if
    (princ)
  )
  (princ "\nShowing layers with XDATA:")
  (if (setq lay_list (tblnext "LAYER" T))
    (progn
      (OrganiseLayerData lay_list)
      (while lay_list
        (if (setq lay_list (tblnext "LAYER")) (OrganiseLayerData lay_list))
      );END while
    );END progn
  );END if
  (princ)
)
(defun c:4( / ent_name )
  ; Function to move back e-workblock polylines and obtain the perimeter of the house.
  ; Remember to change the thickness!
  (while (not kkkk)
    (setq ent_name (car (entsel)))
    (command "offset" "E" "Y" (* 0.5 0.215) ent_name (getpoint) "")
    (vlax-put-property (vlax-ename->vla-object (entlast)) 'Color 5)
  )
)
(defun c:1( / p0)
  (while (not kkkk)
    (setq
      p0 (getpoint "\nInsert point: ")
      p0 (list (car p0) (cadr p0) (DT:clic_or_type_level))
    )
    (entmakex
      (list
        (cons 0 "POINT")
        (cons 8 "--DT")
        (cons 10 p0)
      )
    )
    (setq p0 nil)
  )
)
(defun c:2() (command "3dpoly"))
(defun c:3() (c:3dPT))
(defun c:8()
  ; PENDIENTE - Haz una funcion decente...
  ; 3dpoly dando XY y nivel de forma separada
  (command "3Dpoly")
  (while (not kkkk)
    (command ".xy" (getpoint "\nSelect XY point: ") (DT:clic_or_type_level) )
  )
)
(defun c:4()(c:KTF_3DPLADVX))
(defun c:0() (command "CopyBase" "0,0"))
(defun c:9() (command "_pasteblock" "0,0"))
(defun c:ewl()
  ; create E-Work Layers
  (command "-layer" "m" "e-work-services" "c" "9" "" "")
  (command "-layer" "m" "e-work-hse" "c" "9" "" "")
  (princ)
)
(defun c:rlo( / lay ss ans i)
  (vl-load-com)
  ; Run through layer Objects
  ; Tags: Find Look for Search Buscar objetos capa
  (setq
    lay "e-set-out-road"
    ;lay (getstring t "\nWrite layer name to select all its objects: ")
    ss (ssget "x" (list (cons 8 lay)))
    ss1 (ssadd)
    i 0
  )
  (cond ;cond1
    ((not ss)
      (princ (strcat "\nNo objects found at layer \"" lay "\""))
    )
    (t
      (princ "\n")(princ (sslength ss))(princ (strcat " objects found at layer \"" lay "\""))

      ; ZOOM
      (initget "None All Each")
      (setq ans (getkword "\nWhich object would you like to zoom to [None/All/Each] <None>: "))
      (if (not ans) (setq ans "None"))
      (princ "\nans = ")(princ ans)
      (cond ;cond2
        ((= ans "None")
        )
        ((= ans "All")
          ; Add option to zoom to all objects at once
          (command "zoom" "O" (cadr (sssetfirst nil ss)) "")
        )
        ((= ans "Each")
          ; Add option to zoom in one by one
          (foreach a (ssnamex ss)
            (if (= 'ename (type (cadr a)))
              (progn
                (setq i (+ i 1))
                (princ "\nElement ")(princ i)(princ " out of ")(princ (sslength ss))
                (command "zoom" "O" (cadr a) "")
                (princ ". Press D to delete the object, or any other key to see next.")
                (sssetfirst nil (ssadd (cadr a) ss1))
                (setq
                  gr (grread)
                  ss1 (ssadd)
                )
                (if (and
                      (= (car (grread)) 2)
                      (= (cadr (grread)) 100)
                    )
                    (
                      (vla-delete (vlax-ename->vla-object (cadr a)))
                    )
                );END if2
              );END progn
            );END if1
          );END foreach
        )
      );END cond2

      (sssetfirst nil ss)
    )
  );END cond1
  (princ)
)
(defun c:HALF-OFFSET ( / dis halfdis oldANG OLDCE OLDTE OLDERR RefLine Test )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                                                         ;;;
;;;  HALF OFFSET (by David Torralba Goitia)                                                                 ;;;
;;;                                                                                                         ;;;
;;;  This command works exactly the same way as the standard offset command does, but it makes the offset   ;;;
;;;  to half of the specified distance.                                                                     ;;;
;;;                                                                                                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (setq oldANG (getvar "ANGBASE"))
  (setvar "ANGBASE" 0)
  (setq oldCLA (getvar "CLAYER"))
  (setq OLDCE (getvar "cmdecho"))
  (setvar "cmdecho" 1)
  (setq OLDTE (getvar "texteval"))
  (setvar "texteval" 1)

  ; Save the current value of the error handling subroutine then redefine it.
  (setq OLDERR *error*)
  (defun *error* (errmes)

    (setvar "cmdecho" OLDCE)
    (setq *error* OLDERR)
    (setvar "ANGBASE" oldANG)  ; Reset "ANGBASE" to previous value.
    (setvar "cmdecho" OLDCE)   ; Reset "cmdecho" to previous value.
    (setvar "CLAYER" oldCla)   ; Reset "CLAYER" to previous value.
    (setvar "texteval" OLDTE)  ; Reset "texteval" to previous value.
    (prin1)
  )

  ;; INPUT - Ask the user to specify the line to offset
  (setq RefLine (car (entsel "\nSelect the line to offset: ")))

  ; OPERATION - Calculate the total distance between the points P1 and P2
  (setq dis (getdist "\n\nMARCA DISTANCIA"))

  ; OPERATION - Calculate half distance and prompt it
  (setq halfdis (/ dis 2))

  (princ "\n\n  Distance: ")
  (princ dis)

  (princ "     >>>  Half distance = ")
  (princ halfdis)
  (princ "\n")

  (command "._offset" halfdis RefLine)


  (setvar "ANGBASE" oldANG)
  (setvar "cmdecho" OLDCE)
  (setvar "CLAYER" oldCla)
  (setvar "texteval" OLDTE)
  (setq *error* OLDERR)

  (princ)
)
(defun c:1( / ss)
  ; Move selected objects to an specific layer
  (setq ss (ssget) )
  (foreach a (ssnamex ss)
    (if (= 'ename (type (cadr a)))
      (vla-put-layer (vlax-ename->vla-object (cadr a)) "e-cf-TIN-road-12")
    );END if1
  );END foreach
  (princ)
)
(defun c:2( / ss)
  ; Move selected objects to an specific layer
  (setq ss (ssget) )
  (foreach a (ssnamex ss)
    (if (= 'ename (type (cadr a)))
      (vla-put-layer (vlax-ename->vla-object (cadr a)) "e-cf-TIN-parcel-B")
    );END if1
  );END foreach
  (princ)
)
(defun c:2 (/ obj lay)
;(defun c:nlayg (/ obj lay)
  ; Gray out nested object real layer in the current viewport
  (if
    (and
      (/= "Model" (getvar "CTAB"  ) )
      (<  1       (getvar "CVPORT") )
    );END and
    (progn
      (if (setq obj (car (nentsel "\nSelect object to grey layer out: ")))
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
        (command "vplayer" "C" 250 lay "C" "")
      );END if

    );END progn true
    (progn
      (alert "You are not in a viewport.")
      (quit)
    );END progn false
  );END if
  (princ)
  ; v0.0 - 2016.08.17
  ; Author: David Torralba
  ; Last revision: 2016.08.17
)
(defun c:3 (/ obj lay)
;(defun c:nlayt (/ obj lay)
  ; Gray out nested object real layer in the current viewport
  (if
    (and
      (/= "Model" (getvar "CTAB"  ) )
      (<  1       (getvar "CVPORT") )
    );END and
    (progn
      (if (setq obj (car (nentsel "\nSelect object to fade out: ")))
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
        (command "vplayer" "TR" 80 lay "C" "")
      );END if

    );END progn true
    (progn
      (alert "You are not in a viewport.")
      (quit)
    );END progn false
  );END if
  (princ)
  ; v0.0 - 2016.11.09
  ; Author: David Torralba
  ; Last revision: 2016.11.09
)


(defun c:2()(c:ROAD_SETOUT))
(defun c:ROAD_SETOUT(
                          /
                          oldosmode oldattreq oldattdia oldcmdecho

                          ss VL_ent_name
                          *model-space*
                          i ii
                          total_data ptList ptdataList vertexdata
                          p1 p2 N E
                          )
  ; Road Setting Out

  ; AUXILIARY FUNCTIONS
  (defun *error* ( msg )
    (if (not (member msg '("Function cancelled" "quit / exit abort")))
      (princ (strcat "\nError: " msg))
    )
    ; RESET system variables
    (setvar "osmode" oldosmode)
    (setvar "attreq" oldattreq)
    (setvar "attdia" oldattdia)
    (setvar "cmdecho" oldcmdecho)
    (princ)
  )

  ; SAVE SETTINGS
  (setq
    oldosmode (getvar "osmode")
    oldattreq (getvar "attreq")
    oldattdia (getvar "attdia")
    oldcmdecho (getvar "cmdecho")
  )
  ; CHANGE SETTINGS
  (setvar "osmode" 0)
  (setvar "attreq" 1)
  (setvar "attdia" 0)
  (setvar "cmdecho" 0)

  ; OPERATION - Check if UCS is not World and let know.
  (if (= 0 (getvar "WORLDUCS"))
    (alert "------------------  WARNING!  ------------------\n\nCurrent UCS is not \"World\".\n\nPlease, ensure active UCS is the desired one. If not,\npress Esc to abort routine.")
  );END if

  ; INPUT - Ask user to input scale
  (if (= sf nil)
    (progn
      (setq sf (getreal "\nNominal Scale   1:<500>  1:"))
      (if (= sf nil) (setq sf 500.0))
      (setq sf (/ sf 571.428557))
    );END progn
  );END if

  ; INPUT - Ask user to input amount of decimal places
  (if (= cacc nil)
    (progn
      (setq cacc (getint "\nNumber of Decimal Places for display of Co-ordinates <3> : "))
      (if (= cacc nil) (setq cacc 3))
    );END progn
  );END if

  (setq olay (getvar "clayer"))

  ; INPUT - Select polylines to annotate:
  (setq
    ss (ssget '(( 0 . "LWPOLYLINE")))
    *model-space* (vla-get-modelspace (vla-get-activedocument (vlax-get-acad-object)))
    i 0
    total_data nil
    ptList nil
    ptdataList nil
    vertexdata nil
  )
  ; OPERATION - Zoom to all selected entities
  (command "zoom" "O" ss "")

  ; OPERATION - Run through all selected entities
  (foreach a (ssnamex ss)
    (if (= 'ename (type (cadr a)))
      (progn
        (setq
          i (+ i 1)
          ii 0
          ptList (list i)
          VL_ent_name (vlax-ename->vla-object (cadr a))
        )

        ; OPERATION - Extract point list:
        (foreach xx (entget (cadr a))
          ; 10, 40, 41, 42
          (if (= 10 (car xx)) (setq vertexdata (append vertexdata (cdr xx))) )
          (if (= 42 (car xx))
            (setq
              ; OPERATION - Add vertexdata list to entity list.
              vertexdata (append vertexdata (list (cdr xx)))
              ptList (append ptList (list vertexdata))
              vertexdata nil
            );END setq
          );END if 42
        );END foreach

        ; OPERATION - Run through extracted point list adding coordinates:
        (foreach b ptList
          (if (= 'list (type b)) ; (type b) = 'list
            (progn
              ; OPERATION - Set data for block to be inserted:
              (setq
                p1 (list (nth 0 b) (nth 1 b) 0.0)
                E (strcat "E " (LM:rtos (nth 0 b) 2 3) )
                N (strcat "N " (LM:rtos (nth 1 b) 2 3) )
              )
              ; Meter una coordenada en este punto
              (command "-insert" "XY_advanced" p1 sf "" "" E N)

              ; OPERATION - Check if there is an ark after the vertex
              (if (/= 0.0 (nth 2 b))
                (progn ; there is bulge
                  ; OPERATION - Add dimradius
                  (setq
                    pM (vlax-curve-getPointAtParam VL_ent_name (- ii 0.5))
                  );END setq

                  ; OPERATION - Add dimradius
                  (command "_.dimradius" pM "")

                  ; OPERATION - Add radius centre
                  (if (= "DIMENSION" (cdr (assoc 0 (entget (entlast)))))
                    (progn
                      (setq
                        c (cdr (assoc 10 (entget (entlast))))
                        E (strcat "E " (LM:rtos (nth 0 c) 2 3) )
                        N (strcat "N " (LM:rtos (nth 1 c) 2 3) )
                      )
                      (command "-insert" "XY_advanced" c sf "" "" E N)
                    );END progn
                  );END if
                );END progn
              );END if
              ; si bulge factor no es cero: buscar punto medio, meter un dimrad y buscar el centro y poner otro bloque de coordenada en el centro del arco.
            );END progn (type b) = 'list
          );END if
          (setq ii (+ ii 1))
        );END foreach
      );END progn
    );END if1
  );END foreach

  ; PENDIENTE - Cuando se ejecuta la rutina con el UCS cambiado, los puntos se extraen correctamente,
  ;             pero hay que corregir las coordenadas de inserción de los bloques. Si corriges eso,
  ;             eres el puto amo. xD
  ; PENDIENTE - Comprobar el radio del arco, y si es mayor que X no colocar la coordenada del centro del arco
  ; PENDIENTE - Comprobar el radio del arco, y si es menor que X colocar la coordenada del centro del arco, y colocar el texto de la dimensión a 3m del arco

  ; RESET system variables
  (setvar "osmode" oldosmode)
  (setvar "attreq" oldattreq)
  (setvar "attdia" oldattdia)
  (setvar "cmdecho" oldcmdecho)
  (princ)
)
(defun c:1( / txt blk )
    (setq txt (LM:rtos (+ (DT:clic_or_type_level) -0.025) 2 3) )
    (LM:vl-setattributevalue (vlax-ename->vla-object (car (entsel))) "NO" txt )
)
(defun c:1 ()
  ; Insert selected object area as a text
  (command "area" "Object" (car (entsel "\nSelect closed polyline: ")))
  (command "-text" "S" "ARIAL" "J" "M" (setq pt (cadr (grread 't))) "1.5" "90" (strcat (LM:rtos (getvar "Area") 2 3) " m2"))
)
(defun c:COORDP (
                /
                oldosmode oldcmdecho
                olay
                a b con
                X Y E N pt1
                )
  ; coordinates along more than 1 polyline
  ; PENDIENTE - Reescribir el código porque es una puta basura, es un copia pega apañado del de KTD y es muy cutre..tu lo puedes hacer mucho mejor
  (setq
    oldosmode (getvar "osmode")
    oldcmdecho (getvar "cmdecho")
  )
  (setvar "cmdecho" 0)
  (if (= sf nil)
    (progn
      (setq sf (getreal "\nNominal Scale   1:<500>  1:"))
      (if (= sf nil) (setq sf 500.0))
      (setq sf (/ sf 571.428557))
    );END progn
  );END if

  (if (= cacc nil)
    (progn
      (setq cacc (getint "\nNumber of Decimal Places for display of Co-ordinates <3> : "))
      (if (= cacc nil) (setq cacc 3))
    );END progn
  );END if

  (setq olay (getvar "clayer"))
  (setq lay (getstring (strcat "\nLayer for co-ordinate boxes to be drawn on (current layer) <" olay "> : ")))
  (if (= lay "") (setq lay olay))
  (command "_layer" "_m" lay "")

  (princ "\nSelect polylines:")
  (setq ss (ssget) )
  (foreach a (ssnamex ss)
    (if (= 'ename (type (cadr a)))
      (progn
        (setq con (entget (cadr a)) )
        (if (= (cdr (assoc 0 con)) "LWPOLYLINE")
          (progn
            (setq counter 0)
            (repeat (length con)
              (setq a (nth counter con))
              (if (equal (car a) 10)
                (progn
                  (setq pt1 (cdr a)
                    X (LM:rtos (nth 0 pt1) 2 3)
                    Y (LM:rtos (nth 1 pt1) 2 3)
                    E  (strcat "E " (LM:rtos (nth 0 pt1) 2 cacc))
                    N  (strcat "N " (LM:rtos (nth 1 pt1) 2 cacc))
                  )
                  (setvar "osmode" 0)
                  (command "_insert" "XY_advanced" pt1 sf "" "" E N)
                );END progn
              );END if
              (setq counter (+ counter 1))
            );END repeat
          );END progn
        );END if
        (if (= (cdr (assoc 0 con)) "POLYLINE")
          (progn
            (setq b (entnext (cdr (assoc -1 con))))
            (while  (/= (cdr (assoc 0 (entget b))) "SEQEND")
              (setq
                con (entget b)
                pt1 (cdr (assoc 10 con))
                b (entnext (cdr (assoc -1 con)))
                X (LM:rtos (nth 0 pt1) 2 3)
                Y (LM:rtos (nth 1 pt1) 2 3)
                E  (strcat "E " (LM:rtos (nth 0 pt1) 2 cacc))
                N  (strcat "N " (LM:rtos (nth 1 pt1) 2 cacc))
              );END setq
              (setvar "osmode" 0)
              (command "_insert" "XY_advanced" pt1 sf "" "" E N)
            );END while
          );END progn
        );END if
      );END progn
    );END if1

  );END foreach


  (setvar "osmode" oldosmode)
  (setvar "cmdecho" oldcmdecho)
  (princ)
)
(defun c:1() (while (not kkkk) (princ "\nMove setting out label: ") (c:DT:move_KTF_SettingOutLabel) ))
(defun c:2() (princ "\nRadius dimension: ") (command "_dimradius" pause "") )
(defun c:3() (princ "\nAligned dimension: ") (command "_dimaligned") )
(defun c:4() (while (not kkkk) (vlax-put-property (vlax-ename->vla-object (car (entsel))) 'Color 5)))
(defun c:5() (c:coord))
(defun c:1( / ss)
  ; Find Filter Encuentra todo los elementos (en este caso '(( 8 . "MISC_TEXT") (0 . "TEXT")) y hazles algo
  ; TAGS: sustitute run
  (setq ss (ssget "x" '(( 8 . "MISC_TEXT") (0 . "TEXT"))) )
  (foreach a (ssnamex ss)
    (if (= 'ename (type (cadr a)))
      (if (vlax-property-available-p (vlax-ename->vla-object (cadr a)) 'TextString)
        (if (= "stn" (substr (vlax-get-property (vlax-ename->vla-object (cadr a)) 'TextString) 1 3))
          (vla-put-layer (vlax-ename->vla-object (cadr a)) "STATIONS_TEXT")
        );END if3
      );END if2
    );END if
  );END foreach
);END defun
(defun c:1( / ss)
  ; Find Filter los elementos seleccionados y hazles algo
  ; TAGS: sustitute run
  (setq ss (ssget "x" '((0 . "TEXT"))) )
  (foreach a (ssnamex ss)
    (if (= 'ename (type (cadr a)))
      (if (vlax-property-available-p (vlax-ename->vla-object (cadr a)) 'TextString)
        (if (= "stn" (substr (vlax-get-property (vlax-ename->vla-object (cadr a)) 'TextString) 1 3))
          (vla-put-layer (vlax-ename->vla-object (cadr a)) "STATIONS_TEXT")
        );END if3
      );END if2
    );END if
  );END foreach
);END defun
(defun c:1( / a )
  ; offset entity inside with set distance and mark it with a set width
  (setq a (car (entsel "\nSelect entity to offset: ")))
  (vlax-put-property (vlax-ename->vla-object a) 'ConstantWidth 0.3)
  (command "offset" 0.15 a (DT:AVE_vertex (vlax-ename->vla-object a)) "")

);END defun
(defun c:1()
  ; Copy to layer (copytolayer) to an specific layer
  (vla-copy (vlax-ename->vla-object (car (entsel))))
  (vla-put-layer (vlax-ename->vla-object (entlast)) "e-as-built-levels-road")
  (command "move" (entlast) "" (cadr (grread 't)) pause)
  (command "_TEXTEDIT" (entlast))
);END defun
(defun c:1()
  (vla-put-layer (vlax-ename->vla-object (car (entsel))) "e-as-built-levels-road")
);END defun
(defun c:1( / x )
  (setq
    x (LM:rtos (+ 0.11 (DT:clic_or_type_level)) 2 2)
    rot (- (* -180 (/ (cdr (assoc 50 (entget (car (entsel "\nSelect rotation reference: "))) ) ) pi) ) 90)
  )
  (command "-text" (cadr (grread 't)) 0.350 (+ rot 180) x)
  (command "move" (entlast) "" (cadr (grread 't)) pause)
);END defun
(defun c:1() (fbi "TEMP"))
(defun c:2() (fbi "TEMP2"))
(defun c:1( / disp drafting)
  (vl-load-com)
  (setq
    disp (vla-get-display (vla-get-preferences (vlax-get-acad-object)))
    ;drafting (vla-get-drafting (vla-get-preferences (vlax-get-acad-object)))
  )
  ;(vla-put-GraphicsWinModelBackgrndColor disp 5987163)
  (vla-put-GraphicsWinLayoutBackgrndColor disp 5987163)
  ;(vla-put-LayoutCrosshairColor disp 16777215)
  ;(vla-put-ModelCrosshairColor disp 16777215)
  ;(vla-put-AutoTrackingVecColor disp 16777215)
  ;(vla-put-AutoSnapMarkerColor drafting 2)
  (princ)

  ; para encontrar colores ve a Options > Drafting > Colors > Sheet / layout > Paper background
  ; (vla-get-GraphicsWinLayoutBackgrndColor (vla-get-display (vla-get-preferences (vlax-get-acad-object))))
  ; y el valor del variant es lo que quires poner en XXXXXXXX:
  ;(vla-put-GraphicsWinLayoutBackgrndColor (vla-get-display (vla-get-preferences (vlax-get-acad-object))) XXXXXXXX)
  ; Grey 251 - 5987163
  ; White 7 - 16777215
  ; Black - 0
);END defun

(defun c:1( / coord x y )
  (setq
    coord (cdr (assoc 10 (entget (car (entsel)))) )
    x (strcat "E=" (LM:rtos (car coord) 2 3) )
    y (strcat "N=" (LM:rtos (cadr coord) 2 3) )
  )
  (command "text" "J" "ML" (getpoint) 1.8 "" x)
  (princ "\n")(princ x)
  (command "text" "J" "ML" (getpoint) 1.8 "" y)
  (princ "\n")(princ y)
);END defun
(defun c:1() (vlax-put-property (vlax-ename->vla-object (car (entsel))) 'Color 2) )
(defun c:1() (setvar "osmode" 2048))
(defun c:2() (c:coord))
(defun c:3() (c:DT:move_KTF_SettingOutLabel))
(defun c:4() (princ "\nRadius dimension: ") (command "_dimradius" pause "") )
(defun c:9()(c:ROAD_SETOUT))
; ENTMAKE REFERENCE, ENTMAKE FUNCTIONS -----------------------------------------------------------------------
(defun 3DFace (p1 p2 p3 p4)
  (entmakex (list (cons 0 "3DFACE")
                  (cons 10 p1)
                  (cons 11 p2)
                  (cons 12 p3)
                  (cons 13 p4))))
(defun Arc (cen rad sAng eAng)
  (entmakex (list (cons 0 "ARC")
                  (cons 10  cen)
                  (cons 40  rad)
                  (cons 50 sAng)
                  (cons 51 eAng))))
(defun AttDef (tag prmpt def pt hgt flag)
  (entmakex (list (cons 0 "ATTDEF")
                  (cons 10   pt)
                  (cons 40  hgt)
                  (cons 1   def)
                  (cons 3 prmpt)
                  (cons 2   tag)
                  (cons 70 flag))))
(defun Circle (cen rad)
  (entmakex (list (cons 0 "CIRCLE")
                  (cons 10 cen)
                  (cons 40 rad))))
(defun Ellipse (cen maj ratio)
  (entmakex (list (cons 0 "ELLIPSE")
                  (cons 100 "AcDbEntity")
                  (cons 100 "AcDbEllipse")
                  (cons 10 cen)
                  (cons 11 maj)
                  (cons 40 ratio)
                  (cons 41 0)
                  (cons 42 (* 2 pi)))))
(defun Insert (pt Nme)
  (entmakex (list (cons 0 "INSERT")
                  (cons 2 Nme)
                  (cons 10 pt))))
(defun Line (p1 p2)
  (entmakex (list (cons 0 "LINE")
                  (cons 10 p1)
                  (cons 11 p2))))
(defun LWPoly (lst cls)
  (entmakex (append (list (cons 0 "LWPOLYLINE")
                          (cons 100 "AcDbEntity")
                          (cons 100 "AcDbPolyline")
                          (cons 90 (length lst))
                          (cons 70 cls))
                    (mapcar (function (lambda (p) (cons 10 p))) lst))))
(defun M-Text (pt str)
  (entmakex (list (cons 0 "MTEXT")
                  (cons 100 "AcDbEntity")
                  (cons 100 "AcDbMText")
                  (cons 10 pt)
                  (cons 1 str))))
(defun Point (pt)
  (entmakex (list (cons 0 "POINT")
                  (cons 10 pt))))
(defun Polyline (lst)
  (entmakex (list (cons 0 "POLYLINE")
                  (cons 10 '(0 0 0))))
  (mapcar
    (function (lambda (p)
                (entmake (list (cons 0 "VERTEX") (cons 10 p))))) lst)
  (entmakex (list (cons 0 "SEQEND"))))
(defun Solid (p1 p2 p3 p4)
  (entmakex (list (cons 0 "SOLID")
                  (cons 10 p1)
                  (cons 11 p2)
                  (cons 12 p3)
                  (cons 13 p4))))
(defun Text (pt hgt str)
  (entmakex (list (cons 0 "TEXT")
                  (cons 10  pt)
                  (cons 40 hgt)
                  (cons 1  str))))
  (defun Trce (p1 p2 p3 p4)
  (entmakex (list (cons 0 "TRACE")
                  (cons 10 p1)
                  (cons 11 p2)
                  (cons 12 p3)
                  (cons 13 p4))))
(defun xLine (pt vec)
  (entmakex (list (cons 0 "XLINE")
                  (cons 100 "AcDbEntity")
                  (cons 100 "AcDbXline")
                  (cons 10 pt)
                  (cons 11 vec))))
(defun Layer (Nme)
  (entmake (list (cons 0 "LAYER")
                 (cons 100 "AcDbSymbolTableRecord")
                 (cons 100 "AcDbLayerTableRecord")
                 (cons 2 Nme)
                 (cons 70 0))))
(defun Layer (Nme Col Ltyp LWgt Plt)
  (entmake (list (cons 0 "LAYER")
                 (cons 100 "AcDbSymbolTableRecord")
                 (cons 100 "AcDbLayerTableRecord")
                 (cons 2  Nme)
                 (cons 70 0)
                 (cons 62 Col)
                 (cons 6 Ltyp)
                 (cons 290 Plt)
                 (cons 370 LWgt))))
;;;**************************************** PL:Text ****************************************
;;;
;;; Function of creation a text entity without standard command. Is insensitive to the current
;;; object snap mode.
;;; Returns the name of the text entity created. If PL:Text is unable to create the entity, the
;;; function returns nil.
;;;
;;; Calling's notation: (PL:Text STRING INSPOINT ANG_SEC HIGH JUST STYLE LAYER)
;;;
;;; STRING - text string
;;; INSPOINT - insertion point
;;; ANG_SEC - rotation angle or second point ||
;;; HIGH - text height ||NIL - if it's NIL then is getting from selected text
;;; style, if it's 0 in text style then is getting from system variable TEXTSIZE
;;; JUST - justification |NIL - short or full notation from command DTEXT, if NIL or
;;; unknown then is getting LEFT
;;; STYLE - text style |NIL - if NIL or unknown then is getting current text style
;;; LAYER - layer |NIL - if NIL then is getting current layer, if unknown then new
;;; layer will be created
;;;
(defun PL:Text (_TEXT _INS _ANG _HIGH _J _ST _LA / _J72 _J73 _SP _RE _TAB)
 (setq
   _J (if (not _J) "LEFT" (strcase _J) );END if
   _ST (tblsearch
         "STYLE"
         (if (or
               (not _ST)
               (not (tblsearch "STYLE" _ST))
             );END or
           (getvar "TEXTSTYLE")
           _ST
         );END if
       );END tblsearch
   _SP _ANG
 );END setq
 (if (or (= _J "ALIGN") (= _J "A") (= _J "FIT") (= _J "F"))
   (if (= (type _ANG) 'list)
     (setq _ANG (angle _INS _ANG))
     (setq _J "LEFT")
   );END if
   (setq
     _ANG  (if (= (type _ANG) 'list)
             (angle _INS _ANG)
             (progn
               (setq _SP _INS)
               (* pi (/ _ANG 180.0))
             );END progn
           );END if
   );END setq
 );END if
 (cond
   ((or (= _J "LEFT") (= _J "L")) (setq _J72 0 _J73 0))
   ((or (= _J "CENTER") (= _J "C")) (setq _J72 1 _J73 0 _RE t))
   ((or (= _J "RIGHT") (= _J "R")) (setq _J72 2 _J73 0 _RE t))
   ((or (= _J "ALIGN") (= _J "A")) (setq _J72 3 _J73 0))
   ((or (= _J "MIDDLE") (= _J "M")) (setq _J72 4 _J73 0 _RE t))
   ((or (= _J "FIT") (= _J "F")) (setq _J72 5 _J73 0))
   ((or (= _J "TLEFT") (= _J "TL")) (setq _J72 0 _J73 3 _RE t))
   ((or (= _J "TCENTER") (= _J "TC")) (setq _J72 1 _J73 3 _RE t))
   ((or (= _J "TRIGHT") (= _J "TR")) (setq _J72 2 _J73 3 _RE t))
   ((or (= _J "MLEFT") (= _J "ML")) (setq _J72 0 _J73 2 _RE t))
   ((or (= _J "MCENTER") (= _J "MC")) (setq _J72 1 _J73 2 _RE t))
   ((or (= _J "MRIGHT") (= _J "MR")) (setq _J72 2 _J73 2 _RE t))
   ((or (= _J "BLEFT") (= _J "BL")) (setq _J72 0 _J73 1 _RE t))
   ((or (= _J "BCENTER") (= _J "BC")) (setq _J72 1 _J73 1 _RE t))
   ((or (= _J "BRIGHT") (= _J "BR")) (setq _J72 2 _J73 1 _RE t))
   (t (setq _J72 0 _J73 0))
 );END cond
 (entmakex
   (list
     '(0 . "TEXT")
     '(100 . "AcDbEntity")
     (cons 67
       (if (= (setq _TAB (getvar "CTAB")) "Model")
         0
         1
       );END if
     );END cons
     (cons 410 _TAB)
     (cons 8
       (if (not _LA)
         (getvar "CLAYER")
         _LA
       );END if
     );END cons
     '(100 . "AcDbText")
     (cons 10 _INS)
     (cons 40
       (cond
         ((and _HIGH (/= _HIGH 0)) _HIGH)
         ((/= 0 (setq _HIGH (cdr (assoc 40 _ST)))) _HIGH)
         (t (getvar "TEXTSIZE"))
       );END cond
     );END cons
     (cons 1 _TEXT)
     (cons 50 _ANG)
     (assoc 41 _ST)
     (cons 51 (cdr (assoc 50 _ST)))
     (cons 7 (cdr (assoc 2 _ST)))
     (assoc 71 _ST)
     (cons 72 _J72)
     (cons 11
       (if _RE
         _INS
         _SP
       );END if
     );END cons
     (cons 73 _J73)
   );END list
);END entmake
);END defun
;;;
;;; EOF
(defun c:1( / p )
  (defun DT:text (pt ang str)
    (entmakex
      (list
        (cons 0 "TEXT")
        (cons 10 '(0.0 0.0 0.0))
        (cons 40 0.35)
        (cons 1  str)
        (cons 50 ang)
        (cons 11  pt)
        (cons 72 1)    ; Aquí está la clave de la justificación del texto (y en el campo 11 y 10)
        (cons 73 2)    ; Aquí está la clave de la justificación del texto (y en el campo 11 y 10)
      );END list
    );END entmakex
  )
  (setq p (getpoint "\nSelect text insertion point: "))
  (Text p (angle p (getpoint)) "%%UTEST TEXTS") ; el texto avanza hacia el segundo clic
  ; (Text p (getreal) "%%UTEST TEXTS") ; el angulo 0 es a las 3 en punto y crece antihorario en radianes
  (princ)
)
; ENTMAKE REFERENCE, ENTMAKE FUNCTIONS -----------------------------------------------------------------------
(defun c:1()
  (DT:getrealpositive "\nIntroduce a real positive number (or zero) <0.000>: ")
);END defun
(defun DT:getrealpositive ( msg / ans)
  ; Get Real Positive number
  (while (not ans)
    (setq ans (getstring msg) )
    (cond
      ((= ans "")
        (setq ans 0)
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
        (atof ans)
      );END subcond
      (t
        (princ "\nPlease, introduce a positive real number.")
        (setq ans nil)
      );END subcond
    );END cond
  );END while
);END defun
(defun c:1()
  ; Insert block: first rotation, then position
  (command "-insert" "TEMP_DELETE" (setq p1 (getpoint "\nPunto 1: ")) 1 1 pause)
  (setvar "osmode" 0)
  (command "move" (entlast) "" (cadr (grread 't)) pause)
  (setvar "osmode" 512)
)
(defun c:1()
  (setvar "osmode" 3)
  (command "-insert" "TEMP_DELETE" (setq p1 (getpoint "\nPunto 1: ")) 1 1 pause)
)
(defun c:1(
            /
            msg_base
            msg_level
            base_ls
            level_ls
          )
  (setq
    msg_base "\nIntroduce hatch point: "
    msg_level "\nSelect height: "
    base_ls (list (getpoint msg_base) )
    level_ls (list (getreal msg_level) )
;    head_ls (list )
    i 0
  )
)
(defun c:1(
            /
            old_state

            p0
            msg_base msg_level
            new_base base_ls level_ls
            ang
            i
            exit_var
          )
; STORE VARIABLES
; (setq old_state (store [vars]))


    (setq
        p0 (getpoint "\nSelect building inner point: ")
        msg_base "\nIntroduce hatch point (or press ENTER to exit): "
        msg_level "\nSelect height: "
        new_base (getpoint msg_base)
    )
    (if (not new_base) (exit))
    (setq
        base_ls  (list new_base)
        level_ls (list (getreal msg_level))
        ang_ls   (list (angle p0 new_base))
        head_ls  (list "")
        i 0
    ); END setq

    (while (not exit_var)
        ; INPUT - Ask user new base point
        (setq new_base (getpoint msg_base))

        (cond
            ; User has decided to exit the function
            ((not new_base)
                (setq exit_var 1)
                (princ "\nFunction finished. Bye!")
            );END subcond exit

            ; User has input a new base point
            ((/= nil new_base)

                ; Store: base point, level and angle
                (setq
                    base_ls  (append base_ls (list new_base))
                    level_ls (append level_ls (list (getreal msg_level)))
                    ang_ls   (append ang_ls (list (angle p0 new_base) ))
                )
                (setq
                    i (+ i 1)
                ); END setq

                ; Draw
                ;(Polyline base_ls)
            );END subcond exit
        );END cond
    ); END while

    ; RESTORE VARIABLES
    ;(restore old_state)

    (princ "\nang_ls = ")(princ ang_ls)
    (princ "\nlevels_ls = ")(princ base_ls)
    (princ "\nbase_ls = ")(princ level_ls)
    (Polyline base_ls)
    (princ)
)
; ----------------------------------------------------- ERROR FUNCTIONS TXOMON
; 2016.10.08
(defun get_vars(targets)
; Return variable's values
    (mapcar
        '(lambda (variable) (list variable (getvar variable)))
        targets
    )
)
(defun set_vars(variables)
; Given a list of pairs of var-value, it sets them
    (mapcar
        '(lambda (variable) (setvar (nth 0 variable) (nth 1 variable)))
        variables
    )
)
(defun unsave_environment_handler()
; Create the error handler that is in theory the default
    (defun *error* (msg)
        (princ "error: ")
        (princ msg)
        (princ)
    )
)
(defun save_environment_handler()
; Register a one time error handler
    (defun *error*(msg)
        (unsave_environment_handler)
        (environ_local_error msg)
    )
)
(defun store_environ(handler targets / old_env old_error)
; Store environ and create restore_function
    (setq old_vars (get_vars targets))
    (defun restore_environ()
        (set_vars old_vars)
        (unsave_environment_handler)
    )
    (defun environ_local_error (msg)
        (restore_environ)
        (unsave_environment_handler)
    )
)
(defun c:1 (/old_env old_error local_error)
    (defun local_error(msg)
        princ "Hey ei ei ei"
    )
    (store_environment '("osmode" "angdir" "angbase"))

    (restore_environment)
)
; ----------------------------------------------------- ERROR FUNCTIONS TXOMON

; 2016.10.10 - 5396: FFL
(defun DT:SDIP20( / p1 z0 dist grad)
  ; Light version for DIP
  (setq
    z0 (DT:clic_or_type_level)
	)
	(princ (strcat "\nz1 = " (LM:rtos z0 2 3) "m"))
	(setq
    dist (distance (setq p0 (getpoint "\npoint 1: ")) (setq p1 (getpoint "\npoint 2: ")))
    grad 20
  )
  (command "line" p0 p1 "")
	(list (+ z0 (/ dist grad)) p1)
)
(defun c:1( / p1)
	(setq p1 (DT:SDIP20))
	(princ "\nLevel = ")(princ (car p1))
	(if (/= nil (tblsearch "block" "PI_DT"))
		(command "._insert" "PI_DT" (cadr p1) "0.25" "0.25" "" (LM:rtos (car p1) 2 3))
	);END if
  (princ "\nto clipboard: ")
  (CopyToClipboard (LM:rtos (car p1) 2 3))
)
(defun c:2() (c:INT))
(defun c:3() (c:SDIP))
(defun c:4() (c:DIP))
(defun c:5()
  ; Update text to 0.125 more, level
  (setq
    txt (strcat "%%U" (LM:rtos (+ 0.125 (DT:clic_or_type_level)) 2 3))
    ent_list (entget (car (entsel "\nSelect target text (the one to update): ")))
    ent_list (subst (cons 1 txt) (assoc 1 ent_list) ent_list)
  )
  (entmod ent_list)
  (princ)
);END defun
(defun force_error()
  ; Force *error* function execution
  (itoa nil)
);END defun
(defun c:1()
  (princ (strcat "\nStart time: " (PrintDateTime) "\n") )  ; Print start time
  (ReloadXref "Area P Eng Arch")                           ; Reload XREF
  (princ (strcat "\nEnd time:   " (PrintDateTime) "\n") )  ; Print end time
  (princ)
)
(defun c:1(/ p1 i)
	(command ".-insert" "Private-Square300-Foul-Manhole" (setq p1 (getpoint)) 1 1 0)
  (setq i 0)
  (repeat 3
    (progn
      (setq i (+ i 1))
      (princ (strcat "\nSelect end point for pipe " (itoa i) ": "))
      (command ".pline" p1 pause "")
    )
	)
  (princ "\nSelect end point for outfull pipe: ")
  (command ".pline" p1 "_per" pause "")
)
(defun c:2(/ p1)
	(setq p1 (getpoint))
	(while (not kkkkk)
		(command ".pline" p1 pause "")
	)
)
(defun c:1( / ent_name )
  (setq ent_name (car (entsel)) )
  (DT:SetText ent_name (strcat "%%U" (LM:rtos (+ (atof (substr (DT:GetText ent_name) 4)) -0.025) 2 3) ) )
  (command "move" ent_name "" (cadr (grread 't)) pause)
)
(defun c:1() (c:e-work-layers))
(LoadWithoutSecureload "C:/Users/davidt/Dropbox/MJA/LISP/TORRALBA/CIV_e-work_layers.lsp" "OnFailMessage")
(defun c:2() (c:e-work-copyscaleblock))
(LoadWithoutSecureload "C:/Users/davidt/Dropbox/MJA/LISP/TORRALBA/CIV_e-work-copyscaleblock.lsp" "OnFailMessage")
(defun c:1() (c:BYC))
(defun c:2() (c:INT))
(defun c:4( / ref)
  ; Update target text with reference level minus 0.39
  (princ "\nSubase below TARMAC:\n")
  (setq ref (DT:clic_or_type_level) ) (princ ref)
  (DT:SetText (car (entsel "\nSelect target text: ")) (strcat "SB" (LM:rtos (- ref 0.39) 2 2)) )
);END defun
(defun c:5( / ref)
  ; Update target text with reference level minus 0.39
  (princ "\nSubase below BLOCK paving:\n")
  (setq ref (DT:clic_or_type_level) ) (princ ref)
  (DT:SetText (car (entsel "\nSelect target text: ")) (strcat "SB" (LM:rtos (- ref 0.41) 2 2)) )
);END defun
(defun c:m() (command "move" pause "" (cadr (grread 't)) pause) )
(defun c:mo() (command "move"))
(defun c:mm() (command "move"))













(defun c:INT( /
              *error*
              ans
              set_line reference_circle1 reference_circle2
              mode grad cota level
              p1 p2 p3 p4
              z1 z2 z3 z4
              d12 d13 d14
              ang12 ang123 ang13 ang21
              u0 v0 w0
              oldosmode oldcmdecho oldclayer
            )
  ; SET - Error handling function
  (defun *error* ( msg )
    (if (not (member msg '("Function cancelled" "quit / exit abort")))
      (princ (strcat "\nError: " msg))
    )
    ; OPERATION - Delete auxiliary data, if any
    (if (/= set_line nil) (vla-delete (vlax-ename->vla-object set_line)))
    (if (/= reference_circle1 nil) (vla-delete (vlax-ename->vla-object reference_circle1)))
    (if (/= reference_circle2 nil) (vla-delete (vlax-ename->vla-object reference_circle2)))

    ; Restore previous settings
    (setvar "osmode" oldosmode)
    (setvar "cmdecho" oldcmdecho)
    (setvar "clayer" oldclayer)
    (princ)
  )

  ; SAVE CURRENT SETTINGS - Current layer, OSMODE and CMDECHO
  (setq oldosmode (getvar "osmode")
        oldcmdecho (getvar "cmdecho")
        oldclayer (getvar "clayer")
  )

  ; OPERATION - Turn off the system echo
  (setvar "cmdecho" 0)

  ; OPERATION - Delete auxiliary data, if any
  (if (/= set_line nil) (vla-delete (vlax-ename->vla-object set_line)))
  (if (/= reference_circle1 nil) (vla-delete (vlax-ename->vla-object reference_circle1)))
  (if (/= reference_circle2 nil) (vla-delete (vlax-ename->vla-object reference_circle2)))

  ; INPUT - Point 1
  (princ "\nSelect point A: ")
  (setq ans (DT:PointOrString))
  (cond
    ((= ans T) ; use last points
      (setq p1 (list (car (nth 0 INT_LastPoints)) (cadr (nth 0 INT_LastPoints)) 0.0 ) )
    );END subcond
    ((= 'LIST (type ans)) ; take new points
      (setq p1 ans)
    );END subcond
    (t
      (setq
        ans nil
        p1 (getpoint "\nSelect point A: ")
      )
    );END subcond
  );END if

  ; OPERATION - Create auxiliary data and objects
  (setq
    real_radius 8
    correction_factor 0.001
    radius (* real_radius (* correction_factor (getvar "viewsize")))  ; Calculate circle size at curent zoom

    reference_circle1 ( _Reference_Circle p1 radius)
  )

  ;INPUT - Point 1 level
  (if (= 'LIST (type ans))
      (setq z1 (DT:clic_or_type_level))
      (setq z1 (caddr (nth 0 INT_LastPoints)) )
  );END if

  (princ "\nLevel A = ")(princ z1)(princ "m")

  ; INPUT - Point 2
  (if (= 'LIST (type ans))
      (setq p2 (getpoint "\nSelect point B: "))
      (setq p2 (list (car (nth 1 INT_LastPoints)) (cadr (nth 1 INT_LastPoints)) 0.0 ) )
  );END if

  ; OPERATION - Create auxiliary data and objects
  (setq
    set_line ( _Set_Line p1 p2)
    reference_circle2 ( _Reference_Circle p2 radius)
  )

  ;INPUT - Point 2 level
  (if (= 'LIST (type ans))
      (setq z2 (DT:clic_or_type_level))
      (setq z2 (caddr (nth 1 INT_LastPoints)) )
  );END if

  (princ "\nLevel B = ")(princ z2)(princ "m")


  ; OPERATION - Calculate gradient and print it
  (setq d12 (distance p1 p2))                ; Distance 1-2
  (princ (strcat "\nDistance = " (LM:rtos d12 2 3) "m" ) )
  (if (= z1 z2)
    (princ "\nSelected points are at the same level.")
    (princ
      (strcat
        "\nZ difference = " (LM:rtos (abs (- z2 z1)) 2 3) "m"
        "\nGradient = 1/" (itoa (LM:Round (abs (/ d12 (- z2 z1))))) " (" (LM:rtos (abs (* 100 (/ (- z2 z1) d12))) 2 2) "%)"
      )
    )
  )
  ; OPERATION - Save points as global
  (setq
    INT_LastPoints
    (list
      (list (nth 0 p1) (nth 1 p1) z1) ; p1
      (list (nth 0 p2) (nth 1 p2) z2) ; p2
    )
  )

  ; INPUT - Choose action: pick, find, lowpoint
  (initget "Pick Find Lowpoint W")
  (setq mode (getkword "\nSelect what to do [Pick/Find/Lowpoint] points <Pick>: "))
  (if (not mode) (setq mode "Pick"))

  (setq variable_vacia nil)
  (cond
    ((= mode "Pick") ; Clic and return point level
      (while (= variable_vacia nil)
        ; INPUT - Point 3
        (setvar "osmode" 545)
        (setq p3 (getpoint "\nSelect point to get level (or press Esc to exit): "))
        (if (not p3) (exit))
        (setq
          d13 (distance p1 p3)                ; Distance 1-3
          ang12 (angle p1 p2)                 ; Angle 1-2
          ang13 (angle p1 p3)                 ; Angle 1-3
          ang123 (- ang13 ang12)              ; Angle 1-3
          d14 (* d13 (cos ang123))            ; Distance 1-4
          p4 (polar p1 ang12 d14)             ; Point 4 (intersection between p1-p2 line and a second line perpendicular to p1-p2 line which contains p3)
          z4 (+ z1 (* d14 (/ (- z2 z1) d12))) ; Calculate leves
          level (rtos z4 2 3)                  ; Convert levels to text (3 decimals)
        ); END setq
        (CopyToClipboard level)
        (princ (strcat "\nLevel = " level "  (value copied to clipboard)"))

        ; OPERATION - Introduce point 4
        (setvar "osmode" 0)
        (if (/= nil (tblsearch "block" "PI_DT"))
          (command "._insert" "PI_DT" p3 "0.25" "0.25" "" level)
        );END if
      ); END while Pick
    ); END cond Pick
    ((= mode "Find") ; Introduce level and return point
      (if (= z1 z2)
        (princ "\nAs said, selected points are at the same level.")
        (while (= variable_vacia nil)
          ; INPUT - Level 3
          (setq z3 (getreal "\nIntroduce level to get point (or press Esc to exit): "))
          (if (not z3) (exit))
          (setq
            p1 (list (car p1) (cadr p1) z1 )    ; Convert p1 to 3D point
            p2 (list (car p2) (cadr p2) z2 )    ; Convert p2 to 3D point
            u0 (- (car p2) (car p1))            ; Unit vector u
            v0 (- (cadr p2) (cadr p1))          ; Unit vector v
            w0 (- (caddr p2) (caddr p1))        ; Unit vector w
            d13 (/ (- z3 (caddr p1)) w0)        ; Distance 1-3
            x3 (+ (car p1)  (* d13 u0) )        ; X coordinate
            y3 (+ (cadr p1) (* d13 v0) )        ; y coordinate
            p3 (list x3 y3 0)                   ; Point 3
            level (rtos z3 2 3)                  ; Convert level to text (3 decimals)
          ); END setq
          (princ (strcat "\nLevel = " level))

          ; OPERATION - Introducir punto 3
          (setvar "osmode" 0)
          (command "._insert" "PI_DT" p3 "0.25" "0.25" "" level)
        ); END while Find
      );END if
    ); END cond Find
    ((= mode "Lowpoint") ; Introduce gradient and return low point
      ; INPUT - Gradient
      (setq grad (getint "\nSelect minimum gradient <1/80>: "))
      (if (not grad) (setq grad 80))
      (setq
        ang12 (angle p1 p2)                 ; Angle 1-2
        ang21 (angle p2 p1)                 ; Angle 2-1
        d23 (* (- z2 z1) grad)              ; Distance 2-3
        p3 (polar p2 ang21 d23)             ; Point 3 (same level as point 1)
        d13 (distance p1 p3)                ; Distance 1-3
        d14 (* 0.5 d13)                     ; Distance 1-4
        p4 (polar p1 ang12 d14)             ; Point 4
        z4 (- z1 (/ d14 grad))              ; Level 4
        level (rtos z4 2 3)                  ; Convert level to text (3 decimals)
      ); END setq
      (princ (strcat "\nLevel = " level))

      ; OPERATION - Introduce point 4
      (setvar "osmode" 0)
      (command "._insert" "PI_DT" p4 "0.25" "0.25" "" level)
    ); END cond Find
    ((= mode "W")
      (alert "hola")
    );END subcond
  ); END cond

  ; OPERATION - Delete auxiliary data, if any
  (if (/= set_line nil) (vla-delete (vlax-ename->vla-object set_line)))
  (if (/= reference_circle1 nil) (vla-delete (vlax-ename->vla-object reference_circle1)))
  (if (/= reference_circle2 nil) (vla-delete (vlax-ename->vla-object reference_circle2)))

  ; Restore previous settings
  (setvar "osmode" oldosmode)
  (setvar "cmdecho" oldcmdecho)
  (setvar "clayer" oldclayer)

  ; End without double messages
  (princ)
)
(defun DT:PointOrString( / *error* old_pickbox)
  (defun *error* ( msg ) (setvar "pickbox" old_pickbox) (princ) )
  ; Return "Last" or point
  (setq old_pickbox (getvar "pickbox"))
  (setvar "pickbox" 0)
  (setq in (grread nil 8) )
  (setvar "pickbox" old_pickbox)
  (cond
    ( (and
        (= (car in) 2 )
        (= (cadr in) 119 )
      )
      T
    );END subcond
    ( (= (car in) 3 )
      (cadr in)
    );END subcond
    (t
      (setvar "pickbox" old_pickbox)
    );END subcond
  );END cond
  ; v0.0 - 2016.11.17 - First issue
  ; Author: David Torralba
  ; Last revision: 2016.11.17
);END defun

; How to insert a block with attributes without "(command)"
(entmake
  (list
    '(0 . "INSERT")
    '(66 . 1); has attribute(s)
    (cons 2 "YourBlockName")
    (cons 10 YourInsertionPoint)
    '(41 . 1.0); X
    '(42 . 1.0); Y
    '(43 . 1.0); Z
;      (cons 50 YourRotation); if not default 0
  ); list
)
(entmake
  (list
    '(0 . "ATTRIB")
    (cons 10 YourBaseLineLeftEnd);;;;; get from Attribute data within Block definition
    (cons 40 YourHeight)
    (1 . "YourTextContent")
;      (cons 50 YourRotation); if not default 0
    (cons 72 YourValue); part of justification
    (cons 11 YourInsertionPoint);;;;; get from Attribute data within Block definition
    (cons 2 "YOURTAG")
    '(70 . 0); I don't know what, but necessary
    (cons 73 YourValue); part of justification
  )
)
(entmake
  '((0 . "SEQEND"))
)
; Levels and drainage shortcuts 24.11.2016
(defun c:1() (c:BYC))
(defun c:2() (c:INT))
(defun c:3() (c:SDIP))
(defun c:4( / ref dif)
  ; Update target text with reference level minus 0.310
  (princ "\nSubase below BLOCK (light traffic):\n")
  (setq ref (DT:clic_or_type_level) dif 0.310)
  (princ (strcat (LM:rtos (- ref dif) 2 2) "m\n"))
  (DT:SetText (car (entsel "\nSelect target text: ")) (strcat "SB" (LM:rtos (- ref dif) 2 2)) )
);END defun
(defun c:44( / ref dif)
  ; Update target text with reference level minus 0.380
  (princ "\nSubase below BLOCK (heavy traffic):\n")
  (setq ref (DT:clic_or_type_level) dif 0.380)
  (princ (strcat (LM:rtos (- ref dif) 2 2) "m\n"))
  (DT:SetText (car (entsel "\nSelect target text: ")) (strcat "SB" (LM:rtos (- ref dif) 2 2)) )
);END defun
(defun c:5( / ref dif)
  ; Update target text with reference level minus 0.290
  (princ "\nSubase below TARMAC paving (light traffic):\n")
  (setq ref (DT:clic_or_type_level) dif 0.290)
  (princ (strcat (LM:rtos (- ref dif) 2 2) "m\n"))
  (DT:SetText (car (entsel "\nSelect target text: ")) (strcat "SB" (LM:rtos (- ref dif) 2 2)) )
);END defun
(defun c:55( / ref dif)
  ; Update target text with reference level minus 0.360
  (princ "\nSubase below TARMAC paving (heavy traffic):\n")
  (setq ref (DT:clic_or_type_level) dif 0.360)
  (princ (strcat (LM:rtos (- ref dif) 2 2) "m\n"))
  (DT:SetText (car (entsel "\nSelect target text: ")) (strcat "SB" (LM:rtos (- ref dif) 2 2)) )
);END defun
(defun c:6() (c:garden_gradient))
; Block, Xref or Layout
; Credit to Lee Ambrosius
; http://hyperpics.blogs.com/beyond_the_ui/2012/04/are-you-a-block-an-xref-or-a-layout.html
(vl-load-com)
(defun c:WhatKindOfBlockAmI ( / acadObj doc msg block)
    (setq acadObj (vlax-get-acad-object))
    (setq doc (vla-get-ActiveDocument acadObj))

    (setq msg "")

    ;; Step through all the blocks in the Blocks table
    (vlax-for block (vla-get-Blocks doc)
        (cond
            ;; Standard or dynamic block?
            ((and (= (vla-get-IsLayout block) :vlax-false)
                  (= (vla-get-IsXRef block) :vlax-false))
                (if (= (vla-get-IsDynamicBlock block) :vlax-false)
                    (setq msg (strcat msg (vla-get-Name block) ": Standard"))
                    (setq msg (strcat msg (vla-get-Name block) ": Dynamic"))
                )

                ;; Has attributes?
                (setq attsExist "")
                (vlax-for ent block
                    (if (= (vla-get-ObjectName ent) "AcDbAttributeDefinition")
                        (setq attsExist " with attributes")
                    )
                )

                (setq msg (strcat msg attsExist))
            )
            ;; Xref?
            ((= (vla-get-IsXRef block) :vlax-true)
                (setq msg (strcat msg (vla-get-Name block) ": Xref"))
                (if (= (vla-get-IsLayout block) :vlax-true)
                    (setq msg (strcat msg (vla-get-Name block) " and layout"))
                )
            )
            ;; Layout?
            ((= (vla-get-IsLayout block) :vlax-true)
                (setq msg (strcat msg (vla-get-Name block) ": Layout only"))
            )
        )
        (setq msg (strcat msg "\n"))
    )

    ;; Display the block information for this drawing
    (alert (strcat "This drawing contains blocks of the following types: " msg))
)
(defun c:1() (DT:AddSewerLabel (car (entsel)) ) )
(defun DT:AddSewerLabel( ent_name / p1 p2 ang lay )
  ; NOT FINISHED -------------------------------------------------- NOT FINISHED
  ; Add a readable text aligned with the selected line and centered
  ; ent_name [ename] - Reference line entity name
  (setq
    p1 (cdr (assoc 10 (entget ent_name)) )
    p2 (cdr (assoc 11 (entget ent_name)) )
    ang (angle p1 p2)
    p_ins (DT:mid3dPoint p1 p2)
  )
  (entmakex
    (list
      (cons 0 "TEXT")                       ; entity type
      (cons 1 "test text")                  ; content
      (cons 40 0.35)                        ; text size
      (cons 11 p_ins)                          ; insertion point
      (cons 50 (DT:ReadableTextAngle ang) ) ; rotation
      (cons 10 '(0.0 0.0 0.0))
      (cons 72 1)
      (cons 73 2)
      (if (not (tblsearch "style" "ARIAL")) (cons 7 "standard") (cons 7 "ARIAL")) ; Text style: ARIAL, if possible
    );END list
  );END entmakex
  ; NOT FINISHED -------------------------------------------------- NOT FINISHED
)
(defun c:1() (setvar "clayer" "e-asd-PH2") (DT:LinkedBlocks "0000"))
(defun c:2() (setvar "clayer" "e-psd-PH2") (DT:LinkedBlocks "00rwp"))
(defun c:3() (command "_erase" "L" ""))
(defun DT:LinkedBlocks ( blockName / p1 p2)
  (setq p2 (getpoint) )
  (entmakex (list (cons 0 "INSERT") (cons 2 blockName) (cons 10 p2 ) ))
  (while (not kkkk)
    (setq
      p1 p2
      p2 (getpoint)
    )
    (entmakex (list (cons 0 "LINE") (cons 10 p1) (cons 11 p2) ))
    (entmakex (list (cons 0 "INSERT") (cons 2 blockName) (cons 10 p2 ) ))
  );END while
  (princ)
);END defun
(defun c:3() (command "_erase" "L" ""))
(defun DT:LinkedBlocks ( blockName / p1 p2)
  (if blockName
    (if (setq p2 (getpoint) )
      (progn
        (if (entmakex (list (cons 0 "INSERT") (cons 2 blockName) (cons 10 p2 ) ))
          (while (not kkkk)
            (setq
              p1 p2
              p2 (getpoint)
            )
            (if (and p1 p2)
              (if (entmakex (list (cons 0 "LWPOLYLINE") (cons 100 "AcDbEntity") (cons 100 "AcDbPolyline") (cons 90 2) (cons 10 p1) (cons 10 p2) ) )
                (if (entmakex (list (cons 0 "INSERT") (cons 2 blockName) (cons 10 p2 ) ))
                  T
                  (princ "ERROR: second INESRT entmakex didn't work")
                );END if
                (princ "ERROR: LWPOLYLINE entmakex didn't work")
              );END if
              (princ "ERRRO: p1 or p2 not defined within while loop")
            );END if
          );END while
        );END if
      );END progn
      (princ "\nERROR: p2=nil")
    );END if
    (princ "\nERROR: not blockName defined.")
  );END if
  (princ)
)
; How to wrap groups of commands and UNDO them as a group:
;|
(command "._UNDO" "_Begin") ; or (command "UNDO" "BE")
  (setq old_osmode (getvar "OSMODE"))
  (setvar "OSMODE" 0)
  (command "._circle" "5,5" "2")    ;Draws a circle
  (command "._line" "3,5" "7,5" "") ;Draws a line
  (setq el (entlast))               ;Gets the last entity added
                                    ; to the drawing
  (setq pt '(5 7))                  ;Sets the trim point
  (command "._trim" el "" pt "")    ;Performs the trim
  (setvar "OSMODE" old_osmode)
  (command "._UNDO" "_End"); or (command "UNDO" "END")
)
|;
(defun c:4() (princ "\nRoadOffsetForDrainage: ") (DT:RoadOffsetForDrainage))
(defun DT:RoadOffsetForDrainage( / p ent_name )
  ; Select a polyline within an Xref, and offset it by 0.5m and 1m either side
  (c:N)
  ; Check if groups exists
  (setq ent_name (entlast) )
  (vla-offset (vlax-ename->vla-object ent_name) 0.5)
  (command "_groupedit" "N" "road_offsets" "A" (entlast) "")
  (vla-offset (vlax-ename->vla-object ent_name) 1)
  (command "_groupedit" "N" "road_offsets" "A" (entlast) "")
  (vla-offset (vlax-ename->vla-object ent_name) -0.5)
  (command "_groupedit" "N" "road_offsets" "A" (entlast) "")
  (vla-offset (vlax-ename->vla-object ent_name) -1)
  (command "_groupedit" "N" "road_offsets" "A" (entlast) "")
  (vla-delete (vlax-ename->vla-object ent_name))
  (princ)
)
(defun c:1( / x )
  (defun LM:getdynprops ( blk )
    (mapcar '(lambda ( x ) (cons (vla-get-propertyname x) (vlax-get x 'value)))
        (vlax-invoke blk 'getdynamicblockproperties)
    )
  )
  (setq x (vlax-ename->vla-object (car (entsel))) )
  (LM:getdynprops x )
)
(defun DT:GetHandle( ent_name )
  ; Returns a string with provided entity's handle
  ; ent_name [ename] - Entity name
  (if ent_name
    (cdr (assoc 5 (entget ent_name)))
  );END if
)
(defun c:1( / ent_name blockName a b)
  ; Return selected block attribute DXF data
  (setq ent_name (car (entsel)) )  (princ "\nent_name = ")(princ ent_name)
  (setq blockName (LM:effectivename (vlax-ename->vla-object ent_name)) )        (princ "\nblockName = ")(princ blockName)
  (setq a (cdr (assoc -2 (tblsearch "block" blockName))) )                     (princ "\nblock definition ent_name = ")(princ a1)
  (setq b a)
    (while (setq b (entnext b))
    (if (= "ATTDEF" (cdr (assoc 0 (entget b))))
      (progn
        (princ "\nsubentity = ")(princ b)(princ "   >> ")(princ (cdr (assoc 0 (entget b))))(princ " ")(princ (assoc 2 (entget b)))(princ (assoc 1 (entget b)))(princ (assoc 70 (entget b)))
        ;(princ "\n")(princ (entget b))
        (princ "\n.")
      );END progn
    );END if
  );END while
  (princ)
)
(defun c:1( / xy z )
  ; Insert 3D points with position and level on a loop
  (while T
    (setq
      xy (getpoint "\nSelect XY coordinates: ")
      z (DT:clic_or_type_level)
    )
    (point (list (nth 0 xy) (nth 1 xy) z))
  );END while
)
(defun c:1()
  ; Draw a leader with no text
  (command "_leader" (getpoint) (getpoint) "" "" "N")
)
(defun c:1( / ss lay z)
  ; Sort 2D polylines in layers according to theis elevation
  (setq ss (ssget))
  (foreach a (ssnamex ss)
    (if (= 'ename (type (cadr a)))
      (progn
        (setq
          z (vla-get-elevation (vlax-ename->vla-object (cadr a)))
          lay (strcat "e-EG-" (LM:rtos z 2 1) "m")
        )
        (command "-layer" "m" lay "")
        (vla-put-layer (vlax-ename->vla-object (cadr a)) lay)
      );END progn
    );END if
  );END foreach
)
(defun c:3( / lay ent_name1 ent_name2 )
  ; "Copy to layer" and freeze the target layer, in a loop
  (while T
    (setq
      ent_name1 (car (entsel "\n1: "))
      ent_name2 (car (entsel "\n2: "))
      lay (cdr (assoc 8 (entget ent_name2)))
    )
    (vla-copy (vlax-ename->vla-object ent_name1))
    (vla-put-layer (vlax-ename->vla-object (entlast)) lay)
    (command "-layer" "f" lay "")
  )
)
(defun c:4( / ent_name startZ endZ z lay)
  ; Copy the object on all the layers from "e-EG--0.3m" to "e-EG-1.9m"
  (setq
    ent_name (car (entsel))
    startZ -0.3
    endZ 1.9
    z startZ
  )
  (while (<= z endZ)
    (setq
      lay (strcat "e-EG-" (LM:rtos z 2 1) "m")
    )
    (princ "\n")(princ lay)
    (vla-copy (vlax-ename->vla-object ent_name))
    (vla-put-elevation (vlax-ename->vla-object (entlast)) z)
    (vla-put-layer (vlax-ename->vla-object (entlast)) lay)
    (setq z (+ z 0.1))
  );ENd while
  (princ)
)
(defun c:4( / VL_ent_name )
  ; Remove first vertex and close polyline
  (if (setq ss (ssget '(( 0 . "LWPOLYLINE")) ))
    (foreach a (ssnamex ss)
      (if (= 'ename (type (cadr a)))
        (if (= "LWPOLYLINE" (cdr (assoc 0 (entget (cadr a)))))
          (if (= :vlax-false (vla-get-closed (vlax-ename->vla-object (cadr a))))
            (progn
              ; Check if first and last vertex are the same or very close points ------------------------------------------ TODO
              ; Remove first vertex
              (DT:RemovePolylineVertex (cadr a) (vlax-curve-getEndParam (vlax-ename->vla-object (cadr a))))
              ; Close polyline
              (vla-put-closed (vlax-ename->vla-object (cadr a)) :vlax-true)
              (vlax-put-property (vlax-ename->vla-object (cadr a)) 'Color 256)
            );END progn
          );END if
        );END if
      );END if
    );END foreach
  );END if
  ;(princ)
)
(defun c:5( / VL_ent_name )
  ; <Mark not closed polylines in blue
  (if (setq ss (ssget '(( 0 . "LWPOLYLINE")) ))
    (foreach a (ssnamex ss)
      (if (= 'ename (type (cadr a)))
        (if (= "LWPOLYLINE" (cdr (assoc 0 (entget (cadr a)))))
          (if (= :vlax-false (vla-get-closed (vlax-ename->vla-object (cadr a))))
            (vlax-put-property (vlax-ename->vla-object (cadr a)) 'Color 5)
          );END if
        );END if
      );END if
    );END foreach
  );END if
  ;(princ)
)
(defun c:0( / startZ endZ z layerName)
  ; Create the layers from "e-clash--0.3m" to "e-clash-2.2m"
  (setq
    startZ -0.3
    endZ 2.2
    z startZ
  )
  (while (<= z endZ)
    (setq
      layerName (strcat "e-clash-" (LM:rtos z 2 1) "m")
      z (+ z 0.1)
    )
    (command "-layer" "m" layerName "")
    (princ "\n")(princ layerName)
  );ENd while
  (princ)
)
(defun c:1 ()
  ; This function selects the polylines within the selection set,
  ; closes them and creates an individual associative hatch for each polyline in the polyline layer
  ; and changes the transparency of the hatch to 0.3
  (vl-load-com)
  (foreach a (ssnamex (ssget '((-4 . "<OR") (0 . "LWPOLYLINE") (0 . "POLYLINE") (-4 . "OR>"))) )
    (if (= 'ename (type (cadr a)))
      (progn
        ; If clayer is different ot object layer, update layer to object layer
        (if (/= (getvar "clayer") (cdr (assoc 8 (entget (cadr a)))) )
          (setvar "clayer" (cdr (assoc 8 (entget (cadr a)))))
        );END if

        ; Create hatch
        (DT:ha (cadr a) "SOLID" "1")

        ; Change hatch transparency
        (if (= "HATCH" (cdr (assoc 0 (entget (entlast)))))
          (vla-put-entitytransparency (vlax-ename->vla-object (entlast)) "80")
        );END if
      );END progn
    );END if
  );END foreach
)
(defun c:1 ( / blockName p1 p2 nVertex)
  ; This function selects LINES and POLYLINES,
  ; and inserts in the middle an inline block,
  ; keeping in mind readability angle.
  (vl-load-com)
  (defun DT:InsertAlignedBlock ( blockName p1 p2 )
    (entmakex (list (cons 0 "INSERT") (cons 2 blockName) (cons 10 (DT:mid3dPoint p1 p2)) (cons 50 (DT:ReadableTextAngle (angle p1 p2))) ))
  )
  (setq blockName "000" )
  (foreach a (ssnamex (ssget '((-4 . "<OR") (0 . "LINE") (0 . "LWPOLYLINE") (-4 . "OR>"))) )
    (if (= 'ename (type (cadr a)))
      (progn
        (cond
          ; If it's a line
          ((= "LINE" (cdr (assoc 0 (entget (cadr a)))))
            (setq
              p1 (cdr (assoc 10 (entget (cadr a))))
              p2 (cdr (assoc 11 (entget (cadr a))))
            )
            (DT:InsertAlignedBlock blockName p1 p2)
          );END subcond
          ; If it's not a line (it's a lwpolyline)
          (t
            ; Get ammount of vertexes
            (setq nVertex (vlax-curve-getEndParam (vlax-ename->vla-object (cadr a))) )
            (cond
              ; If it has 2 vertexes (single segment)
              ((= nVertex 1)
                (setq
                  p1 (vlax-curve-getPointAtParam (vlax-ename->vla-object (cadr a)) 0)
                  p2 (vlax-curve-getPointAtParam (vlax-ename->vla-object (cadr a)) 1)
                )
                (DT:InsertAlignedBlock blockName p1 p2)
              );END subcond
              ; If other case (only can have more vertexes, multiple segments)
              (t
                (while (> nVertex 0) ; Run through all segments
                  (setq
                    p1 (vlax-curve-getPointAtParam (vlax-ename->vla-object (cadr a)) nVertex)
                    p2 (vlax-curve-getPointAtParam (vlax-ename->vla-object (cadr a)) (+ nVertex -1))
                    nVertex (+ nVertex -1)
                  )
                  (DT:InsertAlignedBlock blockName p1 p2)
                );END while
              );END subcond
            );END cond
          );END subcond
        );END cond
      );END progn
    );END if
  );END foreach
  (princ)
)
(defun c:11 ( / targetLevel ent_name )
  ; Get a FFL, substract -0.65m and overwrite the target text object content
  ; with the calculated value properly formated: S16.70
  (princ "\nGET STORM LEVEL FROM FFL\n")
  (setq
    targetLevel (+ (DT:clic_or_type_level) -0.65)
    ent_name (car (entsel (strcat "\nSelect text to overwrite with \"S" (LM:rtos targetLevel 2 2) "\": ") ))
  )
  (if ent_name
    (vlax-put-property (vlax-ename->vla-object ent_name) 'TextString (strcat "S" (LM:rtos targetLevel 2 2)) )
    (princ "\nNo target entity selected.")
  );END if
  (princ)
)
(defun c:11 ( / targetLevel ent_name )
  ; Get a FFL, substract -0.65m and overwrite the target text object content
  ; with the calculated value properly formated: S16.70
  (princ "\nGET STORM LEVEL FROM FFL\n")
  (setq
    targetLevel (+ (DT:clic_or_type_level) -0.65)
    ent_name (car (entsel (strcat "\nSelect text to overwrite with \"S" (LM:rtos targetLevel 2 2) "\": ") ))
  )
  (if ent_name
    (vlax-put-property (vlax-ename->vla-object ent_name) 'TextString (strcat "S" (LM:rtos targetLevel 2 2)) )
    (princ "\nNo target entity selected.")
  );END if
  (princ)
)
(defun c:22 ( / targetLevel ent_name )
  ; Get a FFL, substract -0.75m and overwrite the target text object content
  ; with the calculated value properly formated: F16.70
  (princ "\nGET FOUL LEVEL FROM FFL\n")
  (setq
    targetLevel (+ (DT:clic_or_type_level) -0.75)
    ent_name (car (entsel (strcat "\nSelect text to overwrite with \"F" (LM:rtos targetLevel 2 2) "\": ") ))
  )
  (if ent_name
    (vlax-put-property (vlax-ename->vla-object ent_name) 'TextString (strcat "F" (LM:rtos targetLevel 2 2)) )
    (princ "\nNo target entity selected.")
  );END if
  (princ)
)
(defun c:0 ( / targetLevel ent_name )
  ; Get a FFL, substract -0.150m and overwrite the target text object content
  ; with the calculated value properly formated: %%U16.70
  (while T
    (princ "\nGET -0.150mm FROM FFL\n")
    (setq
      targetLevel (+ (DT:clic_or_type_level) -0.15)
      ent_name (car (entsel (strcat "\nSelect text to overwrite with \"%%U" (LM:rtos targetLevel 2 2) "\": ") ))
    )
    (if ent_name
      (progn
        (vlax-put-property (vlax-ename->vla-object ent_name) 'TextString (strcat "%%U" (LM:rtos targetLevel 2 2)) )
        (vla-put-color (vlax-ename->vla-object ent_name) 1)
      );END progn
      (princ "\nNo target entity selected.")
    );END if
  );END while
  (princ)
)

(defun c:1( / ss )
  ; Correct all text angle to be readable within layer "LABELS"
  (princ "\nUPDATE CONTOUR ANNOTATION TEXT READABILITY ANGLE\n")
  (if (setq ss (ssget '(( 0 . "TEXT"))))
    (foreach a (ssnamex ss)
      (if (= 'ename (type (cadr a)))
        (if
          (and
            (= "TEXT" (cdr (assoc 0 (entget (cadr a)))))
            ;(= "LABELS" (cdr (assoc 8 (entget (cadr a)))))
          );END and
        (if
          (/=
            (cdr (assoc 50 (entget (cadr a)) ))
            (setq ang (DT:ReadableTextAngle (cdr (assoc 50 (entget (cadr a)) )) ) )
          )
          (vlax-put-property (vlax-ename->vla-object (cadr a)) 'Rotation ang )
        );END if
        );END if
      );END if
    );END foreach
  );END if
)
(defun c:2() (c:KTF_LABELCNT))
(defun c:1( / ss )
  ; Move selected objects to layer
  (while T
    (if (setq ss (ssget))
      (foreach a (ssnamex ss)
        (if (= 'ename (type (cadr a)))
          (if (= "INSERT" (cdr (assoc 0 (entget (cadr a)))))
            (princ "\nBlock found and skipped.")
            (vla-put-layer (vlax-ename->vla-object (cadr a)) "__BA-pond")
          );END if
        );END if
      );END foreach
    );END if
    (setq ss nil)
  );END while
)
; Create a layer with vla commands
(vla-add
  (vla-Get-Layers
    (vla-get-ActiveDocument
      (vlax-get-Acad-Object)
    )
  )
  "newLayerName"
)
(defun c:zzz ( / i )
  ; Mark in yellow the sewer labels with a gradient bigger than 80
  ; TODO:
  ; Build DT:GetSewerSize and mark just DN100 steeper than 1/80 and D150 steeper 1/150.
  (setq i 0)
  (foreach a (ssnamex (ssget))
    (if (= 'ename (type (cadr a)))
      (if (or (= "TEXT" (cdr (assoc 0 (entget (cadr a))))) (= "MTEXT" (cdr (assoc 0 (entget (cadr a))))))
        (if
          (and
            (DT:GetSewerGradient (cadr a))
            (< (DT:GetSewerGradient (cadr a)) 5)
          );END and
          (progn
            ; Change color to yellow
            (vlax-put-property (vlax-ename->vla-object (cadr a)) 'Color 2)
            ; Add it to counter
            (setq i (+ i 1) )
          );END progn
        )
      );END if
    );END if
  )
  (if (= 1 i)
    (progn (princ (strcat "\n" (itoa i) " object marked.")) (princ))
    (progn (princ (strcat "\n" (itoa i) " objects marked.")) (princ))
  );END if

  ; v0.1 - 2017.03.23 - Marked object counter added
  ;                   - MTEXT object type added
  ; v0.0 - 2017.03.?? - First issue
  ; Author: David Torralba
  ; Last revision: 2017.03.23
)
(defun AT:InsertBlock (#Name #InsPt #XScale #YScale #Rot)
  ;;; Insert block into drawing
  ;;; #Name - name of block
  ;;; #InsPt - insert point
  ;;; #XScale - block X scale
  ;;; #YScale - block Y scale
  ;;; #Rot - block rotation
  ;;; Alan J. Thompson, 04.21.09
  (if (or (tblsearch "block" #Name)
          (findfile #Name)
      ) ;_ or
    (vla-insertblock
      ((if (eq (getvar "cvport") 1)
         vla-get-paperspace
         vla-get-modelspace
       ) ;_ if
        (vla-get-ActiveDocument
          (vlax-get-acad-object)
        ) ;_ vla-get-ActiveDocument
      )
      (vlax-3d-point #InsPt)
      #Name
      #XScale
      #YScale
      #XScale
      #Rot
    ) ;_ vla-insert-block
  ) ;_ if
)
(defun c:ttr ( / obj lay)
  ; Make transparent nested object real layer in the current viewport
  (if
    (and
      (/= "Model" (getvar "CTAB"  ) )
      (<  1       (getvar "CVPORT") )
    );END and
    (progn
      (if (setq obj (car (nentsel "\nSelect object to grey layer out: ")))
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
        (if
          (or
            (= 1 (cdr (assoc 62 (tblsearch "layer" lay))))
            (= 10 (cdr (assoc 62 (tblsearch "layer" lay))))
            (= 240 (cdr (assoc 62 (tblsearch "layer" lay))))
          );END or
          (command "vplayer" "TR" 40 lay "C" "")
          (command "vplayer" "TR" 70 lay "C" "")
        );END if
      );END if

    );END progn true
    (progn
      (alert "You are not in a viewport.")
      (quit)
    );END progn false
  );END if
  (princ)

  ; v0.0 - 2017.03.16
  ; Author: David Torralba
  ; Last revision: 2017.03.16
)
(defun c:xx( / p ans )
  ; Rise or lower selected object
  ; tags: move up, move down, up/down
  (if TEMP_global_diff
    (if (setq ans (getreal (strcat "\nType +/- diff in mm <" (LM:rtos TEMP_global_diff 2 0) ">: ")) )
      (setq TEMP_global_diff ans )
    );END if
    (setq TEMP_global_diff (getreal "\nType +/- diff in mm: ") )
  );END if
  (command
    "_.move" pause ""
    "_non" (setq p (cadr (grread 't)))
    "_non" (list (nth 0 p) (nth 1 p) (+ (nth 2 p) (* 0.001 TEMP_global_diff)) )
  )
  ; v0.0 - 2017.03.20 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.03.20
)
(defun c:xx( / txt )
  ; Add/substract level to selected level.
  (setq
    txt (strcat "%%U" (LM:rtos (+ (DT:clic_or_type_level) 0.050 (/ 1.20 40)) 2 3))
  );END setq
  (princ (strcat "\nLevel footpath backedge = " (substr txt 4) "m"))
  (DT:SetText (car (entsel "\nSelect text to override: ")) txt )

  ; v0.0 - 2017.04.03 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.04.03
)
(defun c:xx ( / VLCurrentLayout currentTabName newTabName )
  ; Rename current tab    name
  ; Rename current layout name
  (setq
    ; Get current layout object
    VLCurrentLayout (vla-get-activelayout (vla-get-ActiveDocument (vlax-get-acad-object)))
    currentTabName (vla-get-Name VLCurrentLayout)
    newTabName (strcat "5613-" (substr currentTabName 6))
  );END setq
  (vla-Put-Name VLCurrentLayout newTabName)

  ; v0.0 - 2017.04.10 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.04.10
)
(defun c:xx ( )
  ; Rename all tabs    starting with "5092-" to "5613-"
  ; Rename all layouts starting with "5092-" to "5613-"
  (vlax-for x (vla-get-Layouts (vla-get-ActiveDocument (vlax-get-acad-object) ))
    (if (= (substr (vla-get-Name x) 1 5) "5092-")
      (vla-Put-Name x (strcat "5613-" (substr (vla-get-Name x) 6)))
    );END if
  );END vlax-for

  ; v0.0 - 2017.04.10 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.04.10
)
(defun c:xx ( / ent_name )
  ; Add "VC" to selected sewer label and relocate it
  (if (DT:ReplaceText
        (setq ent_name (car (entsel "\nSelect sewer label to add \"VC\": ")))
        "@ 1"
        "VC @ 1"
      );END DT:ReplaceText
    (DT:FastMove ent_name)
  );END if

  ; v0.0 - 2017.04.20 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.04.20
)
(defun c:xx ( / ent_name )
  ; Replace "VC" for "CO" at selected sewer label and relocate it
  (if (DT:ReplaceText
        (setq ent_name (car (entsel "\nSelect sewer label to add \"VC\": ")))
        "VC @ 1"
        "CO @ 1"
      );END DT:ReplaceText
    (DT:FastMove ent_name)
  );END if

  ; v0.0 - 2017.04.21 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.04.21
)
(defun c:xx ( / ss size )
  ; Find to every sewer label and mark them if they are DN300 or bigger and they have "VC"
  (if (setq ss (ssget "x" '( ( 0 . "TEXT") )))
    (foreach a (ssnamex ss)
      (if (= 'ename (type (cadr a)))
        (if (>= (DT:GetSewerSize (cadr a)) 300)
          (if (= "VC" (substr (DT:GetText (cadr a)) 13 2))
            (vlax-put-property (vlax-ename->vla-object (cadr a)) 'Color 2)
          );END if
        );END if
      );END if
    );END foreach
  );END if

  ; v0.0 - 2017.04.21 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.04.21
)
(defun c:2 ( / ss )
  ; Change selected entities' color to blue
  (princ "\nMarking in blue:")
  (if (setq ss (ssget))
    (foreach a (ssnamex ss)
      (if (= 'ename (type (cadr a)))
        (vlax-put-property (vlax-ename->vla-object (cadr a)) 'Color 2)
      );END if
    );END foreach
  );END if

  ; v0.0 - 2017.03.13 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.03.13
)
(defun c:xx ()
  ; Step one by one all layer objects and ask if to remove
  (foreach a (ssnamex (ssget "x" '((8 . "00"))))
    (if (= 'ename (type (cadr a)))
      (progn
        (princ "\n")
        (princ (entget (cadr a)))
        (DT:ZoomToEntity (cadr a))
        (initget "Yes No")
        (setq ans (getkword "\nDo you want to delete it [Yes/No]? <Yes>"))
        (if (or (= ans "Yes") (not ans))
          (vla-delete (vlax-ename->vla-object (cadr a)))
        );END if
      );END progn
    );END if
  );END foreach

  ; v0.0 - 2017.06.23 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.06.23
)
(defun c:xx ( / pointList doc )
  ; Mark point of given coordinates
  ;

  (setq
    pointList
    '(
      (390354.583 230886.236 0.0)
      (390355.743 230884.053 0.0)
      (390354.485 230847.260 0.0)
      (390353.152 230846.552 0.0)
      (390352.109 230845.997 0.0)
      (390291.254 230833.984 0.0)
      (390204.809 231084.429 0.0)
      (390204.919 231084.063 0.0)
    )
    doc (vla-get-ActiveDocument (vlax-get-acad-object))
  );END setq

  ; Create layer
  (DT:AddLayer "__marks" 2 "CONTINUOUS")
  (setvar "CLAYER" "__marks")

  (vla-startUndoMark doc)
  (foreach a pointList
    (entmakex
      (list
        (cons 0 "CIRCLE")
        (cons 10 a)
        (cons 40 2)
      );END list
    );END entmakex
  );END foreach
  (vla-endUndoMark doc)

  ; v0.0 - 2017.06.22 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.06.22
)
(defun c:xx ()
  ; Step one by one all layer objects and ask if to remove
  (foreach a (ssnamex (ssget "x" '((8 . "e-contours-ph2-0.025m-text"))))
    (if (= 'ename (type (cadr a)))
      (progn
        (princ "\n")
        (princ (entget (cadr a)))
        (DT:ZoomToEntity (cadr a))
        (initget "Yes No")
        (setq ans (getkword "\nDo you want to delete it [Yes/No]? <Yes>"))
        (if (or (= ans "Yes") (not ans))
          (vla-delete (vlax-ename->vla-object (cadr a)))
        );END if
      );END progn
    );END if
  );END foreach

  ; v0.0 - 2017.06.23 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.06.23
)
(defun c:xx (/ blockName doc FLST)
  (vl-load-com)
  (setq doc (vla-get-activedocument (vlax-get-acad-object)) )
  (setq
    blockName (cdr (assoc 2 (entget (car (entsel "\nSelect block: ")))))
    FLST nil
  )
  (fix1 blockName)
  (vl-cmdf "regen")
  (prin1)
)
(defun fix1 (blockName / blockEntityName)
  ; If the blockName is not in FLST, carry on
  (if (not (member blockName FLST))
    (progn
      (setq
        ; Add blockName to FLST
        FLST (cons blockName FLST)
        ; Get entity name of blockName
        blockEntityName (tblobjname "block" blockName)
      )
      ; Get next object within the drawing object table
      (while (setq blockEntityName (entnext blockEntityName))
        ;(print (entget blockEntityName))
        ; If the next object is an INSERT object
        (if (= (cdr (assoc 0 (entget blockEntityName))) "INSERT")
          ; If true, get the name block and pass it as argument to "fix1" (recursive)
          (fix1 (cdr (assoc 2 (entget blockEntityName))))
          ; If false, check if next object is a dimension, if it is DIMENSION object, delete it
          (if
            ;; EDIT THE CONDITION BELOW TO FILTER OBJECTS YOU WANT TO DELETE --------------------------------
            (= (cdr (assoc 8 (entget blockEntityName))) "5396 EngArch$0$E-SURFACE WATER")
            ;; EDIT THE CONDITION ABOVE TO FILTER OBJECTS YOU WANT TO DELETE --------------------------------
            (progn
              (setq
                objectToDelete (vlax-ename->vla-object blockEntityName)
                blk (vla-ObjectIdToObject doc (vla-get-ownerID objectToDelete) )
              );END setq
              (vla-delete objectToDelete)
              (vla-get-count blk)
            );END progn
          );END if
        );END if
      );END while
    );END progn
  );END if
  (princ)
)
