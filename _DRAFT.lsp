; SET - Error handling function
defun
(defun
(defun
(defun *error* ( msg )
  (cond
    ((or
      (= msg "Function cancelled")
      (= msg "quit / exit abort")
     );END or
      (princ (strcat "\nFunction stopped by user."))
    );END subcond
    (t
      (princ (strcat "\nERROR:" msg 2))
    );END subcond
  );END cond
  ; Restore previous settings
  (setvar "system_variable" oldsystem_variable)
  (setq *error* olderror)
  (princ)
)
(defun GetFromClipboard(/ html result)
  (setq html (vlax-create-object "htmlfile")
	result (vlax-invoke (vlax-get (vlax-get html 'ParentWindow) 'ClipBoardData) 'GetData "Text")
  )
  (vlax-release-object html)
  result
)
;Get serial number of the current product
(getvar "_pkser")
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
(defun c:s ()
(command "-text" (cadr (grread 't)) "0.3" "" (strcat "S" (rtos (getreal"\nPica el nivel: ") 2 3)))
(princ)
)
(defun c:ct()
  ; Copy any nested string into the clipboard
  (CopyToClipboard (DT:destripar_txt))
)

(defun c:k1()
	(command "ncopy" pause "" "" "")
	(command "MATCHPROP" objeto_base "L" "")
	(exit)
)
(defun c:HAS()
	(command "-hatch" "S" (ssget) "" "")
	(princ)
)

(defun c:k2 ( / pt)
	(command "-text" "S" "ROMANS" "J" "M" (setq pt (cadr (grread 't))) "1.5" "90" (getstring 't "\nMete texto: "))
  (princ "\nPunto = ")(princ pt)
;	(c:rt)
)

(defun c:2() (command ".-insert" "Twin Garage 22 35-42 48-55 58-63 82-87 93 29.03.16" pause 1000 1000 pause))
(defun c:1() (command ".-insert" "Single Garage 29.03.16" pause 1000 1000 pause))


(defun c:1()
  (command "-text" "S" "ROMANS" (getpoint "\nElige punto: ") "0.3" "90" (strcat "F" (LM:rtos (- (DT:level_detection) 0.75) 2 2) ))
  (princ)
)

(defun c:1(/ p1)
	(command ".-insert" "Private-Square300-Foul-Manhole" (setq p1 (getpoint)) 1 1 0)
	(while (not kkkkk)
		(command ".pline" p1 pause "")
	)
)
(defun c:2(/ p1)
  (princ "\nSelect point to insert RWP: ")
	(command ".-insert" "e-psd-rwp" (setq p1 (getpoint)) 1 1 0)
	(command ".line" p1 pause "")
)
(defun c:3(/ p1)
	(setvar "OSMODE" 4)
	(setq p1 (getpoint))
	(command ".line" p1 pause "")
)
(defun c:2 ( / pt)
	(command "-text" "S" "ARIAL" "J" "BC" (getpoint "\nSelect insertion point") "0.5" "90" (getstring "\nIntroduce text: "))
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
(defun c:2( / p msg oldosmode)
  (setvar "osmode" 33)
  (setq p (getpoint "\nSelect base point: "))
  (setq
    d (- (getreal "\nIntroduce distancia: ") 58)
    oldosmode (getvar "osmode")
  )
  (setvar "osmode" 0)
  (command "pline" p (polar p (* 0.5 pi) d) "")
  (setvar "osmode" oldosmode)
  (princ)
)
; Igualar propiedades de la última entidad creada con la entidad ent_ref (nombre DXF) y luego enviarla al fondo.
(defun c:3()
	(command ".matchprop" ent_ref (entlast))
	(command "draworder" (entlast) "" "B")
)
; Coger el contenido de un texto desde su carácter número 3 hasta el final y copiarlo al ClipBoard
(defun c:2()
  (CopyToClipboard (substr (cdr (assoc 1 (entget (car (entsel "\nSelect a text: "))))) 3))
)
(defun c:1 ()
  (command "area" "Object" (car (entsel "\nSelect closed polyline: ")))
  (CopyToClipboard (LM:rtos (getvar "Area") 2 3))
)
(defun c:2( / ent_name)
  (while (not kkkk)
    (setq ent_name (car (entsel)))
    (command "offset" "E" "Y" (* 0.5 (cdr (assoc 41 (entget ent_name)))) ent_name (getpoint "\nClica dentro del objeto: ") "")
  )
)
; Medir nivel en el long section CON EXAGERACION y copiarlo al ClipBoard: mide distancia vertical, multiplica por exageracion y suma el datum
(defun c:2 ()
  (princ "\nLongsection level (exageration = 10)")
  (CopyToClipboard (LM:rtos (+ 57 (* 0.1 (distance (getpoint "\nPunto inicial: ") (getpoint "\nPunto final: ")))) 2 5))
)

; Medir nivel en el long section sin exageración y copiarlo al ClipBoard: mide distancia vertical y suma el datum
(defun c:1 ()
  (CopyToClipboard (LM:rtos (+ 57 (distance (getpoint "\nPunto inicial: ") (getpoint "\nPunto final: "))) 2 3))
)

; Medir distancia con 3 decimales y copiarla al clipboard
(defun c:2 ()
  (CopyToClipboard (LM:rtos (distance (getpoint "\nPunto inicial: ") (getpoint "\nPunto final: ")) 2 3))
)
; COORD function shortcut, para meter el bloque con las coordenadas rapido
(defun c:3()
  (c:coord)
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

;EngArch Surfacing
(defun c:1()
  (while (not kkk)
    (princ "\nRoad")
    (command "-hatch" "CO" "." "." "P" "AR-HBONE" "7" "" "A" "A" "N" "" (getpoint) "")
  )
  (princ)
)
(defun c:2()
  (while (not kkk)
    (princ "\nFootpath")
    (command "-hatch" "CO" "." "." "P" "DOTS" "350" "" "A" "A" "N" "" (getpoint) "")
  )
  (princ)
)
(defun c:3()
  (while (not kkk)
    (princ "\nVerge")
    (command "-hatch" "CO" "." "." "P" "SQUARE" "200" "" "A" "A" "N" "" (getpoint) "")
  )
  (princ)
)
(defun c:1()
  ; Insert front door block 1, and rotate 90 degree.
  (command "-insert" "Part-m-primary-0" (setq p1 (getpoint "\nPunto 1: ")) 1 1 pause)
  (command "rotate" (entlast) "" p1 "-90")
)
(defun c:2()
  ; Insert rear door block, and rotate 90 degree.
  (command "-insert" "Part-m-secondary" (setq p1 (getpoint "\nPunto 1: ")) 1 1 pause)
  (command "rotate" (entlast) "" p1 "-90")
)
(defun c:3( / p1 p1a p1b)
  ; Insert rear door block between two points, and rotate 90 degree.
  (setvar "osmode" 513)
  (setq
   p1a (getpoint "\nPoint 1a:")
   p1b (getpoint "\nPoint 1b:")
   p1 (polar p1a (angle p1a p1b) (* 0.5 (distance p1a p1b)) )
  )
  (setvar "osmode" 0)
  ;(command "-insert" "Part-m-secondary" p1 1 1 (* (/ 180 pi) (angle p1a p1b)) )
  (command "-insert" "Part-m-secondary" p1 1 1 (* (/ 180 pi) (+ (angle p1a p1b) (* -0.5 pi) ) -1 ) )
  (command "rotate" (entlast) "" p1 "-90")
  (setvar "osmode" 513)
)
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

(defun c:kk ()
  (vlax-map-collection
    (vla-get-blocks
      (vla-get-activedocument
        (vlax-get-acad-object)
      ); END vla-get-activedocument
    ); END vla-get-blocks
    '(lambda (x)
      (and
      (vlax-property-available-p x 'explodable)
      (eq (vlax-get-property x 'explodable) :vlax-true)
      (not (vlax-put-property x 'explodable :vlax-false)))
    )
  ); END vlax-map-collection
)
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
(defun c:xx() (while (not kkkk) (DT:move_part_m)) )
(defun c:1() (DT:move_part_m))
(defun DT:move_part_m( / VL_ent_name p p0 p1 ang)
  ; Move selected m-part block 0.302 toward the inner part of the building (its rotation - 90 degree)
  (setq oldosmode (getvar "osmode"))
  (setvar "osmode" 0)
  (setq
    ent_name (car (entsel))
    VL_ent_name (vlax-ename->vla-object ent_name)
    ang (vlax-get-property VL_ent_name 'Rotation)
    p (vlax-safearray->list (vlax-variant-value (vlax-get-property VL_ent_name 'InsertionPoint)))
    p0 (list (car p) (cadr p) 0.0)
    p1 (polar p0 (- ang (* -0.5 pi )) 0.302)
    ;p1 (polar p0 (- ang (* -0.5 pi )) 0.215)
  )
  (command "move" ent_name "" p0 p1)
  (setvar "osmode" oldosmode)
  (princ)
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:1(
;defun c:ExtarctManholeChainages (
                                /
                                cntl
                                ans
                                )

  ; ALMOST FINISHED - Purpose: store the chainages of selected manholes, to later draw them in the long section with another function
  ; PRECAUTION - If any manhole is not in the line, the routine will assign them the chainage at the beginning or end of the centreline.
  ;

  ; AUXILIARY FUNCTIONS
  (defun DT:PK( VL_ent_name pt )
    (vlax-curve-getDistAtPoint VL_ent_name (vlax-curve-getClosestPointTo VL_ent_name pt))
  )

  (defun DT:SelectManholes( cntl
                          /
                          ss
                          man_db man_entry man_ch_list
                          sorted_man_db sorted_man_ch_list
                          pt ch id il1 il2 il3 il4 vis_sta
                          )
    ; Returns a sorted list (by chaiange) with the selected manhole data
    ;
    ; INPUT - Ask user to select manholes
    (setq
      ss (ssget '((0 . "INSERT")))
      man_db nil
    )
    (foreach a (ssnamex ss)
      (if (= 'ename (type (cadr a)))
        (progn
          ; Check if is a Manhole Block
          (if (= "W-Manhole" (substr (LM:effectivename (vlax-ename->vla-object (cadr a))) 2 9))
            (progn
              (setq
                man_entry (list
                            (DT:PK (vlax-ename->vla-object cntl) (cdr (assoc 10 (entget (cadr a)))))        ; Get manhole chainage
                            (LM:vl-getattributevalue (vlax-ename->vla-object (cadr a)) "ID")                ; Extract Manhole Block "ID" attribute
                            (LM:vl-getattributevalue (vlax-ename->vla-object (cadr a)) "IL1")               ; Extract Manhole Block "IL1" attribute
                            (LM:vl-getattributevalue (vlax-ename->vla-object (cadr a)) "IL2")               ; Extract Manhole Block "IL2" attribute
                            (LM:vl-getattributevalue (vlax-ename->vla-object (cadr a)) "IL3")               ; Extract Manhole Block "IL3" attribute
                            (LM:vl-getattributevalue (vlax-ename->vla-object (cadr a)) "IL4")               ; Extract Manhole Block "IL4" attribute
                            (LM:getvisibilitystate (vlax-ename->vla-object (cadr a)))                       ; Extract Manhole Block "visibility state"
                            (cdr (assoc "Manhole size" (LM:getdynprops (vlax-ename->vla-object (cadr a))))) ; Extract Manhole Block "Manhole size"
                          )

                man_db (append man_db (list man_entry))
                man_ch (LM:rtos (car man_entry) 2 5)
              );END setq
              ;(princ "\nman_db = ")(princ man_db)
              ; Manhole Block Chainage            = (nth 0 man_data)
              ; Manhole Block "ID" attribute      = (nth 1 man_data)
              ; Manhole Block "IL1" attribute     = (nth 2 man_data)
              ; Manhole Block "IL2" attribute     = (nth 3 man_data)
              ; Manhole Block "IL3" attribute     = (nth 4 man_data)
              ; Manhole Block "IL4" attribute     = (nth 5 man_data)
              ; Manhole Block "visibility state"  = (nth 6 man_data)   para tener el tamano de la arqueta (DN1200, etc.)
              ; Manhole Block "manhole size"      = (nth 7 man_data)

              ; OPERATION - Add zeros till have minimum 5 digits before dot "." to be able to sort them as strings later
              (cond
                ((= 1 (vl-string-search "." man_ch)) (setq man_ch (strcat "0000" man_ch)) )
                ((= 2 (vl-string-search "." man_ch)) (setq man_ch (strcat "000"  man_ch)) )
                ((= 3 (vl-string-search "." man_ch)) (setq man_ch (strcat "00"   man_ch)) )
                ((= 4 (vl-string-search "." man_ch)) (setq man_ch (strcat "0"    man_ch)) )
              );END cond

              ; OPERATION - Create list of chainages as strings
              (setq man_ch_list (append man_ch_list (list man_ch )) )
            );END progn
          );END if
        );END progn
      );END if1
    );END foreach

    ; OPERATION - Return list of manholes in correct order
    (setq sorted_man_ch_list (acad_strlsort man_ch_list))

    ; OPERATION - Rebuild data lists shorted by chainages
  	(foreach ch sorted_man_ch_list
  		(cond
  			((/= "" ch)
  				(setq
            position (vl-position ch man_ch_list) 		                        ; get position of the chainage at man_ch_list (non-sorted list of chainages)
  				  sorted_man_db (append sorted_man_db (list (nth position man_db)))	; copy data associated to the current chainage to sorted_man_db
          );END setq
  			)
  		);END cond
  	);END foreach
    ;(princ "\nsorted_man_db = ")(princ sorted_man_db)
  );END defun DT:SelectManholes()

  ; INPUT - Ask user to select a centreline
  (while (not cntl)
    (if (not (setq cntl (car (entsel "\nSelect centreline: "))))
      (princ "missed. Try again.")
      (progn
        (if
          (or
            (= "POLYLINE"   (cdr (assoc 0 (entget cntl))))
            (= "LWPOLYLINE" (cdr (assoc 0 (entget cntl))))
          );END or
          (princ "object selected.")
          (progn
            (princ "selected object is not a polyline. Try again.")
            (setq cntl nil)
          );END progn
        );END if
      );END progn
    ); END if
  );END while

  ; OPERATION - Check if polyline looks like a centreline
  (if (/= "e-centreline" (cdr (assoc 8 (entget cntl))))
    (progn
      (initget "Yes No")
      (setq ans (getkword "\nSelected object is not on layer \"e-centreline\".\nAre you sure you want to use this object as centreline? <Yes/No>: [No]"))
      (if (not ans) (setq ans "No"))
      (if (= ans "Yes")
        (setq global_variable_manhole_db (DT:SelectManholes cntl))
      );END if
    );END progn
    (progn
      (setq global_variable_manhole_db (DT:SelectManholes cntl))
    )
  );END if
;  (princ)
)
(defun c:2()
  ; ERROR HANDLING FUNCTION ----------------------------------------------------
  (defun *error* ( msg )
    (if (not (member msg '("Function cancelled" "quit / exit abort")))
      (princ (strcat "\nError: " msg))
    )
    ; RESTORE PREVIOUS SETTINGS
    (setvar "osmode" oldosmode)
    (setvar "attdia" oldattdia)
    (setvar "attreq" oldattreq)
    (princ)
  );END defun *error*()

  ; SAVE SETTINGS
  (setq oldosmode (getvar "osmode"))
  (setq oldattdia (getvar "attdia"))
  (setq oldattreq (getvar "attreq"))

  ; CHANGE SETTINGS
  (setvar "osmode" 0)
  (setvar "attdia" 0)
  (setvar "attreq" 1)

  ; AUXILIARY FUNCTIONS
  (defun DT:MarkManholeIL( ls
                          /
                          p0 p_ins
                          ch ID IL1 IL2 IL3 IL4 vis Chamber
                          )
    ; CHECK - Imported list
    (princ "\nimported_list = ")(princ ls)

    ; INPUT - Select datum line with XDATA
    (setq
      p0 (cdr (assoc 10 (entget (car (entsel "\nSelect datum line: ") ))))
      datum (getreal "\nIntroduce datum level:")
    )
    (foreach manhole ls
      (princ "\n")(princ manhole)
      (if
        (or
          (= "SW" (substr (nth 1 manhole) 1 2))
          (= "FW" (substr (nth 1 manhole) 1 2))
        );END or
        (progn
          ; OPERATION - Collect list data
          (setq
            p_ins (polar p0 0 (nth 0 manhole))
            ch (nth 0 manhole)
            ID (nth 1 manhole)
            IL1 (nth 2 manhole)
            IL2 (nth 3 manhole)
            IL3 (nth 4 manhole)
            IL4 (nth 5 manhole)
            vis (nth 6 manhole)
            Chamber (nth 7 manhole)
          );END setq

          ; OPERATION - Choose correct block and layer (Storm/Foul)
          (cond
            ((= "SW" (substr (nth 1 manhole) 1 2))
              (command "-insert" "LongSectionSWM" p_ins 1 1 0 IL1 IL2 IL3 IL4 ID (strcat Chamber " TYPE ?"))
              (while (> (getvar "CMDACTIVE") 0) (command ""))
              (vla-put-layer (vlax-ename->vla-object (entlast)) "e-asd")
            )
            ((= "FW" (substr (nth 1 manhole) 1 2))
              (command "-insert" "LongSectionFWM" p_ins 1 1 0 IL1 IL2 IL3 IL4 ID (strcat Chamber " TYPE ?"))
              (while (> (getvar "CMDACTIVE") 0) (command ""))
              (vla-put-layer (vlax-ename->vla-object (entlast)) "e-afd")
            )
          );END cond

          ; OPERATION - Translate visibility state
          (cond
            ((= vis "1") (setq vis "1IL"))
            ((= vis "2") (setq vis "2IL"))
            ((= vis "3") (setq vis "3IL"))
            ((= vis "4") (setq vis "4IL"))
          );END cond

          ; OPERATION - Update inserted block "Chamber size" lookup parameter
          (LM:setdynprops (vlax-ename->vla-object (entlast)) (list (cons "Chamber size" Chamber)))

          ; OPERATION - Update inserted block visibility state
          (LM:SetVisibilityState (vlax-ename->vla-object (entlast)) vis)

          ; OPERATION - Convert IL (string to real)
          (setq
            IL1 (* 10 (- (atof IL1) datum))
            IL2 (* 10 (- (atof IL2) datum))
            IL3 (* 10 (- (atof IL3) datum))
            IL4 (* 10 (- (atof IL4) datum))
          )
          ; OPERATION - Update inserted block IL and IL marker witdh
          (cond
            ((= vis "1IL")
              (LM:setdynprops (vlax-ename->vla-object (entlast))
                (list
                  (cons "Dist_IL1" IL1) (cons "Width_IL1" (* 0.001 (atof (substr Chamber 3))) )
                );END list
              );END LM:setdynprops
            )
            ((= vis "2IL")
              (LM:setdynprops (vlax-ename->vla-object (entlast))
                (list
                  (cons "Dist_IL1" IL1) (cons "Width_IL1" (* 0.001 (atof (substr Chamber 3))) )
                  (cons "Dist_IL2" IL2) (cons "Width_IL2" (* 0.001 (atof (substr Chamber 3))) )
                );END list
              );END LM:setdynprops
            )
            ((= vis "3IL")
              (LM:setdynprops (vlax-ename->vla-object (entlast))
                (list
                  (cons "Dist_IL1" IL1) (cons "Width_IL1" (* 0.001 (atof (substr Chamber 3))) )
                  (cons "Dist_IL2" IL2) (cons "Width_IL2" (* 0.001 (atof (substr Chamber 3))) )
                  (cons "Dist_IL3" IL3) (cons "Width_IL3" (* 0.001 (atof (substr Chamber 3))) )
                );END list
              );END LM:setdynprops
            )
            ((= vis "4IL")
              (LM:setdynprops (vlax-ename->vla-object (entlast))
                (list
                  (cons "Dist_IL1" IL1) (cons "Width_IL1" (* 0.001 (atof (substr Chamber 3))) )
                  (cons "Dist_IL2" IL2) (cons "Width_IL2" (* 0.001 (atof (substr Chamber 3))) )
                  (cons "Dist_IL3" IL3) (cons "Width_IL3" (* 0.001 (atof (substr Chamber 3))) )
                  (cons "Dist_IL4" IL4) (cons "Width_IL4" (* 0.001 (atof (substr Chamber 3))) )
                );END list
              );END LM:setdynprops
            )
          );END cond

          ; mira como introducir el tema de las escalas, para el tamano del texto: pregunta cual es la escala comun, y si quieren cambiarla..que lo exploten
        );END progn
      );END if
    );END foreach
    ;(princ "\nentget = ")(princ datum)
  );END defun DT:MarkManholeIL()

  ; OPERATION - Execute the function
  (DT:MarkManholeIL global_variable_manhole_db)

  ; RESTORE PREVIOUS SETTINGS
  (setvar "osmode" oldosmode)
  (setvar "attdia" oldattdia)
  (setvar "attreq" oldattreq)

  (princ)

  ; v0.0 - 2016.08.08 - First issue
  ; Author: David Torralba
  ; Last revision: 2016.08.08
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
(defun c:1 (/ obj lay)
;(defun c:nlayfv (/ obj lay)
  ; Freeze nested object real layer in the current viewport
  (if
    (and
      (/= "Model" (getvar "CTAB"  ) )
      (<  1       (getvar "CVPORT") )
    );END and
    (progn
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
        (command "vplayer" "F" lay "C" "")
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
  (command "-text" "S" "ROMANS" "J" "M" (setq pt (cadr (grread 't))) "1.5" "90" (strcat (LM:rtos (getvar "Area") 2 3) " m2"))
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
(defun c:PK( / ent VL_ent_name pt ch)
  ; INPUT - Ask user what to do: type a chainage and mark it or clic and get the chainage
  (initget "Type Pick")
  (setq
    ans (getkword "\nSelect input mode [Type/Pick] <Pick>:")
    i 0
  )
  (if (not ans) (setq ans "Pick") )

  ; OPERATION - Select centreline
  (while (not ent)
    (setq ent (entsel "\nSelect centreline: "))
    (if (not ent)
      (princ "nothing selected.")
      (setq centreline_VL_ent_name (vlax-ename->vla-object (car ent)))
    ); END if
  ); END while centreline selection

  ; OPERATION - Start loop with selected option:
  (while (not kkkk)
    (cond
      ((= ans "Type")
        (alert "Sorry, still this option is not available.")
        (exit)
      );END subcond
      ((= ans "Pick")
        (setq
          ch (DT:PK centreline_VL_ent_name (getpoint "\nSelect a point: "))
        )
        (princ (strcat "\nChainage " i " = " (LM:rtos ch 2 3)))
    );END subcond
    );END cond
  );END while
  (princ)
)
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
   _J (if (not _J) "LEFT" (strcase _J) ) ;_ end of if
   _ST (tblsearch
         "STYLE"
         (if (or
               (not _ST)
               (not (tblsearch "STYLE" _ST))
             );END or
           (getvar "TEXTSTYLE")
           _ST
         ) ;_ end of if
       ) ;_ end of tblsearch
   _SP _ANG
 ) ;_ end of setq
 (if (or (= _J "ALIGN") (= _J "A") (= _J "FIT") (= _J "F"))
   (if (= (type _ANG) 'list)
     (setq _ANG (angle _INS _ANG))
     (setq _J "LEFT")
   ) ;_ end of if
   (setq
     _ANG  (if (= (type _ANG) 'list)
             (angle _INS _ANG)
             (progn
               (setq _SP _INS)
               (* pi (/ _ANG 180.0))
             ) ;_ end of progn
           ) ;_ end of if
   ) ;_ end of setq
 ) ;_ end of if
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
 ) ;_ end of cond
 (entmakex
   (list
     '(0 . "TEXT")
     '(100 . "AcDbEntity")
     (cons 67
       (if (= (setq _TAB (getvar "CTAB")) "Model")
         0
         1
       ) ;_ end of if
     ) ;_ end of cons
     (cons 410 _TAB)
     (cons 8
       (if (not _LA)
         (getvar "CLAYER")
         _LA
       ) ;_ end of if
     ) ;_ end of cons
     '(100 . "AcDbText")
     (cons 10 _INS)
     (cons 40
       (cond
         ((and _HIGH (/= _HIGH 0)) _HIGH)
         ((/= 0 (setq _HIGH (cdr (assoc 40 _ST)))) _HIGH)
         (t (getvar "TEXTSIZE"))
       ) ;_ end of cond
     ) ;_ end of cons
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
       ) ;_ end of if
     ) ;_ end of cons
     (cons 73 _J73)
   ) ;_ end of list
) ;_ end of entmake
) ;_ end of defun
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
(defun c:ListAllBlocks()
;(defun bb ( blockName )
;(defun DT:CheckIfBlockExists( blockName )
  (setq
    acadObject (vlax-get-acad-object)
    acadDocument (vla-get-ActiveDocument acadObject)
    acadBlocks (vla-get-blocks acadDocument)
  )
  (vlax-for item acadBlocks
    (progn
      (princ "\n")
      (princ item)
      (cond
        ((equal (vla-get-IsLayout item) :vlax-true)
          (princ " (modelspace/layout)")
        );END subcond
        ((equal (vla-get-IsXref item) :vlax-true)
          (princ " (xref)")
        );END subcond
        (t
          (princ " (")
          (princ
            (vlax-get-property item
              (if (vlax-property-available-p item 'EffectiveName) 'EffectiveName 'Name)
            )
          )
          (princ ")")
        );END subcond
      );END cond
    );END progn
  ); END vlax-for
  (princ)
);END defun​​
(defun bb ( blockName )
;(defun DT:CheckIfBlockExists( blockName )
  (setq
    acadObject (vlax-get-acad-object)
    acadDocument (vla-get-ActiveDocument acadObject)
    acadBlocks (vla-get-blocks acadDocument)
  )
  (vlax-for item acadBlocks
    (progn
      (if
        (and
          (equal (vla-get-IsLayout item) :vlax-false)
          (equal (vla-get-IsXref item) :vlax-false)
        );END and
        (progn
          (if (= blockName (vlax-get-property item (if (vlax-property-available-p item 'EffectiveName) 'EffectiveName 'Name) ) )
            T   ; The block exists
            nil ; The doesn't block exist
          );END if
        );END progn
      );END cond
    );END progn
  ); END vlax-for
  (princ)
);END defun​​
(bb "testBlock")
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
