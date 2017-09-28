; DO NOT REMOVE THIS LINE. It's a checking.
(vl-load-com)
(defun DT:LoadFromProjects ( fileName onFailMessage / return )
  ; Load "fileName" from %username%\projects\civil-autolisp repository
  ; Return T if succesful
  ; Print error message and return nil otherwise
  (if (and fileName onFailMessage )
    (if (and (= 'str (type fileName)) (= 'str (type onFailMessage)))
      (progn
        (setq
          return (DT:LoadWithoutSecureload
            (strcat "C:\\Users\\" (getvar "LOGINNAME") "\\projects\\civil-autolisp\\" fileName)
            onFailMessage
          ); END DT:LoadWithoutSecureload
        );END setq
        (if (= return onFailMessage)
          (progn
            (princ (strcat "\nERROR @ DT:LoadFromProjects : " onFailMessage "\n"))
            nil
          );END progn
          T
        );END if
      );END progn
      (cond
        ((/= 'str (type filePath))      (princ "\nERROR @ DT:LoadFromProjects : filePath is not a string\n") nil )
        ((/= 'str (type onFailMessage)) (princ "\nERROR @ DT:LoadFromProjects : onFailMessage is not a string\n") nil )
      );END cond
    );END if
    (cond
      ((not filePath)      (princ "\nERROR @ DT:LoadFromProjects : filePath=nil\n") nil )
      ((not onFailMessage) (princ "\nERROR @ DT:LoadFromProjects : onFailMessage=nil\n") nil )
    );END cond
  );END if

  ; v0.0 - 2017.05.11 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.05.11
)
(defun DT:LoadWithoutSecureload ( filePath onFailMessage / old_secureload return)
  ; Load "filePath" skipping SECURELOAD
  ; Return the same output as (load) function
  (if (and filePath onFailMessage)
    (if (and (= 'str (type filePath)) (= 'str (type onFailMessage)))
      (progn
        (setq old_secureload (getvar "secureload")) ; get current SECURELOAD
        (setvar "secureload" 0)                     ; set SECURELOAD to 0
        (setq return (load filePath onFailMessage)) ; load file
        (setvar "secureload" old_secureload)        ; reset SECURELOAD
        return                                      ; Return (load) function output
      );END progn
      (cond
        ((/= 'str (type filePath))      (princ "\nERROR @ DT:LoadWithoutSecureload : filePath is not a string\n") nil )
        ((/= 'str (type onFailMessage)) (princ "\nERROR @ DT:LoadWithoutSecureload : onFailMessage is not a string\n") nil )
      );END cond
    );END if
    (cond
      ((not filePath)      (princ "\nERROR @ DT:LoadWithoutSecureload : filePath=nil\n") nil )
      ((not onFailMessage) (princ "\nERROR @ DT:LoadWithoutSecureload : onFailMessage=nil\n") nil )
    );END cond
  );END if

  ; v0.0 - 2017.05.11 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.05.11
)
(defun DT:LoadCustomRepository ( / customFiles )
  ; Load repository custom libraries
  (setq
    customFiles '(
        "_CustomShortcuts.lsp"
        "_LongitudinalSections.lsp"
        "_Viewport.lsp"
        "_ManholeSchedule.lsp"
        "_AutoLoad.lsp"
        "_Input.lsp"
      )
  );END setq
  (mapcar
    '(lambda (fileName)
        (princ (strcat "\nLoading " fileName " ..."))
        (if (DT:LoadFromProjects fileName (strcat fileName " not loaded")) (princ " done."))
      ); END lambda
    customFiles
  );END mapcar

  (princ)

  ; v0.2 - 2017.05.17 - Manhole Schedule functions added
  ; v0.1 - 2017.05.15 - Viewport functions added
  ; v0.0 - 2017.05.11 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.05.17
)
(DT:LoadCustomRepository)
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
(defun c:r ( / ent_name )
  ; Rotate a single entity given 2 points
  (princ "\nSingle rotate:")

  ; SAVE SETTINGS
  (save_environment (list "osmode" "cmdecho"))

  ; CHANGE "osmode" and "cmdecho"
  (setvar "osmode" 512)
  (setvar "cmdecho" 0)

  (if (setq ent_name (car (entsel "\nSelect object to rotate: ")))
    (progn
      (DT:R2 ent_name (getpoint "\nSelect point 1: ") (getpoint "\nSelect point 2: ") )
      (command "_.move" ent_name "" "_non" (cadr (grread 't)) "_non" pause)
    );END progn
    (princ "nothing selected.")
  );END if

  ; RESTORE SETTINGS
  (restore_environment)

  (princ)

  ; v0.0 - 2017-03.13 - First issue
  ; Author: David Torralba
  ; Last revision: 2017-03.13
)
(defun DT:R2 ( ent_name p1 p2 )
  ; Align ent_name to given two points
  ; ent_name1 [ename] - Object to align
  ; p1 [list] - Reference point 1
  ; p2 [list] - Reference point 2
  ; If ent_name is a text, rotation readability angle will be considered

  (if (and ent_name p1 p2)
    (if (and (= 'ename (type ent_name)) (= 'list (type p1)) (= 'list (type p2)))
      (progn
        (setq ang (angle p1 p2))

        ; If text, correct angle to readability
        (if
          (and
            (if DT:ReadableTextAngle T nil)
            (or
              (= "TEXT" (cdr (assoc 0 (entget ent_name)) ) )
              (= "MTEXT" (cdr (assoc 0 (entget ent_name)) ) )
            );END or
          );END and
          (setq ang (DT:ReadableTextAngle ang))
        );END if
        (vlax-put-property (vlax-ename->vla-object ent_name) 'Rotation ang )
      );END progn
      (cond
        ((/= 'ename (type ent_name)) (princ "\nERROR @ DT:R2 > ent_name is not an ename\n") (princ) nil)
        ((/= 'list  (type p1      )) (princ "\nERROR @ DT:R2 > p1 is not a list\n")         (princ) nil)
        ((/= 'list  (type p2      )) (princ "\nERROR @ DT:R2 > p2 is not a list\n")         (princ) nil)
      );END cond
    );END if
    (cond
      ((not ent_name) (princ "\nERROR @ DT:R2 > ent_name = nil\n")(princ) nil)
      ((not p1      ) (princ "\nERROR @ DT:R2 > p1 = nil\n")      (princ) nil)
      ((not p2      ) (princ "\nERROR @ DT:R2 > p2 = nil\n")      (princ) nil)
    );END cond
  );END if

  ; v0.1 - 2017.03.28 - Error management updated
  ; v0.0 - 2017.03.13 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.03.28
)
(defun c:RTM ()
	; RT and move together
	(c:RT)
	(command "_.move" "P" "" "_non" (cadr (grread 't)) "_non" pause)
	(princ)
  ; v0.2 - 2017.03.01 - NON OSnap transparent reference added for MOVE command first reference point.
  ;                   - NON OSnap transparent reference added for MOVE command second reference point too.
  ; v0.1 - 2016.04.07 - Move reference clic sustituted for current mouse position.
	; v0.0 - 2016.03.29 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.03.01
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
    ( (or
        (= "LINE"       (cdr (assoc 0 (entget (vlax-vla-object->ename VL_ent_name) ))) )
        (= "LWPOLYLINE" (cdr (assoc 0 (entget (vlax-vla-object->ename VL_ent_name) ))) )
      )
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

  ; v0.2 - 2017.03.01 - Added line management
  ; v0.1 - 2016.09.28 - Added 3D polyline management
  ; v0.0 - 2019.??.?? - First issue
  ; Author: David Torralba
  ; Last revision: 2017.03.01
)
(defun c:PK ( / ent centreline_VL_ent_name ans i scapeVariable pt ch chList)
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
  (while (not scapeVariable)
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
              chList (if chList (strcat chList "\n" (LM:rtos ch 2 3)) (LM:rtos ch 2 3))
              i (+ i 1)
            )
            (princ (strcat (itoa i) " chainage = " (LM:rtos ch 2 3)))
          );END progn
          (progn
            (CopyToClipboard chList)
            (setq scapeVariable T)
            (princ (strcat "\n" (itoa i) " points coppied to clipboard."))
          );END progn
        );END if
    );END subcond
    );END cond
  );END while

  (princ)

  ; v0.1 - 2017.05.16 - Picked points copied to clipboard
  ; v0.0 - 2016.??.?? - First issue
  ; Author: David Torralba
  ; Last revision: 2017.05.16
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
(defun c:SDIPP( /
  ; Recursive DT:SDIP along polyline vertexes
                activeDocument ent_name insertionMode
                startZ startPoint startParam endPoint endParam gradient
                p1 p2 param
                )
  ; AUXILIARY FUNCTIONS
  (defun DT:AskBlockInAllVertex( / ans )
    ; Return T if the user wants blocks in every vertex,
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

  (setq activeDocument (vla-get-ActiveDocument (vlax-get-acad-object)))
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
            (vla-startUndoMark activeDocument)
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
            (vla-endUndoMark activeDocument)
          );END subcond
          ((> startParam endParam) ; reverse direction
            (vla-startUndoMark activeDocument)
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
            (vla-endUndoMark activeDocument)
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

  ; v0.1 - 2017.05.22 - Add start and end undo marks
  ; v0.0 - 2016.??.?? - First issue
  ; Author: David Torralba
  ; Last revision: 2017.05.22
)
(defun c:SDIPforPrivateDrainage ( / z1 p1 p2 dist ans targetPoint gradient ent_name)
  ; Get level with fixed gradient between 2 points,
  ; write the level rounded to 2 decimals,
  ; and formated according to target text (foul/storm)
	; Also return all the data in a list:
	; ( p1 p2 gradient )
	; 	p1 [pt] - Start point (3D)
	; 	p2 [pt] - End point (3D)
	; 	gradient [real] - Slope, being the slope 1/gradient

	; SAVE SETTINGS
	(save_environment (list "nomutt" "osmode"))

	; CHANGE SETTINGS
	(setvar "nomutt" 1)

	(setq
		; Ask user start level
    z1 (DT:clic_or_type_level)
		; Ask user start point coordinates
    p1 (getpoint (strcat "\nStart level = " (LM:rtos z1 2 3) "m\npoint 1: "))
		; Asign z1 level to p1 3D point
    p1 (list (nth 0 p1) (nth 1 p1) z1)
		; Ask user target point coordinates
    p2 (getpoint (strcat "\nStart level = " (LM:rtos z1 2 3) "m\npoint 1: OK\npoint 2: "))
  )
  ; Ask user gradient
  (if SDIPgradient
    ; If there is a gradient previously set offer it as default:
    (progn
      (setq ans (getreal (strcat "\nStart level = " (LM:rtos z1 2 3) "m\npoint 1: OK\npoint 2: OK" "\nGradient= 1/<" (LM:rtos SDIPgradient 2 0) ">: ") ) )
      (if ans (setq SDIPgradient ans));END if
    );END progn
    ; If there is not any gradient previously set force the user introduce it:
    (while (not SDIPgradient)
      (setq SDIPgradient (getreal (strcat "\nStart level = " (LM:rtos z1 2 3) "m\npoint 1: OK\npoint 2: OK\nGradient= 1/")))
    );END while
  );END if

	(setq
		; Get p2 3D value
		p2 (DT:SDIP p1 p2 SDIPgradient)
		; Round Z coordinate of p2 to 2 decimals
		p2 (list (nth 0 p2) (nth 1 p2) (atof (LM:rtos (nth 2 p2) 2 2)) )
  )

	; Print chosen gradient
	(princ (strcat "\nRequested gradient: 1/" (LM:rtos SDIPgradient 2 0) "\n"))

	; Ask user to select private manhole label to be updated:
  (setvar "osmode" 0)
	(setq
    targetPoint (getpoint (strcat "\nLevel = " (LM:rtos (nth 2 p2) 2 2) "m\nSelect target: "))
  )
  (cond
    ; If private manhole label selected:
    ( (and
				targetPoint
        (nentselp targetPoint)                                    ; Something selected in targetPoint point
        (setq ent_name (car (nentselp targetPoint)))              ; If this something is an object
        (or
          (= "S" (substr (cdr (assoc 1 (entget ent_name) )) 1 1)) ; If this object is a text that starts with "S"
          (= "F" (substr (cdr (assoc 1 (entget ent_name) )) 1 1)) ; or with "F"
        )
      );END and
      ; Update private manhole label content:
      (vlax-put-property
        (vlax-ename->vla-object ent_name)
        'TextString
        (strcat
          (substr (cdr (assoc 1 (entget ent_name) )) 1 1)
          (LM:rtos (nth 2 p2) 2 2)
        );END strcat
      );END vlax-put-property

			; Recalculate real gradient after rounding p2 level
			(if (setq gradient (DT:Gradient p1 p2))
        (if ; If a sewer label is selected
					(setq ent_name ; Ask user to select sewer label to be updated
						(car (entsel
							(strcat
								(if (or (= 1 (getvar "dynmode")) (= 3 (getvar "dynmode")))
									"\nSelect sewer label to\nupdate gradient to 1/"
									"\nSelect sewer label to update gradient to 1/"
								);END if
								(LM:rtos (abs gradient) 2 0)
								":\n"
							);END strcat
						))
					);END setq
					; Change sewer label gradient
	        (DT:ChangePrivateSewerGradient ent_name (abs gradient))
					; If nothing selected:
					(progn (princ "\nNo sewer label selected.\n"))
				);END if
				; If no gradient:
        (progn (princ "\nERROR @ c:SDIPforPrivateDrainage > gradient = nil\n") nil)
      );END if
    );END subcond
    ; If not private manhole label selected:
    ( (or
				(not targetPoint)									; if nothing is selected
        (not (nentselp targetPoint))      ; if there are no objects in the targetPoint
        (if (assoc 1 (entget ent_name) )  ; or there is a text, but doesn't start with "S" or "F"
          (and
            (/= "S" (substr (cdr (assoc 1 (entget ent_name) )) 1 1))
            (/= "F" (substr (cdr (assoc 1 (entget ent_name) )) 1 1))
          )
        );END if
      );END or
      ; Try to insert block "PI_DT" with level, if it is defined in the drawing
			(setvar "osmode" 0)
    	(if (tblsearch "block" "PI_DT")
    		(command "._insert" "PI_DT" (list (nth 0 p2) (nth 1 p2) 0.0) "0.25" "0.25" "" (LM:rtos (nth 2 p2) 2 3))
    	);END if
    );END subcond
  );END cond

	; RESTORE SETTINGS
	(restore_environment)

	; Return all data
	(list
		p1 				; StartPoint
		p2 				; EndPoint
		gradient	; Real gradient
	);END list

  ; v0.4 - 2017.05.11 - Ensure OSnap is off when inserting
  ; v0.3 - 2017.03.28 - Error management updated
  ; v0.2 - 2017.03.10 - Turn off OSnap when selecting target labels to be updated
  ; v0.2 - 2017.01.30 - Gradient to absolute value
  ; v0.1 - 2017.01.28 - DT:ChangePrivateSewerGradient implementation
  ;                   - Code anotation and tidy up
	;										- CommandLine messages suppressed
  ; v0.0 - 2017.01.23 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.05.11
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
(defun c:DB ( / ss)
  ; Shortcut for "Draworder Back"
  (if (setq ss (ssget))
    (command "draworder" ss "" "Back")
    (command "draworder" (ssget) "" "Back")
  );END if

  ; v0.1 - 2017.05.24 - pickfirst enabled
  ; v0.0 - 2017.??.?? - First issue
  ; Author: David Torralba
  ; Last revision: 2017.05.24
)
(defun c:DF ( / ss)
  ; Shortcut for "Draworder Front"
  (if (setq ss (ssget))
    (command "draworder" ss "" "Front")
    (command "draworder" (ssget) "" "Front")
  );END if

  ; v0.1 - 2017.05.24 - pickfirst enabled
  ; v0.0 - 2017.??.?? - First issue
  ; Author: David Torralba
  ; Last revision: 2017.05.24
)
(defun c:move_KTF_SettingOutLabel ( / ename )
  ; Command for DT:move_KTF_SettingOutLabel
  (if (setq ename (car (entsel "\nSelect setting out block: ")) )
    (DT:move_KTF_SettingOutLabel ename)
  );END if


  ; v0.0 - 2017.08.08 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.08
)
(defun DT:move_KTF_SettingOutLabel ( ename / object labelXCoord labelYCoord p0 insertionPoint gr
                            )
  (if (= "INSERT" (cdr (assoc 0 (entget ename))))
    (progn
      (princ "\"XY_advanced\" setting out block selected.")
      (setq object (vlax-ename->vla-object ename))
      (cond
        ((= "XY_advanced" (LM:effectivename object))
          (setq
            labelXCoord (LM:getdynpropvalue object "Position1 X")   ; label initial position, to block insertion point
            labelYCoord (LM:getdynpropvalue object "Position1 Y")   ; label initial position, to block insertion point
            p0 (cadr (grread 't))                                   ; pointer position
            insertionPoint (cdr (assoc 10 (entget ename)))          ; block insertion point
          )
          (while (= 5 (car (setq gr (grread 't 13 0))))
            (LM:setdynpropvalue object "Position1 X" (+ (- (car (cadr gr)) (car p0)) labelXCoord) )
            (LM:setdynpropvalue object "Position1 Y" (+ (- (cadr (cadr gr)) (cadr p0)) labelYCoord) )
          );END while
          (princ
            (strcat
              "\nLabel moved from ("
              (LM:rtos labelXCoord 2 3)
              " "
              (LM:rtos labelYCoord 2 3)
              " 0.000) to ("
              (LM:rtos (+ (- (car (cadr gr)) (car p0)) labelXCoord) 2 3)
              " "
              (LM:rtos (+ (- (cadr (cadr gr)) (cadr p0)) labelYCoord) 2 3)
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
(defun c:nlayfv ( / obj lay)
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
(defun c:CS( / s s_str )
  ; Choose Scale
  (if (setq currentScale (strcat "1:"(LM:rtos (/ 1000 (DT:GetActiveViewportScale)) 2 0)))
    single_function
  );END if
  (if (setq s (getint (strcat "\nCurrent viewport scale: " currentScale "\nSelect scale: 1:")))
    (progn
      (setq s_str (strcat (LM:rtos (/ 1000 (float s) ) 2 2) "xp") )
      (command "zoom" "S" s_str)
      (princ (strcat "\nViewport scale updated to 1:" (itoa s) "."))
    );END progn
    (princ "\nNothing introduced. Routine finished with no viewport scale change.")
  );END if
  (princ)

	; v0.1 - 2017.09.27 - Print current scale
	; v0.0 - 2016.08.15 - First issue
	; Author: David Torralba
	; Last revision: 2017.09.27
)
(defun DT:GetActiveViewportScale ()
  ; Get active viewport scale
  (vla-get-CustomScale (vla-get-ActivePViewport (vla-get-activedocument (vlax-get-acad-object))))

  ; v0.0 - 2017.05.23 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.05.23
)
(defun c:ccs ( / str )
  ; Copy Current Scale
  ; Print active viewport scale formated as so "1:100"
  ; and print it on screen
  (if (setq str (strcat "1:"(LM:rtos (/ 1000 (DT:GetActiveViewportScale)) 2 0)))
    (progn
      (CopyToClipboard str)
      (princ (strcat "\nCurrent viewport scale = " str "\n"))
      (princ)
    );END progn
    nil
  );END if

  ; v0.0 - 2017.05.23 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.05.23
)
(defun c:mpedit () (command "pedit" "M" (ssget) "" "Y" "") )
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
  ; Author: David Torralba
  ; Last revision: 2016.08.30
);END defun
(defun c:qq()
  ; Quick Property Mode ON/OFF shortcut
  (cond
    ((= -1 (getvar "qpmode"))
      (setvar "qpmode" 1)
      (princ "\nQuick Property Mode ON.")
    );END subcond
    ((= 1 (getvar "qpmode"))
      (setvar "qpmode" -1)
      (princ "\nQuick Property Mode OFF.")
    );END subcond
    (t
      (setvar "qpmode" -1)
      (princ "\nQuick Property Mode OFF.")
    );END subcond
  );END cond
  (princ)

  ; v0.0 - 2017.02.14 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.02.14
)
(defun c:dd()
  ; Quick Property Mode ON/OFF shortcut
  (cond
    ((= -3 (getvar "dynmode"))
      (setvar "dynmode" 3)
      (princ "\nDynamic Mode ON.")
    );END subcond
    ((= 3 (getvar "dynmode"))
      (setvar "dynmode" -3)
      (princ "\nDynamic Mode OFF.")
    );END subcond
    (t
      (setvar "dynmode" -3)
      (princ "\nDynamic Mode OFF.")
    );END subcond
  );END cond
  (princ)

  ; v0.0 - 2017.02.14 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.02.14
)
(defun c:tm()    (c:testmode))
(defun c:tmode() (c:testmode))
(defun c:testmode()
  ; Switch between testmode 0 and 1
  (cond
    ((/= 1 testmode)
      (setq testmode 1)
      (princ "\nTest mode ON")
    );END subcond
    ((= 1 testmode)
      (setq testmode 0)
      (princ "\nTest mode OFF")
    );END subcond
  );END cond
  (princ)

  ; v0.0 - 2017.03.17 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.03.17
)
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
  ; Author: David Torralba
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
(defun DT:Date ( mode / d )
  ; Returns a string with the date formated as requested:
  ; mode=1 - DD.MM.YY
  ; mode=2 - DD.MM.YYYY
  (if mode
    (if (= 'int (type mode))
      (cond
        ( (= 1 mode)
          (setq d (rtos (getvar "CDATE") 2 6))
          (strcat (substr d 7 2) "." (substr d 5 2) "." (substr d 3 2))
        );END subcond
        ( (= 2 mode)
          (setq d (rtos (getvar "CDATE") 2 6))
          (strcat (substr d 7 2) "." (substr d 5 2) "." (substr d 1 4))
        );END subcond
      );END cond
      (progn (princ "\nERROR @ DT:Date > mode is not an integer\n") nil)
    );END if
    (progn (princ "\nERROR @ DT:Date > mode = nil\n") nil)
  );END if

  ; v0.0 - 2017.03.28 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.03.28
)
(defun ReloadXref (file)
  (command "-xref" "R" file)
)
(defun DT:GetText(ent_name)
  ; Returns a string with the text of the selected object, if any
  (if (= 'ename (type ent_name))
    (if (vlax-property-available-p (vlax-ename->vla-object ent_name) 'TextString)
      (vlax-get-property (vlax-ename->vla-object ent_name) 'TextString)
      (if (vlax-property-available-p (vlax-ename->vla-object ent_name) 'TextOverride)
        (vlax-get-property (vlax-ename->vla-object ent_name) 'TextOverride)
      );END if2
    );END if2
  );END if

  ; v0.1 - 2017.08.24 - TextOverride property added
  ; v0.0 - 2017.??.?? - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.24
)
(defun DT:SetText ( ename content / object )
  ; Sets the text of the selected object, if possible
  (if (DT:Arg 'DT:SetText '((ename 'ename)(content 'str)))
    (progn
      (setq object (vlax-ename->vla-object ename))
      (if (vlax-property-available-p object 'TextString)
        (progn
          (vlax-put-property object 'TextString content)
          T ; return nil if sucessfully updated, otherwise vlax-put-property will
            ; throw an error and break
        );END progn
        (if (vlax-property-available-p object 'TextOverride)
          (progn
            (vlax-put-property object 'TextOverride content)
            T ; return nil if sucessfully updated, otherwise vlax-put-property will
              ; throw an error and break
          );END progn
          nil ; property 'TextString or 'TextOverride is not available
        );END if2
      );END if2
    );END progn
  );END if

  ; v0.3 - 2017.08.24 - TextOverride property added
  ; v0.2 - 2017.08.14 - Return T if property properly updated, otherwise nil
  ; v0.1 - 2017.04.20 - T retured if property available
  ; v0.0 - 2017.??.?? - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.14
)
(defun DT:ReplaceText ( ent_name pattern newString )
  ; Replace pattern for newString in TEXTs and MTEXTs content
  (if (and ent_name pattern newString)
    (if (and (= 'ename (type ent_name)) (= 'str (type pattern)) (= 'str (type newString)))
      (if (setq textContent (DT:GetText ent_name))
        (if (setq position (vl-string-search pattern textContent))
          (if (setq patternLength (strlen pattern))
            (if (DT:SetText ent_name (strcat (substr textContent 1 position) newString (substr textContent (+ position patternLength 1)) ))
              T
            );END if
          );END progn
        );END if
      );END if
      (cond
        ((/= 'ename (type ent_name))  (princ "\nERROR @ DT:ReplaceText : ent_name is not a ename\n")   nil )
        ((/= 'str   (type pattern))   (princ "\nERROR @ DT:ReplaceText : pattern is not a string\n")   nil )
        ((/= 'str   (type newString)) (princ "\nERROR @ DT:ReplaceText : newString is not a string\n") nil )
      );END cond
    );END if
    (cond
      ((not ent_name)  (princ "\nERROR @ DT:ReplaceText : ent_name=nil\n")  nil )
      ((not pattern)   (princ "\nERROR @ DT:ReplaceText : pattern=nil\n")   nil )
      ((not newString) (princ "\nERROR @ DT:ReplaceText : newString=nil\n") nil )
    );END cond
  );END if

  ; v0.0 - 2017.04.20 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.04.20
)
(defun DT:FastMove ( ent_name / )
  (command "_.move" ent_name "" "_non" (cadr (grread 't)) "_non" pause)

  ; v0.0 - 2017.04.20 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.04.20
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
(defun GetFileShortName ( strFileName / wshFSO fileObj)
  ;; Credit to Lee Ambrosius
  ;; Source: http://hyperpics.blogs.com/beyond_the_ui/2005/12/obtaining_a_sho.html
  (vl-load-com)

  (setq wshFSO (vlax-create-object "Scripting.FileSystemObject"))
  (setq fileObj (vlax-invoke-method wshFSO 'GetFile strFileName))
  (vlax-get-property fileObj 'ShortPath)
)
(defun GetFolderShortName ( strFileName / wshFSO folderObj)
  (vl-load-com)
  (setq wshFSO (vlax-create-object "Scripting.FileSystemObject"))
  (setq folderObj (vlax-invoke-method wshFSO 'GetFolder strFileName))
  (vlax-get-property folderObj 'ShortPath)

  ; v0.0 - 2017.05.25 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.05.25
)
(defun c:pdfolder()
  ; Open company PDF folder in Windows Explorer and select the file
  (startapp
    (strcat "explorer "
      (GetFolderShortName
        (strcat
          (getvar "dwgprefix")
          "..\\"
          "PDF & DWF\\Civil\\"
        );END strcat
      );END GetFileShortName
      ", /e"
    );END strcat
  );END startapp
  (princ)

  ; v0.0 - 2017.05.25 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.05.25
)
(defun c:cfolder()
  ; Open current file containing folder in Windows Explorer and select the file
  (startapp
    (strcat "explorer /select, "
      (GetFileShortName
        (strcat
          (getvar "dwgprefix")
          (getvar "dwgname")
        );END strcat
      );END GetFileShortName
      ", /e"
    );END strcat
  );END startapp
  (princ)

  ; v0.1 - 2017.04.18 - Path with spaces supported
  ; v0.0 - 2016.??.?? - First issue
  ; Author: David Torralba
  ; Last revision: 2017.04.18
)
(defun c:civ ()
  ; Open civil-autolisp repository folder in explorer
  (command "SH" "explorer %userprofile%\\projects\\civil-autolisp")

  ; v0.0 - 2017.04.19 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.04.19
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
(defun c:xx ()
  ; Trigger
  (defun *error* ( errorMessage ) (princ (strcat "n-------- ERROR: " errorMessage " --------n")) (vl-bt) (DT:ReportError))
  (DT:AutoLoadFileFromCivilTemp "ErrorTracing.lsp")
  (DT:AutoLoadFileFromCivil "_PersonalLibrary.lsp")
  (princ "\n.\n.\n.")
  (c:DNLA)

  ; v0.0 - _DATE_ - First issue
  ; Author: David Torralba
  ; Last revision: _DATE_
)
(defun c:DNLA( / info infoString infoStringSize outputString )
  ; Print deep object nested block name and layer
  (if (setq ename (nentsel "\nSelect object to see its nested layers:"))
    (if (setq info (DT:NestedEntityNames ename))
      (if (setq infoString (DT:ChangeEnameForLevel info))
        (if (setq infoStringSize (DT:GetColumnLength infoString))
          (if (setq outputString (DT:NL_GetTextFormatted infoString infoStringSize))
            (progn
              (princ outputString)      ; Print nested entity report
              (DT:CopyNestedLayer info) ; Allow user to copy a layer name
            );END progn
            (DT:Error 'c:DNLA "outputString = nil")
          );END if
          (DT:Error 'c:DNLA "infoStringSize = nil")
        );END if
        (DT:Error 'c:DNLA "infoString = nil")
      );END if
      (DT:Error 'c:DNLA "info = nil")
    );END if
    (DT:Error 'c:DNLA "ename = nil")
  );END if
  (princ)

  ; v0.1 - 2017.09.18 - refactoring
  ; v0.0 - 2016.??.?? - First issue
  ; Author: David Torralba
  ; Last revision: 2017.09.18
)
(defun DT:EntityInfo (ent_name / blockName blockType )
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

  ; v0.1 - 2017.09.18 - refactoring
  ; v0.0 - 2016.??.?? - First issue
  ; Author: David Torralba
  ; Last revision: 2017.09.18
)
(defun DT:NestedEntityNames ( ename / i info )
  ; Returns a list with nested entity names, being the parent first one and deepest child last one
  (if (DT:Arg 'DT:NestedEntityNames '((ename 'list)))
    (progn
      (setq
        i 0
        info (list (list "Level" "Type" "Block name" "Object layer") (DT:EntityInfo (car ename)) )
      )
      (if (nth 3 ename) ; if any nested entity
        (progn
          (setq nestedLevels (length (nth 3 ename)) )
          (foreach x (nth 3 ename)
            (setq
              i (+ i 1)
              info (append info (list (DT:EntityInfo x)))
            )
          );END foreach
        );END progn
      );END if
      info
    );END progn
  );END if

  ; v0.1 - 2017.09.18 - refactoring
  ; v0.0 - 2016.??.?? - First issue
  ; Author: David Torralba
  ; Last revision: 2017.09.18
)
(defun DT:ChangeEnameForLevel ( info / i x )
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

  ; v0.1 - 2017.09.18 - refactoring
  ; v0.0 - 2016.??.?? - First issue
  ; Author: David Torralba
  ; Last revision: 2017.09.18
)
(defun DT:GetColumnLength ( infoString / q col )
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

  ; v0.1 - 2017.09.18 - refactoring
  ; v0.0 - 2016.??.?? - First issue
  ; Author: David Torralba
  ; Last revision: 2017.09.18
)
(defun DT:NL_GetTextFormatted ( infoString infoStringSize / txt s )
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

  ; v0.1 - 2017.09.18 - refactoring
  ; v0.0 - 2016.??.?? - First issue
  ; Author: David Torralba
  ; Last revision: 2017.09.18
)
(defun DT:CopyNestedLayer ( info / i layerName optionList options ans )
  (if (DT:Arg 'DT:CopyNestedLayer '((info 'list)))
    (progn
      ; Get layer names in a list
      (setq i -1)
      (foreach item info
        (if (> i -1)
          (progn
            (setq layerName (nth 3 item))
            (setq optionList (append optionList (list (list i layerName))))
            (setq i (1+ i))
          );END progn
          (setq i (1+ i))
        );END if
      );END foreach

      ; Create the list of options for the user
      (mapcar '(lambda (x) (cond
          ((= (car x) 0) (setq options "0"))
          ((> (car x) 0) (setq options (strcat options " " (itoa (car x)))))
        ))
        optionList
      );END mapcar

      ; Offer the user the list of options:
      (initget 0 options)
      (if (setq ans (getkword (strcat "\nSelect layer to copy to clipboard [" options "]")))
        (progn
          (setq ans (atoi ans))
          ; Retrieve selected layer and copy it to the clipboard
          (mapcar '(lambda (option)
            (if (= (car option) ans)
              (progn
                (CopyToClipboard (nth 1 option))
                (princ (strcat "\n >> Copied to clipboard: " (nth 1 option) ))
              );END progn
            ))
            optionList
          );END mapcar
        );END progn
      );END if
    );END progn
  );END if

  ; v0.0 - 2017.09.18 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.09.18
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
  (if (DT:Arg 'DT:Gradient '((pa 'list)(pb 'list)))
    (if (and (= 3 (length pA)) (= 3 (length pB)) )
      ; True: 2 point-list type arguments passed
      ; Return gradient value as real
      (/ (distance (DT:flatPoint pA) (DT:flatPoint pB)) (- (nth 2 pB) (nth 2 pA)))
      ; False: not 2 point-list-type arguments passed
      (cond
        ((/= 3 (length pA)) (princ "\nERROR @ DT:Gradient : pA is not a point\n")(princ) )
        ((/= 3 (length pB)) (princ "\nERROR @ DT:Gradient : pB is not a point\n")(princ) )
      );END cond
    );END if
  );END if

  ; v0.1 - 2017.05.31 - DT:Arg implemented
  ; v0.0 - 2017.01.28 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.05.31
)
(defun c:ChangePrivateSewerGradient ( / ent_name ans )
  ; Type and update selected private sewer label gradient
  (setq ent_name (car (entsel "\nSelect sewer label: ")))

  ; Ask user gradient
  (if PrivateSewerGradient
    ; If there is a gradient previously set offer it as default:
    (progn
      (setq ans (getint (strcat "\nChange label \nupdate gradient to 1/<" (LM:rtos PrivateSewerGradient 2 0) ">: ") ) )
      (if ans (setq PrivateSewerGradient ans));END if
    );END progn
    ; If there is not any gradient previously set force the user introduce it:
    (while (not PrivateSewerGradient)
      (setq PrivateSewerGradient (getint (strcat "\nChange label \nupdate gradient to 1/")))
    );END while
  );END if

  (DT:ChangePrivateSewerGradient
    ent_name
    PrivateSewerGradient
  )

  ; v0.0 - 2017.03.13 - Offer SDIPgradient as default gradient
  ; v0.0 - 2017.03.07 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.03.13
)
(defun DT:ChangePrivateSewerGradient ( ent_name gradient / targetGradientText targetGradientTextPosition)
	; Change sewer label gradient to "gradient"
	; ent_name [ename] - Text entity (sewer label)
	; gradient [real/int] - Gradient value to put
  (if (and ent_name gradient)
    ; If ent_name and gradient are not nil:
    (if (and (= 'ename (type ent_name)) (or (= 'real (type gradient)) (= 'int (type gradient))))
      ; If ent_name and gradient type are correct:
      (if (= "TEXT" (cdr (assoc 0 (entget ent_name))))
        ; If ent_name object is a text:
        (if (DT:GetCharPositions (cdr (assoc 1 (entget ent_name))) "/")
          ; If the text contains "/" (ergo, is a sewer label):
          (vlax-put-property
            (vlax-ename->vla-object ent_name)
            'TextString
            (strcat ; Create text new content
              (substr ; Get text before "/" character ("/" included)
                (cdr (assoc 1 (entget ent_name)))
                1
                (+ 1 (vl-string-position (ascii "/") (cdr (assoc 1 (entget ent_name))))) ; "/" position in ent_name text
              );END substr
              (LM:rtos gradient 2 0)
            );END strcat
          );END vlax-put-property
          (progn (princ "\nERROR @ DT:ChangePrivateSewerGradient > text is not sewer label\n") nil)
        );END if
        ; If ent_name object is not a text:
        (progn (princ "\nERROR @ DT:ChangePrivateSewerGradient > ent_name is not a text\n") nil)
      );END if
      ; If ent_name or gradient type is not correct:
      (cond
        ((/= 'ename (type ent_name))
          (princ "\nERROR @ DT:ChangePrivateSewerGradient > ent_name is not an entity\n") nil
        );END subcond
        ((and (/= 'real (type gradient)) (/= 'int (type gradient)))
          (princ "\nERROR @ DT:ChangePrivateSewerGradient > gradient is not real or integer\n") nil
        );END subcond
      );END cond
    );END if
    ; If ent_name or gradient is nil:
    (cond
      ((not ent_name) (princ "\nERROR @ DT:ChangePrivateSewerGradient > ent_name = nil\n") nil)
      ((not gradient) (princ "\nERROR @ DT:ChangePrivateSewerGradient > gradient = nil\n") nil)
    );END cond
  );END if

  ; v0.0 - 2017.01.28 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.01.28
)
(defun DT:GetSewerGradient ( ent_name )
  ; Returns a integer with the ent_name sewer-label gradient
  ; If not, returns nil
  (if ent_name
    (if (or (= "TEXT" (cdr (assoc 0 (entget ent_name)))) (= "MTEXT" (cdr (assoc 0 (entget ent_name)))))
      (if (DT:GetCharPositions (cdr (assoc 1 (entget ent_name))) "/")
        ; If the text contains "/" (ergo, is a sewer label):
        (atoi
          (substr ; Get text after "/" character
            (cdr (assoc 1 (entget ent_name)))
            (+ 2 (vl-string-position (ascii "/") (cdr (assoc 1 (entget ent_name))))) ; "/" position in ent_name text
          );END substr
        );END atoi
      );END if
      (progn (princ "\nERROR @ DT:GetSewerGradient : ent_name is not a text\n") nil )
    );END if
    (progn (princ "\nERROR @ DT:GetSewerGradient : ent_name=nil\n") nil )
  );END if

  ; v0.2 - 2017.04.21 - Error management updated
  ; v0.1 - 2017.03.23 - MTEXT object type added
  ; v0.0 - 2017.03.08 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.04.21
)
(defun DT:GetSewerSize ( ent_name )
  ; Returns a integer with the ent_name sewer-label size
  ; If not, returns nil
  (if ent_name
    (if (or (= "TEXT" (cdr (assoc 0 (entget ent_name)))) (= "MTEXT" (cdr (assoc 0 (entget ent_name)))))
      (if (vl-string-search "DN" (cdr (assoc 1 (entget ent_name))))
        ; If the text contains "DN" (ergo, is a sewer label):
        (atoi
          (substr ; Get text after "DN" character
            (cdr (assoc 1 (entget ent_name)))
            (+ 3 (vl-string-search "DN" (cdr (assoc 1 (entget ent_name))))) ; Position after "DN" in ent_name text
            4 ; Get only the 4 following characters
          );END substr
        );END atoi
      );END if
      (progn (princ "\nERROR @ DT:GetSewerSize : ent_name is not a text\n") nil )
    );END if
    (progn (princ "\nERROR @ DT:GetSewerSize : ent_name=nil\n") nil )
  );END if

  ; v0.0 - 2017.04.21 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.04.21
)
(defun DT:SetGroup ( ent_nameList / i l s )
  ; Create a group with the entities passed on ent_nameList
  ; ent_nameList [list] - List with entity names to include in the group
  ; Returns nil always
  (if ent_nameList
    (if (= 'list (type ent_nameList))
      (progn
        (foreach ent_name ent_nameList
          (if (= 'ename (type ent_name))
            (setq l (cons (vlax-ename->vla-object ent_name ) l))
          );END if
        );END foreach
        (if l
          (vlax-invoke
            (vla-add (vla-get-groups (vla-get-activedocument (vlax-get-acad-object))) "*")
            'appenditems l
          );END vlax-invoke
          (progn (princ "\nERROR @ DT:SetGroup > no ent_name passed on the list, so \"l\" is nil\n")(princ) nil )
        );END if
      )
      (progn (princ "\nERROR @ DT:SetGroup > ent_nameList is not a list\n")(princ) nil )
    );END if
    (progn (princ "\nERROR @ DT:SetGroup > ent_nameList = nil\n")(princ) nil )
  );END if

  ; v0.0 - 2017.01.29 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.01.29
)
(defun DT:SmallerEqualBigger (a b)
  ; Compare a and b and return result
  ; Returns nil if something wrong
  ; Possible results:
  ;  0 : a < b
  ;  1 : a = b
  ;  2 : a > b
  (if (and a b)
    (if (and (or (= 'int (type a)) (= 'real (type a)) ) (or (= 'int (type b)) (= 'real (type b)) ))
      (cond
        ((< a b) 0) ; a is smaller than b
        ((= a b) 1) ; a is equal to b
        ((> a b) 2) ; a is bigger than b
      );END cond
      (cond
        ((and (/= 'int (type a)) (/= 'real (type a)) ) (princ "\nERROR @ DT:SmallerEqualBigger > a is not a number\n")(princ) nil )
        ((and (/= 'int (type b)) (/= 'real (type b)) ) (princ "\nERROR @ DT:SmallerEqualBigger > b is not a number\n")(princ) nil )
      );END cond
    );END if
    (cond
      ((not a) (princ "\nERROR @ DT:SmallerEqualBigger > a = nil\n")(princ) nil )
      ((not b) (princ "\nERROR @ DT:SmallerEqualBigger > b = nil\n")(princ) nil )
    );END cond
  );END if

  ; v0.0 - 2017.01.29 - First issue
  ; Author: David Torralban
  ; Last revision: 2017.01.29
)
(defun DT:CheckIfLayerExists ( lay )
  ; Return T if lay layer exists, or nil if not
  (if lay
    (if (tblsearch "layer" lay)
      T
    );END if
    (progn
      (princ "\nERROR @ DT:CheckIfLayerExists > lay = nil\n")
      (princ)
      nil
    );END progn
  );END if

  ; v0.0 - 2017.02.15 - First issue
  ; Author: David Torralban
  ; Last revision: 2017.02.15
)
(defun c:ShowSet ()
  ; Show available setups
  (alert
    (strcat
      "\nAVAILABLE SETUPS\n\n"
      "EngSet"    "\tEgineering setup\n"
      "LongSet"   "\tLongSection setup\n"
      "WorkSet"   "\tWorking Drawings setup\n"
      "SetSet"    "\tSetting Out setup\n"
      "ModSet"    "\t3D Modelling setup\n"
      "TitSet"    "\tTitle Block setup\n"
      "ArchSet"   "\tEngArch setup\n"
      "KerbSet"   "\tKerbing setup\n"
      "SurvSet"   "\tSurvey setup\n"
      "TrackSet"  "\tTracking setup\n"
      "ManSet"    "\tManhole schedule setup\n"
      "CutSet"    "\tCut and fill setup\n"
      "\n"
    );END strcat
  );END alert

  ; v0.7 - 2017.08.14 - Cut and fill setup added
  ; v0.6 - 2017.03.29 - Tracking setup added
  ; v0.5 - 2017.03.28 - Survey setup added
  ; v0.4 - 2017.03.21 - Kerbing setup added
  ; v0.3 - 2017.03.16 - LongSet setup added
  ; v0.2 - 2017.03.10 - EngArch setup added
  ;                   - Minor printing mistake fixed
  ; v0.1 - 2017.03.08 - Title Block setup added
  ; v0.0 - 2017.02.27 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.14
)
(defun c:EngSet ()
  ; Engineering setup
  (defun c:1() (princ "\nBYC: ") (c:BYC))
  (defun c:2() (princ "\nINT: ") (c:INT))
  (defun c:3() (princ "\nSDIPforPrivateDrainage: ") (c:SDIPforPrivateDrainage))
  (defun c:33() (princ "\nSDIP: ") (c:SDIP))
  (defun c:4() (DT:AddSubstractPlotLevel (car (entsel "\nSelect level: ")) ))
  (defun c:44() (princ "\nRECALC private sewer GRADIENT") (c:UpdatePrivateDrainageLabel))
  (defun c:5()
    (princ "\nDRAW LINEAR DRAINAGE\n")
    (if (not c:linearDrainage) (DT:AutoLoadFileFromCivilTemp "linearDrainage.lsp"))
    (c:linearDrainage)
  )
  (defun c:pa()(fbi "Parking-Fall-Arrow") (vlax-put-property (vlax-ename->vla-object (entlast)) 'Layer "e-road-fall-arrow") )
  (defun c:ra()(fbi "Road-Fall-Arrow")
    (vlax-put-property (vlax-ename->vla-object (entlast)) 'Layer "e-road-fall-arrow")
    (vlax-put-property (vlax-ename->vla-object (entlast)) 'XScaleFactor 0.7)
    (vlax-put-property (vlax-ename->vla-object (entlast)) 'YScaleFactor 0.7)
    (vlax-put-property (vlax-ename->vla-object (entlast)) 'ZScaleFactor 0.7)
  )
  (defun c:ll() (princ "\nPLOT LEVEL: ") (DT:DrawPlotLevel 0 (getpoint "\nSelect point to draw plot level: ") 0) )
  (defun c:oo()(setvar "osmode" 4))
  (defun c:ne()(setvar "osmode" 512))
  (defun c:11 ( / targetLevel ent_name )
    ; Get a FFL, substract -0.65m and overwrite the target text object content
    ; with the calculated value properly formated: S16.70
    (princ "\nGET STORM LEVEL FROM FFL (-0.60m)\n")
    (setq
      targetLevel (+ (DT:clic_or_type_level) -0.60)
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
    (princ "\nGET FOUL LEVEL FROM FFL (-0.75m)\n")
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
  (defun c:z1 ( / ss )
    ; Change selected entities' color to white
    (princ "\nMarking in white:")
    (if (setq ss (ssget))
      (foreach a (ssnamex ss)
        (if (= 'ename (type (cadr a)))
          (vlax-put-property (vlax-ename->vla-object (cadr a)) 'Color 7)
        );END if
      );END foreach
    );END if

    ; v0.0 - 2017.05.11 - First issue
    ; Author: David Torralba
    ; Last revision: 2017.05.11
  )
  (defun c:z2 ( / ss )
    ; Change selected entities' color to yellow
    (princ "\nMarking in yellow:")
    (if (setq ss (ssget))
      (foreach a (ssnamex ss)
        (if (= 'ename (type (cadr a)))
          (vlax-put-property (vlax-ename->vla-object (cadr a)) 'Color 2)
        );END if
      );END foreach
    );END if

    ; v0.0 - 2017.05.11 - First issue
    ; Author: David Torralba
    ; Last revision: 2017.05.11
  )
  (defun c:cheatsheet() (alert
    "ENGINEERING CHEATSHEET\n
    Marking:
        1\tBYC
        z1\tWhite (storm)
        z2\tYellow (foul)\n
    Calculations:
        11\tStorm-FFL
        2\tINT
        22\tFoul-FFL
        3\tSDIP private drainage
        33\tSDIP
        4\tPlotLevel +/-50mm
        44\tUpdate Private Sewer Gradient\n
    Create:
        LL\tplot level
        5\tlinear drainage
        pa\tparking arrow
        ra\troad arrow\n
    Environment:
        oo\tOSMODE = 4
        ne\tOSMODE = 512\n
  "))
  (princ "\nENGINEERING SETUP COMPLETED")(princ)

  ; v0.6 - 2017.06.01 - Private sewer gradient update function updated
  ; v0.5 - 2017.05.11 - White and yellow colouring tools added
  ; v0.4 - 2017.03.29 - Plot level added
  ; v0.3 - 2017.03.10 - Change Private Sewer Gradient added
  ; v0.2 - 2017.03.09 - scale management added to c:ra
  ; v0.1 - 2017.02.21 - Cheatsheet added
  ; v0.0 - 2017.01.29 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.06.01
)
(defun c:LongSet ()
  ; Longitudinal Section setup
  (defun c:1() (princ "\nFILLET: \n") (command "fillet") )
  (defun c:11() (princ "\nExplode polyline: \n") (c:EP) )
  (defun c:3() (princ "\nGET MANHOLE DATA ACCORDING TO CENTRELINE: \n") (DT:ExtractManholeDataAlongCentrelines) )
  (defun c:4() (princ "\nDRAW MANHOLES ONTO LONGITUDINAL SECTION: \n") (DT:DrawExtractedManholesOnLongSection) )
  (defun c:5() (princ "\nDRAW MANHOLES' BODY ONTO LONGITUDINAL SECTION: \n") (c:DrawManholeBodyOnLongSection) )
  (defun c:6()
    (princ "\nKTF: Extract vertical geometry\n")
    (if c:ktf_lsc2vld
      (c:ktf_lsc2vld)
      (progn (load "lsc2vld") (c:ktf_lsc2vld) )
    );END if
  )
  (defun c:cheatsheet() (alert
    "LONGITUDINAL SECTION CHEATSHEET\n
    Manholes to section:
        1\tFillet
        11\tExplode polyline
        3\tGet manholes
        4\tDraw manholes
        5\tDraw manholes body
        6\tKFT Extract vertical geometry\n
  "))
  (princ "\nLONGITUDINAL SECTION SETUP COMPLETED")(princ)

  ; v0.2 - 2017.07.31 - ktf_lsc2vld added
  ; v0.1 - 2017.05.12 - DT:DrawManholeBodyOnLongSection added
  ; v0.0 - 2017.03.16 - Add fillet and c:ep
  ;                   - First issue
  ; Author: David Torralba
  ; Last revision: 2017.07.31
)
(defun c:WorkSet ()
  ; Working Drawing setup
  (defun c:1() (princ "\nMOVE OBJECTS TO LAYER \"e-work-hse\":\n") (DT:MoveSelectionSetToLayer (ssget) "e-work-hse") )
  (defun c:2() (princ "\nMOVE OBJECTS TO LAYER \"e-work-services\":\n") (DT:MoveSelectionSetToLayer (ssget) "e-work-services") )
  (defun c:3() (princ "\nWORKING DRAWING BLOCK CREATION:\n")(DT:CreateWorkingDrawingBlock))
  (defun c:4() (princ "\nWORKING DRAWING LAYERS CREATION:\n") (DT:CreateWorkingDrawingLayers))
  (defun c:cheatsheet() (alert
    "WORKING DRAWING CHEATSHEET\n
    Move to layer:
        1\te-work-hse
        2\te-work-services\n
    Create:
        3\tblock
        4\tlayers\n
  "))
  (princ "\nWORKING DRAWING SETUP COMPLETED")(princ)

  ; v0.1 - 2017.02.21 - Cheatsheet added
  ; v0.0 - 2017.02.15 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.02.21
)
(defun c:SetSet ()
  ; Setting Out setup
  (defun c:1() (princ "\nINDIVIDUAL SETTING OUT\n") (c:coord) )
  (defun c:11() (princ "\nPOLYLINE SETTING OUT\n") (c:coordp) )
  (defun c:2() (princ "\nMOVE SETTING OUT LABEL\n") (c:move_KTF_SettingOutLabel) )
  (defun c:22( / ss ) (princ "\nRESET DIMENSTION POSITION\n") (setq ss (ssget '((0 . "DIMENSION")))) (command "_DIM1" "HOME" ss "") )
  (defun c:3() (princ "\nALIGNED DIMENSION\n") (command "_dimaligned") )
  (defun c:33() (princ "\nCONTINUE DIMENSION\n") (command "_dimcontinue") )
  (defun c:4() (princ "\nRADIUS DIMENSION\n") (command "_dimradius" pause "") )
  (defun c:0() (princ "\nRESET KTF SETTING OUT SCALE\n") (c:RKS) )
  (defun c:cheatsheet() (alert
    "SETTING OUT CHEATSHEET\n
    Coordinates:
        1\tsingle
        11\talong pline
        2\tmove only label
        22\treset dimension position\n
    Dimensions:
        3\taligned
        33\tcontinue
        4\tradius\n
    Environment:
        0\treset scale
  "))
  (princ "\nSETTING OUT SETUP COMPLETED")(princ)

  ; v0.1 - 2017.08.08 - move_KTF_SettingOutLabel command updated
  ; v0.0 - 2017.02.21 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.08
)
(defun c:ModSet ()
  ; 3D Modelling setup
  (defun c:1()  (princ "\n3D POLYLINE\n") (command "_.3dpoly" pause) (princ))
  (defun c:11() (princ "\nJOIN\n") (command "_.join" pause) (princ))
  (defun c:111 ( / escapeVariable xy z)
    (princ "\n3D POLYLINE CLICKING LEVELS\n")
    (command "_.3dpoly")
    (while (not escapeVariable)
      (command
        (if (setq xy (getpoint "\nSelect XY position: "))
          (progn
            (setq return
            (list
              (nth 0 xy)
              (nth 1 xy)
              (progn
                (if (setq z (DT:clic_or_type_level))
                  (progn
                    (princ (strcat "\nXY = (" (LM:rtos (nth 0 xy) 2 3) " " (LM:rtos (nth 1 xy) 2 3) ")\n z = " (LM:rtos z 2 3) "\n"))
                    z
                  );END progn
                  (progn
                    (princ "\nNo level selected! z = 0.000\n")
                    0
                  );END progn
                );END if
              );END progn
            );END list
            )
            (princ "\nreturn = ")(princ return)
            return
          );END progn
          (progn
            (setq escapeVariable T)
            ^C
          );END progn
        );END if
      );END command
    );END while
    (command ^C ^C)
    (princ)

    ; v0.1 - 2017.08.14 - Rewrite it to print point coordinates
    ; v0.0 - 2017.0?.?? - First issue
    ; Author: David Torralba
    ; Last revision: 2017.08.14
  )
  (defun c:2()
    (princ "\nADD VERTICES\n")
    (if c:ktf_3dpladvx
      (c:ktf_3dpladvx)
      (progn (load "3dpladvx") (c:ktf_3dpladvx) )
    );END if
  )
  (defun c:22() (princ "\nEDIT VERTICE LEVEL\n") (c:3dpt) )
  (defun c:3()
    (princ "\n3D OFFSET\n")
    (if c:ktf_3doffset
      (c:ktf_3doffset)
      (progn (load "3doffset") (c:ktf_3doffset) )
    );END if
  )
  (defun c:33( / p ans )
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
  (defun c:4 () (princ "\nSPOT LEVEL\n") (c:3DSpotLevel) )
  (defun c:5()
    (princ "\n3D JUNCTION\n")
    (if c:ktf_revcurve3d
      (c:ktf_revcurve3d)
      (progn (load "revcurve3d") (c:ktf_revcurve3d) )
    );END if

    ; v0.0 - 2017.06.22 - First issue
    ; Author: David Torralba
    ; Last revision: 2017.06.22
  )
  (defun c:55()
    (princ "\nMASTER STRING\n")
    (if c:ktf_pmdsdes
      (c:ktf_pmdsdes)
      (progn (load "pmdsdes") (c:ktf_pmdsdes) )
    );END if

    ; v0.0 - 2017.06.22 - First issue
    ; Author: David Torralba
    ; Last revision: 2017.06.22
  )
  (defun c:cheatsheet() (alert
    "3D MODELLING CHEATSHEET\n
    Draw:
        1\t3dpoly
        111\t3dpoly click
        4\tspot levels\n
    Modify:
        11\tjoin
        2\tadd vertices
        22\tedit vertice level
        3\t3D offset
        33\tup/down\n
    Strings:
        5\t3d junction
        55\tmasterstring\n
  "))
  (princ "\n3D MODELLING SETUP COMPLETED")(princ)

  ; v0.4 - 2017.08.14 - Spot level added
  ; v0.3 - 2017.06.22 - 4 added
  ;                   - 44 added
  ; v0.2 - 2017.03.21 - 111 added
  ;                   - 33 added
  ; v0.1 - 2017.03.20 - c:3dpt added
  ;                   - Load KTF functions if needed
  ; v0.0 - 2017.02.24 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.14
)
(defun c:TitSet ()
  ; Title Block setup
  (defun c:1()
    ; Remove revision box date
    (LM:vl-setattributevalue (vlax-ename->vla-object (car (entsel "\nSelect revision\nbox to remove date: "))) "DATE" "00")
  )
  (defun c:2()
    ; Set current date to revision box
    (LM:vl-setattributevalue (vlax-ename->vla-object (car (entsel "\nSelect revision\nbox to set current date: "))) "DATE" (DT:Date 1))
  )
  (defun c:22()
    ; Set current date to revision box with long format
    (LM:vl-setattributevalue (vlax-ename->vla-object (car (entsel "\nSelect revision\nbox to set current date: "))) "DATE" (DT:Date 2))
  )
  (defun c:3 ( / doNotEscapeVariable ent_name input currentRevisionLetter )
    ; Update revision box or title block letter to the next (+) or previous (-) revision letter

    ; Get current revision letter
    (if (setq ent_name (car (entsel "\nSelect revision box \nto change revision: ")))
      (cond
        ( (= "Revision-box" (LM:effectivename (vlax-ename->vla-object ent_name)))
          (setq doNotEscapeVariable 1)
          (while doNotEscapeVariable
            ; Ask user input
            (princ "\nPress +/- to change revision letter (press ENTER to finish): ")
            (setq input (grread nil 8))
            (if (= 2 (car input))
              (cond
                ( (= (cadr input) 43)
                  (if (setq currentRevisionLetter (LM:vl-getattributevalue (vlax-ename->vla-object ent_name) "REVISION-LETTER" ) )
                    (if (setq updatedRevisionLetter (DT:NextRevisionLetter currentRevisionLetter))
                      (LM:vl-setattributevalue (vlax-ename->vla-object ent_name) "REVISION-LETTER" updatedRevisionLetter)
                    );END if
                  );END if
                );END subcond
                ( (= (cadr input) 45)
                  (if (setq currentRevisionLetter (LM:vl-getattributevalue (vlax-ename->vla-object ent_name) "REVISION-LETTER" ) )
                    (if (setq updatedRevisionLetter (DT:PrevRevisionLetter currentRevisionLetter))
                      (LM:vl-setattributevalue (vlax-ename->vla-object ent_name) "REVISION-LETTER" updatedRevisionLetter)
                    );END if
                  );END if
                );END subcond
                (t
                  (setq doNotEscapeVariable nil)
                )
              );END cond
              (setq doNotEscapeVariable nil)
            );END if
          );END while
        );END subcond
        ( (or
            (= "A0-Portrait_D"  (LM:effectivename (vlax-ename->vla-object ent_name)))
            (= "A0-Landscape_D" (LM:effectivename (vlax-ename->vla-object ent_name)))
            (= "A1-Portrait_D"  (LM:effectivename (vlax-ename->vla-object ent_name)))
            (= "A1-Landscape_D" (LM:effectivename (vlax-ename->vla-object ent_name)))
            (= "A2-Portrait_D"  (LM:effectivename (vlax-ename->vla-object ent_name)))
            (= "A2-Landscape_D" (LM:effectivename (vlax-ename->vla-object ent_name)))
            (= "A3-Portrait_D"  (LM:effectivename (vlax-ename->vla-object ent_name)))
            (= "A3-Landscape_D" (LM:effectivename (vlax-ename->vla-object ent_name)))
            (= "A4-Portrait_D"  (LM:effectivename (vlax-ename->vla-object ent_name)))
            (= "A4-Landscape_D" (LM:effectivename (vlax-ename->vla-object ent_name)))
          );END or
          (setq doNotEscapeVariable 1)
          (while doNotEscapeVariable
            ; Ask user input
            (princ "\nPress +/- to change revision letter (press ENTER to finish): ")
            (setq input (grread nil 8))
            (if (= 2 (car input))
              (cond
                ( (= (cadr input) 43)
                  (if (setq currentRevisionLetter (LM:vl-getattributevalue (vlax-ename->vla-object ent_name) "*" ) )
                    (if (setq updatedRevisionLetter (DT:NextRevisionLetter currentRevisionLetter))
                      (LM:vl-setattributevalue (vlax-ename->vla-object ent_name) "*" updatedRevisionLetter)
                    );END if
                  );END if
                );END subcond
                ( (= (cadr input) 45)
                  (if (setq currentRevisionLetter (LM:vl-getattributevalue (vlax-ename->vla-object ent_name) "*" ) )
                    (if (setq updatedRevisionLetter (DT:PrevRevisionLetter currentRevisionLetter))
                      (LM:vl-setattributevalue (vlax-ename->vla-object ent_name) "*" updatedRevisionLetter)
                    );END if
                  );END if
                );END subcond
                (t
                  (setq doNotEscapeVariable nil)
                )
              );END cond
              (setq doNotEscapeVariable nil)
            );END if
          );END while
        );END subcond
      );END cond

    );END if
  )
  (defun c:4 ( / ent_name pt ang )
    (princ "\nAdd NOT ISSUED YET note:\n")
    (setvar "orthomode" 0)
    (if
      (entmake
        (append
          (list
            (cons 0 "TEXT")                                                               ; entity type
            (cons 1 "NOT ISSUED YET")                                                     ; content
            (cons 8 "MJA-Title")                                                          ; layer
            (cons 10 '(0.0 0.0 0.0))
            (cons 11 (if (setq pt (cadr (grread 't))) pt (setq pt (getvar 'viewctr))) )   ; insertion point (mouse position, if not viewport centre)
            (cons 40 20.0)                                                                ; text size
            (cons 50 0.267424)                                                            ; rotation
            (cons 62 1)
            (cons 72 1)
            (cons 73 2)
          );END list
          (if (not (tblsearch "style" "ARIAL")) (list (cons 7 "standard")) (list (cons 7 "ARIAL"))) ; Text style: ARIAL, if possible
        );END append
      );END entmake
      (command "_.move" (entlast) "" "_non" pt "_non" pause)
      (princ "\nent_name = nil")
    );END if
    (princ)

    ; v0.1 - 2017.05.12 - Ortho management added
    ; v0.0 - 2017.0?.?? - First issue
    ; Author: David Torralba
    ; Last revision: 2017.05.12
  )
  (defun c:44() (princ "\nRemove NOT ISSUED YET note:\n") (princ) )
  (defun c:oo()
    ; Set OSMODE to end + per
    (setvar "osmode" 129)
  )
  (defun c:cheatsheet() (alert
    "TITLE BLOCK CHEATSHEET\n
    Revision box date:
        1\tRemove date
        2\tSet current date
        22\tSet current date (long)\n
    Revision letter:
        3\tup/down\n
    Not Issued Yet note:
        4\tAdd
        4\tRemove TODO\n
  "))
  (princ "\nTITLE BLOCK SETUP COMPLETED")(princ)

  ; v0.3 - 2017.03.28 - Command 22 added to set long formatted date
  ; v0.2 - 2017.03.21 - Command 4 completed
  ;                   - Command 3 completed
  ;                   - TODO added
  ; v0.1 - 2017.03.14 - Custom OSMODE added
  ; v0.0 - 2017.03.08 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.03.28
)
(defun c:ArchSet ()
  ; EngArch setup
  ; TODO
  ; (defun INSERT EMPTY LEVEL (%%U00.00) in all primary accesses. Same in secondary accesses  but copying 3m away too. All with readability angles)
  (defun c:1 ( / p1 p2 )
    ; Insert front door block 1, and rotate 90 degree.
    (command "-insert" "Part-m-primary-0" (setq p1 (getpoint)) 1 1 (setq p2 (getpoint)) ^C ^C)
    (command "rotate" (entlast) "" p1 -90)
    (command "-insert" "e-part_FLAT_AREA" p1 1 1 p2 ^C ^C)
    (if (not (tblsearch "layer" "e-part-m-flat-area"))
      (DT:AddLayer "e-part-m-flat-area" 251 "")
    );END if
    (vlax-put-property (vlax-ename->vla-object (entlast)) 'Layer "e-part-m-flat-area")

    ; v0.1 - 2017.03.10 - Moved to personal library
    ;                   - Local variable management fixed
    ;                   - Layer management added
    ; v0.0 - 2017.01.?? - First issue
    ; Author: David Torralba
    ; Last revision: 2017.03.10
  )
  (defun c:2 ( / p )
    ; Insert rear door block, and rotate 90 degree.
    (command "-insert" "Part-m-secondary" (setq p (getpoint)) 1 1 pause)
    (command "rotate" (entlast) "" p -90)

    ; v0.1 - 2017.03.10 - Moved to personal library
    ;                   - Local variable management fixed
    ; v0.0 - 2017.01.?? - First issue
    ; Author: David Torralba
    ; Last revision: 2017.03.10
  )
  (defun c:22 ( / p1 p1a p1b )
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

    ; v0.1 - 2017.03.10 - Moved to personal library
    ; v0.0 - 2017.01.?? - First issue
    ; Author: David Torralba
    ; Last revision: 2017.03.10
  )
  (defun c:11()
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

    ; v0.1 - 2017.03.10 - Moved to personal library
    ; v0.0 - 2017.01.?? - First issue
    ; Author: David Torralba
    ; Last revision: 2017.03.10
  )
  (defun c:cheatsheet() (alert
    "ENGARCH CHEATSHEET\n
    Add accesses:
        1\tPrimary
        11\tPush in group
        2\tSecondary
        22\tSecondary mid\n
    Add levels:
        11\tFFL TODO
        2\tplot TODO
        22\taccesses TODO\n
  "))
  (princ "\ENGARCH SETUP COMPLETED")(princ)

  ; v0.0 - 2017.03.10 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.03.10
)
(defun c:KerbSet ()
  ; Kerbing setup
  (defun c:1 ( / ang )
    ; Correct TEXT and MTEXT angle to be readable
    (princ "\nUPDATE TEXT READABILITY ANGLE\n")
    (if (setq ss (ssget))
      (foreach a (ssnamex ss)
        (if (= 'ename (type (cadr a)))
          (if (or (= "TEXT" (cdr (assoc 0 (entget (cadr a))))) (= "MTEXT" (cdr (assoc 0 (entget (cadr a))))) )
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

    ; v0.0 - 2017.02.15 - First issue
    ; Author: David Torralba
    ; Last revision: 2017.02.15
  )
  (defun c:oo()
    ; Set OSMODE to end + int + nea
    (setvar "osmode" 545)
  )
  (defun c:cheatsheet() (alert
    "KERBING CHEATSHEET\n
    Text:
        1\treadability angle\n
    OSMODE:
        oo\tend + int + nea\n
  "))
  (princ "\nKERBING SETUP COMPLETED")(princ)

  ; v0.0 - 2017.03.21 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.03.21
)
(defun c:SurvSet ()
  ; Survey setup
  (defun c:1()
    (princ "\nTie blocks and text AUTOMATIC\n")
    (if (not c:TieSurvey) (DT:AutoLoadFileFromCivil "_Survey.lsp")) (c:TieSurvey)
  )
  (defun c:2()
    (princ "\nTie blocks and text MANUAL\n")
    (if (not c:TieSinglePoint) (DT:AutoLoadFileFromCivil "_Survey.lsp")) (c:TieSinglePoint)
  )
  (defun c:3() (c:UpdateTextReadabilityAngle))
  (defun c:cheatsheet() (alert
    "\nSURVEY CHEATSHEET\n
    Draw:
        1\tAutomatic points
        2\tSingle points\n
    Modify
        3\tText readability angle\n
  "))
  (princ "\nSURVEY SETUP COMPLETED")(princ)

  ; v0.3 - 2017.08.22 - DT:AutoLoadFileFromCivil implemented
  ; v0.2 - 2017.04.05 - Cheatsheet text updated
  ; v0.1 - 2017.04.03 - 3 added
  ; v0.0 - 2017.02.28 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.22
)
(defun c:TrackSet ()
  ; Tracking Setup
  (defun c:1() (princ "\nAUTODRIVE:\n") (command "_aeccDRIVEAUTO") )
  (defun c:2() (princ "\nPROPERTIES:\n") (command "'_aeccPATHPROPERTIES") )
  (defun c:3() (princ "\nINSERT PROFILE:\n") (command "_aeccINSERTPROFILE") )

  (defun c:0() (princ "\nRELEASING LICENSE:\n") (command "_aeccRELEASELICENCE") )

  (defun c:cheatsheet() (alert
    "TRACKING CHEATSHEET\n
    autoDrive:
        1\tArc
    Modify:
        2\tProperties
        3\tInsert profile\n
    license:
        0\trelease\n
  "))
  (princ "\nTRACKING SETUP COMPLETED")(princ)

  ; v0.0 - 2017.03.29 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.03.29
)
(defun c:ManSet ()
  ; Manhole schedule setup
  (defun c:0()  (princ "\nMSC: build manhole schedule") (c:MSC))
  (defun c:1()
    (princ "\nUPDATE MANHOLE DIAGRAM\n")
    (if (not c:UMSD) (DT:AutoLoadFileFromCivilTemp "ManholeScheduleDiagram.lsp"))
    (c:UMSD)
  )
  (defun c:11() (princ "\nUPDATE MANHOLE SCHEDULE") (c:UpdateManholeSchedule))
  (defun c:2()  (princ "\nMSCS: calculate manhole schedule (SfA 6th Edition)") (c:MSCS))
  (defun c:22()
    (princ "\nMSCS: calculate manhole schedule (SfA 7th Edition)\n")
    (if (not c:MSCS7) (DT:AutoLoadFileFromCivilTemp "ManholeSchedule7Edition.lsp"))
    (c:MSCS7)
  )
  (defun c:3()
    (princ "\nOLD MANHOLE TO NEW MANHOLE:\n")
    (if (not c:ImportManholeBlock) (DT:AutoLoadFileFromCivilTemp "ImportManholeBlocks.lsp"))
    (c:ImportManholeBlock)
  )
  (defun c:4()
    (princ "\nMANHOLE SCHEDULE: INPUT PIPE SIZE\n")
    (if (not c:ManholeSchedulePipeSize) (DT:AutoLoadFileFromCivilTemp "ManholeScheduleUpdateValues.lsp"))
    (c:ManholeSchedulePipeSize)
  )
  (defun c:44()
    (princ "\nMANHOLE SCHEDULE: Reset hidden values to default\n")
    (if (not c:ManholeScheduleResetHiddenValues) (DT:AutoLoadFileFromCivilTemp "ManholeScheduleUpdateValues.lsp"))
    (c:ManholeScheduleResetHiddenValues)
  )
  (defun c:cheatsheet() (alert
    "MANHOLE SCHEDULE\n
    Create:
        0\tManhole schedule
        1\tUpdate manhole diagram
        11\tUpdate manhole schedule
        3\tOld manhole to new manhole\n
    Calculations:
        2\tSfA 6th Ed
        22\tSfA 7th Ed
        4\tInput pipe size
        44\tReset hidden values\n
  "))
  (princ "\nMANHOLE SCHEDULE SETUP COMPLETED")(princ)

  ; v0.0 - 2017.07.28 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.07.28
)
(defun c:CutSet ()
  ; Cut and fill setup
  (defun c:1() (princ "\nCREATE HOUSE STRINGS") (c:Create_House_Strings))
  (defun c:2() (princ "\nCREATE GRAGE STRINGS") (c:Create_Garage_Strings))
  (defun c:000()
    (princ "\nRELOAD CUT AND FILL LIBRARY\n")
    (DT:AutoLoadFileFromCivilTemp "CutAndFillSet.lsp")
  )
  (defun c:cheatsheet() (alert
    "CUT AND FILL\n
    Create:
        1\tHouse 3D strings
        2\tGarage 3D strings\n
    Library:
        0\t(re)load LSP\n
  "))
  (princ "\nCUT AND FILL SETUP COMPLETED")(princ)

  ; v0.0 - 2017.07.28 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.07.28
)
(defun c:3DSpotLevel ( / escapeVariable xy z )
  ; Draw spot level in 3D
  (while (not escapeVariable)
    (if (setq xy (getpoint "\nSelect XY position: "))
      (if (setq z (DT:clic_or_type_level))
        (entmakex
          (list
            (cons 0 "POINT")
            (cons 10 (list (nth 0 xy) (nth 1 xy) z))
          );END list
        );END entmakex
        (setq escapeVariable T)
      );END if
      (setq escapeVariable T)
    );END if
  );END while

  ; v0.0 - 2017.08.14 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.14
)
(defun DT:OffsetPartM ( ent_name / VL_ent_name p p0 p1 ang )
  ; Move Part M blocks 0.302 toward the inner part of the building (= BlockRotation - 90º)
  (if ent_name
    (if (= 'ename (type ent_name))
      (if
        (or
          (= "Part-m-primary-0"   (LM:effectivename (vlax-ename->vla-object ent_name)))
          (= "Part-m-primary-180" (LM:effectivename (vlax-ename->vla-object ent_name)))
          (= "Part-m-secondary"   (LM:effectivename (vlax-ename->vla-object ent_name)))
        );END or
        (progn
          (setq oldosmode (getvar "osmode"))
          (setvar "osmode" 0)
          (setq
            VL_ent_name (vlax-ename->vla-object ent_name)
            ang (vlax-get-property VL_ent_name 'Rotation)
            p (vlax-safearray->list (vlax-variant-value (vlax-get-property VL_ent_name 'InsertionPoint)))
            p0 (list (car p) (cadr p) 0.0)
            p1 (polar p0 (- ang (* -0.5 pi )) 0.302)
            ;p1 (polar p0 (- ang (* -0.5 pi )) 0.215)
          )
          (command "move" ent_name "" p0 p1)
          (setvar "osmode" oldosmode)
        );END progn
      );END if
      (progn (princ "\nERROR @ DT:OffsetPartM : ent_name is not an ename\n")(princ) )
    );END if
    (progn (princ "\nERROR @ DT:OffsetPartM : ent_name=nil\n")(princ) )
  );END if

  ; v0.1 - 2017.03.10 - Moved to personal library
  ;                   - Input parameter management added
  ; v0.0 - 2017.01.?? - First issue
  ; Author: David Torralba
  ; Last revision: 2017.03.10
)
(defun DT:CreateWorkingDrawingLayers ()
  ; Create layers for Working Drawings blocks
  (command "-layer" "m" "e-work-services" "c" "9" "" "")
  (command "-layer" "m" "e-work-hse" "c" "9" "" "")
  (princ "\nWorking drawings layers created.")
  (princ)

  ; v0.0 - 2017.02.15 - First issue
  ; Author: David Torralban
  ; Last revision: 2017.02.15
)
(defun DT:CreateWorkingDrawingBlock ( / p1 p2 ent_name ss blockName)
  ; Create Working Drawings blocks

  ; SAVE SETTINGS
	(save_environment (list "clayer" "osmode" "cmdecho"))

  ; Create Working Drawing Layers if necessary
  (if
    (and
      (DT:CheckIfLayerExists "e-work-hse")
      (DT:CheckIfLayerExists "e-work-services")
    );END and
    nil
    (DT:CreateWorkingDrawingLayers)
  );END if

  (setvar "cmdecho" 1)
  (setvar "clayer" "e-work-hse")

  ; Ask the user block basepoint
  (setvar "osmode" 1)
  (setq p1 (getpoint "\nSpecify the base point of the block: ") )

  (setvar "osmode" 0)
  (setq
    ; Ask the user point to copy the scaled block
    p2 (getpoint "\nSpecify the point to copy and scale the block: ")
    ; Draw a circle of 2m around the selected point to label the block
    ent_name (entmakex
      (list
        (cons 0 "CIRCLE") ; Entity type [ename]
        (cons 10 p2)      ; Circle centre point point [pt]
        (cons 40 2000)    ; Circle radius
      )
    )
    ; Ask user to select the objects to copy, scale and block
    ss (ssget)
    ; Ask user to introduce block name
    blockName (getstring 't "\nSpecify block name (press Enter to finish): ")
  )

  (command "_.scale" ss "" p1 "0.001")
  (command "_.-Block" blockName p1 ss "")
  (command "_.-insert" blockName p2 "" "" "")
  (command "_.-insert" blockName p1 1000 1000 "")
  (command "_.explode" "L")

  ; RESTORE SETTINGS
	(restore_environment)

  (princ)

  ; v0.0 - 2017.02.15 - First issue
  ; Author: David Torralban
  ; Last revision: 2017.02.15
)
(defun DT:MoveSelectionSetToLayer ( ss lay )
  ; Move items in ss to lay layer and put them by Layer
  (if (and ss lay)
    (if (and (= 'pickset (type ss)) (= 'str (type lay)))
      (if (tblsearch "layer" lay)
        (foreach a (ssnamex ss)
          (if (= 'ename (type (cadr a)))
            (progn
              (vlax-put-property (vlax-ename->vla-object (cadr a)) 'Layer lay)
              (vlax-put-property (vlax-ename->vla-object (cadr a)) 'Color 256)
              (vlax-put-property (vlax-ename->vla-object (cadr a)) 'Linetype "ByLayer")
            );END progn
          );END if
        );END foreach
        (progn (princ (strcat "\nERROR @ DT:MoveSelectionSetToLayer > layer \"" lay "\" does not exist\n"))(princ) nil)
      );END if
      (cond
        ((/= 'pickset (type ss)) (princ "\nERROR @ DT:MoveSelectionSetToLayer > ss is not a pickset\n")(princ) nil )
        ((/= 'str    (type lay)) (princ "\nERROR @ DT:MoveSelectionSetToLayer > lay is not a string\n")(princ) nil )
      );END cond
    );END if
    (cond
      ((not ss) (princ "\nERROR @ DT:MoveSelectionSetToLayer > ss = nil\n")(princ) nil )
      ((not lay) (princ "\nERROR @ DT:MoveSelectionSetToLayer > lay = nil\n")(princ) nil )
    );END cond
  );END if

  ; v0.1 - 2017.04.07 - Add error message when provided layer doesn't exist
  ; v0.0 - 2017.02.15 - First issue
  ; Author: David Torralban
  ; Last revision: 2017.04.07
)
(defun DT:SplitLevelDifference (textLevel)
  ; Return a pair list with the level and the difference, both as strings
  (if textLevel
    (cond
      ((vl-string-search "+" textLevel)
        (DT:StringToList textLevel "+")
      );END subcond
      ((vl-string-search "-" textLevel)
        (DT:StringToList textLevel "-")
      );END subcond
    );END cond
  );END if

  ; v0.0 - 2017.03.01 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.03.01
)
(defun DT:clic_or_type_level (/ in nent txt VL_ent_name)
  ; Clic on any atribute or text with a level and return its text, or type it in
  (setq in (DT:input_string_or_point))
  (cond
    ((= 'LIST (type in)) ; it's a point
      (setq nent (nentselp in))
      (if (not nent)
        (princ "nothing selected.\n")
        (progn
          (setq VL_ent_name (vlax-ename->vla-object (car nent)))
          (if (vlax-property-available-p VL_ent_name 'TextString)
            (setq txt (vlax-get-property VL_ent_name 'TextString))
            (progn
              (setq nent nil)
              (princ "no text found.")
            ); END progn2
          ); END if2
        ); END progn1
      ); END if1
    )
    ((= 'STR (type in)) ; it's a string
    (setq txt in)
    )
  )
  ; Analize the input text
  (DT:ParseLevelOperation txt)

  ; v0.0 - 2017.03.01 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.03.01
)
(defun DT:ParseLevelOperation ( textLevel / operation difference i )
  (if textLevel
    ; Parse the text
    ; - [ ] look for any "+" of "-" sign in the text,
    ;       if any: parse it and process it as it should
    ;       if none: apply the condition used till now
    ; (vl-string-search "who" "pfooyey on you")
    (progn
      ; If any difference requested use it
      (if
        (or
          (vl-string-search "+" textLevel)
          (vl-string-search "-" textLevel)
        );END or
        (setq
          operation
            (cond
              ((vl-string-search "+" textLevel) "+")
              ((vl-string-search "-" textLevel) "-")
            );END cond
          i 0
          difference 0
          textLevel (DT:SplitLevelDifference textLevel)
          difference
            (cond
              ((= operation "+") (*  0.001 (atof (cadr textLevel))) )
              ((= operation "-") (* -0.001 (atof (cadr textLevel))) )
            );END cond
          textLevel (car textLevel)
        );END setq
      );END if
      (cond
        ; Normal number: adoptable manholes, PI_DAVID block
        ( (and (< (strlen textLevel) 8) (> (strlen textLevel) 4) (/= "S" (substr textLevel 1 1)) (/= "F" (substr textLevel 1 1)))
          ;(alert "case 1")
          (setq level (atof textLevel))
        )
        ; FFL
        ( (and (= "FFL " (substr textLevel 1 4)) (= 4 (- (strlen textLevel) (vl-string-search "." textLevel))) )
          (setq level (atof (substr textLevel 5)))
        )
        ; Road level
        ( (and (or (= "%%U" (substr textLevel 1 3)) (= "%%u" (substr textLevel 1 3))) (= 3 (- (strlen textLevel) (vl-string-search "." textLevel))))
          (setq level (atof (substr textLevel 4 10)))
        )
        ; Plot level
        ( (and (or (= "%%U" (substr textLevel 1 3)) (= "%%u" (substr textLevel 1 3))) (= 4 (- (strlen textLevel) (vl-string-search "." textLevel))))
          (setq level (atof (substr textLevel 4 9)))
        )
        ; Private mahole
        ( (and
            (or (= "S" (substr textLevel 1 1)) (= "F" (substr textLevel 1 1)))
            (and (>= (ascii (substr textLevel 2 1)) 48) (<= (ascii (substr textLevel 2 1)) 57))
            (> (strlen textLevel) 5)
          )
          (setq level (atof (substr textLevel 2)))
        )
        ; Sub-base level
        ( (and
            (or (= "SB" (substr textLevel 1 2)) (= "sb" (substr textLevel 1 2)) )
            (= 3 (- (strlen textLevel) (vl-string-search "." textLevel)) )
          )
          (setq level (atof (substr textLevel 3 8)))
        )
        ; Non number
        ( (and
            (= (atof textLevel) 0)
            (or (< (ascii (substr textLevel 2 1)) 97) (> (ascii (substr textLevel 2 1)) 122))
            (/= (ascii (substr textLevel 1 1)) 48)
          )
          (setq level (getreal "\nNumber format not understood. Please, introduce level: "))
        )
        ; Other
        (t
          ;(alert "case 7")
          (if (= 0.0 (atof textLevel))
            (progn
              (initget "Yes No")
              (setq ans (getkword (strcat "\nNo standard format. Verify " textLevel "m level. [Yes/No] <Yes>:")))
              (if (or (not ans) (= ans "Yes"))
                (setq level (atof textLevel))
                (exit)
              )
            );END progn
            (setq level (atof textLevel))
          );END if
        );END cond4
      );END cond
      (if difference
        (+ level difference )
        level
      );END if
    );END progn
  );END if

  ; v0.1 - 2017.03.06 - Function renamed
  ; v0.0 - 2017.03.01 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.03.01
)
(defun DT:AddSubstractPlotLevel ( ent_name / initialContent value )
  ; Add or substract 50mm to plot level
  (if ent_name
    (if
      (or
        (= "TEXT"  (cdr (assoc 0 (entget ent_name)) ))
        (= "MTEXT" (cdr (assoc 0 (entget ent_name)) ))
      ); END or
      (progn
        (setq value (substr (vlax-get-property (vlax-ename->vla-object ent_name) 'TextString) 4) )
        (princ (strcat "\nPress +/- to add/substract 50mm: <" value "m> "))
        (while (setq gr (grread nil 8))
          (cond
            ( (and
                (= 2 (car  gr))
                (or
                  (= 43 (cadr gr))  ;+
                  (= 45 (cadr gr))  ;-
                  (= 49 (cadr gr))  ;1
                  (= 50 (cadr gr))  ;2
                );END or
              );END and
              ; Calculate level value
              (setq
                value
                  (cond
                    ((= 43 (cadr gr)) (LM:rtos (+ (atof value) 0.05) 2 2))
                    ((= 45 (cadr gr)) (LM:rtos (- (atof value) 0.05) 2 2))
                    ((= 49 (cadr gr)) (LM:rtos (+ (atof value) 0.05) 2 2))
                    ((= 50 (cadr gr)) (LM:rtos (- (atof value) 0.05) 2 2))
                  );END cond
              )
              ; Update text value
              (vla-put-textstring (vlax-ename->vla-object ent_name) (strcat "%%U" value))
              (princ (strcat "\nPress +/- to add/substract 50mm: <" value "m> "))
            );END subcond
            (t
              (exit)
            );END subcond
          );END cond
        );END while
      );END progn
    );END if
  );END if
  (princ)

  ; v0.2 - 2017.03.10 - Console break message when pressing Esc removed
  ; v0.1 - 2017.03.08 - 1 and 2 keys added as + and - respectively
  ; v0.0 - 2017.03.06 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.03.10
)
(defun DT:AddLayer ( layerName layerColor layerLineType / layerObject )
  ; Create a new layer and set name, color and linetype.
  (if (and layerName layerColor layerLineType)
    (if
      (and
        (= 'str (type layerName))
        (= 'int (type layerColor))
        (= 'str (type layerLineType))
      )
      (if (not (tblsearch "layer" layerName))
        (progn
          (setq layerObject (vla-add (vla-Get-Layers (vla-get-ActiveDocument (vlax-get-Acad-Object))) layerName) )
          (vla-put-color layerObject layerColor)
          (if (tblsearch "ltype" layerLineType)
            (vla-put-linetype layerObject layerLineType)
            (progn
              (princ
                (strcat
                  "\nLinetype \"" layerLineType "\" not found. Layer \"" layerName "\" linetype left as default."
                );END strcat
              );END princ
              (princ)
            );END progn
          );END if
        );END progn
        (progn (princ (strcat "\nLayer \"" layerName "\" already exists. Command aborted.")) (princ) )
      );END if
      (cond
        ((/= 'str (type layerName))     (princ "\nERROR @ DT:AddLayer : layerName is not a string\n")      (princ) )
        ((/= 'int (type layerColor))    (princ "\nERROR @ DT:AddLayer : layerColor is not an integer\n")   (princ) )
        ((/= 'str (type layerLineType)) (princ "\nERROR @ DT:AddLayer : layerLineType is not a string\n")(princ) )
      );END cond
    );END if
    (cond
      ((not layerName)     (princ "\nERROR @ DT:AddLayer : layerName=nil\n")    (princ) )
      ((not layerColor)    (princ "\nERROR @ DT:AddLayer : layerColor=nil\n")   (princ) )
      ((not layerLineType) (princ "\nERROR @ DT:AddLayer : layerLineType=nil\n")(princ) )
    );END cond
  );END if

  ; v0.1 - 2017.03.16 - Type management fiexd for layerLineType
  ; v0.0 - 2017.03.10 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.03.16
)
(defun DT:GetPointList ( msg / p pointList scapeVariable )
  ; Return a list with selected points, nil if none
  (if (and msg (/= 'str (type msg)))
    (progn
      (princ "\nERROR @ DT:GetPointList : msg is not a string\n")
      (princ)
    );END progn
    (while (not scapeVariable)
      (if (and msg (= 'str (type msg)))
        (setq p (getpoint msg))
        (setq p (getpoint "Select point: "))
      );END if
      (if p
        (if (setq pointList (append pointList (list p)))
          (princ
            (strcat
              "\n" (itoa (length pointList)) " " (if (= 1 (length pointList)) "point" "points") " stored. "
            );END strcat
          );END princ
        );END if
        (setq scapeVariable 1)
      );END if
    );END while
  );END if

  (if pointList pointList nil)

  ; v0.0 - 2017.03.10 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.03.10
)
(defun c:rwp()
  (DT:ib "e-psd-rwp" nil nil 4)
  (command "_.pline" "_non" (getvar "lastpoint") "_nea" pause "")
)
(defun DT:NextRevisionLetter ( letter )
  ; Return next letter for a revision box
  (if letter
    (if (= 'str (type letter))
      (cond
        ( (= (ascii letter) 45)
          ; if "-", return "A"
          "A"
        );END subcond
        ( (or
            (and (>= (ascii letter)  65) (<  (ascii letter)  72)) ; single alphabetical character between "A" (65) and "Z" (90), excluding "H" (72), "I" (73) and "Z" (90)
            (and (>  (ascii letter)  73) (<  (ascii letter)  90))
            (and (>= (ascii letter)  97) (<  (ascii letter) 104)) ; single alphabetical character between "a" (97) and "z" (122), excluding "h" (104) "i" (105) and "z" (122)
            (and (>  (ascii letter) 105) (<  (ascii letter) 122))
          );END or
          (chr (+ (ascii letter) 1))
        );END subcond
        ( (or
            (= (ascii letter)  72) ; "H" (72)
            (= (ascii letter) 104) ; "h" (104)
          );END or
          (chr (+ (ascii letter) 2))
        );END subcond
        ( (or
            (= (ascii letter)  73) ; "H" (73)
            (= (ascii letter) 104) ; "i" (105)
          );END or
          (princ "\nWHY DOES THIS REVISION HAVE AN \"I\" LETTER!?!?\n")(princ)
          nil
        );END subcond
        ( (or
            (= (ascii letter)  90) ; "Z" (90)
            (= (ascii letter) 122) ; "z" (122)
          );END or
          (princ "\nOops... last letter reached!\n")
          nil
        );END subcond
      );END cond
      (progn (princ "\nERROR @ DT:NextLetter : letter is not a string\n")(princ) )
    );END if
    (progn (princ "\nERROR @ DT:NextLetter : letter=nil\n")(princ) )
  );END if

  ; v0.0 - 2017.03.21 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.03.21
)
(defun DT:PrevRevisionLetter ( letter )
  ; Return previous letter for a revision box
  (if letter
    (if (= 'str (type letter))
      (cond
        ( (= (ascii letter) 45)
          ; if "-", return nil
          (princ "\nOops...no previous revision letter where to go!\n")
          nil
        );END subcond
        ( (or
            (and (>  (ascii letter)  65) (<  (ascii letter)  73)) ; single alphabetical character between "A" (65) and "Z" (90), excluding "A" (65), "I" (73) and "J" (74)
            (and (>  (ascii letter)  74) (<= (ascii letter)  90))
            (and (>  (ascii letter)  97) (<  (ascii letter) 105)) ; single alphabetical character between "a" (97) and "z" (122), excluding "a" (97) "i" (105) and "j" (106)
            (and (>  (ascii letter) 106) (<= (ascii letter) 122))
          );END or
          (chr (- (ascii letter) 1))
        );END subcond
        ( (or
            (= (ascii letter)  74) ; "J" (74)
            (= (ascii letter) 106) ; "j" (106)
          );END or
          (chr (- (ascii letter) 2))
        );END subcond
        ( (or
            (= (ascii letter)  73) ; "H" (73)
            (= (ascii letter) 104) ; "i" (105)
          );END or
          (princ "\nWHY DOES THIS REVISION HAVE AN \"I\" LETTER!?!?\n")(princ)
          nil
        );END subcond
        ( (or
            (= (ascii letter)  65) ; "A" (65)
            (= (ascii letter)  97) ; "a" (97)
          );END or
          "-"
        );END subcond
      );END cond
      (progn (princ "\nERROR @ DT:NextLetter : letter is not a string\n")(princ) )
    );END if
    (progn (princ "\nERROR @ DT:NextLetter : letter=nil\n")(princ) )
  );END if

  ; v0.0 - 2017.03.21 - Minor bug fixed
  ;                   - First issue
  ; Author: David Torralba
  ; Last revision: 2017.03.21
)
(defun DT:DegToRad ( angle )
  ; Return angle in radians
  (if angle
    (if (numberp angle)
      (* pi (/ (float angle) 180) )
    );END if
  );END if

  ; v0.0 - 2017.03.23 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.03.23
)
(defun DT:RadToDeg ( angle )
  ; Return angle in sexagesimal degrees
  (if angle
    (if (numberp angle)
      (* 180 (/ (float angle) pi) )
    );END if
  );END if

  ; v0.0 - 2017.03.23 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.03.23
)
(defun DT:GetLayoutStatus ( tab / ss found )
  ; Return 1 if "NOT ISSUED YET" text found within the tab,
  ; return 0 if not found
  (if tab
    (if (= 'str (type tab))
      ; Look for "NOT ISSUED YET" text within "tab" layout
      ;(if (setq ss (ssget "_x" (list (cons 0 "TEXT") ) ))
      (if (setq ss (ssget "_x" (list (cons 0 "TEXT") (cons 410 tab)) ))
        (progn
          ; Set "found" value to "not-found"
          (setq found 0)

          ; Check all objects in ss
          (foreach a (ssnamex ss)
            (if (= 'ename (type (cadr a)))
              (if
                (and
                  (= "NOT ISSUED YET" (cdr (assoc  1 (entget (cadr a)))))
                  (= "MJA-Title"      (cdr (assoc  8 (entget (cadr a)))))
                  (= 1                (cdr (assoc 62 (entget (cadr a)))))
                );END and
                (setq found 1)
              );END if
            );END if
          );END foreach
          ; Return result
          (if found found 0)
        );END progn
        ; Return not found value if ss=nil
        0
      );END if
      (progn (princ "\nERROR @ DT:GetLayoutStatus > tab is not a string\n") nil)
    );END if
    (progn (princ "\nERROR @ DT:GetLayoutStatus > tab = nil\n") nil)
  );END if

  ; v0.1 - 2017.06.12 - Return value if ss=nil updated to be zero (0, as integer)
  ; v0.0 - 2017.03.28 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.06.12
)
(defun DT:GetTabInformation ( tab / tabRevision tabStatus )
  ; Return a list with tab name, status (issued or not), and last revision
  (if tab
    (if (= 'str (type tab))
      (progn
        ; Get last revision
        (if (not (setq tabRevision (DT:GetLayoutLatestRevision tab)))
          (setq tabRevision "??")
        );END if

        ; Get layout status (issued/not issued)
        (cond
          ( (= 0 (DT:GetLayoutStatus tab))  ; ISSUED
            (setq tabStatus "-")
          );END subcond
          ( (= 1 (DT:GetLayoutStatus tab))  ; NOT ISSUED YET
            (setq tabStatus "NOT ISSUED YET")
          );END subcond
          (t                                    ; something when wrong
            (setq tabStatus nil)
          )
        );END cond

        ; Return tab information
        (list
          tab
          tabRevision
          tabStatus
        );END list
      );END progn
      (progn (princ "\nERROR @ DT:GetTabInformation > tab is not a string\n") nil)
    );END if
    (progn (princ "\nERROR @ DT:GetTabInformation > tab = nil\n") nil)
  );END if

  ; v0.2 - 2017.06.14 - Minor bug fixed on DT:GetLayoutLatestRevision implementation
  ; v0.1 - 2017.03.29 - DT:GetLayoutLatestRevision implemented
  ; v0.0 - 2017.03.28 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.06.14
)
(defun DT:ListToTable ( lst / maxColumnLength stringTable )
  ; Return the provided list as a table
  (if lst
    (if (= 'list (type lst))
      (if (= 1 (DT:CheckListTableFormat lst)) ; Check that list has correct format
        (progn
          (setq
            ; Get column width: measure the nth element of each row and get the maximum
            maxColumnLength (DT:GetColumnLength lst)
            ; Get list values as a table formated string
            stringTable (DT:NL_GetTextFormatted lst maxColumnLength)
          )
        );END progn
        (progn (princ "\nERROR @ DT:ListToTable > lst doesn't have table format\n") nil)
      );END if
      (progn (princ "\nERROR @ DT:ListToTable > lst is not a list\n") nil)
    );END if
    (progn (princ "\nERROR @ DT:ListToTable > lst = nil\n") nil)
  );END if

  ; v0.0 - 2017.03.29 - Minor bug fixed
  ; v0.0 - 2017.03.28 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.03.29
)
(defun DT:CheckListTableFormat ( lst / elementAmount return )
  ; Return 1 if the provided list has table format, 0 if not, nil otherwise.
  (if lst
    (if (= 'list (type lst))
      (progn
        (setq
          return 1                            ; set return value to 1
          elementAmount (length (nth 0 lst))  ; get first row (element) length
        )
        ; Run through all elements and check they have the same length
        ; if any row (element) doesn't have the same legth as the first one, return 0
        (foreach element lst
          (if (/= elementAmount (length element))
            (setq return 0)
          );END if
        );END foreach
        return
      );END progn
      (progn (princ "\nERROR @ DT:CheckListTableFormat > lst is not a list\n") nil)
    );END if
    (progn (princ "\nERROR @ DT:CheckListTableFormat > lst = nil\n") nil)
  );END if

  ; v0.0 - 2017.03.28 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.03.28
)
(defun DT:ListNilToString ( lst / returnElement returnRow returnList)
  ; Return provided list (with table format) with all nil values changed to empty strings ("").
  ; NOTE: this function is designed for 2D tables, thus one list (table) with a first levels of
  ; nested lists (rows) in it. Only one nested level is allowed. See example:
  ; (list (list a b c) (list a            b c) (list a b c))  --> OK
  ; (list (list a b c) (list (list a b c) b c) (list a b c))  --> wrong
  (if lst
    (if (= 'list (type lst))
      (if (= 1 (DT:CheckListTableFormat lst)) ; Check that list has correct format
        (progn
          (foreach row lst
            (foreach element row
              (if element
                (setq returnElement element)
                (setq returnElement "")
              );END if
              (setq
                returnRow (append returnRow (list returnElement))
                returnElement nil
              )
            );END foreach
            (setq
              returnList (append returnList (list returnRow))
              returnRow nil
            )
          );END foreach
          returnList
        );END progn
        (progn (princ "\nERROR @ DT:ListNilToString > lst doesn't have table format\n") nil)
      );END if
      (progn (princ "\nERROR @ DT:ListNilToString > lst is not a list\n") nil)
    );END if
    (progn (princ "\nERROR @ DT:ListNilToString > lst = nil\n") nil)
  );END if

  ; v0.0 - 2017.03.29 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.03.29
)
(defun DT:DrawPlotLevel ( level point rotation )
  ; Draw plot level with "level" value at "point" with with the correct format
  (if (and level point rotation)
    (if (and (numberp level) (= 'list (type point)) (numberp rotation))
      (if (tblsearch "style" "ROMANS")
        (entmakex
          (list
            (cons  0 "TEXT")
            (cons  1 (strcat "%%U" (LM:rtos level 2 2)) )
            (cons  7 "ROMANS")
            (cons  8 "e-plot-lev")
            (cons 10 (polar point (* 1.5 pi) 2.2))
            (cons 11 point) ; needed for text justification
            (cons 40 0.35)
            (cons 71 0) ; needed for text justification
            (cons 72 1) ; needed for text justification
            (cons 73 2) ; needed for text justification
          );END list
        );END entmakex
      );END if
      (cond
        ((not (numberp level))    (princ "\nERROR @ DT:DrawPlotLevel : level is not a number\n")    nil )
        ((/= 'list (type point))  (princ "\nERROR @ DT:DrawPlotLevel : point is not a list\n")      nil )
        ((not (numberp rotation)) (princ "\nERROR @ DT:DrawPlotLevel : rotation is not a number\n") nil )
      );END cond
    );END if
    (cond
      ((not level)    (princ "\nERROR @ DT:DrawPlotLevel : level=nil\n")    nil )
      ((not point)    (princ "\nERROR @ DT:DrawPlotLevel : point=nil\n")    nil )
      ((not rotation) (princ "\nERROR @ DT:DrawPlotLevel : rotation=nil\n") nil )
    );END cond
  );END if

  ; v0.0 - 2017.03.29 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.03.29
)
(defun c:UpdateTextReadabilityAngle ( / ss )
  ; Correct all selected text's rotation to be readable
  ; Used to update contour level labels
  (princ "\nUPDATE TEXT READABILITY ANGLE\n")
  (if (setq ss (ssget '(( 0 . "TEXT"))))
    (foreach a (ssnamex ss)
      (if (= 'ename (type (cadr a)))
        (if
          (= "TEXT" (cdr (assoc 0 (entget (cadr a)))))
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

  ; v0.0 - 2017.04.03 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.04.03
)
(defun c:zp ( )
  ; Return to previous view
  (princ "\nReturning to previous view... ")
  (command "zoom" "P" ^C^C)
  (princ "done!\n")
  (princ)

  ; v0.0 - 2017.04.07 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.04.07
)

(defun DT:GetTrustedPaths ( / trustedPaths )
  ; Return a list with each trusted path as a list element
  (if (setq trustedPaths (getvar "trustedpaths"))
    (DT:StringToList trustedPaths ";")
    (progn (princ "\nERROR @ DT:GetTrustedPaths > trustedPaths = nil\n") nil )
  );END if

  ; v0.2 - 2017.04.12 - Code tidy up
  ; v0.1 - 2017.01.27 - DT:StringToList implemented
  ; v0.0 - 2017.01.23 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.04.12
)
(defun DT:AddTrustedPath ( newTrustedPath )
  (if newTrustedPath
    (if (= 'str (type newTrustedPath))
      (setvar "trustedpaths"
        (strcat
          (getvar "trustedpaths")
          ";"
          newTrustedPath
        );END strcat
      );END setvar
      (progn (princ "\nERROR @ DT:AddTrustedPath > newTrustedPath is not a string\n") nil )
    );END if
    (progn (princ "\nERROR @ DT:AddTrustedPath > newTrustedPath = nil\n") nil )
  );END if

  ; v0.0 - 2017.04.12 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.04.12
)
(defun DT:RemoveTrustedPath ( trustedPathToRemove / updatedTrustedPaths currentTrustedPaths return )
  ; Return T if "trustedPathToRemove" was found and removed from trusted paths,
  ; otherwise will return nil
  (if trustedPathToRemove
    (if (= 'str (type trustedPathToRemove))
      (progn
        ; Get current trusted paths
        (if (setq currentTrustedPaths (DT:GetTrustedPaths))
          (foreach path currentTrustedPaths
            (if (/= path trustedPathToRemove)
              (if updatedTrustedPaths
                (setq updatedTrustedPaths (strcat updatedTrustedPaths ";" path))
                (setq updatedTrustedPaths path)
              );END if
            );END if
          );END foreach
        );END if

        ; Set return
        (if (= (getvar "trustedpaths") updatedTrustedPaths )
          (setq return nil)
          (setq return T)
        );END if

        ; Update trusted paths
        (setvar "trustedpaths" updatedTrustedPaths)

        ; Return value
        return

      );END progn
      (progn (princ "\nERROR @ DT:RemoveTrustedPath > newTrustedPath is not a string\n") nil )
    );END if
    (progn (princ "\nERROR @ DT:RemoveTrustedPath > trustedPathToRemove = nil\n") nil )
  );END if

  ; v0.0 - 2017.04.12 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.04.12
)
(defun DT:ShowTrustedPaths ( / i )
  ; Print on screen the trusted paths numbered
  (setq i 0 )
  (foreach path (DT:GetTrustedPaths)
    (setq i (+ i 1))
    (princ (strcat "\n" (itoa i) ": " path) )
  );END foreach
  (princ)

  ; v0.0 - 2017.05.04 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.05.04
)
(defun DT:GetSupportPaths ( / supportPaths )
  ; Return a list with each trusted path as a list element
  (if (setq supportPaths (getenv "ACAD"))
    (DT:StringToList supportPaths ";")
    (progn (princ "\nERROR @ DT:GetSupportPaths > supportPaths = nil\n") nil )
  );END if

  ; v0.0 - 2017.05.04 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.05.04
)
(defun DT:AddSupportPath ( newSupportPath )
  (if newSupportPath
    (if (= 'str (type newSupportPath))
      (setenv "ACAD"
        (strcat
          (getenv "ACAD")
          ";"
          newSupportPath
        );END strcat
      );END setvar
      (progn (princ "\nERROR @ DT:AddSupportPath > newSupportPath is not a string\n") nil )
    );END if
    (progn (princ "\nERROR @ DT:AddSupportPath > newSupportPath = nil\n") nil )
  );END if

  ; v0.0 - 2017.04.12 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.04.12
)
(defun DT:RemoveSupportPath ( supportPathToRemove / updatedSupportPaths currentSupportPaths return )
  ; Return T if "supportPathToRemove" was found and removed from trusted paths,
  ; otherwise will return nil
  (if supportPathToRemove
    (if (= 'str (type supportPathToRemove))
      (progn
        ; Get current trusted paths
        (if (setq currentSupportPaths (DT:GetSupportPaths))
          (foreach path currentSupportPaths
            (if (/= path supportPathToRemove)
              (if updatedSupportPaths
                (setq updatedSupportPaths (strcat updatedSupportPaths ";" path))
                (setq updatedSupportPaths path)
              );END if
            );END if
          );END foreach
        );END if

        ; Set return
        (if (= (getenv "acad") updatedSupportPaths )
          (setq return nil)
          (setq return T)
        );END if

        ; Update trusted paths
        (setenv "acad" updatedSupportPaths)

        ; Return value
        return

      );END progn
      (progn (princ "\nERROR @ DT:RemoveSupportPath > newSupportPath is not a string\n") nil )
    );END if
    (progn (princ "\nERROR @ DT:RemoveSupportPath > supportPathToRemove = nil\n") nil )
  );END if

  ; v0.0 - 2017.05.04 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.05.04
)
(defun c:flip ( / mainMenuPath enterpriseMenuPath )
  ; Flip between Enterprise CUI file read and edition mode
  ; Credit to Lee Ambrosius

  ; Get current main CUI filename
  (setq mainMenuPath (getvar "MenuName") )
  ; Get current enterprise CUI filename
  (setq enterpriseMenuPath (getenv "EnterpriseMenuFile") )
  ; Set the main CUI as as the enterprise CUI
  (setenv "EnterpriseMenuFile" mainMenuPath)
  ; Load the enterprise menu as the main CUI file
  (command "._menu" enterpriseMenuPath)

  ; v0.0 - 2017.05.04 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.05.04
)
(defun DT:TotalArea ( ss / totalArea )
  ; Return total area (if any) of the objects within the pickset ss
  (if ss
    (if (= 'pickset (type ss))
      (progn
        (foreach a (ssnamex ss)
          (if (= 'ename (type (cadr a)))
            (if (vlax-property-available-p (vlax-ename->vla-object (cadr a)) 'Area)
              (if totalArea
                (setq totalArea (+ totalArea (vla-get-area (vlax-ename->vla-object (cadr a)))))
                (setq totalArea (vla-get-area (vlax-ename->vla-object (cadr a))))
              );END if
            );END if
          );END if
        );END foreach

        ; Return total area if any
        (if totalArea totalArea)
      );END progn
      (progn (princ "\nERROR @ DT:TotalArea : ss is not a pickset\n") nil )
    );END if
    (progn (princ "\nERROR @ DT:TotalArea : ss=nil\n") nil )
  );END if

  ; v0.0 - 2017.04.18 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.04.18
)
(defun DT:DrawText ( pt lay str height rotation )
  ; Draw a text
  (entmakex
    (list
      (cons 0 "TEXT")
      (if (tblsearch "style" "ROMANS") (cons 7 "ROMANS"))
      (cons 8 lay)
      (cons 10 (polar pt (* 1.5 pi) 2.2))
      (cons 11 pt) ; needed for text justification
      (cons 40 height )
      (cons 1  str)
      (cons 50 rotation)
      (cons 71 0) ; needed for text justification
      (cons 72 1) ; needed for text justification
    );END list
  );END entmakex

  ; v0.0 - 2017.04.18 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.04.18
)
(defun dump ( VlaObject )
  ; Shortcut to show VLA object properties and methods

  (vlax-dump-object VlaObject)

  ; v0.0 - 2017.05.08 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.05.08
)
(defun c:DrawManholeBodyOnLongSection ()
  ; Draw manhole body on long section using selected manhole axis
  (setq ss (ssget '((-4 . "<OR")(0 . "LINE")(0 . "LWPOLYLINE")(-4 . "OR>"))))
  (if ss
    (foreach a (ssnamex ss)
      (if (= 'ename (type (cadr a)))
        (DT:DrawManholeBodyOnLongSection (cadr a))
      );END if
    );END foreach
  );END if

  (princ)

  ; v0.0 - 2017.05.12 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.05.12
)
(defun DT:Type3DPoint ( pt )
  ; Return T if pt is a 3D point, otherwise return nil
  (if pt
    (if (= 'list (type pt))
      (if (= 3 (length pt))
        (if (and (numberp (nth 0 pt)) (numberp (nth 1 pt)) (numberp (nth 2 pt)))
          T
          (progn (princ "\nERROR @ DT:Type3DPoint : some elements in pt are not numbers\n") nil )
        );END if
        (progn (princ "\nERROR @ DT:Type3DPoint : pt needs to have 3 elements\n") nil )
      );END if
      (progn (princ "\nERROR @ DT:Type3DPoint : pt is not a list\n") nil )
    );END if
    (progn (princ "\nERROR @ DT:Type3DPoint : pt=nil\n") nil )
  );END if

  ; v0.0 - 2017.05.12 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.05.12
)
(defun DT:Type2DPoint ( pt )
  ; Return T if pt is a 2D point, otherwise return nil
  (if pt
    (if (= 'list (type pt))
      (if (= 2 (length pt))
        (if (and (numberp (nth 0 pt)) (numberp (nth 1 pt)))
          T
          (progn (princ "\nERROR @ DT:Type3DPoint : some elements in pt are not numbers\n") nil )
        );END if
        (progn (princ "\nERROR @ DT:Type2DPoint : pt needs to have 2 elements\n") nil )
      );END if
      (progn (princ "\nERROR @ DT:Type2DPoint : pt is not a list\n") nil )
    );END if
    (progn (princ "\nERROR @ DT:Type2DPoint : pt=nil\n") nil )
  );END if

  ; v0.0 - 2017.05.12 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.05.12
)
(defun c:tt ( / text )
  ; Copy any text anywhere with 2 clicks
  (if (setq text (DT:GetText (car (nentselp "\nClick on source text: "))))
    (DT:SetText (car (nentsel (strcat "\nClick to override for \"" text "\": "))) text )
  );END if

  ; v0.0 - 2017.05.12 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.05.12
)
(defun c:ttt ( / text ss )
  ; Same as c:tt but with multiple selection
  (if (setq text (DT:GetText (car (nentselp "\nClick on source text: "))))
    (if (setq ss (ssget '((-4 . "<OR")(0 . "TEXT")(0 . "MTEXT")(-4 . "OR>"))))
      (foreach a (ssnamex ss)
        (if (= 'ename (type (cadr a)))
          (DT:SetText (cadr a) text)
        );END if
      );END foreach
    );END if
  );END if

  ; v0.0 - 2017.05.25 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.05.25
)
(defun c:wt ( / text )
  ; Override selected text content
  (DT:SetText (car (nentsel "\nClick on text: ")) (getstring "\nWrite text (use \"\\P\" as newlines) :" t) )

  ; v0.0 - 2017.05.12 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.05.12
)
(defun c:et ( / text )
  ; Edit selected text content
  (if (setq ename (car (nentsel "\nClick on text: ")))
    (if (setq currentText (DT:GetText ename))
      (if (setq newText (DT:PreFilledGetString "\nWrite text (use \"\\P\" as newlines) :" currentText))
        (DT:SetText ename newText)
      );END if
    );END if
  );END if

  ; v0.0 - 2017.08.11 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.11
)
(defun c:gp ( / pt )
  ; Copy point coordinates to clipboard
  (if (setq pt (getpoint "\nSelect point to copy to clipboard: "))
    (CopyToClipboard
      (setq pt
        (strcat
          (LM:rtos (nth 0 pt) 2 3) ","
          (LM:rtos (nth 1 pt) 2 3) ","
          (LM:rtos (nth 2 pt) 2 3)
        );END strcat
      );END setq
    );END CopyToClipboard
    (progn
      (princ "\nNothing copied.\n")(princ)
    );END progn
  );END if

  ; v0.0 - 2017.05.16 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.05.16
)
(defun c:pt ( / pointString )
  ; Print point coordinates with 3 decimals and copy it to Clipboard
  (if (setq pointString (DT:Pt (getpoint "\nSelect point to get coordinates: ")))
    (progn
      (princ (strcat "\n" pointString))
      (CopyToClipboard pointString)
      (princ)
    );END progn
  );END if
  ; v0.0 - 2017.05.26 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.05.26
)
(defun DT:Pt ( point )
  ; Print point coordinates with 3 decimals and copy it to Clipboard
  (if (DT:Arg 'DT:Pt '((point 'list)))
    (strcat
      (LM:rtos (nth 0 point) 2 3) ","
      (LM:rtos (nth 1 point) 2 3) ","
      (LM:rtos (nth 2 point) 2 3)
    );END strcat
  );END if

  ; v0.0 - 2017.05.26 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.05.26
)
(defun DT:ZoomToEntity ( ent_name / varMinPoint varMaxPoint)
  ; Zoom to passed object
  (if (DT:Arg 'DT:ZoomToEntityVla '((ent_name 'ename)))
    (progn
      (vla-GetBoundingBox (vlax-ename->vla-object ent_name) 'varMinPoint 'varMaxPoint)
      (if (and varMinPoint varMaxPoint)
        (vla-ZoomWindow (vlax-get-acad-object) varMinPoint varMaxPoint)
        (DT:Error 'DT:ZoomToEntityVla "something wrong with vla-GetBoundingBox")
      );END if
    );END progn
  );END if

  ; v0.0 - 2017.05.31 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.05.31
)
(defun DT:PrintVar ( var )
  ; Show variable name, type and value
  ; var argument can be any symbol or a list of symbols
  ;  - Single symbol:    (DT:PrintVar 'variableToCheck)
  ;  - List of symbols:  (DT:PrintVar '(var1 var2 var3 var4 var5))
  ; NOTE: pass quoted values to check variables
  (cond
    ((= 'list (type var))
      (foreach a var
        (if (= 'sym (type a))
          (princ
            (strcat
              "\n" (vl-symbol-name a)
              " [" (vl-princ-to-string (type (eval a))) "]"
              " = " (vl-prin1-to-string (eval a))
            );END strcat
          );END princ
          (princ
            (strcat
              "\n[" (vl-princ-to-string (type a)) "]"
              " = " (vl-prin1-to-string a)
            );END strcat
          );END princ
        );END if
      );END foreach
    );END subcond
    ((= 'sym (type var))
      (princ
        (strcat
          "\n" (vl-symbol-name var)
          " [" (vl-princ-to-string (type (eval var))) "]"
          " = " (vl-prin1-to-string (eval var))
        );END strcat
      );END princ
    );END subcond
    (t
      (princ
        (strcat
          "\n[" (vl-princ-to-string (type var)) "]"
          " = " (vl-prin1-to-string var)
        );END strcat
      );END princ
    );END subcond
  );END cond
  (princ)

  ; v0.1 - 2017.08.24 - Variable value printed as prin1 instead of princ
  ; v0.0 - 2017.05.31 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.24
)
(defun DT:RoundTo ( x r / r )
  ; Round x to the nearest r

  ; Convert to real
  (if (and (numberp x) (numberp r))
    (progn
      (if (and (= 'int (type x)) (= 'int (type r)) )
        (setq r (float r))
      );END if
      (setq multiple (* r (atof (LM:rtos (/ x r) 2 0))) )

    );END progn
    (DT:Error 'DT:RoundTo "x or r are not numbers")
  );END if

  ; v0.0 - 2017.05.31 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.05.31
)
(defun c:UpdatePrivateDrainageLabel ()
  (DT:UpdatePrivateDrainageLabel 5)
  (princ)

  ; v0.0 - 2017.05.31 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.05.31
)
(defun DT:UpdatePrivateDrainageLabel ( round / p1 p2 z1 z2 msg 3Dp1 3Dp2 gradient sewerLabel )
  ; Update private drainage label
  ; if round  = nil --> don't round
  ; if round != nil --> rounded to nearest "round"

  ; INPUT - Point 1
  (if (not (setq p1 (getpoint "\nSelect point A: ")))
    (if (not INT_lastPoints)
      ; Force user to input a point if there is no INT_lastPoints saved yet
      (while (not p1) (setq p1 (getpoint "\nSelect point A: ")) )
      (setq
        p1 (nth 0 INT_lastPoints)
        z1 (nth 1 INT_lastPoints)
        p2 (nth 2 INT_lastPoints)
        z2 (nth 3 INT_lastPoints)
      )
    );END if
  );END if

  ; INPUT - Point 1 level, if not taken from INT_lastPoints
  (if (not z1)
   (setq z1 (DT:clic_or_type_level))
  );END if

  (princ "\nLevel A = ")(princ z1)(princ "m")

  ; INPUT - Point 2, if not taken from INT_lastPoints
  (if (not p2)
    (setq p2 (getpoint "\nSelect point B: "))
  );END if

  ; INPUT - Point 2 level, if not taken from INT_lastPoints
  (if (not z2)
    (setq z2 (DT:clic_or_type_level))
  );END if
  (princ "\nLevel B = ")(princ z2)(princ "m")

  ; Save input as global variables
  (setq INT_lastPoints (list p1 z1 p2 z2) )

  ; Calculate 3D points
  (setq
    3Dp1 (list (nth 0 p1) (nth 1 p1) z1)
    3Dp2 (list (nth 0 p2) (nth 1 p2) z2)
    gradient (abs (DT:Gradient 3Dp1 3Dp2))
  )

  (if (= z1 z2)
    (setq msg "\nSelected points are at the same level.")
    (if round
      (setq msg (strcat
        "\nGradient = 1/" (LM:rtos gradient 2 0) " (" (LM:rtos (/ 100 gradient) 2 2) "%)"
        "\n                ~ 1/" (LM:rtos (DT:RoundTo gradient round) 2 0) " (" (LM:rtos (/ 100 (DT:RoundTo gradient round)) 2 2) "%)"
        )
      )
      (setq msg (strcat "\nGradient = 1/" (LM:rtos gradient 2 0) " (" (LM:rtos (/ 100 gradient) 2 2) "%)"))
    );END if
  )

  ; Select sewer label
  (if (setq sewerLabel (car (entsel (strcat msg "\nSelect pipe label: "))))
    (if (setq sewerSize (DT:GetSewerSize sewerLabel))
      (cond
        ((= 100 sewerSize)
          (if (> gradient 80)
            (progn (princ "\nDN100 flatter than 1/80. WRONG\n") nil)
            (if round
              (DT:ChangePrivateSewerGradient sewerLabel (DT:RoundTo gradient round))
              (DT:ChangePrivateSewerGradient sewerLabel gradient)
            );END if
          )
        );END subcond
        ((= 150 sewerSize)
          (if round
            (DT:ChangePrivateSewerGradient sewerLabel (DT:RoundTo gradient round))
            (DT:ChangePrivateSewerGradient sewerLabel gradient)
          );END if
        );END subcond
        (t
          (alert "I only can handle DN100 and\nDN150 pipe sizes... Sorry! :P")
        );END subcond
      );END cond
      nil
    );END if
    nil
  );END if

  ; v0.0 - 2017.05.31 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.05.31
)
(defun DT:StringifyTableList ( tableList )
  ; Convert all elements withing the list in strings
  (if (DT:Arg 'DT:StringifyTableList '((tableList 'list)))
    (mapcar
      '(lambda (x) (mapcar 'vl-princ-to-string x))
      tableList
    );END mapcar
  );END if

  ; v0.0 - 2017.06.23 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.06.23
)
(defun DT:EntityType ( ent_name )
  ; Return a string with "ent_name" entity type
  (if (DT:Arg 'DT:EntityType '((ent_name 'ename)))
    (cdr (assoc 0 (entget ent_name)))
  );END if

  ; v0.0 - 2017.06.27 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.06.27
)
(defun DT:CheckIfEntityExists ( ename / e return )
  ; Returns T if entity name is found, otherwise returns nil
  (if (and ename (= 'ename (type ename)))
    (if (eq ename (setq e (entnext)))
      T
      (progn
        (while (setq e (entnext e))
          (if (eq ename e)
            (setq return T)
          );END if
        );END while
        return
      );END progn
    );END if
  );END if

  ; v0.0 - 2017.07.05 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.07.05
)
(defun c:RotateOneByOne ( / ss rotation )
  ; Rotate selected entities one by one
  (if (setq ss (ssget))
    (if (setq rotation (getreal "\nInput rotation angle in degrees: "))
      (progn
        ; Convert rotation from degrees to radians
        (setq rotation (DT:DegToRad rotation))
        ; Batch rotate selected objects, if possible
        (foreach a (ssnamex ss)
          (if (= 'ename (type (cadr a)))
            (progn
              (setq object (vlax-ename->vla-object (cadr a)))
              (if (vlax-property-available-p object 'Rotation T)
                (vlax-put-property object 'Rotation
                  (+ rotation (vlax-get-property object 'Rotation))
                );END vlax-put-property
              );END if
            );END progn
          );END if
        );END foreach
      );END progn
    );END if
  );END if

  ; v0.0 - 2017.07.31 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.07.31
)
(DT:AutoLoadFileFromCivilTemp "IssueReport.lsp")
(defun c:gradient ( / p1 p2 dX dY dZ dXY d )
  ; Return the gradient between two points
  (if (setq p1 (getpoint "\nSelect first point:"))
    (if (setq p2 (getpoint "\nSelect second point:"))
      (progn
        (setq dX (abs (- (nth 0 p1) (nth 0 p2))))
        (setq dY (abs (- (nth 1 p1) (nth 1 p2))))
        (setq dZ (abs (- (nth 2 p1) (nth 2 p2))))
        (setq dXY (sqrt (+ (* dX dX) (* dY dY)) ))
        (setq d (sqrt (+ (* dXY dXY) (* dZ dZ)) ))

        (if (= dZ 0)
          (princ "\nGradient = flat")
          (princ (strcat "\nGradient = 1/" (LM:rtos (/ dXY dZ) 2 0)))
        );END if
        (princ)

      );END progn
    );END if
  );END if
)
