(defun _CursorLine ( pa pb )
  (entmakex
    (list (cons 0 "LINE")
      (cons 10 pa)                                            ; Punto de inicial
      (cons 11 pb)                                            ; Punto de final
      (cons 62 8)                                             ; Color de la línea
      (cons 6 "DASHED")                                       ; Tipo de línea discontínua
      (cons 48 0.06)                                          ; Escala de línea
    )
  )
)
(defun get_IL ( obj IL )
  (vl-some '(lambda ( att )
              (if (= IL (strcase (vla-get-tagstring att)))
                (vla-get-textstring att)
              ) ; END if
            )
          (vlax-invoke obj 'getattributes)
        )
)
(defun get_IL_manhole ( txt_msg )  ;
  ; (Re)load VLISP
  (vl-load-com)

  ; INPUT - Select object
  (setq ent (entsel txt_msg))

  ; OPERATION - get the entity and entity name
  (setq ent_name (car ent))

  ;convert to vl object
  (setq VL_ent_name (vlax-ename->vla-object ent_name))

  ; get layer
  (setq capa (vla-get-layer VL_ent_name))

  ; get block coordinates
  (setq point (cdr (assoc 10 (entget ent_name))))

  ; get visibility state
  (setq vis_sta (LM:getvisibilitystate VL_ent_name))

  ; Comprobar si el metodo 'getattributes es aplicable
  (if (vlax-method-applicable-p VL_ent_name 'getattributes) ; Condition
    (progn                                                  ; True. Se puede aplicar el método
      ; Extraer ILs (salen como texto)
      (setq txt_IL1 (get_IL VL_ent_name "IL1"))
      (setq txt_IL2 (get_IL VL_ent_name "IL2"))
      (setq txt_IL3 (get_IL VL_ent_name "IL3"))
      (setq txt_IL4 (get_IL VL_ent_name "IL4"))
      ; Convertir ILs a número (string to real)
      (setq IL1 (atof txt_IL1))
      (setq IL2 (atof txt_IL2))
      (setq IL3 (atof txt_IL3))
      (setq IL4 (atof txt_IL4))
      ; Imprimir los recogido:
      ;(princ (strcat "\nIL1 = " txt_IL1 "\nIL2 = " txt_IL2 "\nIL3 = " txt_IL3 "\nIL4 = " txt_IL4 ))   ; multilinea
      ;(princ (strcat "\nIL1=" txt_IL1 "   IL2=" txt_IL2 "   IL3=" txt_IL3 "   IL4=" txt_IL4 ))   ; línea única

      (cond
      ; CASE A - 1 IL disponible
        ((= vis_sta "1")
        ;((and (/= IL1 0) (= IL2 0) (= IL3 0) (= IL4 0))
          (setq IL IL1)
        ) ; END A - 1 IL disponible

        ; CASE B - 2 IL disponibles
        ((= vis_sta "2")
        ;((and (/= IL1 0) (/= IL2 0) (= IL3 0) (= IL4 0))
          (progn
            ; Mostrar IL posibles:
            (princ (strcat "\nIL1=" txt_IL1 "   IL2=" txt_IL2))
            ; Dar a elegir qué IL quieres:
            (initget 1 "1 2")
            (setq answer (getkword "\nMore than one IL found. Select IL [1/2]: "))
            (cond
              ((= answer "1") (setq IL IL1)) ; IL = IL1
              ((= answer "2") (setq IL IL2)) ; IL = IL2
            )
          )
        ) ; END B - 2 IL disponibles

        ; CASE C - 3 IL disponibles
        ((= vis_sta "3")
        ;((and (/= IL1 0) (/= IL2 0) (/= IL3 0) (= IL4 0))
          (progn
            ; Mostrar IL posibles:
            (princ (strcat "\nIL1=" txt_IL1 "   IL2=" txt_IL2 "   IL3=" txt_IL3))
            ; Dar a elegir qué IL quieres:
            (initget 1 "1 2 3")
            (setq answer (getkword "\nMore than one IL found. Select IL [1/2/3]: "))
            (cond
              ((= answer "1") (setq IL IL1)) ; IL = IL1
              ((= answer "2") (setq IL IL2)) ; IL = IL2
              ((= answer "3") (setq IL IL3)) ; IL = IL3
            )
          )
        ) ; END C - 3 IL disponibles

        ; CASE D - 4 IL disponibles
        ((= vis_sta "4")
        ;((and (/= IL1 0) (/= IL2 0) (/= IL3 0) (/= IL4 0))
          (progn
            ; Mostrar IL posibles:
            (princ (strcat "\nIL1=" txt_IL1 "   IL2=" txt_IL2 "   IL3=" txt_IL3 "   IL4=" txt_IL4 ))
            ; Dar a elegir qué IL quieres:
            (initget 1 "1 2 3 4")
            (setq answer (getkword "\nMore than one IL found. Select IL [1/2/3/4]: "))
            (cond
              ((= answer "1") (setq IL IL1)) ; IL = IL1
              ((= answer "2") (setq IL IL2)) ; IL = IL2
              ((= answer "3") (setq IL IL3)) ; IL = IL3
              ((= answer "4") (setq IL IL4)) ; IL = IL4
            )
          )
        ) ; END D - 4 IL disponibles
      )
    )
    (princ "\nThis object has no IL attributes.")         ; False. No se puede aplicar el método
  ); END if
  (princ "\nSelected IL: ")(princ IL)
  (princ)
)
(defun c:DM (/ oldlayer oldosmode oldcmdecho)
  ; Dynamic Manhole

  ; SET - Error handling function
  (defun *error* ( msg )
    (if (not (member msg '("Function cancelled" "quit / exit abort")))
      (princ (strcat "\nError: " msg))
    )
    ; OPERATION - Borrar objetos del cursor, si existen
    (if (/= cursor_text_L1 nil) (vla-delete (vlax-ename->vla-object cursor_text_L1)))
    (if (/= cursor_text_L2 nil) (vla-delete (vlax-ename->vla-object cursor_text_L2)))
    (if (/= cursor_line nil) (vla-delete (vlax-ename->vla-object cursor_line)))

    ; Restore previous settings
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

  ; CHANGE INITIAL SETTINGS - "osmode" and "cmdecho"
  (setvar "osmode" 0)
  (setvar "cmdecho" 0)

  ; SET - Real text height
  (setq real_text_height 0.02)

  ; INPUT - Elegir como empezar: nueva arqueta o existente ---------------------------------------------------------- AÑADIDO EN CASA, REVISAR
  (initget "Start Continue")
  (setq answer (getkword "\n Select action [Start/Continue] creating manholes <Continue>: "))
  (if (not answer) (setq answer "Continue"))
  (if (= answer "Start")
    (progn
	  ; INPUT - Elegir Storm o Foul System
	  (setq manhole_layer nil)
	  (while (= manhole_layer nil)
		(initget "Storm Foul")
		(setq manhole_layer (getkword "\nSelect drainage system [Storm/Foul]: "))
		(if (not manhole_layer) (princ "Answer not understood. Please, try again."))
	  ); END while

    ; INPUT - Elegir nombre del manhole, el IL inicial y el punto de inserción
	  (cond
        ((= manhole_layer "Storm")  ; Storm Drainge
	      (setq txt_ID (getstring "\nSelect manhole name <SW00>: "))
		  (if (not txt_ID) (setq txt_ID "SW00"))
		); END cond Storm Drainage
		((= manhole_layer "Foul")  ; Foul Drainge
          (setq txt_ID (getstring "\nSelect manhole name <FW00>: "))
		  (if (not txt_ID) (setq txt_ID "FW00"))
	    ); END cond Foul Drainage
    ) ; END cond
	  (setq txt_IL (getstring "\nSelect manhole IL <00.000>: "))
    (if (not txt_IL)
      (setq
        txt_IL "00.000"
        IL (rtos txt_IL 2 3)
      )
      (setq
        IL (atof txt_IL)
        txt_IL (rtos IL 2 3)
      )
    ); END if
	  (setq p_ins (getpoint "\nSelect point to insert manhole: "))

	  ; OPERATION - Cambiar de capa e insertar manhole
	  (cond
      ((= manhole_layer "Storm")  ; Storm Drainge
        (setvar "CLAYER" "e-asd")
        (command "-insert" "SW-Manhole" p_ins "" "" "" txt_ID "" txt_IL)
      ); END cond Storm Drainage
      ((= manhole_layer "Foul")  ; Foul Drainge
        (setvar "CLAYER" "e-afd")
        (command "-insert" "FW-Manhole" p_ins "" "" "" txt_ID "" txt_IL)
	    ); END cond Foul Drainage
      ) ; END cond
	  ; OPERATION - Salir de la rutina
	  (exit)
    ) ; END progn True
  ); END if


  ; ---------------------------------------------------------- AÑADIDO EN CASA, REVISAR
  ; INPUT - Seleccionar el manhole 1 (origen)
  (get_IL_manhole "\nSelect an existing manhole:")  ; Doy un valor a la variable "IL"
  (setq IL1 IL)                                     ; Guardo el valor de "IL"
  (setq MNH1_capa capa)                             ; Guardo el valor de "capa"
  (setq p0 point)                                   ; Guardo el valor de "point"

  ; OPERATION - Abortar la rutina si el manhole está en una capa que no debe.
	(if (and (/= MNH1_capa "e-asd") (/= MNH1_capa "e-afd"))             ; If manhole is not in "e-asd" or "e-afd" layers
    (progn
      (princ "\n---\nSelected manhole is not in \"e-asd\" or \"e-afd\" layers. \nThe routine will be aborted now.\n")
      (alert                                                          ; give alert error message
        "\nSelected manhole is not in \"e-asd\" or \"e-afd\" layers. \nThe routine will be aborted now.")
      (exit)                                                          ; and finish the routine
    )
  )

  ; SET - Switch off OSNAP
  (setvar "osmode" 0)

  ; OPERATION - Clean object names
  (setq
    cursor_text_L1 nil
    cursor_text_L2 nil
    cursor_line nil
  )

  ; INPUT - Ask to make constant grad or IL
  (initget 1 "Gradient Level")
  (setq answer (getkword "\nChoose to keep constant [Gradient/Level]: "))

  ; OPERATION - React according to answer
  (cond
    ; Gradient
    ((= answer "Gradient")
      ; ASK - Gradient
      (setq grad (getreal "\nIntroduce gradient = 1/"))

      ; OPERATION - Alertar de una tuberia plana
      (if (= grad 0)
        (progn
          (princ "\n---\nERROR: this routine can't create flat pipes.\nThe routine will be aborted.\n")
          (alert "ERROR: this routine can't create flat pipes.\n\nRoutine will be aborted.")
          (exit)
        ); END progn
      ) ; END if

      ; OPERATION - Calculate and print level
      (while (= 5 (car (setq gr (grread 't 13 0))))
        (setq p1 (cadr gr)
              dist (distance p0 p1)
              txt_dist (rtos dist 2 1)
              dif (/ dist grad)
              IL2 (+ IL1 dif)
              txt_IL2 (rtos IL2 2 3)
        )

        ; OPERATION - Borrar objetos del cursor, si existen
        (if (/= cursor_text_L1 nil) (vla-delete (vlax-ename->vla-object cursor_text_L1)))
        (if (/= cursor_text_L2 nil) (vla-delete (vlax-ename->vla-object cursor_text_L2)))
        (if (/= cursor_line nil) (vla-delete (vlax-ename->vla-object cursor_line)))

        ; OPERATION - Calcular
        (setq text_height (* real_text_height (getvar "viewsize")))

        ; OPERATION - Crear datos del cursor
        (setq cursor_text_L1 ( _CursorText_L1 p1 (strcat "IL = " txt_IL2) text_height))
        (setq cursor_text_L2 ( _CursorText_L2 p1 (strcat "L = " txt_dist "m") text_height))
        (setq cursor_line ( _CursorLine p0 p1 ))

        ;(princ (strcat "\nL = " txt_dist "m     IL = " txt_IL2))
      ) ; END while
      (setq txt_grad (itoa (LM:Round grad))) ; ------------------------------ COMPROBAR QUE ESTO NO SOBRA
    )

    ; LEVEL
    ((= answer "Level")
      ; ASK - level
      (setq IL2 (getreal "\nIntroduce finish level: "))

      ; OPERATION - Alertar de una tuberia plana
      (if (= IL2 IL1)
        (progn
          (princ "\n---\nERROR: this routine can't create flat pipes.\nThe routine will be aborted.\n")
          (alert "ERROR: this routine can't create flat pipes.\n\nRoutine will be aborted.")
          (exit)
        ); END progn
      ) ; END if

      ; OPERATION - Calculate and print grad
      (while (= 5 (car (setq gr (grread 't 13 0))))
        (setq p1 (cadr gr)
              dist (distance p0 p1)
              txt_dist (rtos dist 2 1)
              dif (- IL2 IL1)
              grad (/ dist dif)
              txt_grad (itoa (LM:Round grad))
        )

        ; OPERATION - Borrar objetos del cursor, si existen
        (if (/= cursor_text_L1 nil) (vla-delete (vlax-ename->vla-object cursor_text_L1) text_height))
        (if (/= cursor_text_L2 nil) (vla-delete (vlax-ename->vla-object cursor_text_L2) text_height))
        (if (/= cursor_line nil) (vla-delete (vlax-ename->vla-object cursor_line)))

        ; OPERATION - Calcular
        (setq text_height (* real_text_height (getvar "viewsize")))

        ; OPERATION - Crear datos del cursor
        (setq cursor_text_L1 ( _CursorText_L1 p1 (strcat "p = 1/" txt_grad)))
        (setq cursor_text_L2 ( _CursorText_L2 p1 (strcat "L = " txt_dist "m")))
        (setq cursor_line ( _CursorLine p0 p1 ))

        ;(princ (strcat "\nL = " txt_dist "m     grad = 1/" txt_grad))
      ) ; END while
      (setq txt_IL2 (rtos dist 2 3)) ; ------------------------------ COMPROBAR QUE ESTO NO SOBRA
    )
  )

  ; INPUT - Ask new manhole name
  (setq txt_ID2 (getstring "\nIntroduce new manhole name: "))

  ; OPERATION (after clic) - Cambiar la pendiente y su texto a valor absoluto
  (if (< grad 0)
    (setq grad (- 0 grad))
  )
  (setq txt_grad (itoa (LM:Round grad)))

  ; OPERATION - Cambiar de capa para insertar manhole y dibujar tubería
  (setvar "CLAYER" MNH1_capa)
  (cond
    ((= MNH1_capa "e-asd")  ; Storm Drainge
      (command "-insert" "SW-Manhole" p1 "" "" "" txt_ID2 "" txt_IL2)
    )
    ((= MNH1_capa "e-afd")  ; Foul Drainge
      (command "-insert" "FW-Manhole" p1 "" "" "" txt_ID2 "" txt_IL2)
    )
  ) ; END cond

  ; OPERATION - Calcular el angulo entre 0 y 1 (para luego colocar y orientar el texto)
	(setq ang (angle p0 p1))

  ; OPERATION - Convertir en ángulo a grados sexagesimales
  (setq ang_grados (/ (* ang 180) pi))

  ; OPERATION - Corregir orientación del angulo para alinear correctamente el texto
  (if
    (or (<= ang_grados -90) (and (>= ang_grados 90) (<= ang_grados 270)))
    (setq ang_grados (+ ang_grados 180))
  ) ; END if

  ; INPUT - Introducir el diámetro de tubería.
  (setq txt_diam (getstring "\nIntroduce pipe diameter in mm : <0mm>"))
  (if (= txt_diam "")
    (setq diam 0)                   ; If true
    (setq diam (atoi txt_diam))     ; If false
  ) ; END if

  ; OPERATION - Calcular el grosor de la línea según el diámetro de la tubería
	(setq pl_wid (/ (float diam) 1000)) ; primero convierto el integer a real y luego lo paso a metros

  ; OPERATION - Convierte el diámetro de la tubería a texto (itoa)
	(setq txt_diam (itoa diam))

  ; OPERATION - Unir las string de distancia, tipo (SWS/FWS), diámetro y pendiente.
	(setq label (strcat txt_dist "m DN" txt_diam " @ 1/" txt_grad))

  ; OPERATION - Dibujar tubería: polilínea del centro de 1 al centro de 2.
	(command "_.pline" p0 "_w" pl_wid pl_wid p1 "")

  ; OPERATION - Calcular el punto medio de la polilínea
  (setq pm (polar p0 ang (/ dist 2)))

  ; OPERATION - Calcular la la distancia para colocar correctamente el texto, e invertirla si corresponde por su orientación
  (setq dist_ins (+ (/ pl_wid 2) 0.15) )
  (if
    (or (<= ang (* -0.5 pi)) (and (>= ang (* 0.5 pi)) (<= ang (* 1.5 pi))))
    (setq dist_ins (- 0 dist_ins))
  ) ; END if

  ; OPERATION - Calcular el punto de inserción del texto (separado un poco de la polilínea, teniendo en cuenta el grosor de la polilínea)
  (setq p_ins (polar pm (+ ang (/ pi 2)) dist_ins))

  ; OPERATION - Calcular el ancho del texto
  (setq ancho_caja (- dist 1.2))
  (if (< ancho_caja 1) (setq ancho_caja 2))

  ; OPERATION - Crear MText
	(command "-mtext" p_ins "R" (- 0 ang_grados) "H" "0.400" "J" "BC" "W" ancho_caja label "")

  ; CLEAN VARIABLES
  ;(setq ent nil )

  ; OPERATION - Borrar datos del cursor, si existen
  (if (/= cursor_text_L1 nil) (vla-delete (vlax-ename->vla-object cursor_text_L1)))
  (if (/= cursor_text_L2 nil) (vla-delete (vlax-ename->vla-object cursor_text_L2)))
  (if (/= cursor_line nil) (vla-delete (vlax-ename->vla-object cursor_line)))

  ; RESTORE PREVIOUS SETTINGS
  (setvar "clayer" oldlayer)
  (setvar "osmode" oldosmode)
  (setvar "cmdecho" oldcmdecho)

  ; End without double messages
  (princ)

  ; v0.1 - 2016.03.02 - Fuente del texto del cursor ("Style") actualizada a Arial.
  ; v0.0 - 2016.03.01
  ; Author: David Torralba
  ; Last revision: 2016.03.01

  ; PENDIENTE:  configurar la rutina como cuando creas una polilínea, un click continuo para introducir la siguiente.
  ;             o que recuerde la ultima arqueta creada, y empiece a partir de ahi
  ; PENDIENTE:  añadir la posibilidad de que la arqueta final sea una ya existente. Solo tiene que cambiar el estado de visibilidad
  ;             de la arqueta existente y añadir otro IL en ella.
  ; PENDIENTE:  permitir, mientras interpolas en vivo, cambiar entre IL fijo y pendiente FIJA, así como modificar el IL o
  ;             pendiente introducida

  ; PENDIENTE: actualizar variables locales
  ; PENDIENTE: actualizar variables a limpiar
  ; PENDIENTE: revisar comentarios pendientes
  ; PENDIENTE: limpiar funciones no usadas
)
