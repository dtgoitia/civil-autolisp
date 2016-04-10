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

  ; INPUT - Select object and get the entity data
  (setq
    ent (entsel txt_msg)
    ent_name (car ent)                            ; entity name
    VL_ent_name (vlax-ename->vla-object ent_name) ; VL entity name
    capa (vla-get-layer VL_ent_name)              ; layer
    point (cdr (assoc 10 (entget ent_name)))      ; block coordinates
    vis_sta (LM:getvisibilitystate VL_ent_name)   ; visibility state
  )

  ; Check if 'getattributes is applicable
  (if (vlax-method-applicable-p VL_ent_name 'getattributes) ; Condition
    (progn                                                  ; True. Se puede aplicar el método
      (setq
        ; Extract ILs (returned as strings)
        txt_IL1 (get_IL VL_ent_name "IL1")
        txt_IL2 (get_IL VL_ent_name "IL2")
        txt_IL3 (get_IL VL_ent_name "IL3")
        txt_IL4 (get_IL VL_ent_name "IL4")
        ; Convert ILs to a real number (string to real)
        IL1 (atof txt_IL1)
        IL2 (atof txt_IL2)
        IL3 (atof txt_IL3)
        IL4 (atof txt_IL4)
      )

      (cond
      ; CASE A - 1 IL available
        ((= vis_sta "1")
          (setq IL IL1)
        ) ; END A - 1 IL available

      ; CASE B - 2 IL available
        ((= vis_sta "2")
          ; Show available ILs:
          (princ (strcat "\nIL1=" txt_IL1 "   IL2=" txt_IL2))
          ; Choose desired IL:
          (initget 1 "1 2")
          (setq answer (getkword "\nMore than one IL found. Select IL [1/2]: "))
          (cond
            ((= answer "1") (setq IL IL1)) ; IL = IL1
            ((= answer "2") (setq IL IL2)) ; IL = IL2
          )
        ) ; END B - 2 IL available

      ; CASE C - 3 IL available
        ((= vis_sta "3")
          ; Show available ILs:
          (princ (strcat "\nIL1=" txt_IL1 "   IL2=" txt_IL2 "   IL3=" txt_IL3))
          ; Choose desired IL:
          (initget 1 "1 2 3")
          (setq answer (getkword "\nMore than one IL found. Select IL [1/2/3]: "))
          (cond
            ((= answer "1") (setq IL IL1)) ; IL = IL1
            ((= answer "2") (setq IL IL2)) ; IL = IL2
            ((= answer "3") (setq IL IL3)) ; IL = IL3
          )
        ) ; END C - 3 IL disponibles

      ; CASE D - 4 IL disponibles
        ((= vis_sta "4")
          ; Show available ILs:
          (princ (strcat "\nIL1=" txt_IL1 "   IL2=" txt_IL2 "   IL3=" txt_IL3 "   IL4=" txt_IL4 ))
          ; Choose desired IL:
          (initget 1 "1 2 3 4")
          (setq answer (getkword "\nMore than one IL found. Select IL [1/2/3/4]: "))
          (cond
            ((= answer "1") (setq IL IL1)) ; IL = IL1
            ((= answer "2") (setq IL IL2)) ; IL = IL2
            ((= answer "3") (setq IL IL3)) ; IL = IL3
            ((= answer "4") (setq IL IL4)) ; IL = IL4
          )
        ) ; END CASE D - 4 IL disponibles
      ); END cond
    )
    (princ "\nThis object has no IL attributes.")         ; False. No se puede aplicar el método
  ); END if
  (princ "\nSelected IL: ")(princ IL)
  ; Return values
  (list IL capa point)
)
(defun c:DM (/
              ; LOCAL VARIABLES
              oldlayer oldosmode oldcmdecho
              answer
              real_text_height text_height
              manhole_layer MNH1_capa capa
              IL IL1 IL2 txt_ID txt_IL2
              p_ins point p0 p1 pm
              cursor_text_L1 cursor_text_L2 cursor_line
              grad txt_grad
              dist txt_dist
              dif
              ang ang_grados
              diam txt_diam
              pl_wid
              label ancho_caja
            )
  ; Dynamic Manhole

  ; SET - Error handling function
  (defun *error* ( msg )
    (if (not (member msg '("Function cancelled" "quit / exit abort")))
      (princ (strcat "\nError: " msg))
    )
    ; OPERATION - Delete cursor objects, if exist
    (if (/= cursor_text_L1 nil) (vla-delete (vlax-ename->vla-object cursor_text_L1)))
    (if (/= cursor_text_L2 nil) (vla-delete (vlax-ename->vla-object cursor_text_L2)))
    (if (/= cursor_line nil) (vla-delete (vlax-ename->vla-object cursor_line)))

    ; Restore previous settings
    (setvar "clayer" oldlayer)
    (setvar "osmode" oldosmode)
    (setvar "cmdecho" oldcmdecho)
    (setvar "attdia" oldattdia)
    (setvar "attreq" oldattreq)
    (setvar "angbase" oldangbase)

    ; End without double messages
    (princ)
  )

  ; SAVE CURRENT SETTINGS - Current layer, OSMODE and CMDECHO
  (setq
    oldlayer    (getvar "clayer")
    oldosmode   (getvar "osmode")
    oldcmdecho  (getvar "cmdecho")
    oldattdia   (getvar "attdia")
    oldattreq   (getvar "attreq")
    oldangbase  (getvar "angbase")
    oldangdir   (getvar "angdir")
  )

  ; CHANGE INITIAL SETTINGS - "osmode" and "cmdecho"
  (setvar "osmode" 0)
  (setvar "cmdecho" 0)
  (setvar "attdia" 0)
  (setvar "attreq" 1)
  (setvar "angbase" 0)
  (setvar "angdir" 0)

  ; SET - Real text height
  (setq real_text_height 0.02)

  ; INPUT - Elegir como empezar: nueva arqueta o existente ---------------------------------------------------------- AÑADIDO EN CASA, REVISAR
  (initget "Start Continue")
  (setq ans1 (getkword "\nSelect action [Start/Continue] creating manholes <Continue>: "))
  (if (not ans1) (setq ans1 "Continue"))
  (if (= ans1 "Start")
    (progn
  	  ; INPUT - Choose Storm o Foul System
  	  (setq manhole_layer nil)
  	  (while (= manhole_layer nil)
    		(initget "Storm Foul")
    		(setq manhole_layer (getkword "\nSelect drainage system [Storm/Foul]: "))
    		(if (not manhole_layer) (princ "Answer not understood. Please, try again."))
  	  ); END while

      ; INPUT - Manhole data: name, IL and location
  	  (cond
        ( (= manhole_layer "Storm")  ; Storm Drainge
  	      (setq txt_ID (getstring "\nSelect manhole name <SW00>: "))
  		    (if (not txt_ID) (setq txt_ID "SW00"))
  		  ); END cond Storm Drainage
  		  ( (= manhole_layer "Foul")  ; Foul Drainge
          (setq txt_ID (getstring "\nSelect manhole name <FW00>: "))
          (if (not txt_ID) (setq txt_ID "FW00"))
  	    ); END cond Foul Drainage
      ) ; END cond
  	  (setq txt_IL (getstring "\nSelect manhole IL <00.000>: "))
      (if (not txt_IL)
        (setq ; IF true
          txt_IL "00.000"
          IL (rtos txt_IL 2 3)
        )
        (setq ; IF false
          IL (atof txt_IL)
          txt_IL (LM:rtos IL 2 3)
        )
      ); END if
  	  (setq p_ins (getpoint "\nSelect point to insert manhole: "))

  	  ; OPERATION - Change layer and insert manhole block
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
  	  ; OPERATION - Exit routine
  	  (exit)
    ) ; END progn True
  ); END if

  ; INPUT - Select existing manhole
  (setq
    IL_data_list (get_IL_manhole "\nSelect an existing manhole:") ; Extract existing manhole data
    IL1 (car IL_data_list)                                        ; Save IL
    MNH1_capa (cadr IL_data_list)                                 ; Save layer
    p0 (caddr IL_data_list)                                       ; Save coordinates
  )
  ; OPERATION - Abort routine if manhole is in the wrong layer.
	(if (and
          (/= MNH1_capa "e-asd")
          (/= MNH1_capa "e-afd")
          (/= MNH1_capa "e-asd-lateral")
          (/= MNH1_capa "e-afd-lateral")
      )
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
  (initget 1 "Slope Gradient Level")
  (setq ans2 (getkword "\nChoose to keep constant [Gradient/Level]: "))
  (cond
    ((or (= ans2 "Gradient") (= ans2 "Slope"))
      ; ASK - Gradient
      (setq grad nil)
      (while (not grad)
        (setq grad (getreal "\nIntroduce gradient = 1/"))
        (if (not grad) (princ " ... what?\nMate, read the instructions before running please."))
      ); END while

      ; OPERATION - Warn about flat pipe and abort routine without creating it.
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

        ; OPERATION - Delete cursor objects, if exist
        (if (/= cursor_text_L1 nil) (vla-delete (vlax-ename->vla-object cursor_text_L1)))
        (if (/= cursor_text_L2 nil) (vla-delete (vlax-ename->vla-object cursor_text_L2)))
        (if (/= cursor_line nil) (vla-delete (vlax-ename->vla-object cursor_line)))

        ; OPERATION - Calculate
        (setq text_height (* real_text_height (getvar "viewsize")))

        ; OPERATION - Create cursor data
        (setq cursor_text_L1 ( _CursorText_L1 p1 (strcat "IL = " txt_IL2) text_height))
        (setq cursor_text_L2 ( _CursorText_L2 p1 (strcat "L = " txt_dist "m") text_height))
        (setq cursor_line ( _Set_Line p0 p1 ))
      ) ; END while
      (setq txt_grad (itoa (LM:Round grad)))
    ); END cond grad

    ; LEVEL
    ((= ans2 "Level")
      ; ASK - level
      (setq IL2 nil)
      (while (not IL2)
        (setq IL2 (getreal "\nIntroduce finish level: "))
        (if (not IL2) (princ " ... what?\nMate, read the instructions before running please."))
      ); END while

      ; OPERATION - Warn about flat pipe and abort routine without creating it.
      (if (= IL2 IL1)
        (progn
          (princ "\n---\nERROR: this routine can't create flat pipes.\nThe routine will be aborted.\n")
          (alert "ERROR: this routine can't create flat pipes.\n\nRoutine will be aborted.")
          (exit)
        ); END progn
      ) ; END if

      ; OPERATION - Calculate and print gradient in real time
      (while (= 5 (car (setq gr (grread 't 13 0))))
        (setq p1 (cadr gr)
              dist (distance p0 p1)
              txt_dist (rtos dist 2 1)
              dif (- IL2 IL1)
              grad (/ dist dif)
              txt_grad (itoa (LM:Round grad))
        )

        ; OPERATION - Delete cursor objects, if exist
        (if (/= cursor_text_L1 nil) (vla-delete (vlax-ename->vla-object cursor_text_L1)))
        (if (/= cursor_text_L2 nil) (vla-delete (vlax-ename->vla-object cursor_text_L2)))
        (if (/= cursor_line nil) (vla-delete (vlax-ename->vla-object cursor_line)))

        ; OPERATION - Calculate
        (setq text_height (* real_text_height (getvar "viewsize")))

        ; OPERATION - Create cursor data
        (setq cursor_text_L1 ( _CursorText_L1 p1 (strcat "p = 1/" txt_grad) text_height))
        (setq cursor_text_L2 ( _CursorText_L2 p1 (strcat "L = " txt_dist "m") text_height))
        (setq cursor_line ( _Set_Line p0 p1 ))
      ) ; END while
      (setq txt_IL2 (LM:rtos IL2 2 3))
    ); END cond Level
  );END cond

  ; INPUT - Ask new manhole name
  (setq txt_ID2 (getstring "\nIntroduce new manhole name: "))

  ; OPERATION - Format gradient
  (if (< grad 0) (setq grad (- 0 grad)))
  (setq txt_grad (itoa (LM:Round grad)))

  ; OPERATION - Change layer to insert manhole block and draw pipe
  (setvar "CLAYER" MNH1_capa)
  (cond
    ((= MNH1_capa "e-asd")  ; Storm Drainge
      (command "-insert" "SW-Manhole" p1 "" "" "" txt_ID2 "" txt_IL2)
    )
    ((= MNH1_capa "e-afd")  ; Foul Drainge
      (command "-insert" "FW-Manhole" p1 "" "" "" txt_ID2 "" txt_IL2)
    )
  ) ; END cond
  ;  PENDIENTE --------------------------------------------------------------------------------- PENDIENTE
  ; Crear una función auxiliar para calcular correctamente el ángulo del texto y toda esta mierda...
  ; OPERATION - Calculate angle between p0 and p1 to locate and rotate label
	(setq
    ang (angle p0 p1)
    ang_grados (/ (* ang 180) pi) ; Convert rad to deg
  )
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

  ; OPERATION - Calculate text coordinate and angle
  (setq
    pm (polar p0 ang (/ dist 2))                ; Pipe midle point coordinates
    dist_ins (+ (/ pl_wid 2) 0.15)              ; Distance from pipe axis to text insertion point
  )
  ; OPERATION - Inverse dist_ins if needed
  (if (or (<= ang (* -0.5 pi)) (and (>= ang (* 0.5 pi)) (<= ang (* 1.5 pi)) ) ) (setq dist_ins (- 0 dist_ins)))
  (setq
    p_ins (polar pm (+ ang (/ pi 2)) dist_ins)  ; Text insertion point
    ancho_caja (- dist 1.2)                     ; Text box width
  )
  (if (< ancho_caja 1) (setq ancho_caja 2))     ; Text box width correction, if needed

  ; OPERATION - Create MText
	(command "-mtext" p_ins "R" ang_grados "H" "0.400" "J" "BC" "W" ancho_caja label "")

  ; OPERATION - Clean cursor objects, if exist
  (if (/= cursor_text_L1 nil) (vla-delete (vlax-ename->vla-object cursor_text_L1)))
  (if (/= cursor_text_L2 nil) (vla-delete (vlax-ename->vla-object cursor_text_L2)))
  (if (/= cursor_line nil) (vla-delete (vlax-ename->vla-object cursor_line)))

  ; RESTORE PREVIOUS SETTINGS
  (setvar "clayer" oldlayer)
  (setvar "osmode" oldosmode)
  (setvar "cmdecho" oldcmdecho)
  (setvar "attdia" oldattdia)
  (setvar "attreq" oldattreq)
  (setvar "angbase" oldangbase)
  (setvar "angdir" 0)

  ; End without double messages
  (princ)

  ; v0.3 - 2016.04.10 - Code tidy up and translation.
  ;                   - Reformat typed levels.
  ;                   - getkword variables renamed to avoid possible errors.
  ;                   - Fix angle calculation bug.
  ; v0.2 - 2016.04.08 - Bug fixed when setting finish level.
  ; v0.1 - 2016.03.02 - Cursor text font updated to Arial style.
  ; v0.0 - 2016.03.01
  ; Author: David Torralba
  ; Last revision: 2016.04.10

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
