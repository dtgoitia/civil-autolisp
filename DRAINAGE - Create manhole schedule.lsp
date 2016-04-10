(defun get_manhole_att ( txt_msg )
  ; (Re)load VLISP
  (vl-load-com)

  ; INPUT - Select object
  (setq block_effective_name nil)
  (while  (and
            (/= block_effective_name "FW-Manhole")
            (/= block_effective_name "SW-Manhole")
          )
    (setq ent (entsel txt_msg))
    (if (not ent)
      (princ "\nNothing selected. Try again, please.\n") ; True
      (progn
        ; OPERATION - get the entity and entity name
        (setq ent_name (car ent))
        (if (= (cdr (assoc 0 (entget ent_name))) "INSERT")
          (progn
            ; OPERATION - Convert to name to VL object
            (setq VL_ent_name (vlax-ename->vla-object ent_name))
            ; OPERATION - Comprobar si es un manhole, y si no lo es abortar la rutina.
            (setq block_effective_name (LM:effectivename VL_ent_name))
            (if
              (or (= block_effective_name "FW-Manhole") (= block_effective_name "SW-Manhole") )
              (princ "\nManhole selected.\n")                               ; True
              (princ "\nNot manhole block selected. Please, try again.\n")  ; False
            ); END if
          )
          (princ "\nNot manhole block selected. Please, try again.\n") ; False
        ); END if
      ); END progn
    ); END if
  ); END while

  ; get visibility state
  (setq vis_sta (LM:getvisibilitystate VL_ent_name))

  ; Comprobar si el metodo 'getattributes es aplicable
  (if (vlax-method-applicable-p VL_ent_name 'getattributes) ; Condition
    (progn                                                ; True. Se puede aplicar el método
      ; Extraer el ID y el CL
      (setq ID (get_block_att VL_ent_name "ID")
            CL (get_block_att VL_ent_name "CL")
      )
      ; OPERATION - Extrer las coordenadas de la etiqueta
      (setq coord (cdr (assoc 10 (entget ent_name))))
      (setq
        E_coord (car coord)  txt_E_coord (rtos E_coord 2 3)
        N_coord (cadr coord) txt_N_coord (rtos N_coord 2 3)
      )
      (princ "\ncoord = ")(princ coord)
      (princ "\nE_coord = ")(princ E_coord)
      (princ "\nN_coord = ")(princ N_coord)
      ; Extraer los ILs (salen como texto), convertirlos a número (string to real)
      ; y convertirlos a string otra vez bien formateados con 3 decimales
      (setq txt_IL1 (get_block_att VL_ent_name "IL1") IL1 (atof txt_IL1) txt_IL1 (rtos IL1 2 3)
            txt_IL2 (get_block_att VL_ent_name "IL2") IL2 (atof txt_IL2) txt_IL2 (rtos IL2 2 3)
            txt_IL3 (get_block_att VL_ent_name "IL3") IL3 (atof txt_IL3) txt_IL3 (rtos IL3 2 3)
            txt_IL4 (get_block_att VL_ent_name "IL4") IL4 (atof txt_IL4) txt_IL4 (rtos IL4 2 3)
      )

      ; Imprimir los recogido:
      (princ (strcat "\nID = " ID "\nCL = " CL "\nIL1 = " txt_IL1 "\nIL2 = " txt_IL2 "\nIL3 = " txt_IL3 "\nIL4 = " txt_IL4 ))   ; multilinea
    ); END progn true
    (princ "\nThis object is not a manhole.")         ; False. No se puede aplicar el método
  ); END if
  (princ)
)
(defun c:MSC (/
              oldlayer oldosmode oldcmdecho oldattdia oldattreq
              ID CL
              IL0 IL1 IL2 IL3 IL4 txt_IL0 txt_IL1 txt_IL2 txt_IL3 txt_IL4
              p_ins p_ins2
              last_ent VL_last_ent ent ent_name VL_ent_name
              vis_sta
              )
  ; SET - Error handling function
  (defun *error* ( msg )
    (if (not (member msg '("Function cancelled" "quit / exit abort")))
      (princ (strcat "\nError: " msg))
    )
    ; RESTORE PREVIOUS SETTINGS
    (setvar "clayer" oldlayer)
    (setvar "osmode" oldosmode)
    (setvar "cmdecho" oldcmdecho)
    (setvar "attdia" oldattdia)
    (setvar "attreq" oldattreq)
    (princ)
  )

  ; SAVE CURRENT SETTINGS - Current layer, OSMODE and CMDECHO
  (setq
    oldlayer (getvar "clayer")
    oldosmode (getvar "osmode")
    oldcmdecho (getvar "cmdecho")
    oldattdia (getvar "attdia")
    oldattreq (getvar "attreq")
  )

  ; SET INITIAL SETTINGS
  (setvar "osmode" 0)
  (setvar "cmdecho" 0)
  (setvar "attdia" 0)
  (setvar "attreq" 1)

  ; INPUT - Seleccionar el manhole
	(get_manhole_att "\nSelect a manhole:")  ; Extraigo los atributos a las correspondientes variables (ver funcion mas arriba)

  ; OPERATION - Activar referencias
  (setvar "osmode" 1)

  ; INPUT - Seleccionar el punto de inserción del bloque
  (setq p_ins (getpoint "\nSelect point to insert schedule: ") )

  ; OPERATION - Desactivar referencias
  (setvar "osmode" 0)

  ; OPERATION - Asignar al IL de salida (IL0) el menor de los IL disponible diferente de 0
    ; procesar valores igual a cero para que sean descomunales
    (if (= IL1 0) (setq IL1 9999.999))
    (if (= IL2 0) (setq IL2 9999.999))
    (if (= IL3 0) (setq IL3 9999.999))
    (if (= IL4 0) (setq IL4 9999.999))
    ; buscar el mínimo y asignarlo a IL0
    (setq IL0 (min IL1 IL2 IL3 IL4) )
    ; devolver  alos valores descomunales el valor 0
    (if (= IL1 9999.999) (setq IL1 0.0))
    (if (= IL2 9999.999) (setq IL2 0.0))
    (if (= IL3 9999.999) (setq IL3 0.0))
    (if (= IL4 9999.999) (setq IL4 0.0))
    ;convertir IL0 a string (modo Engineering (2) con 3 decimales)
    (setq txt_IL0 (rtos IL0 2 3))
    (princ "\nIL1 = ")(princ IL1)
    (princ "\nIL2 = ")(princ IL2)
    (princ "\nIL3 = ")(princ IL3)
    (princ "\nIL4 = ")(princ IL4)
    (princ "\nIL0 = ")(princ IL0)

    ; OPERATION - Create and/or change current layer to insert block
    (princ "\nLooking for e-manhole-schedule layer...")
    (if (/= (tblsearch "LAYER" "e-manhole-schedule") nil)
      (progn
        (princ " found.")
        (command "._layer" "S" "e-manhole-schedule" "C" "7" "" nil)
      ) ; END progn true
      (progn
        (princ " not found.")
        ; Create "e-manhole-schedule" layer
        (command "._-layer" "N" "e-manhole-schedule" "S" "e-manhole-schedule" "C" "7" "" nil)
        (princ "\n\"e-manhole-schedule\" layer created.")
      ) ; END progn false
    ) ; END if

  (princ "\n")(princ "p_ins = ")(princ p_ins)
  (princ "\n")(princ "ID = ")(princ ID)
  (princ "\n")(princ "CL = ")(princ CL)
  (princ "\n")(princ "txt_IL1 = ")(princ txt_IL1)
  (princ "\n")(princ "txt_IL2 = ")(princ txt_IL2)
  (princ "\n")(princ "txt_IL3 = ")(princ txt_IL3)
  (princ "\n")(princ "txt_IL4 = ")(princ txt_IL4)
  (princ "\n")(princ "txt_IL0 = ")(princ txt_IL0)
  (princ "\n")(princ "txt_E_coord = ")(princ txt_E_coord)
  (princ "\n")(princ "txt_N_coord = ")(princ txt_N_coord)
  ; OPERATION - Insertar el bloque
  (command "._insert" "ManScheduleBody" p_ins "1" "1" "0" ID CL "" "" "" txt_IL1 txt_IL2 txt_IL3 txt_IL4 txt_IL0 "" "" "" "" "" "" "" "" "" "" txt_E_coord txt_N_coord)

  ; OPERATION - Sincronizar ambos estados de visibilidad
    ; seleccionar ultimo objeto creado
    (setq last_ent (entlast))
    ; traducir su nombre VLA
    (setq VL_last_ent (vlax-ename->vla-object last_ent))
    ; cambiar el estado de visibilidad
    (LM:SetVisibilityState VL_last_ent vis_sta)

  ; OPERATION - Insert ManDetail block
  (setq
    p_ins2 (polar p_ins   0 121.5)
    p_ins2 (polar p_ins2 (* -0.5 pi) 8.25)
  )
  (command "._insert" "ManDetail" p_ins2 "1" "1" "0")

  ; OPERATION - Sync ManDetail block visibility state
  (setq
    last_ent (entlast)                            ; select last object's name
    VL_last_ent (vlax-ename->vla-object last_ent) ; get last object's VL name
  )
  (LM:SetVisibilityState VL_last_ent vis_sta)     ; change ManDetail block visibility state

  ; RESTORE PREVIOUS SETTINGS
  (setvar "clayer" oldlayer)
  (setvar "osmode" oldosmode)
  (setvar "cmdecho" oldcmdecho)
  (setvar "attdia" oldattdia)
  (setvar "attreq" oldattreq)

  ; End without double messages
  (princ)

  ; v0.1 - 2016.04.09 - Code tidy up and translation
  ;                   - Change and reset ATTDIA and ATTREQ system variables
  ; v0.0 - 2016.02.23
  ; Author: David Torralba
  ; Last revision: 2016.04.09
)
(defun c:MSCS (/ oldlayer oldosmode oldcmdecho)
  ; SET - Error handling function
  (defun *error* ( msg )
    (if (not (member msg '("Function cancelled" "quit / exit abort")))
      (princ (strcat "\nError: " msg))
    )
    ; Restore previous settings
    (setvar "clayer" oldlayer)
    (setvar "osmode" oldosmode)
    (setvar "cmdecho" oldcmdecho)
    (princ)
  )

  ; SAVE CURRENT SETTINGS - Current layer, OSMODE and CMDECHO
  (setq oldlayer (getvar "clayer")
        oldosmode (getvar "osmode")
        oldcmdecho (getvar "cmdecho")
  )

  ; (Re)load VLISP
  (vl-load-com)

  ; CHANGE INITIAL SETTINGS - "osmode" and "cmdecho"
  (setvar "osmode" 0)
  (setvar "cmdecho" 0)

  ; INPUT - Seleccionar el schedule
	(princ "\nLooking for existing Manhole Schedule blocks... ")
  (setq ss (ssget))
  (setq n (sslength ss))
  (setq i 0)
  (while (< i n)
    ; OPERATION - get the entity and entity name
    (setq ent_name (ssname ss i))

    ;convert to vl object
    (setq VL_ent_name (vlax-ename->vla-object ent_name))

    ; get visibility state
    (setq vis_sta (LM:getvisibilitystate VL_ent_name))

    ; Comprobar si el metodo 'getattributes es aplicable
    (if (and (vlax-method-applicable-p VL_ent_name 'getattributes))  ; Condition
      (progn                                                ; True. Se puede aplicar el método
        ; Extraer el CL y los ILs y pipe sizes (salen como texto), convertirlos a número (string to real)
        (setq
          txt_CL (get_block_att VL_ent_name "CL") CL (atof txt_CL)
          txt_IL0 (get_block_att VL_ent_name "IL0") IL0 (atof txt_IL0)
          txt_IL1 (get_block_att VL_ent_name "IL1") IL1 (atof txt_IL1)
          txt_IL2 (get_block_att VL_ent_name "IL2") IL2 (atof txt_IL2)
          txt_IL3 (get_block_att VL_ent_name "IL3") IL3 (atof txt_IL3)
          txt_IL4 (get_block_att VL_ent_name "IL4") IL4 (atof txt_IL4)
          txt_P0 (substr (get_block_att VL_ent_name "P0") 3 5) P0 (* 0.001 (atof txt_P0))
          txt_P1 (substr (get_block_att VL_ent_name "P1") 3 5) P1 (* 0.001 (atof txt_P1))
          txt_P2 (substr (get_block_att VL_ent_name "P2") 3 5) P2 (* 0.001 (atof txt_P2))
          txt_P3 (substr (get_block_att VL_ent_name "P3") 3 5) P3 (* 0.001 (atof txt_P3))
          txt_P4 (substr (get_block_att VL_ent_name "P4") 3 5) P4 (* 0.001 (atof txt_P4))
        )
      )
      (princ "\nThis object is not a manhole.")         ; False. No se puede aplicar el método
    ); END if

    ; OPERATION - Calcular IL + DN segun estado de visibilidad
    (setq
      top0 (+ IL0 P0)
      top1 (+ IL1 P1)
      top2 (+ IL2 P2)
      top3 (+ IL3 P3)
      top4 (+ IL4 P4)
    )

    ; OPERATION - Hallar en punto mas alto
    (cond
      ((or (= vis_sta "0") (= vis_sta "1")) (setq top_level (max top0 top1) ) )   ; 1 IL used
      ((= vis_sta "2") (setq top_level (max top0 top1 top2) ) )                   ; 2 IL used
      ((= vis_sta "3") (setq top_level (max top0 top1 top2 top3) ) )              ; 3 IL used
      ((= vis_sta "4") (setq top_level (max top0 top1 top2 top3 top4) ) )         ; 4 IL used
    )

    ; OPERATION - Hallar la tubería más grande
    (cond
      ((or (= vis_sta "0") (= vis_sta "1")) (setq biggest_pipe (max P0 P1) ) )    ; 1 IL used
      ((= vis_sta "2") (setq biggest_pipe (max P0 P1 P2) ) )                      ; 2 IL used
      ((= vis_sta "3") (setq biggest_pipe (max P0 P1 P2 P3) ) )                   ; 3 IL used
      ((= vis_sta "4") (setq biggest_pipe (max P0 P1 P2 P3 P4) ) )                ; 4 IL used
    )
    (setq txt_biggest_pipe (rtos biggest_pipe 2 3))
    ; OPERATION - Cambiar el attributo Chamber Size (CS) del bloque
    (cond                                                                                                                   ; Biggest pipe diameter = Dmax
      ((< biggest_pipe 0.375)                              (LM:vl-setattributevalue VL_ent_name "CS" "DN1200"))             ; Dmax < 375
      ((and (> biggest_pipe 0.375) (< biggest_pipe 0.700)) (LM:vl-setattributevalue VL_ent_name "CS" "DN1500"))             ; 375 < Dmax < 700
      ((and (> biggest_pipe 0.700) (< biggest_pipe 0.900)) (LM:vl-setattributevalue VL_ent_name "CS" "DN1800"))             ; 700 < Dmax < 900
      ((> biggest_pipe 0.9)                                (LM:vl-setattributevalue VL_ent_name "CS" "Consult Undertaker")) ; 900 < Dmax
    ); END cond

    ; OPERATION - Calcular DTS ("depth to soffit")
    (setq
      DTS (- CL top_level)
      txt_DTS (LM:rtos DTS 2 3)
    )

    ; OPERATION - Cambiar el attributo SOFFIT del bloque
    (LM:vl-setattributevalue VL_ent_name "SOFFIT" txt_DTS)

    ; OPERATION - Calcular el tipo de manhole y cambiar el attributo MT del bloque
    (cond
      ((and (<= 3 DTS)(<= DTS 6))
        (LM:vl-setattributevalue VL_ent_name "MT" "A")
        (princ "\nDTS = ")(princ DTS)(princ "  3-6m")
      )
      ((and (<= 1.5 DTS)(<= DTS 3))
        (LM:vl-setattributevalue VL_ent_name "MT" "B")
        (princ "\nDTS = ")(princ DTS)(princ "  1.5-3m")
      )
      ((and (<= 1 DTS)(<= DTS 1.5))
        (LM:vl-setattributevalue VL_ent_name "MT" "E")
        (princ "\nDTS = ")(princ DTS)(princ "  1-1.5m")
      )
      ((< DTS 1)
        (LM:vl-setattributevalue VL_ent_name "MT" "CHECK")
        (princ "\nDTS = ")(princ DTS)(princ " < 1m")
      )
      ((> DTS 6)
        (LM:vl-setattributevalue VL_ent_name "MT" "CHECK")
        (princ "\nDTS = ")(princ DTS)(princ " > 6m")
      )
    )

    ; OPERATION - Preparar contador para la siguiente vuelta
    (setq i (+ i 1))
  ) ; END while

    ; CLEAN VARIABLES
  (setq
    ss nil
    n nil
    i nil
    CL nil
    IL0 nil
    IL1 nil
    IL2 nil
    IL3 nil
    IL4 nil
    P0 nil
    P1 nil
    P2 nil
    P3 nil
    P4 nil
    txt_CL nil
    txt_IL0 nil
    txt_IL1 nil
    txt_IL2 nil
    txt_IL3 nil
    txt_IL4 nil
    txt_P0 nil
    txt_P1 nil
    txt_P2 nil
    txt_P3 nil
    txt_P4 nil
    ent nil
    ent_name nil
    VL_ent_name nil
    vis_sta nil
    top0 nil
    top1 nil
    top2 nil
    top3 nil
    top4 nil
    top_level nil
    DTS nil
    txt_DTS nil
  )

  ; RESTORE PREVIOUS SETTINGS
  (setvar "clayer" oldlayer)
  (setvar "osmode" oldosmode)
  (setvar "cmdecho" oldcmdecho)
  (setvar "attdia" oldattdia)
  (setvar "attreq" oldattreq)
  (princ)

  ; End without double messages
  (princ)
  ; v0.4 - 2016.04.10 - Fix depth to soffit value return bug.
  ; v0.3 - 2016.03.10 - Feature added: chamber size is now calculated based on pipe sizes
  ;                   - Feature added: cuando vas a elegir el manhole, si no seleccionas nada te avisa y te deja volver a intentarlo
  ;                   - Feature added: cuando vas a elegir el manhole, si seleccionas otra cosa que no es un bloque de manhole te avisa y te deja volver a intentarlo
  ; v0.2 - 2016.03.08 - Feature added: coordinates extraction and plot
  ; v0.1 - 2016.03.01 - DTS<1m and DTS>6m added (out of rank conditions)
  ; v0.0 - 2016.02.24
  ; Author: David Torralba
  ; Last revision: 2016.04.10
)
