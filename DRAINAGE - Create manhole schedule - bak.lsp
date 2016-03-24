(defun LM:vl-setattributevalue ( blk tag val )
;; Set Attribute Value  -  Lee Mac
;; Sets the value of the first attribute with the given tag found within the block, if present.
;; blk - [vla] VLA Block Reference Object
;; tag - [str] Attribute TagString
;; val - [str] Attribute Value
;; Returns: [str] Attribute value if successful, else nil.
    (setq tag (strcase tag))
    (vl-some
       '(lambda ( att )
            (if (= tag (strcase (vla-get-tagstring att)))
                (progn (vla-put-textstring att val) val)
            )
        )
        (vlax-invoke blk 'getattributes)
    )
)
(defun LM:getdynpropvalue ( blk prp )
;; Get Dynamic Block Property Value  -  Lee Mac
;; Returns the value of a Dynamic Block property (if present)
;; blk - [vla] VLA Dynamic Block Reference object
;; prp - [str] Dynamic Block property name (case-insensitive)
    (setq prp (strcase prp))
    (vl-some '(lambda ( x ) (if (= prp (strcase (vla-get-propertyname x))) (vlax-get x 'value)))
        (vlax-invoke blk 'getdynamicblockproperties)
    )
)
(defun LM:getdynpropallowedvalues ( blk prp )
;; Get Dynamic Block Property Allowed Values  -  Lee Mac
;; Returns the allowed values for a specific Dynamic Block property.
;; blk - [vla] VLA Dynamic Block Reference object
;; prp - [str] Dynamic Block property name (case-insensitive)
;; Returns: [lst] List of allowed values for property, else nil if no restrictions
    (setq prp (strcase prp))
    (vl-some '(lambda ( x ) (if (= prp (strcase (vla-get-propertyname x))) (vlax-get x 'allowedvalues)))
        (vlax-invoke blk 'getdynamicblockproperties)
    )
)
(defun LM:setdynpropvalue ( blk prp val )
;; Set Dynamic Block Property Value  -  Lee Mac
;; Modifies the value of a Dynamic Block property (if present)
;; blk - [vla] VLA Dynamic Block Reference object
;; prp - [str] Dynamic Block property name (case-insensitive)
;; val - [any] New value for property
;; Returns: [any] New value if successful, else nil
    (setq prp (strcase prp))
    (vl-some
       '(lambda ( x )
            (if (= prp (strcase (vla-get-propertyname x)))
                (progn
                    (vla-put-value x (vlax-make-variant val (vlax-variant-type (vla-get-value x))))
                    (cond (val) (t))
                )
            )
        )
        (vlax-invoke blk 'getdynamicblockproperties)
    )
)
(defun LM:getvisibilityparametername ( blk / vis )  
;; Get Visibility Parameter Name  -  Lee Mac
;; Returns the name of the Visibility Parameter of a Dynamic Block (if present)
;; blk - [vla] VLA Dynamic Block Reference object
;; Returns: [str] Name of Visibility Parameter, else nil
    (if
        (and
            (vlax-property-available-p blk 'effectivename)
            (setq blk
                (vla-item
                    (vla-get-blocks (vla-get-document blk))
                    (vla-get-effectivename blk)
                )
            )
            (= :vlax-true (vla-get-isdynamicblock blk))
            (= :vlax-true (vla-get-hasextensiondictionary blk))
            (setq vis
                (vl-some
                   '(lambda ( pair )
                        (if
                            (and
                                (= 360 (car pair))
                                (= "BLOCKVISIBILITYPARAMETER" (cdr (assoc 0 (entget (cdr pair)))))
                            )
                            (cdr pair)
                        )
                    )
                    (dictsearch
                        (vlax-vla-object->ename (vla-getextensiondictionary blk))
                        "ACAD_ENHANCEDBLOCK"
                    )
                )
            )
        )
        (cdr (assoc 301 (entget vis)))
    )
)
(defun LM:getvisibilitystate ( blk )
;; Get Dynamic Block Visibility State  -  Lee Mac
;; Returns the value of the Visibility Parameter of a Dynamic Block (if present)
;; blk - [vla] VLA Dynamic Block Reference object
;; Returns: [str] Value of Visibility Parameter, else nil
    (LM:getdynpropvalue blk (LM:getvisibilityparametername blk))
)
(defun LM:SetVisibilityState ( blk val / vis )
;; Set Dynamic Block Visibility State  -  Lee Mac
;; Sets the Visibility Parameter of a Dynamic Block (if present) to a specific value (if allowed)
;; blk - [vla] VLA Dynamic Block Reference object
;; val - [str] Visibility State Parameter value
;; Returns: [str] New value of Visibility Parameter, else nil
    (if
        (and
            (setq vis (LM:getvisibilityparametername blk))
            (member (strcase val) (mapcar 'strcase (LM:getdynpropallowedvalues blk vis)))
        )
        (LM:setdynpropvalue blk vis val)
    )
)
(defun get_block_att ( obj IL )
  (vl-some '(lambda ( att )
              (if (= IL (strcase (vla-get-tagstring att)))
                (vla-get-textstring att)
              ) ; END if
            )
          (vlax-invoke obj 'getattributes)
        )
)
(defun get_manhole_att ( txt_msg )
  ; (Re)load VLISP
  (vl-load-com)
  
  ; INPUT - Select object
  (setq ent (entsel txt_msg))
    
  ; OPERATION - get the entity and entity name
	(setq ent_name (car ent))
  
	;convert to vl object
	(setq VL_ent_name (vlax-ename->vla-object ent_name))
  
  ; get visibility state
  (setq vis_sta (LM:getvisibilitystate VL_ent_name))
  
  ; Comprobar si el metodo 'getattributes es aplicable
  (if (vlax-method-applicable-p VL_ent_name 'getattributes) ; Condition
    (progn                                                ; True. Se puede aplicar el método
      ; Extraer el ID y el CL
      (setq ID (get_block_att VL_ent_name "ID")
            CL (get_block_att VL_ent_name "CL")
      )
      
      ; Extraer los ILs (salen como texto), convertirlos a número (string to real)
      ; y convertirlos a string otra vez bien formateados con 3 decimales
      (setq txt_IL1 (get_block_att VL_ent_name "IL1") IL1 (atof txt_IL1) txt_IL1 (rtos IL1 2 3)
            txt_IL2 (get_block_att VL_ent_name "IL2") IL2 (atof txt_IL2) txt_IL2 (rtos IL2 2 3)
            txt_IL3 (get_block_att VL_ent_name "IL3") IL3 (atof txt_IL3) txt_IL3 (rtos IL3 2 3)
            txt_IL4 (get_block_att VL_ent_name "IL4") IL4 (atof txt_IL4) txt_IL4 (rtos IL4 2 3)
      )
      
      ; Imprimir los recogido:
      (princ (strcat "\nID = " ID "\nCL = " CL "\nIL1 = " txt_IL1 "\nIL2 = " txt_IL2 "\nIL3 = " txt_IL3 "\nIL4 = " txt_IL4 ))   ; multilinea
      ;(princ (strcat "\nIL1=" txt_IL1 "   IL2=" txt_IL2 "   IL3=" txt_IL3 "   IL4=" txt_IL4 ))   ; línea única
    )
    (princ "\nThis object is not a manhole.")         ; False. No se puede aplicar el método
  ); END if
  (princ)
)
(defun c:MSC (/ oldlayer oldosmode oldcmdecho p_ins p_ins2 IL0 IL1 IL2 IL3 IL4 txt_IL0 txt_IL1 txt_IL2 txt_IL3 txt_IL4 ID CL)
   
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
  
  ; CHANGE INITIAL SETTINGS - "osmode" and "cmdecho"
  (setvar "osmode" 0)
  (setvar "cmdecho" 0)
  
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
  
  ; OPERATION - Crear y/o cambiar capa para insertar el bloque ------------------------------------------------------------------- CONTINUAR
    ; Comprobar que la capa e-manhole-schedule existe
    (princ "\nBuscando capa e-manhole-schedule...")
    (if (/= (tblsearch "LAYER" "e-manhole-schedule") nil)
      (progn  ; START TRUE si encuentra la capa e-manhole-schedule
        (princ " encontrada.")
        (command "._layer" "S" "e-manhole-schedule" "C" "7" "" nil)
      ) ; END TRUE si encuentra la capa e-manhole-schedule
      (progn  ; START FALSE si NO encuentra la capa e-manhole-schedule
        (princ " no encontrada.")
        ; Crear capa e-manhole-schedule
        (command "._-layer" "N" "e-manhole-schedule" "S" "e-manhole-schedule" "C" "7" "" nil)
        (princ "\nCapa e-manhole-schedule creada.")
      ) ; END FALSE si NO encuentra la capa e-manhole-schedule
    ) ;END if
  
  ; OPERATION - Insertar el bloque
  (command "._insert" "ManScheduleBody" p_ins "1" "1" "0" ID CL "" "" "" txt_IL1 txt_IL2 txt_IL3 txt_IL4 txt_IL0 "" "" "" "" "" "" "" "" "" "" )
  
  ; OPERATION - Sincronizar ambos estados de visibilidad
    ; seleccionar ultimo objeto creado
    (setq last_ent (entlast))
    ; traducir su nombre VLA
    (setq VL_last_ent (vlax-ename->vla-object last_ent))
    ; cambiar el estado de visibilidad
    (LM:SetVisibilityState VL_last_ent vis_sta)
  
  ; OPERATION - Insertar el bloque del esquema
  (setq p_ins2 (polar p_ins 0 121.5) ) 
  (setq p_ins2 (polar p_ins2 (* -0.5 pi) 8.25) ) 
  (command "._insert" "ManDetail" p_ins2 "1" "1" "0")
  
  ; OPERATION - Sincronizar el estado de visibilidad del bloque del esquema
    ; seleccionar ultimo objeto creado
    (setq last_ent (entlast))
    ; traducir su nombre VLA
    (setq VL_last_ent (vlax-ename->vla-object last_ent))
    ; cambiar el estado de visibilidad
    (LM:SetVisibilityState VL_last_ent vis_sta)
  
  ; CLEAN VARIABLES
  (setq
    IL0 nil
    IL1 nil
    IL2 nil
    IL3 nil
    IL4 nil
    txt_IL0 nil
    txt_IL1 nil
    txt_IL2 nil
    txt_IL3 nil
    txt_IL4 nil
    ID nil
    CL nil
    p_ins nil
    p_ins2 nil
    last_ent nil
    ent nil
    ent_name nil
    VL_ent_name nil
    VL_last_ent nil
    vis_sta nil
  )
  
  ; RESTORE PREVIOUS SETTINGS
  (setvar "clayer" oldlayer)
  (setvar "osmode" oldosmode)
  (setvar "cmdecho" oldcmdecho)
  
  ; End without double messages
  (princ)
  
  ; v0.0 - 2016.02.23
  ; Author: David Torralba
  ; Last revision: 2016.02.23
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
	;(setq ent (entsel "\nSelect a schedule block: "))
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
        ;(if (get_block_att VL_ent_name "CL") ((setq txt_CL (get_block_att VL_ent_name "CL") CL (atof txt_CL))))-------------------------------------- CONTINUAR AQUI
        ; Imprimir los recogido:
  ;      (princ (strcat
  ;                    "\nCL = " txt_CL
  ;                    "\nIL0 = " txt_IL0
  ;                    "\nIL1 = " txt_IL1
  ;                    "\nIL2 = " txt_IL2
  ;                    "\nIL3 = " txt_IL3
  ;                    "\nIL4 = " txt_IL4
  ;                    "\nP0 = " txt_P0
  ;                    "\nP1 = " txt_P1
  ;                    "\nP2 = " txt_P2
  ;                    "\nP3 = " txt_P3
  ;                    "\nP4 = " txt_P4
  ;             )
  ;      )
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
      ((= vis_sta "1") (setq top_level (max top0 top1) ) )                  ; 1 IL used
      ((= vis_sta "2") (setq top_level (max top0 top1 top2) ) )             ; 2 IL used
      ((= vis_sta "3") (setq top_level (max top0 top1 top2 top3) ) )        ; 3 IL used
      ((= vis_sta "4") (setq top_level (max top0 top1 top2 top3 top4) ) )   ; 4 IL used
    )
    
    
    ; OPERATION - Calcular DTS ("depth to soffit")
    (setq
      DTS (- CL top_level)
      txt_DTS (rtos DTS 2 3)
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
        (LM:vl-setattributevalue VL_ent_name "MT" "??")
        (princ "\nDTS = ")(princ DTS)(princ " < 1m")
      )
      ((> DTS 6)
        (LM:vl-setattributevalue VL_ent_name "MT" "??")
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
  
  ; End without double messages
  (princ)
  
  ; v0.1 - 2016.03.01 - DTS<1m and DTS>6m added (out of rank conditions)
  ; v0.0 - 2016.02.24
  ; Author: David Torralba
  ; Last revision: 2016.03.01
)