; COMMAND: pipe
;
; CONTENT
; This LISP file contains a command based on 3 functions:
;   1. LM:Round (by Mac Lee): auxiliar function to round numbers properly
;   2. get_IL: auxiliar function to find and choose IL in manhole dynamic block
;   3. pipe: main function
;
; HOW DOES IT WORK?
; Once LISP file loaded, trigger the command typing "pipe"
; Select both manhole
; When selecting each manhole, if there are multiple invert levels, you will need to choose one
; Introduce pipe diameter in mm
; Pipe and its label (length + diameter + gradient) will be drawn for you
;
(defun LM:Round ( n ) (fix (+ n (if (minusp n) -0.5 0.5))))
(defun get_IL ( obj IL )
  (vl-some '(lambda ( att )
              (if (= IL (strcase (vla-get-tagstring att)))
                (vla-get-textstring att)
              ) ; END if
            )
          (vlax-invoke obj 'getattributes)
        )
)
(defun get_IL_manhole ( txt_msg )
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
  
  ; Comprobar si el metodo 'getattributes es aplicable
  (if (vlax-method-applicable-p VL_ent_name 'getattributes) ; Condition
    (progn                                                ; True. Se puede aplicar el método
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
      (princ (strcat "\nIL1=" txt_IL1 "   IL2=" txt_IL2 "   IL3=" txt_IL3 "   IL4=" txt_IL4 ))   ; línea única
      
      (cond
      ; CASE A - 1 IL disponible
        ((and (/= IL1 0) (= IL2 0) (= IL3 0) (= IL4 0))
          (setq IL IL1)
        ) ; END A - 1 IL disponible
    
        ; CASE B - 2 IL disponibles
        ((and (/= IL1 0) (/= IL2 0) (= IL3 0) (= IL4 0))
          (progn
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
        ((and (/= IL1 0) (/= IL2 0) (/= IL3 0) (= IL4 0))
          (progn
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
        ((and (/= IL1 0) (/= IL2 0) (/= IL3 0) (/= IL4 0))
          (progn
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
(defun c:pipe (/ capa MNH1_IL MNH1_capa p1 MNH2_IL MNH2_capa p2 dist txt_dist grad txt_grad ang ang_grados diam pl_wid txt_diam label pm dist_ins p_ins ancho_caja)
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
  (setq oldlayer (getvar "clayer"))
  (setq oldosmode (getvar "osmode"))
  (setq oldcmdecho (getvar "cmdecho"))
  
  ; CHANGE INITIAL SETTINGS - "osmode" and "cmdecho"
  (setvar "osmode" 0)
  (setvar "cmdecho" 0)
  
  ; INPUT - Seleccionar el manhole 1 (origen)
	(get_IL_manhole "\nSelect first manhole:")  ; Doy un valor a la variable "IL"
  (setq MNH1_IL IL)                           ; Guardo el valor de "IL"
  (setq MNH1_capa capa)                       ; Guardo el valor de "capa"
  (setq p1 point)                             ; Guardo el valor de "point"

  ; INPUT - Seleccionar el manhole 2 (destino)
	(get_IL_manhole "\nSelect second manhole:") ; Doy un valor a la variable "IL"
  (setq MNH2_IL IL)                           ; Guardo el valor de "IL"
  (setq MNH2_capa capa)                       ; Guardo el valor de "capa"
  (setq p2 point)                             ; Guardo el valor de "point"
  
  ; OPERATION - Abortar la rutina si los manholes están en capas diferentes
	(if (/= MNH1_capa MNH2_capa)                              ; If layers are different
    (progn
      (alert "Selected manholes are in different layer.")   ; give alert error message
      (exit)                                                ; and finish the routine
    )
  )
  
  ; OPERATION - Calcular distancia entre 1 y 2.
	(setq dist (distance p1 p2))

  ; OPERATION - Redondea la distancia a un decimal y conviértelo a texto (rtos ... 2 1) 
	(setq txt_dist (rtos dist 2 1))
  
  ; OPERATION - Calcular la pendiente (gradiente) en tanto por 1
	(if (= MNH1_IL MNH2_IL)                             ; If IL1=IL2
    (progn                                            ; True:
      (alert "Both manhole have same IL.")            ; give alert error message
      (exit)                                          ; and finish the routine
    )
    (setq grad (/ dist (abs (- MNH2_IL MNH1_IL))))    ; False: calculate difference between IL
  )

  ; OPERATION - Redondea la pendiente a un número entero (fix) y conviértelo a texto (itoa)
	(setq txt_grad (itoa (LM:Round grad)))
  
  ; OPERATION - Calcular el angulo entre 1 y 2 (para luego colocar y orientar el texto)
	(setq ang (angle p1 p2))
  
  ; OPERATION - Convertir en ángulo a grados sexagesimales
  (setq ang_grados (/ (* ang 180) pi))
  
  ; OPERATION - Corregir orientación del angulo para alinear correctamente el texto
  (if
    (or (<= ang_grados -90) (and (>= ang_grados 90) (<= ang_grados 270)))
    (setq ang_grados (+ ang_grados 180))
  )

  ; INPUT - Introducir el diámetro de tubería.
	(setq diam (getint "\nIntroduce pipe diameter in mm: "))
  
  ; OPERATION - Calcular el grosor de la línea según el diámetro de la tubería
	(setq pl_wid (/ (float diam) 1000)) ; primero convierto el integer a real y luego lo paso a metros
  
  ; OPERATION - Convierte el diámetro de la tubería a texto (itoa)
	(setq txt_diam (itoa diam))
  
  ; OPERATION - Unir las string de distancia, tipo (SWS/FWS), diámetro y pendiente.
	(setq label (strcat txt_dist "m DN" txt_diam " @ 1/" txt_grad))
  
  ; OPERATION - Cambiar de capa para dibujar tubería
	(setvar "CLAYER" capa)

  ; OPERATION - Dibujar tubería: polilínea del centro de 1 al centro de 2.
	(command "_.pline" p1 "_w" pl_wid pl_wid p2 "")
  
  ; OPERATION - Calcular el punto medio de la polilínea
  (setq pm (polar p1 ang (/ dist 2)))

  ; OPERATION - Calcular la la distancia para colocar correctamente el texto, e invertirla si corresponde por su orientación
  (setq dist_ins (+ (/ pl_wid 2) 0.15) )
  (if
    (or (<= ang (* -0.5 pi)) (and (>= ang (* 0.5 pi)) (<= ang (* 1.5 pi))))
    (setq dist_ins (- 0 dist_ins))
  )
	
  ; OPERATION - Calcular el punto de inserción del texto (separado un poco de la polilínea, teniendo en cuenta el grosor de la polilínea)
  (setq p_ins (polar pm (+ ang (/ pi 2)) dist_ins))
  
  ; OPERATION - Calcular el ancho del texto
  (setq ancho_caja (- dist 1.2))
  (if (< ancho_caja 1) (setq ancho_caja 2))
  
  ; OPERATION - Crear MText
	(command "-mtext" p_ins "R" (- 0 ang_grados) "H" "0.400" "J" "BC" "W" ancho_caja label "")
  
;  ; OPERATION - Calcular el nivel centro de la arqueta con el menor IL
;  (if (< MNH1_IL MNH2_IL)   ; If IL of MNH 1 is lower than IL of MNH2
;    (setq p_low p1)          ; True: MNH1 is lower
;    (setq p_low p2)          ; False: MNH2 is lower
;  )
;  
;  ; OPERATION - Insertar el bloque con la flecha del sentido de flujo ---------------------------------------------------------------------------------------- REVISAR
;  (command "._insert" "nombre_del_bloque" pm "1" "1" p_low)
;
;  ; OPERATION - Si la pendiente es inferior a 1/150, saca un (alert (strcat "Low pipe gradient: 1/") texto_pendiente )

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