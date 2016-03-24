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
(defun update_settingout_label ( entidad )
  (vl-load-com)
  ; OPERATION - get the entity name and translate it to VLA
  (setq VL_ent_name (vlax-ename->vla-object entidad))
  (if (vlax-method-applicable-p VL_ent_name 'getattributes) ; Condition
    (progn
      ; OPERATION - Extrer las coordenadas de la etiqueta
      (setq p_ins (cdr (assoc 10 (entget entidad))))
      (setq
        E_coord (car p_ins)  txt_E_coord (strcat "E " (rtos E_coord 2 3))
        N_coord (cadr p_ins) txt_N_coord (strcat "N " (rtos N_coord 2 3))
      )
      
      ; OPERATION - Update block attributes with new coordinates
      (LM:vl-setattributevalue VL_ent_name "E" txt_E_coord)
      (LM:vl-setattributevalue VL_ent_name "N" txt_N_coord)
    ) ; END progn
  ) ; END if
)
(defun c:sou (/ sset ent i)
  ; Settting Out Update
  
  ; INPUT - Select objects
  ;(setq sset (ssget "\nSelect setting out labels to  update: ") )
  (setq sset (ssget '((-4 . "<OR")(0 . "INSERT")
                      (0 . "BLOCK")(-4 . "OR>")))
  )
  (setq i 0) ; Contador while
  (while (< i (sslength sset))
    ; OPERATION - Get entity name
    (setq ent (ssname sset i) )
    
    ; OPERATION - Actualizar las etiquetas
    (update_settingout_label ent)
    
    ; OPERATION - Next loop
    (setq i (+ i 1))
    
  )
  
  (princ)
  ;(setvar "osmode" old_osmode)
  ;(setvar "cmdecho" old_cmdecho)
  
  ; PENDIENTE - Actualizar multiples labels a la vez, filtrando que sean bloques: (0 . "INSERT") ---- hecho
  ; PENDIENTE - Introducir un IF que compruebe si el objeto tiene los atributos "E" y "N", por si se ha seleccionado un bloque que no era.
  ;             No lo hago todo de una pasada, por eficiencia. Si tengo hacer un bucle y comprobar una a una si todas las entidades que no son
  ;             bloques tienen los atributos "E" y "N", tardaría la vida. Solo compruebo los que son bloques.
  
  ; v0.1 - 2016.03.04 - Coreción de errores menores.
  ; v0.0 - 2016.03.03
  ; Author: David Torralba
  ; Last revision: 2016.03.04
)