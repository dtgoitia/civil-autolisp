(defun C:RDL ( / *error* )
  ; RoaD Levels
  
  ; SET - Error handling function
  (defun *error* ( msg )
    (if (not (member msg '("Function cancelled" "quit / exit abort")))
      (princ (strcat "\nError: " msg))
    )
    ; RESET to original "osmode" and "cmdecho".
    (setvar "osmode" oldosmode)
    (setvar "cmdecho" oldcmdecho)
    (princ)
  )
  
  ; SAVE "osmode" and "cmdecho"
  (setq oldosmode (getvar "osmode"))
  (setq oldcmdecho (getvar "cmdecho"))
  
  ; CHANGE "osmode" and "cmdecho"
  (setvar "osmode" 0)
  (setvar "cmdecho" 0)
  
  ; INPUT - Pedir incremento
  (if (= prev_increment nil) (setq prev_increment 0)) ; Si no se ha dado un valor anterior al incremento, que sea cero
  (setq msg (strcat "\nIntroduce the increment in mm <"  (itoa prev_increment) ">: " ))  ; mensaje para el siguiente comando
  (setq increment (getint msg)) ;se ofrece la posibilidad de cambiarlo
  (if (= increment nil)
    (setq increment prev_increment)  ; Si se da al espacio o enter y no se cambia
    (setq prev_increment increment)  ; Si se modifica, se almacena
  ); END if
  
  ; LOOP infinito
  (setq a nil)
  (while (not a)
    ; INPUT - Seleccionar objeto del que extraer el level, obtener su nombre y lueg su lista de propiedades
    (setq ent nil)
    (while (= ent nil)
      (setq ent (entsel "\nSelect reference text level (or press Escape to exit):"))
      (cond
        ((not ent)
          (princ " nothing selected.")
        ); END cond 1
        ((/= (cdr (assoc 0 (entget (car ent)))) "TEXT")
          (princ " selected object is not a text.")
          (setq ent nil)
        ); END cond 2
        (t
          (princ " text object selected.")
        ); END cond else
      ); END cond
    ); END while
    (setq entList (entget (car ent)))
    
    ; OPERATION - Extraer la lista del objeto, de ahi su lista con indice 1 (que es "Content") y de ahí el texto en sí
    (setq txt_level (cdr (assoc 1 entList) ) )
    
    ; OPERATION - Check if its an underlined text or an FFL
    (cond
      ((= (substr txt_level 1 3) "FFL")
        ; OPERATION - Cut the extracted text
        (setq txt_level (substr txt_level 5))
        ; OPERATION - Convert the cutted text, to be a real number
        (setq level (atof txt_level))
      )
      ((= (substr txt_level 1 3) "%%U")
        ; OPERATION - Cut the extracted text
        (setq txt_level (substr txt_level 4))
        ; OPERATION - Convert the cutted text, to be a real number
        (setq level (atof txt_level))
      )
    ) ; END cond
    
    
    (princ (strcat "\n" txt_level "m to ") )
    ; OPERATION - Recalculate the level
    (setq level (+ level (* 0.001 increment)) )
    (princ (strcat (rtos level 2 3) "m\n" ))
    
    ; OPERATION - Convert new level to string with underline code before
    (setq txt_level (strcat "%%U" (rtos level 2 3) ))
    
    ; INPUT - Select object to modify
    (setq destiny_ent nil)
    (while (= destiny_ent nil)
      (setq destiny_ent (entsel "\nSelect text to modify (or press Escape to exit):"))
      (cond
        ((not destiny_ent)
          (princ " nothing selected.")
        ); END cond 1
        ((/= (cdr (assoc 0 (entget (car destiny_ent)))) "TEXT")
          (princ " selected object is not a text.")
          (setq destiny_ent nil)
        ); END cond 2
        (t
          (princ " text object selected.")
        ); END cond else
      ); END cond
    ); END while
    (setq destiny_entList (entget (car destiny_ent)))
    
    ; OPERATION - Modify the object
    (setq destiny_entList (subst (cons 1 txt_level)(assoc 1 destiny_entList) destiny_entList))
    (entmod destiny_entList)
    
    ; OPERATION - Set object properties ByLayer
    (command "SETBYLAYER" destiny_ent "" "" "")
  ); END while
  
    ; RESET to original "osmode" and "cmdecho".
  (setvar "osmode" oldosmode)
  (setvar "cmdecho" oldcmdecho)
    
  ; End without double messages
  (princ)
  
  ; v0.0 - 2016.03.09 - First issue
  ; Author: David Torralba
  ; Last revision: 2016.03.09
)