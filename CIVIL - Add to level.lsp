(defun C:ATL ( / *error* )
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

  ; INPUT - Calcular incremento
  (if (= prev_increment nil) (setq prev_increment 0.0)) ; Si no se ha dado un valor anterior al incremento, que sea cero
  (setq msg (strcat "\nIntroduce the increment <"  (rtos prev_increment 2 3) ">: " ))  ; mensaje para el siguiente comando
  (setq increment (getreal msg)) ;se ofrece la posibilidad de cambiarlo
  (if (= increment nil)
    (setq increment prev_increment)  ; Si se da al espacio o enter y no se cambia
    (setq prev_increment increment)  ; Si se modifica, se almacena
  )


  ; INPUT - Seleccionar objeto del que extraer el level, obtener su nombre y lueg su lista de propiedades
  (setq entList (entget (car (entsel "Select level text:"))))

  ; OPERATION - Extraer la lista del objeto, de ahi su lista con indice 1 (que es "Content") y de ahí el texto en sí
  (setq txt_level (cdr (assoc 1 entList) ) )

  ; OPERATION - Cut the extracted text
  (setq txt_level (substr txt_level 4))

  ; OPERATION - Convert the cutted text, to be a real number
  (setq level (atof txt_level))

  ; OPERATION - Recalculate the level
  (setq level (+ level increment) )

  ; OPERATION - Convert new level to string with underline code before
  (setq txt_level (strcat "%%U" (rtos level 2 3) ))

  ; OPERATION - Modify the object
  (setq entList (subst (cons 1 txt_level)(assoc 1 entList) entList))
  (entmod entList)

    ; RESET to original "osmode" and "cmdecho".
  (setvar "osmode" oldosmode)
  (setvar "cmdecho" oldcmdecho)

  ; End without double messages
  (princ)

  ; Author: David Torralba
  ; Last revision: 2016.03.01
)
