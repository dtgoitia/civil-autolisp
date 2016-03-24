(defun C:datumkick ( /
                      *error*
                      txt_level level
                      diff
                      datum new_datum exag new_exag
                      ans
                      ent entList ss
                    )
  ; Moves object upward the difference between datum and selected text taking vertical exageration in account

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

  (setq
    datum 50  ; OPERATION - Set datum
    exag 10   ; OPERATION - Set exageration
  )

  ; INPUT - Change datum and exageration
  (initget "Yes No")
  (setq ans (getkword (strcat "\nUpdate datum (" (rtos datum 2 3) "m) or exageration (H1:V" (itoa exag) ") [Yes/No] <No>: ")))
  (if (not ans) (setq ans "No"))

  (if (= ans "Yes")
    (progn
      ; INPUT - New datum
      (setq new_datum (getreal (strcat "\nIntroduce datum level <" (rtos datum 2 3) ">: ")))
      (if (= new_datum nil) (setq new_datum datum) (setq datum new_datum))

      ; INPUT - New exageration
      (setq new_exag (getreal (strcat "\nIntroduce exageration <" (rtos exag 2 3) ">: ")))
      (if (= new_exag nil) (setq new_exag exag) (setq exag new_exag))
    ) ; END progn
  ) ; END if

  ; INPUT - Select object to extrar level from
  (setq ent nil)
  (while (= ent nil)
    (setq ent (entsel "\nSelect text to extract level:"))
    (princ "\nNo entity selected. Please, try again. ")
  )

  (setq
    entList (entget (car ent))              ; OPERATION - Extrar objetc property list
    txt_level (cdr (assoc 1 entList))       ; OPERATION - Extract "Content" property
    level (atof txt_level)                  ; OPERATION - Convert extracted text to real number
    diff (- level datum)                    ; OPERATION - Calculate difference
    ss (entsel "\nSelect object to copy: ") ; INPUT - Select object to kick up
  )

  ; OPERATION - Princ level, datum and difference
  (princ
    (strcat
      "\n     Level = " (rtos level 2 3) "m"
      "\n     Datum = " (rtos datum 2 3) "m"
      "\nDifference = " (rtos diff  2 3) "m"
    ); END strcat
  ); END princ

  ; OPERATION - Aplicar la exageracion
  (setq diff (* diff exag))

  ; OPERATION - Mover el objeto
  (command ".copy" ss ""  (list 0 0) (list 0 diff))

    ; RESET to original "osmode" and "cmdecho".
  (setvar "osmode" oldosmode)
  (setvar "cmdecho" oldcmdecho)

  ; End without double messages
  (princ)

  ; v0.2 - 2016.03.21 - Optimize code and translate into English
  ; v0.1 - 2016.03.09 - Remember last datum and exageration introduced (if any)
  ;                   - Allow to change default datum and exageration at the beginning.
  ; v0.0 - 2016.02.15 - First issue.
  ; Author: David Torralba
  ; Last revision: 2016.02.21
)
