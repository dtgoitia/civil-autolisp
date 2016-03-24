(defun c:coord ()
  (setq oldcmdecho (getvar "cmdecho"))
  (setq oldosmode (getvar "osmode"))
  
  ; SET - Error handling function
  (defun *error* ( msg )
    (if (not (member msg '("Function cancelled" "quit / exit abort")))
      (princ (strcat "\nError: " msg))
    )
    ; Restore previous settings
    (setvar "osmode" oldosmode)
    (setvar "cmdecho" oldcmdecho)
    (princ)
  )
  (setvar "cmdecho" 0)
  (setvar "osmode" 37)
  
  (if (= sf nil) (progn
    (setq sf (getreal "\nNominal Scale   1:<500>  1:"))
    (if (= sf nil) (setq sf 500.0))
    (setq sf (/ sf 571.428557))
  ))
  (if (= cacc nil) (progn
    (setq cacc (getint "\nNumber of Decimal Places for display of Co-ordinates <3> : "))
    (if (= cacc nil) (setq cacc 3))
  ))

  (setq olay (getvar "clayer"))
  ;(setq lay (getstring (strcat "\nLayer for co-ordinate box to be drawn on (current layer) <" olay "> : ")))
  ;(if (= lay "") (setq lay olay))
  ;(command "_layer" "_m" lay "")

  (setq cs (getpoint "\nSelect point to Co-ordinate :"))
  (if (/= cs nil)
    (progn
      (setq E (STRCAT "E " (RTOS (NTH 0 CS) 2 cacc))
            N (STRCAT "N " (RTOS (NTH 1 CS) 2 cacc))
      )
      (setvar "osmode" 0)
      (command "_insert" "XY_advanced" CS sf "" "" E N)
    )
    
  )
  (princ)
  
  
  (setvar "osmode" oldosmode)
  (setvar "cmdecho" oldcmdecho)
  
  ; v0.0 - 2016.03.03
  ; Author: David Torralba
  ; Last revision: 2016.03.03
)