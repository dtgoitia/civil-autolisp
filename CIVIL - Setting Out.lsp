;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Shortcuts:                                                                ;;
;;    COORD: insert a single setting out block                                ;;
;;    COORDP: insert setting out blocks at every vertex of a Polyline         ;;
;;    RKS: reset setting out blocks scale                                     ;;
;;    SOU: update setting out blocks                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun update_settingout_label ( ent_name )
  (if (= (LM:effectivename (vlax-ename->vla-object ent_name)) "XY_advanced")
    (if (vlax-method-applicable-p (vlax-ename->vla-object ent_name) 'getattributes) ; Condition
      (progn
        ; OPERATION - Update block attributes with new coordinates
        (LM:vl-setattributevalue (vlax-ename->vla-object ent_name) "E" (strcat "E " (LM:rtos (car (cdr (assoc 10 (entget ent_name)))) 2 3)))
        (LM:vl-setattributevalue (vlax-ename->vla-object ent_name) "N" (strcat "N " (LM:rtos (cadr (cdr (assoc 10 (entget ent_name)))) 2 3)))
      ) ; END progn
    ) ; END if2
  );END if1
)
(defun c:COORD (
                /
                oldosmode oldcmdecho
                olay cs
                E N
                )
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

  (setq
    oldcmdecho (getvar "cmdecho")
    oldosmode (getvar "osmode")
  )

  (setvar "cmdecho" 0)

  (if (= sf nil)
    (progn
      (setq sf (getreal "\nNominal Scale   1:<500>  1:"))
      (if (= sf nil) (setq sf 500.0))
      (setq sf (/ sf 571.428557))
    );END progn
  );END if

  (if (= cacc nil)
    (progn
      (setq cacc (getint "\nNumber of Decimal Places for display of Co-ordinates <3> : "))
      (if (= cacc nil) (setq cacc 3))
    );END progn
  );END if

  (setq olay (getvar "clayer"))
  (setq cs (getpoint "\nSelect point to Co-ordinate :"))

  (if (/= cs nil)
    (progn
      (setq
        E (strcat "E " (LM:rtos (nth 0 CS) 2 cacc))
        N (strcat "N " (LM:rtos (nth 1 CS) 2 cacc))
      )
      (setvar "osmode" 0)
      (command "_insert" "XY_advanced" CS sf "" "" E N)
    );END progn
  );END if

  (setvar "osmode" oldosmode)
  (setvar "cmdecho" oldcmdecho)
  (princ)

  ; v0.1 - 2016.06.27 - Code tidy up.
  ;                   - Merge code into a single file.
  ; v0.0 - 2016.03.03 - First issue.
  ; Author: David Torralba
  ; Last revision: 2016.03.03
)
(defun c:COORDP (
                /
                oldosmode oldcmdecho
                olay
                p a b con
                X Y E N pt1
                )
  (setq
    oldosmode (getvar "osmode")
    oldcmdecho (getvar "cmdecho")
  )
  (setvar "cmdecho" 0)
  (if (= sf nil)
    (progn
      (setq sf (getreal "\nNominal Scale   1:<500>  1:"))
      (if (= sf nil) (setq sf 500.0))
      (setq sf (/ sf 571.428557))
    );END progn
  );END if

  (if (= cacc nil)
    (progn
      (setq cacc (getint "\nNumber of Decimal Places for display of Co-ordinates <3> : "))
      (if (= cacc nil) (setq cacc 3))
    );END progn
  );END if

  (setq olay (getvar "clayer"))
  (setq lay (getstring (strcat "\nLayer for co-ordinate boxes to be drawn on (current layer) <" olay "> : ")))
  (if (= lay "") (setq lay olay))
  (command "_layer" "_m" lay "")

  (setq p 0)
  (princ "\nPick Polyline  [1 Polyline ONLY] :")
  (setq a (ssget)
        b (ssname a 0)
        con (entget b)
  )
  (if (= (cdr (assoc 0 con)) "LWPOLYLINE")
    (progn
      (setq counter 0)
      (repeat (length con)
        (setq a (nth counter con))
        (if (equal (car a) 10)
          (progn
            (setq pt1 (cdr a)
              X (LM:rtos (nth 0 pt1) 2 3)
              Y (LM:rtos (nth 1 pt1) 2 3)
              E  (strcat "E " (LM:rtos (nth 0 pt1) 2 cacc))
              N  (strcat "N " (LM:rtos (nth 1 pt1) 2 cacc))
            )
            (setvar "osmode" 0)
            (command "_insert" "XY_advanced" pt1 sf "" "" E N)
          )
        )
        (setq counter (+ counter 1))
      )
    )
  )
  (if (= (cdr (assoc 0 con)) "POLYLINE")
    (progn
      (setq b (entnext (cdr (assoc -1 con))))
      (while  (/= (cdr (assoc 0 (entget b))) "SEQEND")
        (setq
          con (entget b)
          pt1 (cdr (assoc 10 con))
          b (entnext (cdr (assoc -1 con)))
          X (LM:rtos (nth 0 pt1) 2 3)
          Y (LM:rtos (nth 1 pt1) 2 3)
          E  (strcat "E " (LM:rtos (nth 0 pt1) 2 cacc))
          N  (strcat "N " (LM:rtos (nth 1 pt1) 2 cacc))
        );END setq
        (setvar "osmode" 0)
        (command "_insert" "XY_advanced" pt1 sf "" "" E N)
      )
    )
  )

  (setvar "osmode" oldosmode)
  (setvar "cmdecho" oldcmdecho)
  (princ)

  ; v0.1 - 2016.06.27 - Code tidy up.
  ;                   - Merge code into a single file.
  ; v0.0 - 2016.03.03 - First issue.
  ; Author: David Torralba
  ; Last revision: 2016.06.27
)
(defun c:SOU (/ ans ss i)
  ; INPUT - Ask user if he wants to select blocks manually or select all in the drawing
  (initget "All Manually")
  (setq ans (getkword "\nChoose selection mode [All/Manually] <All>: "))

  ; OPERATION - Select blocks according to previous step
  (if (or (not ans) (= ans "All"))
    (setq ss (ssget "x" '((0 . "INSERT"))))
    (setq ss (ssget '((0 . "INSERT"))))
  )
  (setq i 0 )
  (foreach e (ssnamex ss)
    (if (>= (car e) 0)
      (progn
        (setq i (+ i 1) )
        (update_settingout_label (cadr e))
      );END progn
    );END if
  );END foreach
  (princ (strcat "\n" (itoa i) " setting out blocks processed."))
  (princ)
  ; v0.1 - 2016.06.27 - Prompt summary at routine end.
  ;                   - Change while command for a foreach command.
  ; v0.1 - 2016.03.04 - Minor bug fix.
  ; v0.0 - 2016.03.03 - First issue.
  ; Author: David Torralba
  ; Last revision: 2016.06.27
)
(defun c:RKS ()
  ;Reset KTF Scale
  (setq sf nil)
  (princ "\nKTF scale removed from memory.")
  (princ)
  ; v0.1 - 2016.06.27 - Code tidy up.
  ;                   - Merge code into a single file.
  ; v0.0 - 2016.03.03 - First issue.
  ; Author: David Torralba
  ; Last revision: 2016.06.27
)
