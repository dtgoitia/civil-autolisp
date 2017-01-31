;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Shortcuts:                                                                ;;
;;    COORD: insert a single setting out block                                ;;
;;    COORDP: insert setting out blocks at every vertex of a Polyline         ;;
;;    RKS: reset setting out blocks scale                                     ;;
;;    SOU: update setting out blocks                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun DT:UpdateSettingOutLabel ( ent_name )
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
(defun c:COORD() (c:InsertSettingOutLabel))
(defun c:InsertSettingOutLabel ( / pt )
  ; Insert setting out label blocks one by one

  ; Get scale
  (if (= sf nil)
    (progn
      (setq sf (getreal "\nNominal Scale 1:<500>  1:"))
      (if (= sf nil) (setq sf 500.0))
      (setq sf (/ sf 571.428557))
    );END progn
  );END if

  ; Get precision
  (if (= cacc nil)
    (progn
      (setq cacc (getint "\nNumber of Decimal Places for display of Co-ordinates <3> : "))
      (if (= cacc nil) (setq cacc 3))
    );END progn
  );END if

  ; Get points and insert setting out label blocks
  (while (not pt)
    ; INPUT - Ask user to pick a point
    (if (setq pt (getpoint "\nSelect point to Co-ordinate (press Esc to exit):"))
      (progn
        (DT:InsertSettingOutLabel pt)
        (setq pt nil)
      );END progn
      (exit)
    );END if
  );END while

  (princ)

  ; v0.2 - 2017.01.31 - Code split up and DT:InsertSettingOutLabel implementation.
  ;                   - Function renamed to avoid conflicts, and shortcut kept to avoid transition problems.
  ; v0.1 - 2016.09.02 - Add loop to pick points.
  ;                   - *error* function update.
  ; v0.1 - 2016.06.27 - Code tidy up.
  ;                   - Merge code into a single file.
  ; v0.0 - 2016.03.03 - First issue.
  ; Author: David Torralba
  ; Last revision: 2017.01.31
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
        (DT:UpdateSettingOutLabel (cadr e))
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
(defun DT:InsertSettingOutLabel ( pt / old_error old_sysvars )
  ; Insert setting out label block in pt

  ; SAVE SETTINGS
  (save_environment (list "osmode" "cmdecho" "attreq" "attdia"))

  ; CHANGE SETTINGS
  (setvar "osmode" 0)
  (setvar "cmdecho" 0)
  (setvar "attreq" 1)
  (setvar "attdia" 0)

  (command
    "_insert" "XY_advanced" pt sf "" ""
    (strcat "E " (LM:rtos (nth 0 pt) 2 cacc))
    (strcat "N " (LM:rtos (nth 1 pt) 2 cacc))
  )

  ; RESTORE SETTINGS
  (restore_environment)

  ; v0.0 - 2017.01.31 - First issue.
  ; Author: David Torralba
  ; Last revision: 2017.01.31
)
(defun DT:GetPolylineVertexCoordinates ( ent_name / l )
  ; Return a list with polyline vertex 2D coordinates
  ; ent_name [ename] - Polyline to extract vertexes
  (if ent_name
    (if (= "LWPOLYLINE" (cdr (assoc 0 (entget ent_name))))
      (progn
        (foreach dxf (entget ent_name)
          (if (= 10 (car dxf))
            (setq l (append l (list (cdr dxf))))
          );END if
        );END foreach

        ; Return vertex coordinate list
        l
      );END progn
      nil
    );END if
    nil
  );END if

  ; v0.0 - 2017.01.31 - First issue.
  ; Author: David Torralba
  ; Last revision: 2017.01.31
)
