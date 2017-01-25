;|
(dt:int (getpoint) (getpoint) (getpoint))
(defun DT:int ( pA pB pC )
  ; Returns a 3D point with the interpolated value
  ; pA [pt] - 3D point of reference
  ; pB [pt] - 3D point of reference
  ; pC [pt] - 3D/2D point where to calculate the level
  (if (and pA pB pC)
    (progn ; True: 3 no-nil arguments passed
      (if (and (= 'LIST (type pA)) (= 'LIST (type pB)) (= 'LIST (type pC)) )
        (progn ; True: 3 list-type arguments passed
          (if (and (= 3 (length pA)) (= 3 (length pB)) (= 3 (length pC)) )
            (progn ; True: 3 point-type arguments passed
              (princ "\nUnder construction...")
              (princ)
            );END progn
            (cond
              ((/= 3 (length pA)) (princ "\nERROR @ DT:int : pA is not a point")(princ) )
              ((/= 3 (length pB)) (princ "\nERROR @ DT:int : pB is not a point")(princ) )
              ((/= 3 (length pC)) (princ "\nERROR @ DT:int : pC is not a point")(princ) )
            );END cond
          );END if
        );END progn
        (cond
          ((/= 'LIST (type pA)) (princ "\nERROR @ DT:int : pA is not a list")(princ) )
          ((/= 'LIST (type pB)) (princ "\nERROR @ DT:int : pB is not a list")(princ) )
          ((/= 'LIST (type pC)) (princ "\nERROR @ DT:int : pC is not a list")(princ) )
        );END cond
      );END if
    );END progn
    (cond
      ((not pA) (princ "\nERROR @ DT:int : pA=nil")(princ) )
      ((not pB) (princ "\nERROR @ DT:int : pB=nil")(princ) )
      ((not pC) (princ "\nERROR @ DT:int : pC=nil")(princ) )
    );END cond
  );END if
)
|;
(defun c:SDIPforPrivateSewers( / z1 p1_2D p1 p2_2D p2 dist ans)
  ; Get level with fixed gradient between 2 points,
  ; write the level rounded to 2 decimals,
  ; and formated according to target text (foul/storm)
	(setq
    z1 (DT:clic_or_type_level)
    p1_2D (getpoint (strcat "\nStart level = " (LM:rtos z1 2 3) "m\npoint 1: "))
    p1 (list (nth 0 p1_2D) (nth 1 p1_2D) z1)
    p2_2D (getpoint (strcat "\nStart level = " (LM:rtos z1 2 3) "m\npoint 1: OK\npoint 2: "))
  )
  (if SDIPgradient
    (progn
      (setq ans (getreal (strcat "\nStart level = " (LM:rtos z1 2 3) "m\npoint 1: OK\npoint 2: OK" "\nGradient= 1/<" (LM:rtos SDIPgradient 2 0) ">: ") ) )
      (if ans (setq SDIPgradient ans));END if
    );END progn
    (while (not SDIPgradient)
      (setq SDIPgradient (getreal (strcat "\nStart level = " (LM:rtos z1 2 3) "m\npoint 1: OK\npoint 2: OK\nGradient= 1/")))
    );END while
  );END if
  (setq
    p2 (DT:SDIP p1 p2_2D SDIPgradient)
  )
  (princ (strcat "\nUsed gradient: 1/" (LM:rtos SDIPgradient 2 0) ))
  (setq
    targetPoint (getpoint (strcat "\nLevel = " (LM:rtos (nth 2 p2) 2 2) "m\nSelect target: "))
  )
  (cond
    ( (and
        (nentselp targetPoint)
        (setq ent_name (car (nentselp targetPoint)))
        (or
          (= "S" (substr (cdr (assoc 1 (entget ent_name) )) 1 1))
          (= "F" (substr (cdr (assoc 1 (entget ent_name) )) 1 1))
        )
      );END and
      (vlax-put-property
        (vlax-ename->vla-object ent_name)
        'TextString
        (strcat
          (substr (cdr (assoc 1 (entget ent_name) )) 1 1)
          (LM:rtos (nth 2 p2) 2 2)
        )
      );END vlax-put-property
      ; TODO crea un (DT:INT) ya!!
      (setq
        z2 (atof (LM:rtos (nth 2 p2) 2 2))
        d12 (distance (DT:flatPoint p1) (DT:flatPoint p2))
        gradient (abs (/ d12 (- z2 z1)))
        )
      (if (CopyToClipboard (LM:rtos gradient 2 0) )
        (princ (strcat "\ngradient (1/..) to clipboard: " (LM:rtos gradient 2 0) ))
        (princ "\nNo possible to copy the gradient to the clipboard.")
      );END if
      (princ)
    );END subcond
    ( (or
        (not (nentselp targetPoint))      ; there is no objects in the targetPoint
        (if (assoc 1 (entget ent_name) )  ; it's a text, but doesn't start with "S" or "F"
          (nor
            (= "S" (substr (cdr (assoc 1 (entget ent_name) )) 1 1))
            (= "F" (substr (cdr (assoc 1 (entget ent_name) )) 1 1))
          )
        );END if

      );END or
    	(if (tblsearch "block" "PI_DT")
    		(command "._insert" "PI_DT" (list (nth 0 p2) (nth 1 p2) 0.0) "0.25" "0.25" "" (LM:rtos (nth 2 p2) 2 3))
    	);END if
    );END subcond
  );END cond
  ;|
  (if (nentselp targetPoint)
    ; si es un texto, y puedes procesarlo, dale
    ; Else: continuar como si fuera un punto cualquiera, un no-texto
  );END if
  (setq
    target (entsel (strcat "\nLevel = " (LM:rtos (nth 2 p2) 2 2) "m\nSelect target text: "))
    targetText (car target)
    targetPoint (cadr target)
    targetTextLayer (cdr (assoc 8 (entget targetText)))
    targetTextContent (cdr (assoc 1 (entget targetText)))
  )
  (princ "\nto clipboard: ")
  (CopyToClipboard (LM:rtos (nth 2 p2) 2 3))
  |;
)
(DT:ChangePrivateSewerGradient ent_name gradient)
(defun DT:ChangePrivateSewerGradient ( ent_name gradient )
  ; ent_name [ename] - Text entity with sewer data
  ; gradient [real]

  ;(entsel (strcat "\nSelect sewer text to update gradient to 1/" (LM:rtos gradient 2 0) ))
  (if ent_name
    (if (= 'ename (type (car targetGradient)))
      (if (= "TEXT" (cdr (assoc 0 (entget ent_name))))
        (progn
          (setq
            targetGradient (car targetGradient)
            targetGradientText (cdr (assoc 1 (entget (car targetGradient))))
            targetGradientTextPosition (+ 1 (vl-string-position (ascii "/") targetGradientText))
            targetGradientText (substr targetGradientText 1 targetGradientTextPosition)
          )
          (princ "\ntargetGradientText = ")(princ targetGradientText)
        );END progn
        (princ "\nERROR @ DT:ChangePrivateSewerGradient : ent_name is not a text")
      );END if
      (princ "\nERROR @ DT:ChangePrivateSewerGradient : ent_name is not an entity")
    );END if
    (princ "\nERROR @ DT:ChangePrivateSewerGradient : ent_name=nil")
  );END if
)
