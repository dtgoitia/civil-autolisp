(defun c:0 ()
  ; Set datum line and centreline
  (setq *datumLine* (car (entsel "\nSelect datum line: ")) )
  (princ "\n*datumLine* = ")(princ *datumLine*)
  (setq *VL_centreline* (vlax-ename->vla-object (car (entsel "\nSelect centreline (global variable): \n"))))
  (princ "\n*VL_centreline* = ")(princ *VL_centreline*)
)
(defun c:1 ( / pt )
  ; Capture chainages
  (setvar "osmode" 4)
  (if (setq pt (getpoint "\nSelect manhole centre") )
    (if (setq #chainage (DT:PK *VL_centreline* pt) )
      (princ (strcat "\nch = " (LM:rtos #chainage 2 5)))
    );END if
  );END if
  (princ)

  ; v0.0 - 2017.04.13 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.04.13
)
(defun c:2 ( / datum verticalExageration p0 p1 p2 )
  ; Mark IL at the required chainage
  (setvar "osmode" 0)
  (setq
    datumLevel 48
    verticalExageration 10
    p0 (cdr (assoc 10 (entget *datumLine*)))
  )
  ;(if (setq ILlevel (getreal "\nType IL to mark: "))
  (if (setq ILlevel 50)
    (progn
      (setq
        ; find chainage at datum line
        p1 (polar p0 0 #chainage)
        ; find IL point
        p2 (polar p1 (* 0.5 pi) (* verticalExageration (- ILlevel datumLevel) ) )
      );END setq
      (entmakex (list (cons 0 "POINT") (cons 10 p2) ) )
    );END progn
  );END if
  (princ)

  ; v0.0 - 2017.04.13 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.04.13
)
(defun c:xx ( / currentString )
  (foreach a (ssnamex (ssget))
    (if (= 'ename (type (cadr a)))
      (progn
        (setq
          currentString (DT:GetText (cadr a))
          newString (strcat (substr currentString 1 2) (itoa (+ 1 (atoi (substr currentString 3 3)))) (substr currentString 6) )
          currentEntityList (entget (cadr a))
        )
        (DT:SetText (cadr a) newString)
      );END progn
    );END if
  );END foreach
  (princ)
)
