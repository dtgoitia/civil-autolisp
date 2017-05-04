; DO NOT REMOVE THIS LINE. It's a checking.
(defun c:xx ( / ss totalArea pt )
  ; Insert text whith total area of selected objects
  (if (setq ss (ssget))
    (if (setq totalArea (DT:TotalArea ss))
      (if (setq ent_name (DT:DrawText (setq pt (cadr (grread 't))) (getvar "clayer") (strcat (LM:rtos totalArea 2 2) "m2") 1 0 ))
        (command "_.move" ent_name "" "_non" pt "_non" pause)
      );END if
    );END if
  );END if

  ; Return total area
  (if totalArea totalArea)

  ; v0.0 - 2017.04.18 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.04.18
)
(defun c:1 ()  (command "_.JOIN"))
(defun c:2 ( / ss )
  ; Change selected entities' color to blue
  (princ "\nMarking in blue:")
  (if (setq ss (ssget))
    (foreach a (ssnamex ss)
      (if (= 'ename (type (cadr a)))
        (vlax-put-property (vlax-ename->vla-object (cadr a)) 'Color 2)
      );END if
    );END foreach
  );END if

  ; v0.0 - 2017.03.13 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.03.13
)
