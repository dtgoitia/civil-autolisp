; DO NOT REMOVE THIS LINE. It's a checking.
(defun c:1( / ss lay z)
  ; Sort 2D polylines in layers according to theis elevation
  (setq ss (ssget))
  (foreach a (ssnamex ss)
    (if (= 'ename (type (cadr a)))
      (progn
        (setq
          z (vla-get-elevation (vlax-ename->vla-object (cadr a)))
          lay (strcat "e-EG-" (LM:rtos z 2 1) "m")
        )
        (command "-layer" "m" lay "")
        (vla-put-layer (vlax-ename->vla-object (cadr a)) lay)
      );END progn
    );END if
  );END foreach
)
(defun c:3( / lay ent_name1 ent_name2 )
  ; "Copy to layer" and freeze the target layer, in a loop
  (while T
    (setq
      ent_name1 (car (entsel "\n1: "))
      ent_name2 (car (entsel "\n2: "))
      lay (cdr (assoc 8 (entget ent_name2)))
    )
    (vla-copy (vlax-ename->vla-object ent_name1))
    (vla-put-layer (vlax-ename->vla-object (entlast)) lay)
    (command "-layer" "f" lay "")
  )
)
(defun c:4( / ent_name startZ endZ z lay)
  ; Copy the object on all the layers from "e-EG--0.3m" to "e-EG-1.9m"
  (setq
    ent_name (car (entsel))
    startZ -0.3
    endZ 1.9
    z startZ
  )
  (while (<= z endZ)
    (setq
      lay (strcat "e-EG-" (LM:rtos z 2 1) "m")
    )
    (princ "\n")(princ lay)
    (vla-copy (vlax-ename->vla-object ent_name))
    (vla-put-elevation (vlax-ename->vla-object (entlast)) z)
    (vla-put-layer (vlax-ename->vla-object (entlast)) lay)
    (setq z (+ z 0.1))
  );ENd while
  (princ)
)
