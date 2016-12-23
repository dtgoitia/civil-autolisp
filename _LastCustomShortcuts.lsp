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
(defun c:1() (command "_laymcur" pause) (princ))
(defun c:2()
  (command "_-hatch" pause "")
  (princ)
)
(defun c:1() (princ "\nTRIM: ") (command "_trim" pause))
(defun c:2() (princ "\nJOIN: ") (command "_join"))
(defun c:3() (c:BY))
(defun c:4( / VL_ent_name )
  ; Remove first vertex and close polyline
  (if (setq ss (ssget '(( 0 . "LWPOLYLINE")) ))
    (foreach a (ssnamex ss)
      (if (= 'ename (type (cadr a)))
        (if (= "LWPOLYLINE" (cdr (assoc 0 (entget (cadr a)))))
          (if (= :vlax-false (vla-get-closed (vlax-ename->vla-object (cadr a))))
            (progn
              ; Check if first and last vertex are the same or very close points ------------------------------------------ TODO
              ; Remove first vertex
              (DT:RemovePolylineVertex (cadr a) (vlax-curve-getEndParam (vlax-ename->vla-object (cadr a))))
              ; Close polyline
              (vla-put-closed (vlax-ename->vla-object (cadr a)) :vlax-true)
              (vlax-put-property (vlax-ename->vla-object (cadr a)) 'Color 256)
            );END progn
          );END if
        );END if
      );END if
    );END foreach
  );END if
  ;(princ)
)
(defun c:5( / VL_ent_name )
  ; <Mark not closed polylines in blue
  (if (setq ss (ssget '(( 0 . "LWPOLYLINE")) ))
    (foreach a (ssnamex ss)
      (if (= 'ename (type (cadr a)))
        (if (= "LWPOLYLINE" (cdr (assoc 0 (entget (cadr a)))))
          (if (= :vlax-false (vla-get-closed (vlax-ename->vla-object (cadr a))))
            (vlax-put-property (vlax-ename->vla-object (cadr a)) 'Color 5)
          );END if
        );END if
      );END if
    );END foreach
  );END if
  ;(princ)
)
(defun c:0( / startZ endZ z layerName)
  ; Create the layers from "e-clash--0.3m" to "e-clash-2.2m"
  (setq
    startZ -0.3
    endZ 2.2
    z startZ
  )
  (while (<= z endZ)
    (setq
      layerName (strcat "e-clash-" (LM:rtos z 2 1) "m")
      z (+ z 0.1)
    )
    (command "-layer" "m" layerName "")
    (princ "\n")(princ layerName)
  );ENd while
  (princ)
)
(defun c:1 ()
  ; This function selects the polylines within the selection set,
  ; closes them and creates an individual associative hatch for each polyline in the polyline layer
  ; and changes the transparency of the hatch to 0.3
  (vl-load-com)
  (foreach a (ssnamex (ssget '((-4 . "<OR") (0 . "LWPOLYLINE") (0 . "POLYLINE") (-4 . "OR>"))) )
    (if (= 'ename (type (cadr a)))
      (progn
        ; If clayer is different ot object layer, update layer to object layer
        (if (/= (getvar "clayer") (cdr (assoc 8 (entget (cadr a)))) )
          (setvar "clayer" (cdr (assoc 8 (entget (cadr a)))))
        );END if

        ; Create hatch
        (DT:ha (cadr a) "SOLID" "1")

        ; Change hatch transparency
        (if (= "HATCH" (cdr (assoc 0 (entget (entlast)))))
          (progn
            ;Change transparency
            (princ "\nHatch created!")
          );END progn
          (princ "\nSelected object is not a hatch")
        );END if
      );END progn
    );END if
  );END foreach
)
