; DO NOT REMOVE THIS LINE. It's a checking.
(defun c:1() (princ "\nBY: ") (c:BY))
(defun c:2() (princ "\nINT: ") (c:INT))
(defun c:3() (princ "\nSDIP: ") (c:SDIP))
(defun c:11 ( / targetLevel ent_name )
  ; Get a FFL, substract -0.65m and overwrite the target text object content
  ; with the calculated value properly formated: S16.70
  (princ "\nGET STORM LEVEL FROM FFL\n")
  (setq
    targetLevel (+ (DT:clic_or_type_level) -0.65)
    ent_name (car (entsel (strcat "\nSelect text to overwrite with \"S" (LM:rtos targetLevel 2 2) "\": ") ))
  )
  (if ent_name
    (vlax-put-property (vlax-ename->vla-object ent_name) 'TextString (strcat "S" (LM:rtos targetLevel 2 2)) )
    (princ "\nNo target entity selected.")
  );END if
  (princ)
)
(defun c:22 ( / targetLevel ent_name )
  ; Get a FFL, substract -0.75m and overwrite the target text object content
  ; with the calculated value properly formated: F16.70
  (princ "\nGET FOUL LEVEL FROM FFL\n")
  (setq
    targetLevel (+ (DT:clic_or_type_level) -0.75)
    ent_name (car (entsel (strcat "\nSelect text to overwrite with \"F" (LM:rtos targetLevel 2 2) "\": ") ))
  )
  (if ent_name
    (vlax-put-property (vlax-ename->vla-object ent_name) 'TextString (strcat "F" (LM:rtos targetLevel 2 2)) )
    (princ "\nNo target entity selected.")
  );END if
  (princ)
)
(defun c:pa()(fbi "Parking-Fall-Arrow") (vlax-put-property (vlax-ename->vla-object (entlast)) 'Layer "e-road-fall-arrow") )
(defun c:ra()(fbi "Road-Fall-Arrow") (vlax-put-property (vlax-ename->vla-object (entlast)) 'Layer "e-road-fall-arrow") )
(defun c:rr()(fbi2 "e-psd-rwp"))
(defun c:os()(setvar "osmode" 513))
(defun c:qqq( / ss pt rotation )
  (if (setq ss (ssget '((-4 . "<AND") (0 . "INSERT") (8 . "e-part-m") (-4 . "AND>"))))
    (foreach a (ssnamex ss)
      (if (= 'ename (type (cadr a)))
        (if (= "Part-m-primary-0" (LM:effectivename (vlax-ename->vla-object (cadr a))))
          (progn
            (setq
              pt (cdr (assoc 10 (entget (cadr a))))
              rotation (vlax-get-property (vlax-ename->vla-object (cadr a)) 'Rotation)
            )
            (princ "\nrotation = ")(princ rotation)
            (if (and (>= rotation (* 0.5 pi)) (< rotation (* 1.5 pi)) )
              (if (not (entmakex (list (cons 0 "INSERT") (cons 2 "primary-part-m-SCT_2") (cons 10 pt) (cons 50 rotation) )))
                (princ "\nSomething went wrong!")
              );END if
              (if (not (entmakex (list (cons 0 "INSERT") (cons 2 "primary-part-m-SCT_1") (cons 10 pt) (cons 50 rotation) )))
                (princ "\nSomething went wrong!")
              );END if
            );END if
          );END progn
        );END if
      );END if
    );END foreach
  );END if
  (princ)
)
