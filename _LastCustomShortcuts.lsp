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
