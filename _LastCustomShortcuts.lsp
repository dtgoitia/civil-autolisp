; DO NOT REMOVE THIS LINE. It's a checking.
(defun c:1() (princ "\nBY: ") (c:BY))
(defun c:2() (princ "\nINT: ") (c:INT))
(defun c:3() (princ "\nSDIP: ") (c:SDIP))
(defun c:pa()(fbi "Parking-Fall-Arrow") (vlax-put-property (vlax-ename->vla-object (entlast)) 'Layer "e-road-fall-arrow") )
(defun c:ra()(fbi "Road-Fall-Arrow") (vlax-put-property (vlax-ename->vla-object (entlast)) 'Layer "e-road-fall-arrow") )
(defun c:rr()(c:RTM))
(defun c:os()(setvar "osmode" 4))
(defun c:ne()(setvar "osmode" 512))
