; DO NOT REMOVE THIS LINE. It's a checking.
(defun DT:flatPoint( pt ) (list (nth 0 pt) (nth 1 pt) 0.0) )
(defun Point (pt) (entmakex (list (cons 0 "POINT") (cons 10 pt))))
(defun c:1( / p2D p3D  )
  (while T
    (princ "\nDT:InsertPoint > ")
    (setq
      p2D (DT:flatPoint (getpoint "\nSelect 2D location: ") )
      p3D (list (nth 0 p2D) (nth 1 p2D) (DT:clic_or_type_level) )
    )
    (if p3D (Point p3D) )
  );END while
)
(defun c:2() (command "_3dpoly") )
