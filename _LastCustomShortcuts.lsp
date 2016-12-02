; DO NOT REMOVE THIS LINE. It's a checking.
(defun c:1() (setvar "clayer" "e-afd-PH2") (DT:LinkedBlocks "0000foul") )
(defun c:2() (setvar "clayer" "e-pfd-PH2") (DT:LinkedBlocks "Private-Square475-Foul-Manhole") )
(defun c:3() (command "_erase" "L" ""))
(defun DT:LinkedBlocks ( blockName / p1 p2)
  (setq p2 (getpoint) )
  (entmakex (list (cons 0 "INSERT") (cons 2 blockName) (cons 10 p2 ) ))
  (while (not kkkk)
    (setq
      p1 p2
      p2 (getpoint)
    )
    (entmakex (list (cons 0 "LWPOLYLINE") (cons 100 "AcDbEntity") (cons 100 "AcDbPolyline") (cons 90 2) (cons 10 p1) (cons 10 p2) ) )
    (entmakex (list (cons 0 "INSERT") (cons 2 blockName) (cons 10 p2 ) ))
  );END while
  (princ)
)
(defun c:4( / p ent_name)
  (c:N)
  (setq
    ent_name (entlast)
    ent_name_block
      (entmakex
        (list
          (cons 0 "BLOCK")
          (cons 10 (list 0 0 0))
          (cons 2 "lalalala")
        )
      )
  )
  (vla-offset (vlax-ename->vla-object ent_name) 0.5)
  (vla-offset (vlax-ename->vla-object ent_name) 1)
  (vla-offset (vlax-ename->vla-object ent_name) -0.5)
  (vla-offset (vlax-ename->vla-object ent_name) -1)
  (vla-delete (vlax-ename->vla-object ent_name))
)
(defun c:2()
  (command "-hatch" "LA" "." "P" "ANSI31" "0.1" "" "A" "A" "Y" "" "S" (entlast) "" "")
);END defun
