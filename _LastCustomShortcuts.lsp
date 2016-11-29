; DO NOT REMOVE THIS LINE. It's a checking.
(defun c:1() (setvar "clayer" "e-asd-PH2") (DT:LinkedBlocks "0000"))
(defun c:2() (setvar "clayer" "e-psd-PH2") (DT:LinkedBlocks "Private-Round475-Storm-Manhole"))
(defun c:3() (command "_erase" "L" ""))
(defun DT:LinkedBlocks ( blockName / p1 p2)
  (setq p2 (getpoint) )
  (entmakex (list (cons 0 "INSERT") (cons 2 blockName) (cons 10 p2 ) ))
  (while (not kkkk)
    (setq
      p1 p2
      p2 (getpoint)
    )
    (entmakex (list (cons 0 "LINE") (cons 10 p1) (cons 11 p2) ))
    (entmakex (list (cons 0 "INSERT") (cons 2 blockName) (cons 10 p2 ) ))
  );END while
  (princ)
);END defun
(defun c:4( / p )
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
  (vla-offset (vlax-ename->vla-object ent_name) 0.5)  (setq ent_name1 (entlast))
  (vla-offset (vlax-ename->vla-object ent_name) 1)    (setq ent_name2 (entlast))
  (vla-offset (vlax-ename->vla-object ent_name) -0.5) (setq ent_name3 (entlast))
  (vla-offset (vlax-ename->vla-object ent_name) -1)   (setq ent_name4 (entlast))
  (vla-delete (vlax-ename->vla-object ent_name))

)
