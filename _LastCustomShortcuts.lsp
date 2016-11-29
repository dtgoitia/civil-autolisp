; DO NOT REMOVE THIS LINE. It's a checking.
(defun c:1() (setvar "clayer" "e-asd-PH2") (DT:LinkedBlocks "0000"))
(defun c:2() (setvar "clayer" "e-psd-PH2") (DT:LinkedBlocks "00rwp"))
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
