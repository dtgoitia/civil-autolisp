; DO NOT REMOVE THIS LINE. It's a checking.
(defun c:1() (c:BYC))
(defun c:2() (c:INT))
(defun c:3() (c:SDIP))
(defun c:4( / ref)
  ; Update target text with reference level minus 0.39
  (princ "\nSubase below TARMAC:\n")
  (setq ref (DT:clic_or_type_level) ) (princ ref)
  (DT:SetText (car (entsel "\nSelect target text: ")) (strcat "SB" (LM:rtos (- ref 0.39) 2 2)) )
);END defun
(defun c:5( / ref)
  ; Update target text with reference level minus 0.39
  (princ "\nSubase below BLOCK paving:\n")
  (setq ref (DT:clic_or_type_level) ) (princ ref)
  (DT:SetText (car (entsel "\nSelect target text: ")) (strcat "SB" (LM:rtos (- ref 0.41) 2 2)) )
);END defun
(defun c:m() (command "move" pause "" (cadr (grread 't)) pause) )
(defun c:mo() (command "move"))
(defun c:mm() (command "move"))
