; DO NOT REMOVE THIS LINE. It's a checking.
(defun c:1() (c:BYC))
(defun c:2() (c:INT))
(defun c:3() (c:SDIP))
(defun c:4( / ref dif)
  ; Update target text with reference level minus 0.39
  (princ "\nSubase below TARMAC (light traffic):\n")
  (setq ref (DT:clic_or_type_level) dif 0.310)
  (princ (strcat (LM:rtos (- ref dif) 2 2) "m\n"))
  (DT:SetText (car (entsel "\nSelect target text: ")) (strcat "SB" (LM:rtos (- ref dif) 2 2)) )
);END defun
(defun c:44( / ref dif)
  ; Update target text with reference level minus 0.39
  (princ "\nSubase below TARMAC (heavy traffic):\n")
  (setq ref (DT:clic_or_type_level) dif 0.380)
  (princ (strcat (LM:rtos (- ref dif) 2 2) "m\n"))
  (DT:SetText (car (entsel "\nSelect target text: ")) (strcat "SB" (LM:rtos (- ref dif) 2 2)) )
);END defun
(defun c:5( / ref dif)
  ; Update target text with reference level minus 0.39
  (princ "\nSubase below BLOCK paving (light traffic):\n")
  (setq ref (DT:clic_or_type_level) dif 0.290)
  (princ (strcat (LM:rtos (- ref dif) 2 2) "m\n"))
  (DT:SetText (car (entsel "\nSelect target text: ")) (strcat "SB" (LM:rtos (- ref dif) 2 2)) )
);END defun
(defun c:55( / ref dif)
  ; Update target text with reference level minus 0.39
  (princ "\nSubase below BLOCK paving (heavy traffic):\n")
  (setq ref (DT:clic_or_type_level) dif 0.360)
  (princ (strcat (LM:rtos (- ref dif) 2 2) "m\n"))
  (DT:SetText (car (entsel "\nSelect target text: ")) (strcat "SB" (LM:rtos (- ref dif) 2 2)) )
);END defun
(defun c:m() (command "move" pause "" (cadr (grread 't)) pause) )
(defun c:mo() (command "move"))
(defun c:mm() (command "move"))
