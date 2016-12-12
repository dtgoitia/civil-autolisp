; DO NOT REMOVE THIS LINE. It's a checking.
(defun c:1() (command "_offset" "e" "y" 0.25))
(defun c:2() (command "_offset" "e" "n" 0.25))
(defun c:3() (setvar "osmode" 32) (CopyToClipboard (LM:rtos (+ 10 (* 0.1 (distance (getpoint) (getpoint) )) ) 2 3) ))
