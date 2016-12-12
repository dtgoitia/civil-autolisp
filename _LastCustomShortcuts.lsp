; DO NOT REMOVE THIS LINE. It's a checking.
;(alert "nothing saved!")
(defun c:1( / p )
  (setq p (getpoint "\nSelect manhole insertion point: "))
  (setvar "osmode" 0)
  (command "._insert" "SW-Manhole" (DT:flatPoint p) "1" "1")
  (while (> (getvar "CMDACTIVE") 0) (command ""))
  (setvar "osmode" 4)
)
