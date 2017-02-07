; DO NOT REMOVE THIS LINE. It's a checking.
(defun c:1() (princ "\nBYC: ") (c:BYC))
(defun c:2() (command "-hatch" "S"))
(defun c:oo()(setvar "osmode" 35))
(defun c:rr()(c:RTM))
(defun c:zz()(princ "\noldosmode = ")(princ oldosmode)(princ))
(defun c:zzz()(setq oldosmode nil))
