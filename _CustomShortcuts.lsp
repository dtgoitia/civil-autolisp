; UNDEFINE
;(command
;  "_.undefine" "intersect"
;  "_.undefine" "interfere"
;  "_.undefine" "redraw"
;  "_.undefine" "redrawall"
;  "_.undefine" "fill"
;  "_.undefine" "filletedge"
;  "_.undefine" "surffillet"
;  "_.undefine" "donut"
;  "_.undefine" "donutid"
;  "_.undefine" "donutod"
;  "_.undefine" "targetpoint"
;)
(defun c:iso() (command "_.isolateobjects")(princ))
(defun c:iso() (command "_.isolateobjects")(princ))
(defun c:uiso() (command "_.unisolateobjects")(princ))
(defun c:c() (command "_.copy" pause "" "_non" (cadr (grread 't)) "_non" pause) )
(defun c:cc() (command "_.copy") )
(defun c:m() (command "_.move" pause "" "_non" (cadr (grread 't)) "_non" pause) )
(defun c:mo() (command "_.move"))
(defun c:mm() (command "_.move"))
(defun c:p00()
  (command
    "_.pasteblock" "0,0"
    "_.scale" "L" "" "0,0" "1000"
    "_.explode" "L" ""
    "_.zoom" "O" "L" ""
  )
  (princ)
)
(defun c:ci()	(command "_.circle")(princ))
(defun c:n() (command "_.NCOPY" pause "" "" ""))
(defun c:xu() (command "_-xref" "u" "*")(alert "Xref Unload finished!")(princ)) ;Unload all Xrefs
(defun c:xr() (command "_-xref" "r" "*")(alert "Xref Reload finished!")(princ)) ;Reload all Xrefs
(defun c:t () (command "_.textedit" "M" "S" pause))
(defun c:tt () (command "_.textedit" "M" "M" pause))
(defun c:pp()(command "_.publish"))
(defun c:las() (command "_.layerstate")(princ))
(defun c:zz ( / ss )
  ; Change selected entities' color to blue
  (princ "\nMarking in blue:")
  (if (setq ss (ssget))
    (foreach a (ssnamex ss)
      (if (= 'ename (type (cadr a)))
        (vlax-put-property (vlax-ename->vla-object (cadr a)) 'Color 5)
      );END if
    );END foreach
  );END if

  ; v0.0 - 2017.03.13 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.03.13
)
(defun c:o ( / x ) (if (setq x (getint (strcat "\nObject Snap Mode <" (itoa (getvar "osmode")) ">: "))) (setvar "osmode" x) (princ "*Cancel*"))(princ))
(defun c:os () (c:o))
(defun c:rr () (princ "\nMultiple rotate:") (c:RTM))
(defun c:rrr() (princ "\nRegenerating...") (command "_.regenall") (princ " done.")(princ))
