; DO NOT REMOVE THIS LINE. It's a checking.
(defun c:1( / VL_ent_name )
  (setq VL_ent_name (vlax-ename->vla-object (car (entsel "\nSelect centerline: "))))
  (while VL_ent_name
    (DT:BlockPerpendicularToPolyline VL_ent_name "TEMP")
  )
)
(defun c:rr()(c:RTM))
(defun c:zz()(princ "\noldosmode = ")(princ oldosmode)(princ))
(defun c:zzz()(setq oldosmode nil))
(defun c:11( / ang )
  ; Correct TEXT and MTEXT angle to be readable
  (princ "\nUPDATE TEXT READABILITY ANGLE\n")
  (if (setq ss (ssget))
    (foreach a (ssnamex ss)
      (if (= 'ename (type (cadr a)))
        (if (or (= "TEXT" (cdr (assoc 0 (entget (cadr a))))) (= "MTEXT" (cdr (assoc 0 (entget (cadr a))))) )
          (if
            (/=
              (cdr (assoc 50 (entget (cadr a)) ))
              (setq ang (DT:ReadableTextAngle (cdr (assoc 50 (entget (cadr a)) )) ) )
            )
            (vlax-put-property (vlax-ename->vla-object (cadr a)) 'Rotation ang )
          );END if
        );END if
      );END if
    );END foreach
  );END if
)
