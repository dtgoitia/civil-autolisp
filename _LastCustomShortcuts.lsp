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
