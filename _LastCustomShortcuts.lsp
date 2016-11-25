; DO NOT REMOVE THIS LINE. It's a checking.
(defun c:1( / ent )
  (fbi "000")
  (setq ent_name (entlast))
  (vla-explode (vlax-ename->vla-object ent_name))
  (vla-delete (vlax-ename->vla-object ent_name))
);END defun
