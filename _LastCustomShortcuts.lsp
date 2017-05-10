; DO NOT REMOVE THIS LINE. It's a checking.
(defun c:xx ( / ent_name )
  ; Select block and insert new one
  (if (setq ent_name (car (entsel "\nSelect block of reference: ")))
    (if (setq blockName (LM:effectivename (vlax-ename->vla-object ent_name)))
      (while T (fbi2 blockName))
    );END if
  );END if

  ; v0.0 - 2017.05.10 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.05.10
)
