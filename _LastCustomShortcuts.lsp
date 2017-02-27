; DO NOT REMOVE THIS LINE. It's a checking.
(defun c:1( / ss )
  ; Move selected objects to layer
  (while T
    (if (setq ss (ssget))
      (foreach a (ssnamex ss)
        (if (= 'ename (type (cadr a)))
          (if (= "INSERT" (cdr (assoc 0 (entget (cadr a)))))
            (princ "\nBlock found and skipped.")
            (DT:temp (cadr a) "__BA-pond")
          );END if
        );END if
      );END foreach
    );END if
    (setq ss nil)
  );END while
)
(defun c:2( / ss )
  ; Move selected objects to layer
  (while T
    (if (setq ss (ssget))
      (foreach a (ssnamex ss)
        (if (= 'ename (type (cadr a)))
          (if (= "INSERT" (cdr (assoc 0 (entget (cadr a)))))
            (princ "\nBlock found and skipped.")
            (DT:temp (cadr a) "__BA-pond-annotations")
          );END if
        );END if
      );END foreach
    );END if
    (setq ss nil)
  );END while
)
(defun DT:temp (ent_name lay)
  (if ent_name
    (progn
      (vla-put-layer (vlax-ename->vla-object ent_name) lay)
      (vlax-put-property (vlax-ename->vla-object ent_name) 'Color 256)
    );END progn
  );END if
)
