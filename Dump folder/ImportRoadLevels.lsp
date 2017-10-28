(defun c:1 ( / ss ent )
  ; Convert point block to road level
  (if (setq ss (ssget '(( 0 . "INSERT"))))
    (foreach a (ssnamex ss)
      (if (= 'ename (type (cadr a)))
        (DT:BlockToRoadLevel (cadr a))
      );END if
    );END foreach
  );END if

  ; v0.0 - 2017.07.13 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.07.13
)
(defun DT:BlockToRoadLevel ( ename / z xy insertionPoint return )
  ; Get passed ename block "LEVEL" attribute and create a road level in its coordinates with such level as content

  (if (DT:Arg 'DT:BlockToRoadLevel '((ename 'ename)))
    (progn
      (setq z (LM:vl-getattributevalue (vlax-ename->vla-object ename) "LEVEL"))
      (setq xy (cdr (assoc 10 (entget ename))))
      (setq insertionPoint (list (nth 0 xy) (nth 1 xy) 0.0) )
      (setq return (DT:DrawRoadLevel xy (atof z) ))
      (vlax-put-property (vlax-ename->vla-object return) 'Rotation (cdr (assoc 50 (entget ename))))
      return
    );END progn
    nil
  );END if

  ; v0.0 - 2017.07.13 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.07.13
)
