(defun c:2 ( / ss )
  ; Convert point block to plot level
  (if (setq ss (ssget '(( 0 . "INSERT"))))
    (foreach a (ssnamex ss)
      (if (= 'ename (type (cadr a)))
        (DT:BlockToPlotLevel (cadr a))
      );END if
    );END foreach
  );END if

  ; v0.0 - 2017.07.13 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.07.13
)
(defun DT:BlockToPlotLevel ( ename / z xy insertionPoint return )
  ; Get passed ename block "LEVEL" attribute and create a plot level in its coordinates with such level as content

  (if (DT:Arg 'DT:BlockToPlotLevel '((ename 'ename)))
    (progn
      (setq z (LM:vl-getattributevalue (vlax-ename->vla-object ename) "LEVEL"))
      (setq xy (cdr (assoc 10 (entget ename))))
      (setq insertionPoint (list (nth 0 xy) (nth 1 xy) 0.0) )
      (setq return (DT:DrawPlotLevel xy (atof z) ))
    );END progn
    nil
  );END if

  ; v0.0 - 2017.07.13 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.07.13
)
