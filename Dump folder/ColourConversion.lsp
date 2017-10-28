(defun VxSetTrueCol2 (Obj Lst / ColObj)
  (setq ColObj (vla-get-truecolor Obj))
  (vla-put-ColorMethod ColObj acColorMethodByRGB)
  (setq Test (vl-catch-all-apply (function vla-setrgb) (list ColObj (car Lst) (cadr Lst) (caddr Lst))))
  (cond
    ( (vl-catch-all-error-p Test)
      (prompt (strcat "\nError: " (vl-catch-all-error-message Test)))
      (setq Test nil)
    )
    (T
      (vla-put-truecolor Obj ColObj)
      (setq Test t)
    )
  )
  Test

  ; Author: joselggalan (forums.autodesk.com)
)
