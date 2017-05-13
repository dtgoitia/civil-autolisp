(defun c:xx ( / testFunction )
  (setq testFunction '(DT:TestFunction "lakdslaksmd" 12.3) )
  (princ "\n")(princ testFunction)(princ "\n")(princ (eval (DT:TestFunction "lakdslaksmd" 123)))
  (princ )
)
(defun DT:TestFunction ( arg1 arg2 / errorList )
  (DT:Arg "DT:FunctionName" '((arg1 'str)(arg2 'int)) )
)
(defun DT:Arg ( functionName argumentList / errorList )
  ; Return T if the passed arguments are right, otherwise print error message and return nil
  (if (and functionName argumentList)
    (if (and (= 'str (type functionName)) (= 'list (type argumentList)))
      (progn
        (foreach a argumentList
          (if (not (or (not (eval (nth 0 a))) (not (eval (nth 1 a)))))
            (progn
              (if (= (eval (nth 1 a)) (type (eval (nth 0 a))))
                nil
                (progn
                  (setq errorList (append errorList (list (strcat "ERROR @ " functionName " : " (vl-symbol-name (nth 0 a)) " is not a " (vl-symbol-name (eval (nth 1 a))) ) )) )
                )
              );END if
            );END progn
            (progn (princ (strcat "\nERROR @ " functionName " : " (vl-symbol-name (nth 0 a)) "=nil\n")) nil )
          );END if
        );END foreach
      );END progn
      (cond
        ((/= 'str (type functionName)) (princ "\nERROR @ function : functionName is not a string\n") nil )
        ((/= 'list (type argumentList)) (princ "\nERROR @ function : argumentList is not a list\n") nil )
      );END cond
    );END if
    (cond
      ((not functionName) (princ "\nERROR @ function : functionName=nil\n") nil )
      ((not argumentList) (princ "\nERROR @ function : argumentList=nil\n") nil )
    );END cond
  );END if

  ; If any error message print them and return nil, otherwise return T
  (if errorList
    (progn
      (foreach a errorList
        (princ "\n")
        (princ a)
      );END foreach
      nil
    );END progn
    T
  );END if

  ; v0.0 - 2017.05.12 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.05.12
)
