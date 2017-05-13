(defun c:xx ( / functionToTest argumentConvinations )
  ; Execute test
  (setq
    functionToTest 'DT:MyFunction
    argumentConvinations '(
      ("lakdslaksmd" 123)
      ("lakdslaksmd" 12.3)
      ("a" nil)
      (nil "a")
      (nil nil)
    )
  );END setq

  (DT:Test functionToTest argumentConvinations)

  (princ)
  ; v0.0 - 2017.05.13 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.05.13
)
(defun DT:Test ( functionToTest argumentConvinations / i msg return )
  ; Test "functionToTest"
  (setq i 0)
  (foreach argumentList argumentConvinations
    ; Update counter
    (setq i (1+ i) )

    ; Compose case presentation message and print it:
    (setq msg (strcat "\n\n" (itoa i) " : (" (vl-symbol-name functionToTest)))
    (foreach argument argumentList
      (setq msg (strcat msg " " (vl-princ-to-string argument)))
    );END foreach
    (setq msg (strcat msg ")"))
    (princ msg)

    ; Evaluate and print result:
    (setq return (apply functionToTest argumentList))
    (setq msg (strcat "\n >> " (vl-princ-to-string return)))
    (princ msg)
  );END foreach
  (princ )
)
(defun DT:MyFunction ( arg1 arg2 / errorList )
  (DT:Arg "DT:MyFunction" '((arg1 'str) (arg2 'int)) )
)
(defun DT:Arg ( functionName argumentList / errorList )
  ; Return T if the passed arguments are right, otherwise print error message and return nil
  (if (and functionName argumentList)
    (if (and (= 'str (type functionName)) (= 'list (type argumentList)))
      (progn
        (foreach a argumentList
          ; if both elements in "a" are different to nil
          (if (and (/= nil (eval (nth 0 a))) (/= nil (eval (nth 1 a))))
            (progn
              (if (= (eval (nth 1 a)) (type (eval (nth 0 a))))
                nil
                (progn
                  (setq errorList (append errorList (list
                        (strcat
                          "ERROR @ " functionName " : " (vl-symbol-name (nth 0 a))
                          " is not a " (vl-symbol-name (eval (nth 1 a)))
                        );END strcat
                      );END list
                    );END append
                  );END setq
                );END progn
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
