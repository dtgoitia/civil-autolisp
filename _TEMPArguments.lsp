(defun c:xx ( / functionToTest argumentCombinations )
  ; Execute test
  (setq
    functionToTest 'DT:MyFunction
    argumentCombinations '(
      ("lakdslaksmd" 123)
      ("lakdslaksmd" 12.3)
      ("a" nil)
      (nil "a")
      (nil nil)
      ((1 2) "a")
    )
  );END setq

  (DT:Test functionToTest argumentCombinations)

  (princ)
  ; v0.0 - 2017.05.13 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.05.13
)
(defun DT:MyFunction ( arg1 arg2 / errorList )
  (DT:Arg 'DT:MyFunction '((arg1 'str) (arg2 'int)) )
)
(defun DT:Test ( functionToTest argumentCombinations / i msg return )
  ; Test "functionToTest" with the "argumentCombinations" argument combinations
  (if (DT:Arg 'DT:Test '((functionToTest 'sym) (argumentCombinations 'list)))
    (progn
      (setq i 0)
      (foreach argumentList argumentCombinations
        ; Update counter
        (setq i (1+ i) )

        ; Compose case presentation message and print it
        (setq msg (strcat "\n\n" (itoa i) " : (" (vl-symbol-name functionToTest)))
        (foreach argument argumentList
          (cond
            ((= 'str (type argument))
              (setq msg (strcat msg " \"" (vl-princ-to-string argument) "\"" ))
            );END subcond
            (t
              (setq msg (strcat msg " " (vl-princ-to-string argument)))
            );END subcond
          );END cond
        );END foreach
        (setq msg (strcat msg ")"))
        (princ msg)

        ; Evaluate (to print any error message during the process) and print evaluation result
        (setq return (apply functionToTest argumentList))
        (setq msg (strcat "\n >> " (vl-princ-to-string return) " [" (vl-princ-to-string (type return)) "]"))
        (princ msg)
      );END foreach
      (princ)
    );END progn
  );END if
)
(defun DT:Arg ( functionName argumentList / argumentSymbol argumentType errorList err )
  ; Return T if the passed arguments are right, otherwise print error message and return nil
  ; functionName [SYM]  - Symbol of the function to check
  ; argumentList [LIST] - Non-evaluated list of argument symbols and argument types as bellow:
  ;                     - '( (argumentSymbol argumentType) (argumentSymbol argumentType) (argumentSymbol argumentType) )
  (if (and functionName argumentList)
    (if (and (= 'sym (type functionName)) (= 'list (type argumentList)))
      (progn
        (foreach argument argumentList
          ; Save argumentSymbol and argumentType
          (setq
            argumentSymbol (nth 0 argument)
            argumentType   (nth 1 argument)
          );END setq

          ; If argumentSymbol and argumentType values are different to nil
          (if (and (/= nil (eval argumentSymbol)) (/= nil (eval argumentType)))
            (progn
              ; If argument type is correct
              (if (= (eval argumentType) (type (eval argumentSymbol)))
                nil
                (setq errorList (append errorList (list (strcat "ERROR @ " (vl-symbol-name functionName) " : " (vl-symbol-name argumentSymbol) " is not a " (vl-symbol-name (eval argumentType))))))
              );END if
            );END progn
            (cond
              ((not (eval argumentSymbol)) (setq errorList (append errorList (list (strcat "ERROR @ DT:Arg : " (vl-symbol-name argumentSymbol) "=nil")))))
              ((not (eval argumentType))   (setq errorList (append errorList (list (strcat "ERROR @ DT:Arg : " (vl-symbol-name (eval argumentType)) "=nil")))))
            );END cond
          );END if
        );END foreach
      );END progn
      (cond
        ((/= 'str (type functionName))  (setq errorList (append errorList (list "ERROR @ DT:Arg : functionName is not a symbol"))))
        ((/= 'list (type argumentList)) (setq errorList (append errorList (list "ERROR @ DT:Arg : argumentList is not a list"))))
      );END cond
    );END if
    (cond
      ((not functionName) (setq errorList (append errorList (list "ERROR @ DT:Arg : functionName=nil"))))
      ((not argumentList) (setq errorList (append errorList (list "ERROR @ DT:Arg : argumentList=nil"))))
    );END cond
  );END if

  ; If any error message print them and return nil, otherwise return T
  (if errorList
    (progn
      (foreach err errorList
        (princ "\n")(princ err)
      );END foreach
      nil
    );END progn
    T
  );END if

  ; v0.1 - 2017.05.13 - Update variable names for clarity purporse
  ;                   - Update error printing system
  ; v0.0 - 2017.05.12 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.05.13
)
