(defun DT:Test ( functionToTest argumentCombinations / i msg return )
  ; Test "functionToTest" with the "argumentCombinations" argument combinations
  (if (DT:Arg 'DT:Test '((functionToTest 'sym) (argumentCombinations 'list)))
    (progn
      ; If functionToTest is declared
      (if (eval functionToTest)
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
        (progn (princ (strcat "\nERROR @ DT:Test : \'" (vl-princ-to-string functionToTest) " is not a function\n")) nil)
      );END if
    );END progn
  );END if
)
(defun DT:MyExampleFunction ( arg1 arg2 / errorList )
 ; Example function to show DT:Test function behaviour
  (DT:Arg 'DT:MyExampleFunction '((arg1 'str) (arg2 'int)) )

  ; v0.0 - 2017.05.13 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.05.13
)
(defun c:xx ( / functionToTest argumentCombinations )
  ; Example of DT:Test function behaviour
  (setq
    functionToTest 'DT:MyExampleFunction
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
