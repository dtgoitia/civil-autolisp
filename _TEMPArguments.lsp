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
