(defun c:xx ( / ss listToCheck return )
  ; Find duplicates
  (setq ss (ssget "x" '(( 0 . "TEXT"))))
  (foreach a (ssnamex ss)
    (if (= 'ename (type (cadr a)))
      (setq listToCheck (append listToCheck (list (cdr (assoc 1 (entget (cadr a)))))))
    );END if
  );END foreach

  ; Check and show return
  (setq return (DT:FindDuplicatesSingleList listToCheck))
  (DT:PrintVar 'return)

  ; v0.0 - 2017.06.27 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.06.27
)
(defun DT:FindDuplicatesTwoList ( listA listB )
  ; Compare both lists and return a list referencing duplicated

  ; v0.0 - 2017.06.27 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.06.27
)
(defun DT:FindDuplicatesSingleList ( listToCheck / i ii listLength foreachList return )
  ; Compare both lists and return a list referencing duplicated
  (if (DT:Arg 'DT:FindDuplicatesSingleList '((listToCheck 'list)))
    (progn
      (setq i 0)
      (setq ii 0)
      (setq listLength (length listToCheck))
      (setq foreachList (LM:sublst listToCheck 0 (- listLength 1)))
      (while (< i listLength)
        ; Get first element
        (setq reference (nth 0 foreachList))

        ; Check for duplicates
        (foreach item foreachList
          (if (= ii 0)
            nil
            (if (= reference item)
              (setq return (append return (list (list reference i (+ ii i)))))
            );END if
          );END if
          (setq ii (+ ii 1))
        );END foreach

        (DT:PrintVar 'i)
        (DT:PrintVar 'ii)
        (DT:PrintVar 'reference)
        (DT:PrintVar 'listToCheck)
        (DT:PrintVar 'foreachList)
        (DT:PrintVar 'return)
        (getreal "\nPress ENTER")

        ; Prepare next iteration
        (setq foreachList (LM:sublst listToCheck i (- listLength i)))
        (setq i (+ i 1))
        (setq ii 0)
      );END while
    );END progn
  );END if

  return

  ; v0.0 - 2017.06.27 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.06.27
)
