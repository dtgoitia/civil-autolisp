(defun c:xx ()
  ; Trigger
  (DT:AutoLoadFileFromCivilTemp "AverageBySelection.lsp")
  (c:AverageBySelection)

  ; v0.0 - 2017.08.17 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.17
)
(defun c:AverageBySelection ()
  ; Command version of (DT:AverageBySelection)
  (DT:AverageBySelection (ssget))

  ; v0.0 - 2017.08.17 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.17
)
(defun DT:AverageBySelection ( ss / txt x collectedValues average )
  ; DT:AverageBySelection function description
  (if (DT:Arg 'DT:AverageBySelection '((ss 'pickset)))
    (foreach a (ssnamex ss)
      (if (= 'ename (type (cadr a)))
        (if (setq txt (DT:GetText (cadr a)))
          (if (setq x (atof txt))
            (progn
              (setq collectedValues (append collectedValues (list x)))
              (setq average (DT:Average collectedValues))
              (princ (strcat " >> " txt "  :  " (LM:rtos average 2 5) "\n"))
            );END progn
          );END if
        );END if
      );END if
    );END foreach
  );END if
  average

  ; v0.0 - 2017.08.17 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.17
)
(defun c:AverageByClick ()
  ; Command version of (DT:AverageByClick)
  (DT:AverageByClick)

  ; v0.0 - 2017.08.17 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.17
)
(defun DT:AverageByClick ( / ename txt x collectedValues escapeVariable )
  ; Return the average of the values clicked
  (while (not escapeVariable)
    (if (setq ename (car (nentsel)) )
      (if (setq txt (DT:GetText ename))
        (if (setq x (atof txt))
          (progn
            (setq collectedValues (append collectedValues (list x)))
            (setq average (DT:Average collectedValues))
            (princ (strcat " " txt "  :  " (LM:rtos average 2 5)))
          );END progn
          (setq escapeVariable T)
        );END if
        (setq escapeVariable T)
      );END if
      (setq escapeVariable T)
    );END if
  );END while
  average

  ; v0.0 - 2017.08.17 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.17
)
(defun DT:Average ( l / wrongInput )
  ; Return a real with the average of the values passed in the list "l"
  ; l [list] - List of integers or reals
  ; Examples:
  ; (DT:Average (list 0 pi))   >>  1.5708
  ; (DT:Average (list 0 3 0))  >>  1.0
  (if (DT:Arg 'DT:Average '((l 'list)))
    (progn
      ; Check passed values are numbers
      (setq checkNumbers (mapcar '(lambda (x) (numberp x)) l))
      (foreach item checkNumbers
        (if (not item) (setq wrongInput T))
      );END foreach

      (if wrongInput
        ; Return an error message
        (DT:Error 'DT:Average "some of the values passed are not numbers")
        ; Calculate and return the average
        (progn
          (setq l (mapcar '(lambda (n) (if (= 'int (type n)) (float n) n)) l ))
          (/ (apply '+ l) (length l))
        );END progn
      );END if
    );END progn
  );END if

  ; v0.0 - 2017.08.17 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.17
)
