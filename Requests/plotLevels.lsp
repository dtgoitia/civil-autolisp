(defun c:plotLevels ( / fflEntity plotEntity scapeVariable )
  ; FFL - 0.150m
  (if (setq fflEntity (car (entsel "\nSelect FFL label: \n")))
    (if (= "TEXT" (cdr (assoc 0 (entget fflEntity))))
      (while (not scapeVariable)
        (princ "\nOK")
        (if (setq plotEntity (car (entsel "\nSelect plot level (press ESC to exit): \n")))
          (DT:SetText plotEntity (strcat "%%U" (LM:rtos (- (atof (substr (DT:GetText fflEntity) 4)) 0.150 ) 2 2)))
          (setq scapeVariable T)
        );END if
      );END while
      (princ "\nOoops! I need a TEXT (MTEXT is not a TEXT)")
    );END if
    (princ "\nNothing selected. Try again!")
  );END if
  (princ)

  ; v0.0 - 2017.06.01 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.06.01
)
