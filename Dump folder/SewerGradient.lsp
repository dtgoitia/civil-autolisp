(defun DT:UpdatePrivateDrainageLabel ( round / p1 p2 z1 z2 msg gradient sewerLabel )
  ; Update private drainage label
  ; if round  = nil --> don't round
  ; if round != nil --> rounded to nearest "round"

  ; INPUT - Point 1
  (if (not (setq p1 (getpoint "\nSelect point A: ")))
    (if (not INT_lastPoints)
      ; Force user to input a point if there is no INT_lastPoints saved yet
      (while (not p1) (setq p1 (getpoint "\nSelect point A: ")) )
      (setq
        p1 (nth 0 INT_lastPoints)
        z1 (nth 1 INT_lastPoints)
        p2 (nth 2 INT_lastPoints)
        z2 (nth 3 INT_lastPoints)
      )
    );END if
  );END if

  ; INPUT - Ask for z1, p2 and z2, if not taken from INT_lastPoints
  (if (or (not z1) (not p2) (not z2))
    (if (setq z1 (DT:clic_or_type_level))
      (if (setq p2 (getpoint (strcat "\nSelect point B: " (setq msg (strcat "\nLevel A = " (LM:rtos z1 2 3) "m")) )))
        (if (setq z2 (DT:clic_or_type_level))
          (progn
            (setq msg (strcat msg "\nLevel B = " (LM:rtos z2 2 3) "m" ))
            (if (= z1 z2)
              (setq msg (strcat "\nSelected points are at the same level (" (LM:rtos z1 2 3) "m)."))
              (if round
                (progn
                  (setq
                    ; Save input as global variables
                    INT_lastPoints (list p1 z1 p2 z2)
                    ; Get gradient
                    gradient (abs (DT:Gradient (list (nth 0 p1) (nth 1 p1) z1) (list (nth 0 p2) (nth 1 p2) z2) ) )
                    msg (strcat msg
                      "\nGradient = 1/" (LM:rtos gradient 2 0) " (" (LM:rtos (/ 100 gradient) 2 2) "%)"
                      "\n                ~ 1/" (LM:rtos (DT:RoundTo gradient round) 2 0) " (" (LM:rtos (/ 100 (DT:RoundTo gradient round)) 2 2) "%)"
                    );END strcat
                  );END setq

                    ; Select sewer label
                    (if (setq sewerLabel (car (entsel (strcat "\nSelect pipe label: \n" msg))))
                      (if (setq sewerSize (DT:GetSewerSize sewerLabel))
                        (cond
                          ((= 100 sewerSize)
                            (if (> gradient 80)
                              (progn (princ "\nDN100 flatter than 1/80. WRONG\n") nil)
                              (if round
                                (DT:ChangePrivateSewerGradient sewerLabel (DT:RoundTo gradient round))
                                (DT:ChangePrivateSewerGradient sewerLabel gradient)
                              );END if
                            )
                          );END subcond
                          ((= 150 sewerSize)
                            (if round
                              (DT:ChangePrivateSewerGradient sewerLabel (DT:RoundTo gradient round))
                              (DT:ChangePrivateSewerGradient sewerLabel gradient)
                            );END if
                          );END subcond
                          (t
                            (alert "I only can handle DN100 and\nDN150 pipe sizes... Sorry! :P")
                          );END subcond
                        );END cond
                        nil
                      );END if
                      nil
                    );END if
                );END progn
                (setq msg (strcat "\nGradient = 1/" (LM:rtos gradient 2 0) " (" (LM:rtos (/ 100 gradient) 2 2) "%)"))
              );END if
            );END if
          );END progn
        );END if
      );END if
    );END if
  );END if

  ; v0.0 - 2017.05.31 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.05.31
)
