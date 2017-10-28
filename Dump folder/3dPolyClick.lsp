(defun c:xx ()
  ; Trigger
  (DT:AutoLoadFileFromCivilTemp "3dPolyClick.lsp")
  (c:111)

  ; v0.0 - 2017.08.14 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.14
)
(defun c:111 ( / escapeVariable xy z)
  (princ "\n3D POLYLINE CLICKING LEVELS\n")
  (command "_.3dpoly")
  (while (not escapeVariable)
    (command
      (if (setq xy (getpoint "\nSelect XY position: "))
        (progn
          (setq return
          (list
            (nth 0 xy)
            (nth 1 xy)
            (progn
              (if (setq z (DT:clic_or_type_level))
                (progn
                  (princ (strcat "\nXY = (" (LM:rtos (nth 0 xy) 2 3) " " (LM:rtos (nth 1 xy) 2 3) ")\n z = " (LM:rtos z 2 3) "\n"))
                  z
                );END progn
                (progn
                  (princ "\nNo level selected! z = 0.000\n")
                  0
                );END progn
              );END if
            );END progn
          );END list
          )
          (princ "\nreturn = ")(princ return)
          return
        );END progn
        (progn
          (setq escapeVariable T)
          ^C
        );END progn
      );END if
    );END command
  );END while
  (command ^C ^C)
  (princ)
)
