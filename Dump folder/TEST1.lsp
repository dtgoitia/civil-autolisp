(defun c:xx ()
  ; Trigger
  (DT:AutoLoadFileFromCivilTemp "TEST1.lsp")
  (c:TESTFUNCTION)

  ; v0.0 - 2017.08.01 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.01
)
(defun c:TESTFUNCTION ( / ename manSchedule )
  ; TEST FUNCTION
  (while (not manSchedule)
    (if (setq ename (car (entsel "\nSelect Manhole Schedule block to update coordinates: ")))
      (progn
        (princ "object selected.\n")
        (if (= "ManScheduleBody" (LM:effectivename (vlax-ename->vla-object ename)))
          (progn
            (setq manSchedule ename)
            T
          );END progn
          (progn
            (alert "Selected object is not a Manhole Schedule Block.")
            (exit)
          );END progn
        );END if
      );END progn
      (princ "missed. Try again.")
    );END if
  );END while

  ; v0.0 - 2017.08.01 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.01
)
