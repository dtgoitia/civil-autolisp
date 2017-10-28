(defun DT:DrawRoadLevel ( insertionPoint level )
  ; Create road level given an insertion point and a level value
  ; insertionPoint [3Dpoint]
  ; level [int or real]
  (if (and insertionPoint level)
    (if
      (and
        (= 3 (length insertionPoint))
        (numberp level)
      );END and
      (entmakex
        (list
          (cons 0 "TEXT")
          (cons 1 (strcat "%%U" (LM:rtos level 2 3)))
          (cons 7 "ROMANS")
          (cons 8 "e-road-levels")
          (cons 10 '(0.0 0.0 0.0))
          (cons 11 insertionPoint)
          (cons 40 0.350)
          (cons 72 1)
        );END list
      );END entmakex
      nil
    );END if
    nil
  );END if

  ; v0.0 - 2017.07.13 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.07.13
)
