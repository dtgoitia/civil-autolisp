(defun c:xx ( / manholeScheduleBlockEntity )
  ; Trigger
  (if (setq pt (getpoint "\nSelect point to extract coordinates"))
    (if (setq manholeScheduleBlockEntity (car (entsel "\nSelect manhole schedule block to udpate coordinates: ")))
      (DT:SetManholeScheduleCoordinates manholeScheduleBlockEntity pt)
    );END if
  );END if

  ; v0.0 - 2017.06.30 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.06.30
)
