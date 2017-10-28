; (defun c:xx ()
;   ; Trigger
;   (DT:AutoLoadFileFromCivilTemp "ManholeScheduleUpdateValues.lsp")
;   (c:ManholeScheduleResetHiddenValues)
;
;   ; v0.0 - 2017.07.28 - First issue
;   ; Author: David Torralba
;   ; Last revision: 2017.07.28
; )
(defun c:ManholeSchedulePipeSize ( / ename msg asnwer pipeSize )
  ; Update manhole schedule pipe size
  (if (setq ename (car (nentselp)))
    (progn
      ; Get pipe size value
      (if ManholeSchedulePipeSize
        (setq msg (strcat "\nInput pipe size <" (itoa ManholeSchedulePipeSize) ">: "))
        (setq msg (strcat "\nInput pipe size: "))
      );END if
      (if (setq answer (getint msg))
        (setq ManholeSchedulePipeSize answer)
        (if (not ManholeSchedulePipeSize)
          (while (not ManholeSchedulePipeSize) (setq ManholeSchedulePipeSize (getint msg)))
        );END if
      );END if

      ; Format and convert value to a string
      (cond
        ((= 0 ManholeSchedulePipeSize)
          (setq pipeSize "000")
        );END subcond
        (t
          (setq pipeSize (itoa ManholeSchedulePipeSize))
        );END subcond
      );END cond

      ; Update selected entity (ename) value
      (if (DT:SetText ename (strcat "DN" pipeSize))
        (princ)
      );END if
    );END progn
  );END if

  ; v0.0 - 2017.07.28 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.07.28
)
(defun c:ManholeScheduleResetIlValue ( / ename )
  ; Set selected entity text value to "0.000"
  (if (setq ename (car (nentselp)))
    (if (DT:SetText ename "0.000")
      (princ)
    );END if
  );END if

  ; v0.0 - 2017.07.28 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.07.28
)
(defun c:ManholeScheduleResetHiddenValues ( / ss )
  ; Set selected manholeSchedule blocks' hidden pipe size and IL values to default value.

  (if (setq ss (ssget))
    (foreach a (ssnamex ss)
      (if (= 'ename (type (cadr a)))
        (if (= "ManScheduleBody" (LM:EffectiveName (vlax-ename->vla-object (cadr a))))
          (DT:ManholeScheduleResetHiddenValues (cadr a))
        );END if
      );END if
    );END foreach
  );END if

  ; v0.0 - 2017.07.28 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.07.28
)
(defun DT:ManholeScheduleResetHiddenValues ( ename / object visibilityState )
  ; Set selected manholeSchedule block's hidden pipe size and IL values to default value.

  (if (DT:Arg 'DT:ManholeScheduleResetHiddenValues '((ename 'ename)))
    (progn
      (setq object (vlax-ename->vla-object ename))
      (setq visibilityState (LM:GetVisibilityState object))
      (cond
        ( (= "0" visibilityState)
          (setq
            attributeValueList (list
              (cons "IL1" "0.000")(cons "IL2" "0.000")(cons "IL3" "0.000")(cons "IL4" "0.000")
              (cons "P1"  "DN000")(cons "P2"  "DN000")(cons "P3"  "DN000")(cons "P4"  "DN000")
            );END list
          );END setq
        );END subcond
        ( (= "1" visibilityState)
          (setq
            attributeValueList (list
              (cons "IL2" "0.000")(cons "IL3" "0.000")(cons "IL4" "0.000")
              (cons "P2"  "DN000")(cons "P3"  "DN000")(cons "P4"  "DN000")
            );END list
          );END setq
        );END subcond
        ( (= "2" visibilityState)
          (setq
            attributeValueList (list
              (cons "IL3" "0.000")(cons "IL4" "0.000")
              (cons "P3"  "DN000")(cons "P4"  "DN000")
            );END list
          );END setq
        );END subcond
        ( (= "3" visibilityState)
          (setq
            attributeValueList (list
              (cons "IL4" "0.000")
              (cons "P4"  "DN000")
            );END list
          );END setq
        );END subcond
        ( (= "4" visibilityState)
          (princ "\nNo IL hidden, nothing to reset.")
        );END subcond
      );END cond

      (if attributeValueList
        (LM:vl-setattributevalues object attributeValueList)  ; Reset attributes
        (princ "\nNo IL hidden, nothing to reset.")           ; Return warning message
      );END if

      (princ)
    );END progn
  );END if

  ; v0.0 - 2017.07.28 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.07.28
)
