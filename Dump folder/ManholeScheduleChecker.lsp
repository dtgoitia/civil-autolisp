(defun c:xx ()
  ; Trigger
  (DT:AutoLoadFileFromCivilTemp "ManholeScheduleChecker.lsp")
  (c:ManholeScheduleCheckerCommand)

  ; v0.0 - 2017.08.02 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.02
)
(defun c:ManholeScheduleCheckerCommand ( / ss result wrongEntityList )
  ; Command
  (if (setq ss (ssget '(( 0 . "INSERT"))))
    (foreach a (ssnamex ss)
      (if (= 'ename (type (cadr a)))
        (progn
          (setq result (DT:ManholeScheduleChecker (cadr a)))
          (if (= 'list (type result))
            (setq wrongEntityList (append wrongEntityList (list result)))
          );END if
        );END progn
      );END if
    );END foreach
  );END if

  (if wrongEntityList
    (foreach wrongBlock wrongEntityList
      (setq msg (strcat "\n" (nth 1 wrongBlock) ": zero value(s) found at "))
      (setq tail (cddr wrongBlock))
      (foreach wrong tail
        (setq msg (strcat msg wrong " "))
      );END foreach
      (setq msg (strcat msg "\n"))
      (princ msg)
      (setq msg nil)
    );END foreach
  );END if
  (princ)

  ; v0.0 - 2017.08.02 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.02
)
(defun DT:ManholeScheduleChecker ( ename / return object attributeList visibilityState attributeTagsToCheck )
  ; Return T if the manhole schedule block passed (ename) doesn't show
  ; any zero pipe size (DN000) or any zero IL (0.000).
  ; If any wrong, return a list containing the entity name and all the incorrect attribute tags

  (if (DT:Arg 'DT:ManholeScheduleChecker '((ename 'ename)))
    (progn
      (setq object (vlax-ename->vla-object ename))
      (setq return (list ename (LM:vl-getattributevalue object "ID")))
      (setq attributeList (LM:vl-getattributes object))
      (setq visibilityState (LM:GetVisibilityState object))
      (cond
        ((= "0" visibilityState) (setq attributeTagsToCheck (list "P0" "IL0")))
        ((= "1" visibilityState) (setq attributeTagsToCheck (list "P0" "IL0" "P1" "IL1")))
        ((= "2" visibilityState) (setq attributeTagsToCheck (list "P0" "IL0" "P1" "IL1" "P2" "IL2")))
        ((= "3" visibilityState) (setq attributeTagsToCheck (list "P0" "IL0" "P1" "IL1" "P2" "IL2" "P3" "IL3")))
        ((= "4" visibilityState) (setq attributeTagsToCheck (list "P0" "IL0" "P1" "IL1" "P2" "IL2" "P3" "IL3" "P4" "IL4")))
      );END cond
      (if attributeTagsToCheck
        (foreach attributeTag attributeTagsToCheck
          ; Check pipe sizes
          (if (= "P" (substr attributeTag 1 1) )
            (if (= "DN000" (setq kk (cdr (assoc attributeTag attributeList))))
              (setq return (append return (list attributeTag)))
            );END if
          );END if
          ; Check IL
          (if (= "IL" (substr attributeTag 1 2) )
            (if (= "0.000" (setq kk (cdr (assoc attributeTag attributeList))))
              (setq return (append return (list attributeTag)))
            );END if
          );END if
        );END foreach
      );END subcond
    );END progn
  );END if
  (if (< 2 (length return)) return)

  ; v0.0 - 2017.08.02 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.02
)
