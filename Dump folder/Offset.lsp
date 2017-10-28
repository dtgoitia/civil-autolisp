(defun c:xx ()
  ; Trigger
  (DT:AutoLoadFileFromCivilTemp "Offset.lsp")
  (c:MultipleOffset)

  ; v0.0 - 2017.07.27 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.07.27
)
; (defun c:1 ()
;   (DT:OffSet (car (entsel)) 1 (getpoint))
; )
(defun DT:OffSet ( ent_name d p / p1 p2 relAng return )
  ; Offset ent_name entity a "d" distance towards "p" point
  (if (and ent_name d p)
    (if
      (and
        (= 'ename (type ent_name))
        (numberp d)
        (= 'list (type p))
      );END and
      (progn
        (setq
          p1 (vlax-curve-getPointAtParam (vlax-ename->vla-object ent_name) 0)
          p2 (vlax-curve-getPointAtParam (vlax-ename->vla-object ent_name) 1)
          relAng (DT:GetRelativeAngle (angle p1 p) (angle p1 p2))
        );END setq
        (cond
          ((> relAng 0)
            (vla-offset (vlax-ename->vla-object ent_name) (- 0 d))
            (setq return (entlast))
          );END subcond
          ((< relAng 0)
            (vla-offset (vlax-ename->vla-object ent_name) d)
            (setq return (entlast))
          );END subcond
          (t ; p is aligned with selected segment
            (setq return nil)
          );END subcond
        );END cond
        return
      );END progn
      (cond
        ((/= 'ename (type ent_name)) (princ "\nERROR @ DT:OffSet : ent_name is not a ename\n")(princ) )
        ((= nil (numberp d))         (princ "\nERROR @ DT:OffSet : d is not a number\n")      (princ) )
        ((/= 'list (type p))         (princ "\nERROR @ DT:OffSet : p is not a point\n")       (princ) )
      );END cond
    );END if
    (cond
      ((not ent_name) (princ "\nERROR @ DT:OffSet : ent_name=nil\n")(princ) )
      ((not d)        (princ "\nERROR @ DT:OffSet : d=nil\n")       (princ) )
      ((not p)        (princ "\nERROR @ DT:OffSet : p=nil\n")       (princ) )
    );END cond
  );END if

  ; v0.0 - 2017.07.27 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.07.27
)
(defun c:MultipleOffset ( / ename escapeVariable newRelativeOffset relativeOffsetList  )
  ; Command version of DT:MultipleOffset
  (if (setq ename (car (entsel "\nSelect object to offset: ")))
    (if (setq point (getpoint "\nSpecify point on side to offset: "))
      (progn
        (while (not escapeVariable)
          (if (setq newRelativeOffset (getreal "\nAdd new relative offset (press ENTER to end offset list): "))
            (setq relativeOffsetList (append relativeOffsetList (list newRelativeOffset)))
            (setq escapeVariable T)
          );END if
        );END while
        (if relativeOffsetList
          (DT:MultipleOffset ename point relativeOffsetList)
          (DT:Error 'c:MultipleOffset "no relative offsets introduced")
        );END if
      );END progn
    );END if
  );END if

  ; v0.0 - 2017.08.21 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.21
)
(defun DT:MultipleOffset ( ename targetPoint relativeOffsetList / lastEname )
  ; Offset multiple ename by the intervals passed in relativeOffsetList list.
  ; Each offset will be executed over the entity created in the previous step.
  ; ename [ename]             - Entity of reference to start offseting.
  ; targetPoint [2D/3D point] - Point to set the offseting side.
  ; relativeOffsetList [list] - List of offsets (reals) relatives. Each offset
  ;                             will refer to the last created entity.

  (if (DT:Arg 'DT:MultipleOffset '((ename 'ename)(relativeOffsetList 'list)))
    (progn
      ; Get last entity name
      (setq lastEname (entlast))
      (foreach offsetValue relativeOffsetList
        (DT:OffSet ename offsetValue targetPoint)
        (if (/= lastEname (entlast))
          (setq ename (entlast))
          (exit) ; abort!
        );END if
      );END foreach

    );END progn
  );END if

  ; v0.0 - 2017.08.21 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.21
)
