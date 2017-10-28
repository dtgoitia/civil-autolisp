; (defun c:xx ()
;   ; Trigger
;   (DT:AutoLoadFileFromCivilTemp "BoundingBox.lsp")
;   (c:GetBoundingBox)
;
;   ; v0.0 - 2017.07.28 - First issue
;   ; Author: David Torralba
;   ; Last revision: 2017.07.28
; )
(defun c:GetBoundingBox ( / ename xy )
  ; Command to draw selected object boundary box
  (if (setq ename (car (entsel)))
    (if (setq xy (DT:GetBoundingBox ename))
      (progn
        (DT:PrintVar 'xy)
        (command "_.rectangle" (nth 0 xy) (nth 1 xy))
      );END progn
    );END if
  );END if


  ; v0.0 - 2017.07.28 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.07.28
)
(defun DT:GetBoundingBox ( ename / minPoint maxPoint )
  ; Return ename entity's bounding box coordinates in a list: (minPoint maxPoint)
  ; ename [ename] - Entity name from which to obtain the boundary box
  (if (DT:Arg 'DT:GetBoundingBox '((ename 'ename)))
    (progn
      ; Assign rectangle coordinates to minPoint and maxPoint
      (vla-GetBoundingBox (vlax-ename->vla-object ename) 'minPoint 'maxPoint)
      ; Return values, if any
      (if (and minPoint maxPoint)
        (list (vlax-safearray->list minPoint) (vlax-safearray->list maxPoint))
      );END if
    );END progn
  );END if

  ; v0.0 - 2017.07.28 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.07.28
)
