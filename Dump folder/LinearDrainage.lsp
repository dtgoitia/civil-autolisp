; (defun c:xx ()
;   ; Trigger
;   (DT:AutoLoadFileFromCivilTemp "LinearDrainage.lsp")
;   (DT:AutoLoadFileFromCivilTemp "Offset.lsp")
;   (DT:AutoLoadFileFromCivilTemp "PointUtils.lsp")
;   (c:linearDrainage)
;
;   ; v0.0 - 2017.07.27 - First issue
;   ; Author: David Torralba
;   ; Last revision: 2017.07.27
; )

; Load auxiliary libraries
(DT:AutoLoadFileFromCivilTemp "Offset.lsp")
(DT:AutoLoadFileFromCivilTemp "PointUtils.lsp")

(defun c:linearDrainage ( / p1 p2 linearDrainageTextEname linearDrainageLineEname )
  (setq p1 (getpoint))
  (setq p2 (getpoint))
  ; Loop
  (while (= 5 (car (setq gr (grread 't 13 0))))
    (DT:GrreadLinearDrainageLine gr p1 p2)
    (DT:GrreadLinearDrainageText gr (angle p1 p2) (DT:MidPoint p1 p2))
  );END while

  (princ)

  ; v0.0 - 2017.07.27 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.07.27
)
(defun DT:GrreadLinearDrainageLine ( gr p1 p2 / x )
  (if (and linearDrainageLineEname (entget linearDrainageLineEname))
    (vla-delete (vlax-ename->vla-object linearDrainageLineEname))
  );END if
  (setq linearDrainageLineEname (entmakex (append (list (cons 0 "LWPOLYLINE") (cons 100 "AcDbEntity") (cons 100 "AcDbPolyline") (cons 70 0) (cons 90 (length (list p1 p2)))) (mapcar '(lambda (pt) (cons 10 pt)) (list p1 p2) ))))
  (vlax-put-property (vlax-ename->vla-object linearDrainageLineEname) 'ConstantWidth 0.2)
  (setq x (DT:OffSet linearDrainageLineEname 0.1 (cadr gr)))
  (vla-delete (vlax-ename->vla-object linearDrainageLineEname))
  (setq linearDrainageLineEname x)

  ; v0.0 - 2017.07.27 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.07.27
)
(defun DT:GrreadLinearDrainageText ( gr ang pt / relAng ang2 justification )
  ; Destroy and create entity
  (if (and linearDrainageTextEname (entget linearDrainageTextEname))
    (vla-delete (vlax-ename->vla-object linearDrainageTextEname))
  );END if
  ; Calculate relative angle to see in which side is the mouse (gr)
  (setq relAng (DT:GetRelativeAngle ang (angle pt (cadr gr))) )
  (cond
    ; Base  on relative angle sign (+/-), calculate:
    ;  - angle correction (ang2)
    ;  - text justification
    ( (> relAng 0)
      (setq ang2 (* -0.5 pi))
      (setq justification (list (cons 71 2)(cons 72 5)(cons 73 1)))
    )
    ( (< relAng 0)
      (setq ang2 (*  0.5 pi))
      (setq justification (list (cons 71 8)(cons 72 5)(cons 73 1)))
    )
    (t
      (setq ang2 (*  0.5 pi))
      (setq justification (list (cons 71 8)(cons 72 5)(cons 73 1)))
    )
  );END cond
  (setq ; Build line
    linearDrainageTextEname (entmakex (append
        (list
          (cons 0 "MTEXT")
          (cons 100 "AcDbEntity")
          (cons 100 "AcDbMText")
          (cons 1 "Linear drainage")            ; Content
          (cons 10 (polar pt (+ ang ang2) 0.3)) ; Insertion point
          (cons  7 "ROMANS")                    ; Style
          (cons 40 0.3)                         ; Text height
          (cons 50 (DT:ReadableTextAngle ang))  ; Rotation
        );END list
        justification
      );END append
    );END entmakex
  );END setq

  ; v0.0 - 2017.07.27 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.07.27
)
