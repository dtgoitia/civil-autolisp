(defun c:xx ( / DT:CreateTextEntiy doc currentSpaceIndex textSize gr ent )
  ; Trigger
  (defun DT:CreateTextEntiy ( pt textSize )
    (entmakex
      (list
        (cons 0 "TEXT")
        (cons 1 "content")
        (cons 10 '(0.0 0.0 0.0))
        (cons 11 pt)
        (cons 40 textSize)
        (cons 72 1)
      );END list
    );END entmakex
  )

  (setq currentSpaceIndex (DT:GetCurrentSpace))
  (cond
    ((= 0 currentSpaceIndex) (setq textSize 1) ) ; Model space
    ((= 1 currentSpaceIndex) (setq textSize 4) ) ; Paper Space
    ((= 2 currentSpaceIndex) (setq textSize 1) ) ; Paper Space, inside a locked viewport
    ((= 3 currentSpaceIndex) (setq textSize 1) ) ; Paper Space, inside an unlocked viewport
  );END cond

  ; (DT:GetActiveViewportScale)
  (while (= 5 (car (setq gr (grread 't 13 0))))
    (if ent
      (progn
        (vla-delete (vlax-ename->vla-object ent))
        (setq ent (DT:CreateTextEntiy (cadr gr) textSize))
      );END progn
      (setq ent (DT:CreateTextEntiy (cadr gr) textSize))
    );END if
  );END while

  (if ent (vla-delete (vlax-ename->vla-object ent)))


  ; v0.0 - 2017.07.05 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.07.05
)
