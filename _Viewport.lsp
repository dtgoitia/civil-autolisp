(defun c:lk ( / space )
  ; Lock viewport
  ; Inside a viewport: lock current viewport
  ; In paperspace: select viewport(s) to lock

  ; Get current situation
  (setq space (DT:GetCurrentSpace))

  (cond
    ((= space 0)
      (princ "\nNo viewports to lock around...")
    );END subcond
    ((= space 1)
      (princ "\nYou are in paper space. No viewport selected")
      (princ "\nSelect viewport's to lock/unlock")
      ; if some lock and others unlock, prompt message asking for what to do (lock all, unlock all or alternate all?), if not, execute

    );END subcond
    ((= space 2)
      ; Current space: Paper Space, inside a locked viewport. Unlock it
      (vlax-put-property (vla-get-ActivePViewport (vla-get-activedocument (vlax-get-acad-object))) 'DisplayLocked :vlax-false)
      (princ "\nViewport unlocked.")
    );END subcond
    ((= space 3)
      ; Current space: Paper Space, inside an unlocked viewport. Lock it
      (vlax-put-property (vla-get-ActivePViewport (vla-get-activedocument (vlax-get-acad-object))) 'DisplayLocked :vlax-true)
      (princ "\nViewport locked.")
    );END subcond
  );END cond

  (princ)

  ; v0.0 - 2017.05.08 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.05.08
);END defun
(defun DT:GetCurrentSpace ()
  ; Return 0, 1 or 2 regarding user current space situation
  ;  0 - Model space
  ;  1 - Paper Space
  ;  2 - Paper Space, inside a locked viewport
  ;  3 - Paper Space, inside an unlocked viewport

  (if (= "Model" (getvar "ctab"))
    0
    (if (= 1 (getvar "cvport"))
      1
      (if (= (vlax-get-property (vla-get-ActivePViewport (vla-get-activedocument (vlax-get-acad-object))) 'DisplayLocked) :vlax-true)
        2
        3
      );END if
    );END if
  );END if

  ; v0.0 - 2017.05.08 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.05.08
)
