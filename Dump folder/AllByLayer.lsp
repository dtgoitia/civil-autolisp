(defun c:AllByLayer ( / *error* activeDocument lockedLayersFound )
  (setq activeDocument (vla-get-ActiveDocument (vlax-get-acad-object)))
  (vla-startUndoMark activeDocument)
  (if (DT:AnyUnlockedLayers)
    (progn
      (setq lockedLayersFound T)
      (defun *error* () (command "_.layerp") (vla-endUndoMark activeDocument))
      (command "_.-layer" "u" "*" "")
    );END progn
  );END if
  ; (setq ss (ssget "x"))
  (command "._SetByLayer" (ssget "x") "" "y" "y")
  (if lockedLayersFound (command "_.layerp") )
  (vla-endUndoMark activeDocument)
  (princ)

  ; v0.0 - 2017.10.09 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.10.09
)
(defun DT:AnyUnlockedLayers ( / return )
  ; Return T if any layer is locked
  (vlax-for l (vla-get-layers (vla-get-activedocument (vlax-get-acad-object)))
    (if (vlax-get-property l 'Lock)
      (if (= :vlax-true (vlax-get-property l 'Lock))
        (setq return T)
      );END if
    );END if
  );END vlax-for
  return

  ; v0.0 - 2017.10.09 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.10.09
)
