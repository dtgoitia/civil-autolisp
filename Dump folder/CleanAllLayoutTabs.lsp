(defun c:CleanAllLayoutTabs ( / layersToRemain deleteCondition layoutName )
  ; Clean all layouts except the ones named on layersToRemain list
  (setq layersToRemain (list
      "Model"
      "Layout1"
    );END list
  );END setq
  (if (= 666 (getint "\nType \"666\" to erase all layout tabs: "))
    (vlax-for x (vla-get-Layouts (vla-get-ActiveDocument (vlax-get-acad-object) ))
      (setq layoutName (vla-Get-Name x))
      (setq deleteCondition T)
      ; If layoutName matches any layout within layersToRemain, set deleteCondition=nil
      (foreach layout layersToRemain
        (if (= layoutName layout) (setq deleteCondition nil))
      );END foreach

      ; Delete layer if deleteCondition=true
      (if deleteCondition (vla-delete x) )
    );END vlax-for
  );END if
  (princ)

  ; v0.0 - 2017.08.02 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.02
)
