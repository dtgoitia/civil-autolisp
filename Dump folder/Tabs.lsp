(defun DT:GetLayoutObjectByName ( name )
  ; Return the layout object (if any) of the tab called "name"
  (if name
    (if (= 'str (type name))
      (vla-item (vla-get-layouts (vla-get-activedocument (vlax-get-acad-object))) name)
      (progn (princ "\nERROR @ DT:GetLayoutObjectByName : name is not a string\n") nil )
    );END if
    (progn (princ "\nERROR @ DT:GetLayoutObjectByName : area=nil\n") nil )
  );END if

  ; v0.0 - 2017.05.05 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.05.05
)
(defun c:CleanLayouts ()
  (vlax-for layout (vla-get-layouts (vla-get-activedocument (vlax-get-acad-object)))
    (if (= :vlax-false (vlax-get-property layout 'ModelType))
      (progn
        (princ "\n")
        (princ (vlax-get-property layout 'Name))
        (getint)
        (vla-delete layout)
      );END progn
    );END if
  )

  ; v0.0 - 2017.05.05 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.05.05
)
(defun c:xx ( / tabObjectList tabList i )
  ; Rename tabs adding project name at the beginnig

  ; Get all tabs
  (setq tabObjectList (vla-get-Layouts (vla-get-ActiveDocument (vlax-get-acad-object) )))

  ; Get list of pairs with "tab name"-"tab object"
  (vlax-for x tabObjectList
    (setq tabList (append tabList (list (cons (vla-get-Name x) x))))
  );END vlax-for

  ; Run through list and do changes with conditions
  (if tabList
    (foreach tabPair tabList
      (vlax-put-property (cdr tabPair) 'Name (strcat "5396-" (car tabPair)) )
    );END foreach
  );END if
  (princ)

  ; v0.0 - 2017.06.09 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.06.09
)
