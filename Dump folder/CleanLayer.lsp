(defun c:xx ()
  ; Step one by one all layer objects and ask if to remove
  (foreach a (ssnamex (ssget "x" '((8 . "_PointsWithoutLevels"))))
    (if (= 'ename (type (cadr a)))
      (progn
        (princ "\n")
        (princ (entget (cadr a)))
        (DT:ZoomToEntity (cadr a))
        (initget "Yes No")
        (setq ans (getkword "\nDo you want to delete it [Yes/No]? <Yes>"))
        (if (or (= ans "Yes") (not ans))
          (vla-delete (vlax-ename->vla-object (cadr a)))
        );END if
      );END progn
    );END if
  );END foreach

  ; v0.0 - 2017.06.23 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.06.23
)
(defun DT:InputLayerPickOrType ( / answer layerName ent_name )
  ; Allow user to select a layer by picking an object or typing layer name

  (initget "Type Pick")
  (setq answer (getkword "\nSelect input mode [Pick/Type] <Pick>:"))
  (if (= "Type" answer)
    (if (setq layerName (getstring 't "\nType target layer name:"))
      (if (tblsearch "layer" layerName)
        layerName
        nil
      );END if
    );END if
    (if (setq ent_name (car (entsel "\nSelect object of target layer:")))
      (cdr (assoc 8 (entget ent_name)))
    );END if
  );END if

  ; v0.0 - 2017.06.27 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.06.27
)
(defun c:xx ( / layerName )
  ; Get layer
  (setq layerName (DT:InputLayerPickOrType))
  ; check selected layer is not current, if it is, return error message and abort
  (if (= layerName (getvar "clayer"))
    (progn
      (princ "\nSelected layer is the current active layer. Please change the layer and try again.\n")
      (alert "Selected layer is the current active layer.\nPlease change the layer and try again.")
      (exit)
    );END progn
  );END if

  ; run through layer objects asking to delete them or not
  (initget "No Yes")
  (setq answer (getkword "\nAsk in each object before delete [Yes/No] <Yes>:"))
  (if (= "No" answer)
    (setq return (DT:CleanLayerAtOnce layerName))
    (setq return (DT:CleanLayerStepByStep layerName))
  );END if

  ; Check return and end function
  (cond
    ((= T return)
      nil
    );END subcond
    ((= 'list (type return))
      (princ
        (strcat
          "\n"
          (itoa (cdr (nth 1 return)))
          " elements deleted out of "
          (itoa (cdr (nth 0 return)))
          " checked.\n"
        );END strcat
      );END
    );END subcond
    (t
      (princ "\nUnexpected return from deleting functions... :S\n")
      (DT:PrintVar 'return)

    );END subcond
  );END cond

  (princ)

  ; v0.0 - 2017.06.27 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.06.27
)
(defun DT:CleanLayerAtOnce ( layerName / ent_name i ii )
  ; Delete all objects in "layerName" layer without asking
  ; If succesfull, returns a pair list as so:
  ;    ( ("Checked" . X) ("Deleted" . Y) )
  ; being:
  ;   - X: number of entities checked
  ;   - Y: number of entities removed

  (if (DT:Arg 'DT:CleanLayerAtOnce '((layerName 'str)))
    (progn
      ; Get first entity
      (setq ent_name (entnext))
      (setq i 1)
      (setq ii 0)
      ; Delete if it's in "layerName" layer
      (if (= layerName (cdr (assoc 8 (entget ent_name))))
        (if (DT:Delete ent_name)
          ; If entity has been deleted and confirmed
          (setq ii (+ ii 1))
        );END if
      );END if
      (while (setq ent_name (entnext ent_name))
        (setq i (+ i 1))
        ; Delete if it's in "layerName" layer
        (if (= layerName (cdr (assoc 8 (entget ent_name))))
          (if (DT:Delete ent_name)
            ; If entity has been deleted and confirmed
            (setq ii (+ ii 1))
          );END if
        );END if
      );END while

      ; Return report
      (if (and i ii)
        (list (cons "Checked" i) (cons "Deleted" ii))
      );END if
    );END progn
    nil
  );END if

  ; v0.0 - 2017.06.27 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.06.27
)
(defun Dt:CleanLayerStepByStep ( layerName / i total ss entityList ent_name ans )
  ; Delete all objects in "layerName" layer asking in each one
  ; Return T if everything went as expected,
  ; otherwise return nil
  (if (DT:Arg 'DT:CleanLayerAtOnce '((layerName 'str)))
    (progn
      (setq i 0)
      (if (setq ss (ssget "x" (list (cons 8 layerName)) ))
        (progn
          ; Extract entity names form the selection set and put them in a list
          (foreach a (ssnamex ss)
            (if (= 'ename (type (cadr a)))
              (setq entityList (append entityList (list (cadr a))))
            );END if
          );END foreach

          ; If any entity selected,
          (if entityList
            (progn
              ; Count amount of entities in the selection
              (setq total (length entityList))
              ; Run through every entity
              (foreach ent_name entityList
                (setq i (+ i 1))
                ; If the user is inside a viewport, alert him the zoom option might not work properly
                (if (and (= 3 (DT:GetCurrentSpace)) (not warningDone01))
                  (progn
                    (alert "--- WARNING --- You are inside an unloked viewport.\nZoom option might work unexpectedly.")
                    (setq warningDone01 T)
                  );END progn
                );END if
                ; Ask user if wants to delete the entity or not, or zoom
                (initget "Yes No Zoom")
                (setq ans (getkword (strcat "\n" (itoa i) "/" (itoa total) ": " (DT:EntityType ent_name) " found. Do you want delete it? [Yes/Zoom/No] <Yes>")))
                ; Zoom to the entity
                (cond
                  ((or (= ans "Yes") (not ans))
                    (vla-delete (vlax-ename->vla-object ent_name))
                  );END subcond
                  ((= ans "Zoom")
                    ; Zoom to entity
                    (DT:ZoomToEntity ent_name)
                    ; Ask user if wants to delete the entity or not
                    (initget "Yes No")
                    (setq ans (getkword "\nDo you want delete it? [Yes/No] <Yes>"))
                    ; Delete the entity if so
                    (if (/= ans "No") (vla-delete (vlax-ename->vla-object ent_name)) )
                  );END subcond
                  (t
                    nil
                  );END subcond
                );END cond

                ; Reset answer value for next loop
                (setq ans nil)
              );END foreach

              ; Return T, as the routine reached the end without errors
              T
            );END progn
          );END if
        );END progn
      );END if
    );END progn
    nil
  );END if

  ; v0.0 - 2017.06.27 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.06.27
)
