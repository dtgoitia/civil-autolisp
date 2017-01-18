(defun c:1()
  ; TODO - Extract all the information on a list with a function
  ; TODO - Create a function to process the list above and format it to print it
  (princ "\nCheck nested layers:\n")
  (setq pt (getpoint) )
  (NestedEntityNames pt)
)
(defun ReturnNestedBlockInfo (ent_name / blockName blockType blockLayer )
  ; Return a list with useful information of the nested object
  (if ent_name
    (if (= "INSERT" (cdr (assoc 0 (entget ent_name))))
      (progn
        (if (setq blockName (LM:effectivename (vlax-ename->vla-object ent_name)) )
          (if (assoc 1 (tblsearch "block" blockName))
            (setq blockType "Xref")
            (setq blockType "Block")
          );END if
        );END if
        (list blockType blockName blockLayer )
      );END progn
      (princ "\nThis entity is not a block.")
    );END if
  );END if
)
(defun DT:PrintLayerInfo (ent_name i)
  ; Returns a string with layer information
  (princ "\n")
  ;(princ ent_name)
  ;(princ " - ")
  (princ "Level ")
  (princ i)
  (princ " - Layer: ")
  (princ (DT:nla ent_name))
  (princ " - ")
  (if (= "INSERT" (cdr (assoc 0 (entget ent_name))) )
    (princ (ReturnNestedBlockInfo ent_name))
    (princ (cdr (assoc 0 (entget ent_name))))
  );END if
)
(defun NestedEntityNames ( pt / a i )
  ; Returns a list with nested entity names, being the parent first one and deepest child last one
  (if pt ; if point provided, continue
    (if (setq a (nentselp pt)) ; if entity selected, continue
      (progn
        (DT:PrintLayerInfo (car a) (setq i 0))
        (if (nth 3 a) ; if nested entity
          (progn
            (setq
              nestedLevels (length (nth 3 a))
            )
            (foreach x (nth 3 a)
              (setq i (+ i 1))
              (DT:PrintLayerInfo x i)
            );END foreach
          );END progn
        );END if
        (princ)
      );END progn
    );END if
  );END if
)
