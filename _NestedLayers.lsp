(defun c:1() (c:DNLA))
(defun c:DNLA( / info infoString infoStringSize txt)
  ; Deep Nested Layer information
  (setq
    info (NestedEntityNames (getpoint "\nSelect object to see its nested layers:"))
    infoString (ChangeEnameForLevel info)
    infoStringSize (GetColumnLength infoString)
    txt (NL_GetTextFormatted infoString infoStringSize)
  )
  (princ txt)
  (princ)
)
(defun LM:SubstNth ( a n l / i )
    (setq i -1)
    (mapcar '(lambda ( x ) (if (= (setq i (1+ i)) n) a x)) l)
)
(defun EntityInfo (ent_name / blockName blockType )
  ; Return a list with useful information of the nested object
  ; (ename (Block/Xref Name Layer))
  (if (= 'ename (type ent_name))
    (if (= "INSERT" (cdr (assoc 0 (entget ent_name))))
      (progn
        (if (setq blockName (LM:effectivename (vlax-ename->vla-object ent_name)) )
          (if (assoc 1 (tblsearch "block" blockName))
            (setq blockType "XREF")
            (setq blockType "BLOCK")
          );END if
        );END if
        (list ent_name blockType blockName (cdr (assoc 8 (entget ent_name))) )
      );END progn
      (list ent_name (cdr (assoc 0 (entget ent_name))) "-" (cdr (assoc 8 (entget ent_name))) )
    );END if
    nil
  );END if
)
(defun NestedEntityNames ( pt / ent_name i info )
  ; Returns a list with nested entity names, being the parent first one and deepest child last one
  (if (= 'list (type pt)) ; if point provided, continue
    (if (setq ent_name (nentselp pt)) ; if entity selected, continue
      (progn
        (setq
          i 0
          info (list (list "Level" "Type" "Block name" "Object layer") (EntityInfo (car ent_name)) )
        )
        (if (nth 3 ent_name) ; if any nested entity
          (progn
            (setq nestedLevels (length (nth 3 ent_name)) )
            (foreach x (nth 3 ent_name)
              (setq
                i (+ i 1)
                info (append info (list (EntityInfo x)))
              )
            );END foreach
          );END progn
        );END if
        info
      );END progn
    );END if
  );END if
)
(defun ChangeEnameForLevel ( info / i x )
  ; Return info replacing ename for level
  (if (= 'list (type info))
    (progn
      (setq i -1)
      (foreach a info
        (setq
          x (append x (list (if (< i 0) a (LM:SubstNth (itoa i) 0 a)) ))
          i (+ i 1)
        )
      );END foreach
      x
    );END progn
  );END if
)
(defun GetColumnLength ( infoString / q col )
  ; Return list with the biggest length needed on each column
  (if (= 'list (type infoString))
    (progn
      ; initialise an zero list to contain each column length
      (repeat (length (car infoString))
        (setq col (append col (list 0)) )
      )

      ; populate the list col with the biggest lengths found
      (mapcar
        '(lambda ( x )
          (setq q nil)
          (mapcar '(lambda ( xx yy ) (setq q (append q (list (max (strlen xx) yy)) )) ) x col )
          (setq col q)
        );END lambda
        infoString
      );END mapcar
      col
    );END progn
  );END if
)
(defun NL_GetTextFormatted ( infoString infoStringSize / txt s )
  ; Format text correctly and concatenate it
  (setq txt "\n")
  (mapcar
    '(lambda (x)
      (setq txt (strcat txt "\n"))
      (mapcar
        '(lambda (xx yy)
          (setq s " ")
          (if (< (strlen xx) yy)
            (setq txt (strcat txt xx (repeat (+ 1 (- yy (strlen xx))) (setq s (strcat s " "))) ))
            (setq txt (strcat txt xx "  "))
          );END if
        );END lambda
        x infoStringSize
      );END mapcar
    );END lambda
    infoString
  );END mapcar
  txt
)
