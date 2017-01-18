(defun c:1( / info infoString infoStringSize i a b x y xx yy q qq)
  ; TODO - Extract all the information on a list with a function
  ; TODO - Create a function to process the list above and format it to print it
  (princ "\nCheck nested layers:\n")
  (setq
    ;pt (getpoint)
    info (NestedEntityNames pt)
    i -1
  )
  (princ "\nNON-FORMAT:")
  (foreach a info
    (princ "\n")
    (foreach b a
      (princ "\t")(princ b)
      (princ " ")
    );END foreach
  );END foreach

  (princ "\n.\nFORMATED:")
  ; Change enames for levels in text format:
  (foreach a info
    (setq
      infoString (append infoString (list (if (< i 0) a (LM:SubstNth (itoa i) 0 a)) ))
      infoStringSize (append infoStringSize '((0 0 0 0)))
      i (+ i 1)
    )
  );END foreach

  (foreach a infoString
    (princ "\n")
    (foreach b a
      (princ "\t")(princ b)
      (princ " ")
    );END foreach
  );END foreach

  (princ "\ninfoStringSize = ")(princ infoStringSize)
  ; Measure each field length
  (setq
    infoStringSize
    (mapcar
      '(lambda ( x y )
        (princ "\n")(princ x)(princ " ")(princ y)(princ ":\n")
        (setq qq nil)
        (mapcar
          '(lambda ( xx yy )
            (princ "\n\t\t\t")
            (princ "(max (strlen xx) (strlen (itoa yy))) = ")(princ (max (strlen xx) (strlen (itoa yy))))
            (setq qq (append qq (list (max (strlen xx) (strlen (itoa yy)))) ))
          );END lambda
          x y
        );END mapcar
        (setq q (append q (list qq)))
        (princ "\n")(princ q)
      );END lambda
      infoString infoStringSize
    );END mapcar
  );END setq
  (princ)
)
(defun LM:SubstNth ( a n l / i )
    (setq i -1)
    (mapcar '(lambda ( x ) (if (= (setq i (1+ i)) n) a x)) l)
)
(defun EntityInfo (ent_name / blockName blockType )
  ; Return a list with useful information of the nested object
  ; (ename (Block/Xref Name Layer))
  (if ent_name
    (if (= "INSERT" (cdr (assoc 0 (entget ent_name))))
      (progn
        (if (setq blockName (LM:effectivename (vlax-ename->vla-object ent_name)) )
          (if (assoc 1 (tblsearch "block" blockName))
            (setq blockType "XREF")
            (setq blockType "BLOCK")
          );END if
        );END if
        (list
          ent_name
          blockType
          blockName
          (cdr (assoc 8 (entget ent_name)))
        );END list
      );END progn
      (list
        ent_name
        (cdr (assoc 0 (entget ent_name)))
        "-"
        (cdr (assoc 8 (entget ent_name)))
      );END list
    );END if
    nil
  );END if
)
(defun NestedEntityNames ( pt / ent_name i info )
  ; Returns a list with nested entity names, being the parent first one and deepest child last one
  (if pt ; if point provided, continue
    (if (setq ent_name (nentselp pt)) ; if entity selected, continue
      (progn
        (setq
          i 0
          info
            (list
              (list "Level" "Type" "Block name" "Object layer")
              (EntityInfo (car ent_name))
            );END list
        );END setq
        (if (nth 3 ent_name) ; if nested entity
          (progn
            (setq
              nestedLevels (length (nth 3 ent_name))
            )
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
