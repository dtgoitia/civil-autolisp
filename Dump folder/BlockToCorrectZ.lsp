(defun c:xx ( / ss i )
  (if (setq ss (ssget '(( 0 . "INSERT"))))
    (progn
      (foreach a (ssnamex ss)
        (if (= 'ename (type (cadr a)))
          (if (DT:BlockZToLevelAttributeValue (cadr a) "LEVEL")
            (if i
              (setq i (+ 1 i))
              (setq i 1)
            );END if
            (if i
              (progn (princ (strcat "\n" (itoa i) " objects processed.\n")) (exit) )
              (progn (princ "\nNo objects processed.\n") (exit) )
            );END if
          );END if
        );END if
      );END foreach
      (if i
        (princ (strcat "\n" (itoa i) " objects processed.\n"))
        (princ "\nNo objects processed.\n")
      );END if
    );END progn
  );END if
  (princ)

  ; v0.0 - 2017.07.13 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.07.13
)
(defun DT:BlockZToLevelAttributeValue (ename attributeTag / z entList oldCoords newCoords newEntList )
  ; Update block Z coordinate to match its the value of the attribute "attributeTag", if any

  (if (DT:Arg 'DT:BlockZToLevelAttributeValue '((ename 'ename)(attributeTag 'str)))
    (progn
      (if (setq z (LM:vl-getattributevalue (vlax-ename->vla-object ename) attributeTag))
        (progn
          (setq entList (entget ename))
          (setq oldCoords (assoc 10 entList))
          (setq newCoords (subst (atof z) (nth 2 (cdr oldCoords)) oldCoords))
          (setq newEntList (subst newCoords oldCoords entList))
          (entmod newEntList)

          ; Test the change has been effective
          (if (= (atof z) (nth 2 (cdr (assoc 10 (entget ename)))))
            T   ; return T if correctly processed
            nil ; return nil otherwise
          );END if
        );END progn
      );END if
    );END progn
    nil
  );END if

  ; v0.0 - 2017.07.13 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.07.13
)
