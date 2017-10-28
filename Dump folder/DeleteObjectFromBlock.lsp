;; Credit to Jummy BergMark ;;
;; For DeleteObjectFromBlock Routine ;;
;; Manusoft for the Recursive Routine ;;
;; Me for tweaking it for you ;;
(defun c:fxb (/ blockName doc FLST)
  (vl-load-com)
  (setq doc (vla-get-activedocument (vlax-get-acad-object)) )
  (setq
    blockName (cdr (assoc 2 (entget (car (entsel "\nSelect block: ")))))
    FLST nil
  )
  (fix1 blockName)
  (vl-cmdf "regen")
  (prin1)
)
(defun fix1 (blockName / blockEntityName)
  ; If the blockName is not in FLST, carry on
  (if (not (member blockName FLST))
    (progn
      (setq
        ; Add blockName to FLST
        FLST (cons blockName FLST)
        ; Get entity name of blockName
        blockEntityName (tblobjname "block" blockName)
      )
      ; Get next object within the drawing object table
      (while (setq blockEntityName (entnext blockEntityName))
        ;(print (entget blockEntityName))
        ; If the next object is an INSERT object
        (if (= (cdr (assoc 0 (entget blockEntityName))) "INSERT")
          ; If true, get the name block and pass it as argument to "fix1" (recursive)
          (fix1 (cdr (assoc 2 (entget blockEntityName))))
          ; If false, check if next object is a dimension, if it is DIMENSION object, delete it
          (if (= (cdr (assoc 0 (entget blockEntityName))) "DIMENSION")
            (progn
              (setq
                dimToDelete (vlax-ename->vla-object blockEntityName)
                blk (vla-ObjectIdToObject doc (vla-get-ownerID dimToDelete) )
              );END setq
              ;(vla-delete dimToDelete)
              (vla-get-count blk)
            );END progn
          );END if
        );END if
      );END while
    );END progn
  );END if
  (princ)
)
