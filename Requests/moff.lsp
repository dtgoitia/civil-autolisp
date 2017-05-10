(defun c:mof ( / ss offsetDistance colorIndex activeDocument i )
  ; Offset either side selected objects
  (vl-load-com)
  (if (setq ss (ssget '((-4 . "<OR")(0 . "ARC")(0 . "CIRCLE")(0 . "LWPOLYLINE")(0 . "LINE")(-4 . "OR>"))))
    (if (setq offsetDistance (getreal "\nType offset distance: "))
      (if (setq colorIndex (getint "\nType color index: "))
        (progn
          (vla-StartUndoMark (setq activeDocument (vla-get-ActiveDocument (vlax-get-acad-object))))
          (setq i 0)
          (foreach a (ssnamex ss)
            (if (= 'ename (type (cadr a)))
              (progn
                (vla-offset (vlax-ename->vla-object (cadr a)) offsetDistance)
                (vlax-put-property (vlax-ename->vla-object (Entlast)) 'Color colorIndex)
                (vla-offset (vlax-ename->vla-object (cadr a)) (* -1 offsetDistance))
                (vlax-put-property (vlax-ename->vla-object (Entlast)) 'Color colorIndex)
                (setq i (+ i 1))
              );END progn
            );END if
          );END foreach
          (vla-EndUndoMark activeDocument)
        );END progn
        (princ "\nNot color index introduced.")
      );END if
      (princ "\nNot distance introduced.")
    );END if
    (princ "\nNo objects selected.")
  );END if

  (if i (princ (strcat "\n" (itoa i) " object(s) processed.")) )

  (princ)

  ; v0.1 - 2017.05.10 - ARC type added to selection filter
  ; v0.0 - 2017.05.09 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.05.09
)
