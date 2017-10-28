(defun DT:UpdateEntity ( ename index value / )
  ; Update entity
  (if (DT:Arg 'DT:UpdateEntity '((ename 'ename)(index 'int)))
    (progn
      (setq oldEntGet (entget ename))
      (foreach element oldEntGet
        (if (= index (car element))
          (progn
            (princ element)
          );END progn
        );END if
      );END foreach
    );END progn
  );END if

  ; v0.0 - 2017.06.23 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.06.23
)
(defun c:1 ( / pt doc newPolyline newPolylineArea )
  ; Create a boundary, label its area, and group everything together

  (princ "\nLABEL AREA\n")
  (if (setq pt (getpoint "\nSelect point: ") )
    (progn
      (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
      (vla-startUndoMark doc)
      (command
        "_.-boundary"   ; execute command
        "A" "O" "P" ""  ; change settings to output a polyline
        pt ""           ; pass point
      )
      ; Get last created entity
      (setq newPolyline (entlast))

      ; Change thickness to 0.100
      (command
        "_.pedit"       ; execute command
        newPolyline     ; pass new polyline
        "W" 0.1         ; change width
        ""              ; exit command
      )

      ; Get area an print it
      (if (setq newPolylineArea (vlax-get-property (vlax-ename->vla-object newPolyline) 'Area))
        (if
          (setq
            areaText (entmakex
              (append
                (list
                  (cons 0 "MTEXT")
                  (cons 100 "AcDbEntity")
                  (cons 100 "AcDbMText")
                  (cons 1 (strcat (LM:rtos newPolylineArea 2 0) "m2"))
                  (cons 10 pt)
                  (cons 71 5)
                  (cons 72 5)
                );END list
                (if (tblsearch "style" "ROMANS") (list (cons 7 "ROMANS")))
              );END append
            );END entmakex
          );END setq
          (DT:SetGroup (list newPolyline areaText))
        );END if
      );END if
      (vla-endUndoMark doc)
    );END progn
  );END if

  (princ)

  ; v0.0 - 2017.06.23 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.06.23
)
(defun c:2() (princ "\nMOVE:\n") (c:M))
