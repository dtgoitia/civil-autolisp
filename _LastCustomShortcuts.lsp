; DO NOT REMOVE THIS LINE. It's a checking.
(defun c:1() (princ "\n0.000") (c:SDIPP0))
(defun c:2() (princ "\n0.650") (c:SDIPP65))
(defun c:SDIPP65( /
  ; Recursive DT:SDIP along polyline vertexes
                ent_name insertionMode
                startZ startPoint startParam endPoint endParam gradient
                p1 p2 param
                )
  (if (setq ent_name (car (entsel "\nSelect a 2D polyline: ")) )
    (if (= "LWPOLYLINE" (cdr (assoc 0 (entget ent_name))) )
      (progn
        (setq
          insertionMode (DT:AskBlockInAllVertex)
          startZ ( - (DT:clic_or_type_level) 0.650)
        )
      	(princ (strcat "\nStart level = " (LM:rtos startZ 2 3) "m"))
        (setq
          startPoint (getpoint "\nSelect start point: ")
          startParam (vlax-curve-getParamAtPoint (vlax-ename->vla-object ent_name) startPoint)
          endPoint (getpoint "\nSelect end point: ")
          endParam (vlax-curve-getParamAtPoint (vlax-ename->vla-object ent_name) endPoint)
          gradient (getreal "\nGradient= 1/")
          p1 (list (nth 0 startPoint) (nth 1 startPoint) startZ)
          param startParam
        )
        (cond
          ((= startParam endParam)
            (princ "\nStart and end point are the same.")
          );END subcond
          ((< startParam endParam) ; normal direction
            (while (< param endParam)
              (setq
                param (+ param 1)
                p2 (vlax-curve-getPointAtParam (vlax-ename->vla-object ent_name) param)
                p2 (DT:SDIP p1 p2 gradient)
              )
              (if insertionMode (DT:InsertFlatLevelBlock p2))
              (setq p1 p2)
            );END while
            (DT:InsertFlatLevelBlock p2)
          );END subcond
          ((> startParam endParam) ; reverse direction
            (while (> param endParam)
              (setq
                param (- param 1)
                p2 (vlax-curve-getPointAtParam (vlax-ename->vla-object ent_name) param)
                p2 (DT:SDIP p1 p2 gradient)
              )
              (if insertionMode (DT:InsertFlatLevelBlock p2))
              (setq p1 p2)
            );END while
            (DT:InsertFlatLevelBlock p2)
          );END subcond
        );END cond
        (princ "\nto clipboard: ")
        (CopyToClipboard (LM:rtos (nth 2 p2) 2 3))
      );END progn
      (princ "\nSelected object is not a 2D polyline.")
    );END if
    (princ "\nNothing selected.")
  );END if
  (princ)
)
(defun c:SDIPP0( /
  ; Recursive DT:SDIP along polyline vertexes
                ent_name insertionMode
                startZ startPoint startParam endPoint endParam gradient
                p1 p2 param
                )
  (if (setq ent_name (car (entsel "\nSelect a 2D polyline: ")) )
    (if (= "LWPOLYLINE" (cdr (assoc 0 (entget ent_name))) )
      (progn
        (setq
          insertionMode (DT:AskBlockInAllVertex)
          startZ (DT:clic_or_type_level)
        )
      	(princ (strcat "\nStart level = " (LM:rtos startZ 2 3) "m"))
        (setq
          startPoint (getpoint "\nSelect start point: ")
          startParam (vlax-curve-getParamAtPoint (vlax-ename->vla-object ent_name) startPoint)
          endPoint (getpoint "\nSelect end point: ")
          endParam (vlax-curve-getParamAtPoint (vlax-ename->vla-object ent_name) endPoint)
          gradient (getreal "\nGradient= 1/")
          p1 (list (nth 0 startPoint) (nth 1 startPoint) startZ)
          param startParam
        )
        (cond
          ((= startParam endParam)
            (princ "\nStart and end point are the same.")
          );END subcond
          ((< startParam endParam) ; normal direction
            (while (< param endParam)
              (setq
                param (+ param 1)
                p2 (vlax-curve-getPointAtParam (vlax-ename->vla-object ent_name) param)
                p2 (DT:SDIP p1 p2 gradient)
              )
              (if insertionMode (DT:InsertFlatLevelBlock p2))
              (setq p1 p2)
            );END while
            (DT:InsertFlatLevelBlock p2)
          );END subcond
          ((> startParam endParam) ; reverse direction
            (while (> param endParam)
              (setq
                param (- param 1)
                p2 (vlax-curve-getPointAtParam (vlax-ename->vla-object ent_name) param)
                p2 (DT:SDIP p1 p2 gradient)
              )
              (if insertionMode (DT:InsertFlatLevelBlock p2))
              (setq p1 p2)
            );END while
            (DT:InsertFlatLevelBlock p2)
          );END subcond
        );END cond
        (princ "\nto clipboard: ")
        (CopyToClipboard (LM:rtos (nth 2 p2) 2 3))
      );END progn
      (princ "\nSelected object is not a 2D polyline.")
    );END if
    (princ "\nNothing selected.")
  );END if
  (princ)
)
(defun DT:flatPoint( pt )
  (list (nth 0 pt) (nth 1 pt) 0.0)
)(defun DT:AskBlockInAllVertex( / ans )
  (initget "All Last")
  (if (not (setq ans (getkword "\nSelect block vertexes to insert the blocks [All/Last] <Last>? ") ))
    (setq ans Last)
  );END if
  (cond
    ((= ans "All")
      T
    );END subcond
    ((= ans "Last")
      nil
    );END subcond
  );END cond
)
(defun DT:InsertFlatLevelBlock ( pt )
  (if (tblsearch "block" "PI_DT")
    (command "._insert" "PI_DT" (DT:flatPoint pt) "0.25" "0.25" "" (LM:rtos (nth 2 pt) 2 3))
  );END if
)
