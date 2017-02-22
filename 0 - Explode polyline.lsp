(defun c:ep ( / ent_name ptList width )
  ; Draws individually every single segment of selected polylines
  (foreach a (ssnamex (ssget '((-4 . "<OR") (0 . "LWPOLYLINE") (-4 . "OR>"))))
    (if (= 'ENAME (type (cadr a)))
      (progn
        (ExplodeSinglePolyline (cadr a))
        (vla-delete (vlax-ename->vla-object (cadr a)))
      );END progn
    );END if1
  );END foreach

  ; End without double messages
  (princ)

  ; v0.4 - 2016.11.14 - Linetype Scale management added
  ; v0.3 - 2016.11.14 - All code rewritten
  ; v0.2 - 2016.03.21 - Code optimized and comments translated into English.
  ; v0.1 - 2016.03.02 - Command-line name changed from BRP to EP.
  ;                     Variables added as local variables not to overlap with other routines.
  ; v0.0 - 2016.02.16 - First issue
  ; NOTE: It supports heavy and light polylines.
  ; NOTE: Can be used in polylines with 2 vertexes in the same position.
  ; NOTE: No problem with closed polylines.
  ; Author: David Torralba
  ; Last revision: 2016.11.14
)
(defun ExplodeSinglePolyline (ent_name / i layerName ptList width color lineType lineTypeScale)
  (setq
    i 0
    layerName (GetPolylineLayer ent_name)
    ptList (GetPolylinePointList ent_name)
    width (GetPolylineWidth ent_name)
    color (GetPolylineColor ent_name)
    lineType (GetPolylineStyle ent_name)
    lineTypeScale (GetPolylineStyleScale ent_name)
  )
  (while (< i (- (length ptList) 1) )
    (setq
      p1 (nth i ptList)
      p2 (nth (+ i 1) ptList)
    )
    (if (DrawPolylineSegment p1 p2 width color layerName lineType lineTypeScale)
      (setq i (+ i 1) )
      (progn
        (princ "\nError drawing polyline segment from ")
        (princ p1)
        (princ " to ")
        (princ p2)
        (princ ".")
      );END progn
    );END if
  );END while
)
(defun GetPolylinePointList (ent_name / param endParam pt ptList)
  (setq
    param 0
    endParam (vlax-curve-getEndParam (vlax-ename->vla-object ent_name))
  )
  (while (<= param endParam)
    (setq
      pt (vlax-curve-getPointAtParam (vlax-ename->vla-object ent_name) param)
      ptList (append ptList (list pt) )
      param (+ param 1)
    )
  );END while
  ptList
)
(defun GetPolylineWidth (ent_name)
  (cdr (assoc 43 (entget ent_name)))
)
(defun GetPolylineColor (ent_name)
  (if (not (assoc 62 (entget ent_name)))
    256
    (cdr (assoc 62 (entget ent_name)))
  );END if
)
(defun GetPolylineLayer (ent_name)
  (cdr (assoc 8 (entget ent_name)))
)
(defun GetPolylineStyle (ent_name)
  (if (not (assoc 6 (entget ent_name)))
    nil
    (cdr (assoc 6 (entget ent_name)))
  );END if
)
(defun GetPolylineStyleScale (ent_name)
  (if (not (assoc 48 (entget ent_name)))
    nil
    (cdr (assoc 48 (entget ent_name)))
  );END if
)
(defun DrawPolylineSegment (p1 p2 width color layerName lineType lineTypeScale)
  (entmakex
    (append
      (list
        (cons 0 "LWPOLYLINE")
        (cons 100 "AcDbEntity")
        (cons 100 "AcDbPolyline")
        (cons 8 layerName)
        (cons 90 2)     ; Number of vertex
        (cons 70 128)  ; Not closed polyline
        (cons 43 width)
        (cons 48 lineTypeScale)
        (cons 62 color)
        (cons 10 (list (nth 0 p1) (nth 1 p1) ))
        (cons 10 (list (nth 0 p2) (nth 1 p2) ))
      );END list
      (if lineType (list (cons 6 lineType)) )
    );END append
  );END entmakex
)
