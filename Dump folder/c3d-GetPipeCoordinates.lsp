(defun c:xx ()
  ; Trigger
  (defun *error* ( errorMessage ) (princ (strcat "n-------- ERROR: " errorMessage " --------n")) (vl-bt) (DT:ReportError))
  (DT:AutoLoadFileFromCivilTemp "ErrorTracing.lsp")
  (DT:AutoLoadFileFromCivilTemp "c3d-GetPipeCoordinates.lsp")
  (c:entryFunction)

  ; v0.0 - _DATE_ - First issue
  ; Author: David Torralba
  ; Last revision: _DATE_
)
(defun c:entryFunction ( / coords coordList mergedCoordinateList )
  (textpage)
  (foreach a (ssnamex (ssget))
    (if (= 'ename (type (cadr a)))
      (if (setq coords (DT:C3dGetPipeStartAndEndPoint (cadr a)))
        (setq coordList (append coordList (list coords)))
      );END if
    );END if
  );END foreach

  ; Return result
  (setq mergedCoordinateList (DT:TempRemoveDuplicatedPoints coordList))

  (entmakex
    (append
      (list
        (cons   0 "LWPOLYLINE")
        (cons 100 "AcDbEntity")
        (cons 100 "AcDbPolyline")
        (cons  70 0) ; Open(0)/Closed(1)
        (cons  90 (length mergedCoordinateList)) ; Number of vertices
      )
      (mapcar
        '(lambda (pt) (cons 10 pt) )
        mergedCoordinateList
      );END mapcar
    );END append
  )

  ; v0.0 - 2017.09.15 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.09.15
)
(defun DT:C3dGetPipeStartAndEndPoint ( ename / object )
  ; Returns a list with the start and end point coordinates of the C3D pipe
  ; ename [ename] - C3D pipe entity name
  (if (DT:Arg 'DT:C3dGetPipeStartAndEndPoint '((ename 'ename)))
    (if (setq object (vlax-ename->vla-object ename))
      (progn
        (mapcar
          '(lambda (x) (vlax-safearray->list (vlax-variant-value (vlax-get-property object 'pointatparam x))))
          (list 0 1)
        );END mapcar
      );END progn
      (DT:Error 'DT:C3dGetPipeStartAndEndPoint "it was not possible to obtain vla-object from passed entity name")
    );END if
  );END if

  ; v0.0 - 2017.09.15 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.09.15
)
(defun DT:TempRemoveDuplicatedPoints ( pointList / i nextPointPair return )
  (if (DT:Arg 'DT:TempRemoveDuplicatedPoints '((pointList 'list)))
    (progn
      (setq i 0)
      (foreach currentPointPair pointList
        (if (setq nextPointPair (nth (+ i 1) pointList))
          (if (not return)                                              ; Not last point
            (setq return (list (car currentPointPair)))                 ;  - First point
            (setq return (append return (list (car currentPointPair)))) ;  - Nor first, nor last point
          );END if
          (setq return (append return currentPointPair))                ; Last point
        );END if
        (setq i (+ i 1))
      );END foreach

    );END progn
  );END if

  return

  ; v0.0 - 2017.09.15 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.09.15
)
