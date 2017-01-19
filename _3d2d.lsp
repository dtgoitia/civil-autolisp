(defun c:Convert3DpolyTo2Dpoly ( / old_error old_sysvars iSuccess iFail )
  ; Convert multiple 3Dpolylines to 2Dpolylines
  (setq
    iSuccess  0
    iFail     0
  )
  (princ "\nCONVERT 3D POLYLINES TO 2D POLYLINES\nSelect 3D polyilines to convert:")
  (if (setq ss (ssget '((0 . "POLYLINE"))) )
    (foreach a (ssnamex ss)
      (if (= 'ename (type (cadr a)))
        (if (Convert3DpolyTo2Dpoly (cadr a))
          (setq iSuccess (+ iSuccess 1))
          (setq iFail (+ iFail 1))
        );END if
      );END if
    );END foreach
  );END if

  ; Summary comment
  (princ
    (strcat
      "\n" (itoa (+ iSuccess iFail) ) " objects processed. "
      (cond
        ((and (> iSuccess 0) (> iFail 0))
          (strcat
            (itoa iSuccess) " successful, "
            (itoa iFail) " not successful."
          );END strcat
        );END subcond
        ((= iFail 0)
          "All successful."
        );END subcond
        ((= iSuccess 0)
          "None successful."
        );END subcond
      );END cond
    )
  )
  (princ)

  ; v0.0 - 2017.01.19 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.01.19
);END defun
(defun Convert3DpolyTo2Dpoly ( ent_name / coordinateList )
  ; Extract 3D coordinates
  (if (setq
        coordinateList (Get3DpolylineCoordinateList ent_name)
        3DpolyOpenClose
          (cond
            ((= :vlax-false (vla-get-closed (vlax-ename->vla-object ent_name))) 0 )
            ((= :vlax-true (vla-get-closed (vlax-ename->vla-object ent_name)))  1 )
          );END cond
      );END setq
    ; Create 2Dpoly
    (if (CreateLWPOLYINE coordinateList 3DpolyOpenClose)
      (progn
        (vla-delete (vlax-ename->vla-object ent_name))
        T
      );END progn
      (princ "\nERROR @ Convert3DpolyTo2Dpoly: (CreateLWPOLYINE) = nil")
    );END if
    (princ "\nERROR @ Convert3DpolyTo2Dpoly: (Get3DpolylineCoordinateList) = nil")
  );END if
)
(defun Get3DpolylineCoordinateList ( ent_name / l m)
  ; Return 3D polyline coordinate list
  (mapcar
    '(lambda ( x )
      (if (or (< (length l) 3) (not l)) ; si hay menos de 3 elementos en la lista
        (setq l (append l (list x)) )
        (setq
          m (append m (list l)) ; main list
          l (list x)
        )
      );END if
    );END lambda
    (vlax-safearray->list (vlax-variant-value
      (vla-get-Coordinates (vlax-ename->vla-object ent_name)) ))
  );END mapcar
  (append m (list l))
)
(defun CreateLWPOLYINE (l cl)
  (entmakex
    (append
      (list
        (cons 0 "LWPOLYLINE")
        (cons 100 "AcDbEntity")
        (cons 100 "AcDbPolyline")
        (cons 90 (length l))
        (cons 70 cl)
      );END list
      (mapcar '(lambda (p) (cons 10 p)) l)
    );END append
  );END entmakex
)
