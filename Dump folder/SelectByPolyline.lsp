(defun c:xx ()
  ; Trigger
  (DT:AutoLoadFileFromCivilTemp "SelectByLWPolyline.lsp")
  (c:SelectByLWPolyline)

  ; v0.0 - 2017.08.08 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.08
)
(defun c:SelectByLWPolyline ( / ename ss )
  ; Select by polyline
  (if (setq ename (car (entsel "\nSelect polyline: ")))
    (if (setq ss (DT:SelectByLWPolyline ename))
      (progn
        (sssetfirst nil ss)
      );END progn
    );END if
  );END if

  ; v0.0 - 2017.08.08 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.08
)
(defun DT:SelectByLWPolyline ( ename / verticesList ss )
  ; Return a selection set with the objects found within "ename" polyline
  (if (DT:Arg 'DT:SelectByLWPolyline '((ename 'ename)))
    (if (= "LWPOLYLINE" (cdr (assoc 0 (entget ename))))
      (if (setq verticesList (DT:GetPolylineVertexCoordinates ename))
        (setq ss (ssget "_CP" verticesList))
      );END if
    );END if
  );END if

  ss

  ; v0.0 - 2017.08.08 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.08
)
