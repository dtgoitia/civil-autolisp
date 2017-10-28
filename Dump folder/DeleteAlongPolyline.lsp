(defun c:xx ()
  ; Trigger
  (DT:AutoLoadFileFromCivilTemp "DeleteAlongPolyline.lsp")
  (c:DeleteAlongPolyline)

  ; v0.0 - 2017.08.16 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.16
)
(defun c:DeleteAlongPolyline ( / ename entList )
  ; Command version of DT:DeleteAlongPolyline
  (if (setq ename (car (entsel "\nSelect polyline of reference: ")))
    (progn
      (DT:DeleteAlongPolyline ename)

    );END progn
  );END if

  ; v0.0 - 2017.08.16 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.16
)
(defun DT:DeleteAlongPolyline ( ename / vertexList )
  ; Delete objects along ename polyline (using "F" mode selection)
  (if (DT:Arg 'DT:DeleteAlongPolyline '((ename 'ename)))
    (progn
      ; Remove objects crossed by ename
      (if (setq vertexList (DT:GetLwpolyPoints ename))
        (progn
          (command "_.erase" "F")
          (foreach vertex vertexList
            (command vertex)
          );END foreach
          (command "" "")
        );END progn
      );END if
    );END progn
  );END if

  ; v0.0 - 2017.08.16 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.16
)
