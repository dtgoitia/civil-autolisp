; (defun c:xx ()
;   ; Trigger
;   (DT:AutoLoadFileFromCivilTemp "SelectAlongPolyline.lsp")
;   (c:SelectAlongPolyline)
;
;   ; v0.0 - 2017.08.16 - First issue
;   ; Author: David Torralba
;   ; Last revision: 2017.08.16
; )
(defun c:SelectAlongPolyline ( / ename entList )
  ; Command version of DT:SelectAlongPolyline
  (if (setq ename (car (entsel "\nSelect polyline of reference: ")))
    (progn
      (DT:SelectAlongPolyline ename)
    );END progn
  );END if

  ; v0.0 - 2017.08.16 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.16
)
(defun DT:SelectAlongPolyline ( ename / vertexList )
  ; Select objects along ename polyline (using "F" mode selection)
  (if (DT:Arg 'DT:SelectAlongPolyline '((ename 'ename)))
    (progn
      ; Remove objects crossed by ename
      (if (setq vertexList (DT:GetLwpolyPoints ename))
        (if (setq ss (ssget "CP" vertexList))
          (sssetfirst nil ss)
        );END if
      );END if
    );END progn
  );END if

  ; v0.0 - 2017.08.16 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.16
)
