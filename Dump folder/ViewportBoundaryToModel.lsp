; (defun c:xx ()
;   ; Trigger
;   (defun *error* ( errorMessage ) (princ (strcat "n-------- ERROR: " errorMessage " --------n")) (vl-bt) (DT:ReportError))
;   (DT:AutoLoadFileFromCivilTemp "ErrorTracing.lsp")
;   (DT:AutoLoadFileFromCivilTemp "ViewportBoundaryToModel.lsp")
;   (c:ViewportBoundaryToModel)
;
;   ; v0.0 - 2017.09.18 - First issue
;   ; Author: David Torralba
;   ; Last revision: 2017.09.18
; )
(defun c:ViewportBoundaryToModel ( / doc ss viewportCenter )

  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setvar "tilemode" 0) ; Force change to paper space

  ; Select viewport data
  (if (setq ss (ssget '(( 0 . "VIEWPORT"))))
    (foreach a (ssnamex ss)
      (if (= 'ename (type (cadr a)))
        (progn
          (vla-startUndoMark doc)               ; Set undo start mark
          (DT:ViewportBoundaryToModel (cadr a)) ; Draw viewport boundary in model space
          (vla-endUndoMark doc)                 ; Set undo end mark
        );END progn
      );END if
    );END foreach
  );END if

  ; v0.0 - 2017.09.18 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.09.18
)
(defun DT:ViewportBoundaryToModel ( ename / viewportId viewportCoordinates viewportCenter viewportBoundary )
  ; Draw viewport boundary in model space
  (if (DT:Arg 'DT:ViewportBoundaryToModel '((ename 'ename)))
    (progn
      ; Set selected viewport as last activated viewport
      (setq viewportId (cdr (assoc 69 (entget ename)))) ; Get selected viewport id
      (command "_.MSPACE")                              ; Activate any viewport
      (setvar "CVPORT" viewportId)                      ; Change into selected viewport
      (command "_.PSPACE")                              ; Go back to paper space

      (if (setq viewportCoordinates (DT:GetViewportBoundary ename))
        (if (setq viewportBoundary (DT:DrawViewport viewportCoordinates))
          (if viewportBoundary
            (progn
              (command ".chspace" viewportBoundary "" "")
              (command "_.PSPACE")
            );END progn
          );END if
        );END if
      );END if
    );END progn
  );END if

  ; v0.0 - 2017.09.18 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.09.18
)
(defun DT:GetViewportBoundary ( ename / viewportType )
  ; Return viewport boundary data
  (if (DT:Arg 'DT:GetViewportBoundary '((ename 'ename)))
    (progn
      (setq viewportType (DT:GetViewportType ename))
      (setq viewportId (cdr (assoc 69 (entget ename))))
      (cond
        ((= 0 viewportType) (DT:GetRegularViewportBoundary ename))    ; Regular viewport
        ((= 1 viewportType) (DT:GetPolygonalViewportBoundary ename))  ; Polygonal viewport
        (t                                                            ; Other
          (princ (strcat "\nViewport " (itoa viewportId) " has a not supported shape\n"))
          nil
        )
      );END cond
      ; return
    );END progn
  );END if

  ; v0.0 - 2017.09.18 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.09.18
)
(defun DT:GetViewportType ( ename / associatedEname )
  ; Returns an integer expressing the viewport type:
  ; - 0: regular viewport
  ; - 1: polygonal viewport

  (if (DT:Arg 'DT:GetViewportType '((ename 'ename)))
    (if (assoc 340 (entget ename))  ; if viewport clipped to an object
      (if (= "LWPOLYLINE" (cdr (assoc 0 (entget (cdr (assoc 340 (entget ename)))))))
        1   ; polygonal viewport
        2   ; other: circle, ellipse, spline, etc.
      );END if
      0     ; regular viewport
    );END if
  );END if

  ; v0.0 - 2017.09.18 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.09.18
)
(defun DT:GetPolygonalViewportBoundary ( ename )
  ; Return polygonal viewport boundary data
  (if (DT:Arg 'DT:GetPolygonalViewportBoundary '((ename 'ename)))
    (DT:GetLwpolyPoints (cdr (assoc 340 (entget ename))))
  );END if

  ; v0.0 - 2017.09.18 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.09.18
)
(defun DT:GetRegularViewportBoundary ( ename / viewportElist viewportHalfWidth viewportHalfHeight pC )
  ; Return regular viewport boundary data
  (if (DT:Arg 'DT:GetRegularViewportBoundary '((ename 'ename)))
    (progn
      (setq viewportElist (entget ename))
      (setq viewportHalfWidth  (* 0.5 (cdr (assoc 40 viewportElist))))
      (setq viewportHalfHeight (* 0.5 (cdr (assoc 41 viewportElist))))
      (setq pC (list (cadr (assoc 10 viewportElist)) (caddr (assoc 10 viewportElist)) 0.0))
      ; Return rectangle coordinates
      (list
        (list (- (car  pC) viewportHalfWidth) (- (cadr pC) viewportHalfHeight) 0.0)
        (list (+ (car  pC) viewportHalfWidth) (- (cadr pC) viewportHalfHeight) 0.0)
        (list (+ (car  pC) viewportHalfWidth) (+ (cadr pC) viewportHalfHeight) 0.0)
        (list (- (car  pC) viewportHalfWidth) (+ (cadr pC) viewportHalfHeight) 0.0)
      )
    );END progn
  );END if

  ; v0.0 - 2017.09.18 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.09.18
)
; (defun DT:GetViewportCenter ( ename / viewportElist )
;   ; Return viewport center coordinates (WCS) in model space
;   (if (DT:Arg 'DT:GetViewportCenter '((ename 'ename)))
;     (progn
;       (setq viewportElist (entget ename))
;       (setq pC (list (cadr (assoc 10 viewportElist)) (caddr (assoc 10 viewportElist)) 0.0))
;     );END progn
;   );END if
;
;   ; v0.0 - 2017.09.18 - First issue
;   ; Author: David Torralba
;   ; Last revision: 2017.09.18
; )
(defun DT:DrawViewport ( ptList )
  ; Draw a polyline with same vertexes as viewport
  (entmakex
    (append
      (list
        (cons   0 "LWPOLYLINE")
        (cons 100 "AcDbEntity")
        (cons 100 "AcDbPolyline")
        (cons  90 (length ptList)) ; Number of vertices
        (cons  70 1) ; Open(0)/Closed(1)
      )
      (mapcar
        '(lambda (pt) (cons 10 pt) )
        ptList
      );END mapcar
    );END append
  )

  ; v0.0 - 2017.09.18 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.09.18
)
