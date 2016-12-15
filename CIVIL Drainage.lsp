(defun DT:SelectByDistance (pC dist layerName / pn pt pList ang ss )
  ; Return a selection set of all the objects within "dist" distance from "pC" point
  ; pC [pt] - Center point coordinates
  ; dist [real] - Radial distance from pC within objects will be selected
  (setq
    pn 20  ; number of points of the selection polygon
    ang 0 ; angle
  )
  (repeat pn
    (setq
      pt (polar pC ang dist)
      pList (append pList (list pt) )
      ang (+ ang (/ (* 2 pi) pn) )
    );END setq
  );END repeat
  (ssget "CP" pList )  ; return selection set
)
(defun DT:SelectConnectedPipes ( manholeEnt_name / pn ang dist pC lay pt pList ss )
  ; Return a selection set of all the polylines within "dist" distance from
  ; "pC" point and within the layer "layerName"; or nil if nothing selected.
  ; manholeEnt_name [ename] - Entity name of manhole where pipes connect
  (setq
    pn 20     ; number of points of the selection polygon
    ang 0     ; angle
    dist 1 ; selection distance
    pC  (cdr (assoc 10 (entget ent_name))) ; manhole insertion point
    lay (cdr (assoc  8 (entget ent_name))) ; manhole layer
  )

  ; Selec polylines within "dist" ditance from seleted block insertion point and in the same layer as block
  (repeat pn
    (setq
      pt (polar pC ang dist)
      pList (append pList (list pt) )
      ang (+ ang (/ (* 2 pi) pn) )
    );END setq
  );END repeat
  (setq
    ss (ssget "CP" pList (list (cons 0 "LWPOLYLINE") (cons 8 lay) ) )
  )

  (foreach a (ssnamex ss)
    (if (= 'ename (type (cadr a)))
      (progn
        ; Remove polylines with no thickness
        (if (< 0 (DT:GetPolylineGlobalWidth (cadr a)))
          ; Remove polylines without any close vertex to block insertion point
          (if (setq closestVertexPoint (DT:GetClosestVertex pC (cadr a)) )
            (if (< dist (distance pC closestVertexPoint ) )
              (ssdel (cadr a) ss)
            );END if
          );END if
          (ssdel (cadr a) ss)
        );END if
      );END progn
    );END if
  );END foreach
  (princ)
  ;(ssget "CP" pList (list (cons 0 "LWPOLYLINE") (cons 8 lay) ) )  ; return selection set
  ss
)
(defun DT:GetClosestVertex ( pt ent_name / closestPoint closestParam closestVertexParam closestVertexPoint )
  ; Returns 3D point with the closest vertex coordinates
  ; pt [pt] - Point of reference to find closest point
  ; ent_name [ename] - LWPolyline where to look for the closest vertex
  (if (= "LWPOLYLINE" (cdr (assoc 0 (entget ent_name))))
    (progn
      (setq
        closestPoint (vlax-curve-getClosestPointTo (vlax-ename->vla-object ent_name) pt)
        closestParam (vlax-curve-getParamAtPoint (vlax-ename->vla-object ent_name) closestPoint)
        closestVertexParam (atof (LM:rtos closestParam 2 0))
        closestVertexPoint (vlax-curve-getPointAtParam (vlax-ename->vla-object ent_name) closestVertexParam)
      )
    );END progn
    nil
  );END if
)
(defun DT:GetPolylineGlobalWidth (ent_name)
  ; Return polyline width as real if ent_name is a LWPOLYLINE
  ; ent_name [ename] - Entity name of the polyline
  (if (= "LWPOLYLINE" (cdr (assoc 0 (entget ent_name) ) ))
    (cdr (assoc 43 (entget ent_name) ) )
  );END if
)
(defun c:1( / ent_name p_ins lay ss)
  ; Selects the pipes connected to selected manhole block
  (setq
    ent_name (car (entsel "\nSelect manhole: "))
    ss (DT:SelectConnectedPipes ent_name)
  )
  (sssetfirst nil ss)

  ; TODO
  (princ)
)
;(defun DT:GetManholeIL ( ent_name )
;  ;(LM:getdynprops (vlax-ename->vla-object ent_name) )
;  (LM:vl-getattributes  (vlax-ename->vla-object ent_name))
;)
;(defun c:1()
;  (DT:GetManholeIL (car (entsel)))
;)
(defun c:1( / stringManholeIL)
; String handling functions:
; http://help.autodesk.com/view/ACD/2015/ENU/?guid=GUID-8543549B-F0D7-43CD-87A3-F6B827FF0B88
  (setq
    ;stringManholeIL "75.175(450%%C)"
    stringManholeIL "75.175(450∅)"
  )
  ;(setq stringManholeIL "75.175(450∅)") THIS ONE SHOULD BE PARSE PROPERLY TOO!

  (DT:ParseManholeIL stringManholeIL)
)
(defun DT:ParseManholeIL ( stringManholeIL )
  ; Return a pair list with the IL and the pipe diameter
  (setq
    stringLength (strlen stringManholeIL)
    openParenthesisPosition (vl-string-search "(" stringManholeIL)
    closeParenthesisPosition (vl-string-search ")" stringManholeIL)
    contentBetweenParenthesis (substr stringManholeIL (+ 2 openParenthesisPosition) (- (- closeParenthesisPosition openParenthesisPosition) 1))
    diameterPosition (vl-string-search "Ø" "aØa")
  )
  (princ "\nstringManholeIL = ")(princ stringManholeIL)
  (princ "\nopenParenthesisPosition = ")(princ openParenthesisPosition)
  (princ "\ncloseParenthesisPosition = ")(princ closeParenthesisPosition)
  (princ "\ncontentBetweenParenthesis = ")(princ contentBetweenParenthesis)
  (princ "\ndiameterPosition = ")(princ diameterPosition)

  (setq
    invertLevel "75.175"
    pipeSize "0.450" ; don't add "DN", that's outside this function
  )

  (list (cons "IL" invertLevel) (cons "Pipe size" pipeSize))
  (princ)
)
