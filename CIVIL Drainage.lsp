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
(defun DT:GetPipeData ( ent_name / DN )
  ; Return the pipe data
  ; ( ent_name diameter )
  (if ent_name
    (list ent_name (cdr (assoc 43 (entget ent_name))))
  );END if
)
(defun DT:ParseManholeIL ( stringManholeIL
                          /
                          stringLength
                          openParenthesisPosition closeParenthesisPosition contentBeforeParenthesis contentBetweenParenthesis
                          invertLevel DN
                         )
  ; Return a pair list with the IL and the pipe diameter.
  ; Cases understands:
  ;  - IL without parenthesis
  ;  - IL with pipe size between parenthesis
  ;  - IL with pipe size + "%%C" between parenthesis.
  ;  - IL with pipe size + "Ø" between parenthesis.
  ;  - IL with pipe size + "∅" between parenthesis.
  ;  - IL with non-numeric string between parenthesis.

  (if stringManholeIL
    (progn
      ; if any diameter symbol found, substitute it
      (cond
        ((vl-string-search "Ø" stringManholeIL) (setq stringManholeIL (vl-string-subst "%%C" "Ø" stringManholeIL)) )
        ((vl-string-search "∅" stringManholeIL) (setq stringManholeIL (vl-string-subst "%%C" "∅" stringManholeIL)) )
      );END cond
      (setq stringLength (strlen stringManholeIL) )
      (if (setq openParenthesisPosition (vl-string-search "(" stringManholeIL))
        (progn
          ; Parenthesis found within the string
          (setq
            closeParenthesisPosition (vl-string-search ")" stringManholeIL)
            contentBeforeParenthesis (substr stringManholeIL 1 openParenthesisPosition)
            contentBetweenParenthesis (substr stringManholeIL (+ 2 openParenthesisPosition) (- (- closeParenthesisPosition openParenthesisPosition) 1))
            invertLevel (atof contentBeforeParenthesis)
          )

          ; Parse content between parenthesis
          (cond
            ; Number with %%C at the end between parenthesis
            ((= "%%C" (DT:GetStringLastNChar contentBetweenParenthesis 3))
              (setq
                DN (* 0.001 (atof (substr contentBetweenParenthesis 1 (- (strlen contentBetweenParenthesis) 3)) ))
              )
            );END subcond
            ; Number between parenthesis
            ((/= nil (numberp (read contentBetweenParenthesis)) )
              (setq
                DN (* 0.001 (atof contentBetweenParenthesis))
              )
            );END subcond
            ; Non-numeric string between parenthesis
            (t
              (princ "\nSorry, pipe diameter not understood :(")
            )
          );END cond
        );END progn

        ; No parenthesis within the string
        (setq invertLevel (atof stringManholeIL) )
      );END if
      (list (cons "IL" invertLevel) (cons "DN" DN))
    );END progn
    nil
  );END if
)
(defun c:1() (DT:GetManholeData (car (entsel))))
(defun DT:GetManholeData ( ent_name
                      /
                      manholeAttributeList
                      ID CL ILs DNs
                      item itemPosition checkedILamount
                      )
  ; Returns a list with manhole data with the following format:
  ; ( ent_name ID type layer CL ILs DNs )
  ; or nil if the object ent_name is not an INSERT
  (if ent_name
    (if (= "INSERT" (cdr (assoc 0 (entget ent_name))))
      (if (LM:getvisibilityparametername (vlax-ename->vla-object ent_name))
        (progn
          (setq
            manholeAttributeList (LM:vl-getattributes (vlax-ename->vla-object ent_name))
            ID (cdr (assoc "ID" manholeAttributeList))
            CL (cdr (assoc "CL" manholeAttributeList))
            type (LM:effectivename (vlax-ename->vla-object ent_name))
            lay (cdr (assoc 8 (entget ent_name)))
            shownILamount (atoi (LM:getvisibilitystate (vlax-ename->vla-object ent_name)))
          )
          ; Get IL and DN values
          (setq
            itemPosition 0
            checkedILamount 0
          )
          (while (< checkedILamount shownILAmount)
            (setq
              item (nth itemPosition manholeAttributeList)
              itemPosition (+ itemPosition 1)
            )
            (if (= "IL" (substr (car item) 1 2))
              (progn
                (setq checkedILamount (+ checkedILamount 1))
                (if (assoc "IL" (DT:ParseManholeIL (cdr item)) )
                  (setq ILs (append ILs (list (cdr (assoc "IL" (DT:ParseManholeIL (cdr item)) )))))
                );END if
                (if (cdr (assoc "DN" (DT:ParseManholeIL (cdr item)) ))
                  (setq DNs (append DNs (list (cdr (assoc "DN" (DT:ParseManholeIL (cdr item)) )))) )
                  (setq DNs (append DNs (list "noPipeSize")) )
                );END if
              );END progn
            );END if
          );END repeat
          ; Return manhole data
          (list ID CL type lay ILs DNs)
        );END progn
      );END if
    );END if
    nil
  );END if
)
(defun c:1( / ent_name p_ins lay ss)
  ; Selects the pipes connected to selected manhole block
  (setq
    ent_name (car (entsel "\nSelect manhole: "))
    manholeData (DT:GetManholeData ent_name)
    ssConnectedPipes (DT:SelectConnectedPipes ent_name)
  )
  (foreach pipe (ssnamex ss)
    (setq
      connectedPipesData (append connectedPipesData (DT:GetPipeData (cadr pipe)))
    )
  );END foreach
  ;(sssetfirst nil ss)
  connectedPipesData
)
