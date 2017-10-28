(defun DT:AddVertexInterval ( ename interval / int in pt ppt )
  ; Add vertices to "ename" LWPolyline every "interval" distance.
  ; ename [ename] - Entity name of the polyline to modify
  ; interval [real] - Real number specifying interval distance

  (if (DT:Arg 'DT:AddVertexInterval '((ename 'ename) (interval 'real)))
    (if (= "LWPOLYLINE" (cdr (assoc 0 (entget ename))))
      (if
        (and
          (setq int interval)
          (setq in int)
        );END and
        (while (setq pt (vlax-curve-getPointAtDist ename int))
          (setq ppt (vlax-curve-getparamatpoint ename pt))
          (vlax-invoke
            (vlax-ename->vla-object ename)
            'AddVertex
            (1+ (fix ppt))
            (list (car pt) (cadr pt))
          )
          (setq int (+ int in))
        )
      )
      (DT:Error 'DT:AddVertexInterval "ename is not a LWPOLYLINE")
    );END if
  );END if

  ; v0.0 - 2017.07.27 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.07.27
)
(defun DT:CloseEndToEndPolylines ( ename / pointList pointListLength firstVertex lastVertex )
  ; Fixes LWPOYLINE passed, if first and last vertexes match on coordinates
  ; Fixing means:
  ;  - Remove last vertex
  ;  - Close polyline
  (if (DT:Arg 'DT:CloseEndToEndPolylines '((ename 'ename)))
    (if (= "LWPOLYLINE" (cdr (assoc 0 (entget ename))))
      (if (setq pointList (DT:GetLwpolyPoints ename))
        (progn
          (setq firstVertex (nth 0 pointList))
          (setq lastVertex  (nth (- (length pointList) 1) pointList))
          ; (setq result (DT:req firstVertex lastVertex))
          ; (princ "\n(DT:req firstVertex lastVertex) = ")(princ result)
          (if (DT:req firstVertex lastVertex)
            (progn
              (DT:RemoveNthVertex ename 0)
              (vla-put-closed (vlax-ename->vla-object ename) :vlax-true)
            );END progn
            nil
          );END if
        );END progn
      );END if
      (DT:Error 'DT:CloseEndToEndPolylines "ename should be a LWPOYLINE")
    );END if
  );END if

  ; v0.0 - 2017.10.19 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.10.19
)
(defun DT:RemoveNthVertex ( ename n / pointList gc:2dPointListToVariant i newPointList newPointListVariant )
  ; Remove nth vertex of passed LWPOLY
  (if (DT:Arg 'DT:RemoveNthVertex '((ename 'ename)(n 'int)))
    (if (= "LWPOLYLINE" (cdr (assoc 0 (entget ename))))
      (if (setq pointList (DT:GetLwpolyPoints ename) pointListLength (length pointList))
        (if (< 2 pointListLength)
          (if (< n pointListLength)
            (progn
              (defun gc:2dPointListToVariant (lst)
                (vlax-make-variant
                  (vlax-safearray-fill
                    (vlax-make-safearray
                      vlax-VbDouble
                      (cons 0 (1- (* 2 (length lst))))
                    )
                    (apply 'append lst)
                  )
                )
              )
              (setq i 0)
              (foreach pt pointList
                (if (= i n) nil (setq newPointList (append newPointList (list pt))))
                (setq i (1+ i))
              );END foreach
              (vlax-put-property
                (vlax-ename->vla-object ename)
                'Coordinates
                (gc:2dPointListToVariant newPointList)
              )
            );END progn
            (DT:Error 'DT:RemoveNthVertex (strcat "imposible to get vertex number " (itoa n)))
          );END if
          (DT:Error 'DT:RemoveNthVertex "you can't remove more vertexes at selected polyline")
        );END if
      );END if
      (DT:Error 'DT:RemoveNthVertex "ename should be a LWPOYLINE")
    );END if
  );END if

  ; v0.0 - 2017.10.19 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.10.19
)
(defun lwpTo3d (vbalwp / c1 c2 pts pts3d mydoc myowner my3d ut i lwc)
  ;;This function loads the extended AutoLISP
  ;;functions provided with Visual LISP.
  ;;The Visual LISP extensions implement ActiveX
  ;;and AutoCAD reactor support through AutoLISP,
  ;;and also provide ActiveX utility and data
  ;;conversion functions, dictionary handling functions,
  ;;and curve measurement functions.

  ;;John W Anstaett 09/03/2006
  ;;draw a 3dPolyline using the vector of a LWPOLYLINE
  ;;vbalwp is a vba object = to the LwPolyLine
  ;;Return a vba Object = to the new 3DPolyLine
  ;;Drawed in the same space as the LwPolyLine
  ;;The layer line type and other vaule are the same as the LwPolyLine
  ;;All z vaule of the 3DPoly are = to the Elevation of the LwPolyline
  (setq c1 (vlax-variant-value
	  (vla-get-Coordinates vbalwp) ;get the coordinates of the
	)	;LwPolyLine as a safearray
  ) ;_ end of setq
  (setq c1 (vlax-safearray-get-u-bound c1 1))
	 ;get number of coordinates
  (setq c1 (/ (- c1 1) 2))  ;set c1 to one less then the number of vectors
  (setq c2 (- (* (+ c1 1) 3) 1)) ;set c2 to the number of coordinates in 3DPoly

  ;;make a safearray to use with the add3DPoly
  (setq pts (vlax-make-safearray
	   vlax-vbDouble
	   (cons 0 c2)
	 ) ;_ end of vlax-make-safearray
  ) ;_ end of setq

  ;;Make a Safearray to use as 3dPoly coordinte
  (setq pts3d (vlax-make-safearray
  vlax-vbDouble
  (cons 0 2)
	   ) ;_ end of vlax-make-safearray
  ) ;_ end of setq

  ;;get the autocad document that the LwPolyline is in
  (setq myDoc (vla-get-Document vbalwp)
  ) ;_ end of setq
  (setq myowner (vla-ObjectIdToObject ;get the owner of the Lwpolyline
	myDoc   ;This will be a block modeSpace or paperSpace
	(vla-get-ownerid vbalwp)
  ) ;_ end of vla-ObjectIdToObject
  ) ;_ end of setq
  (setq my3d (vla-Add3DPoly myowner pts) ;add the 3DPoly
  ) ;_ end of setq

  ;;get Autocad Utility to Translate the Coordinates
  ;;I do this so I do not need to covent the VBA Safearray to list
  ;; to use the lisp trans function
  (setq ut (vla-get-Utility mydoc)
  ) ;_ end of setq
  (setq i 0)
  ;;copy the Coordinate of LwPolyLine to the 3dpoly
  (repeat (+ c1 1)
	(setq lwc (vlax-variant-value
  (vla-get-Coordinate vbalwp i)
	 ;Get LwPolyLine Coordinate
	   ) ;_ end of vlax-variant-value
	) ;_ end of setq
	(vlax-safearray-put-element
	  pts3d
	  0
	  (vlax-safearray-get-element lwc 0) ;set x
	) ;_ end of vlax-safearray-put-element
	(vlax-safearray-put-element
	  pts3d
	  1
	  (vlax-safearray-get-element lwc 1) ;sete y
	) ;_ end of vlax-safearray-put-element
	(vlax-safearray-put-element
	  pts3d
	  2
	  (vla-get-Elevation vbalwp) ;set z = Elevation
	) ;_ end of vlax-safearray-put-element

;;;Translate Coordinates form LwPolyLine ocs to world
	;;use vlax-variant-value so pts3d is return as a safearray
	(setq pts3d (vlax-variant-value
	(vla-TranslateCoordinates
	  ut
	  pts3d
	  acOCS
	  acWorld
	  0
	  (vla-get-Normal vbalwp)
	) ;_ end of vla-TranslateCoordinates
  ) ;_ end of vlax-variant-value

	) ;_ end of setq
	(vla-put-coordinate my3d i pts3d) ;set the 3dPoly coordinate
	(setq i (+ i 1))

  ) ;_ end of repeat
  ;;match the 3Dpoly to the LwPolyLine
  (vla-put-layer my3d (vla-get-layer vbalwp)) ;Set Layer
  (vla-put-Closed my3d (vla-get-Closed vbalwp)) ;Set Closed
  (vla-put-Color my3d (vla-get-Color vbalwp)) ;Set Color
  (vla-put-Linetype my3d (vla-get-Linetype vbalwp)) ;Set Linetype
  (vla-put-LinetypeScale my3d (vla-get-LinetypeScale vbalwp))
	 ;Set LinetypeScale
  (vla-put-Lineweight my3d (vla-get-Lineweight vbalwp)) ;set Lineweight
  (vla-put-Visible my3d (vla-get-visible vbalwp)) ;set Visible
  (vla-Update my3d)   ;UpDate the 3DPoly
  (vlax-release-object mydoc)
  (vlax-release-object myowner)
  (vlax-release-object ut)
  (vlax-release-object vbalwp)
  (setq my3d my3d)   ;return the 3DPoly

) ;_ end of defun
