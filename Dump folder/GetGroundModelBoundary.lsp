(defun c:xx ()
  ; Trigger
  (defun *error* ( errorMessage ) (princ (strcat "n-------- ERROR: " errorMessage " --------n")) (vl-bt) (DT:ReportError))
  (DT:AutoLoadFileFromCivilTemp "ErrorTracing.lsp")
  (DT:AutoLoadFileFromCivilTemp "GetGroundModelBoundary.lsp")
  (princ (strcat "\nTemp file loaded (" (DT:Now) ")\n"))(princ)
  (c:FindGroundModelPerimeter)

  ; v0.0 - 2017.10.24 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.10.24
)
(defun c:FindGroundModelPerimeter ()
  (if (setq ss (ssget '((0 . "3DFACE"))))
    (if (setq pt (getpoint "\nPick a point on the model edge to start scan: "))
      (if (setq innerPt (getpoint "\nSelect a point inside the model:"))
        ; (DT:FindGroundModelPerimeter ss pt innerPt)
        (DT:ExportData ss pt innerPt "C:/Users/davidt/Desktop/test.txt")
      );END if
    );END if
  );END if

  ; v0.0 - 2017.10.24 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.10.24
)
(defun DT:FindGroundModelPerimeter ( ss pt innerPt / 3dFaceList nextPoint return )
  (alert "DT:FindGroundModelPerimeter is still very inefficient!\nit gets blocked when it needs to process\na small amount of 3D faces")
  (if (DT:Arg 'DT:FindGroundModelPerimeter '((ss 'pickset)(pt 'list)))
    (progn
      (setq return (list pt))
      (setq nextPoint pt)

      ; Get 3d faces enames from pickset
      (foreach a (ssnamex ss)
        (if (= 'ename (type (cadr a)))
          (setq 3dFaceList (append 3dFaceList (list (cadr a))))
        );END if
      );END foreach

      (while (or (not (DT:req nextPoint pt)) (= 1 (length return)))
        (setq nextPoint (DT:GetNearBoundaryPoint 3dFaceList nextPoint innerPt))
        (setq return (append return (list nextPoint)))
      );END repeat


      (entmakex
        (append
          (list
            (cons   0 "LWPOLYLINE")
            (cons 100 "AcDbEntity")
            (cons 100 "AcDbPolyline")
            (cons  70 1) ; Open(0)/Closed(1)
            (cons  90 (length return)) ; Number of vertices
          )
          (mapcar
            '(lambda (ptt) (cons 10 ptt) )
            return
          );END mapcar
        );END append
      )

      kk

    );END progn
  );END if

  ; v0.0 - 2017.10.24 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.10.24
)
(defun DT:Get3dFacePoints ( ename / eList return )
  (if (DT:Arg 'DT:Get3dFacePoints '((ename 'ename)))
    (progn
      (setq eList (entget ename))
      ; Remove duplicated point
      (setq return (list (cdr (assoc 10 eList))))
      (foreach pt (list (cdr (assoc 11 eList)) (cdr (assoc 12 eList)) (cdr (assoc 13 eList)))
        (if (not (DT:ListContains return pt))
          (setq return (append return (list pt)))
        );END if
      );END foreach
      return
    );END progn
  );END if

  ; v0.0 - 2017.10.24 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.10.24
)
(defun DT:ListContains ( l item / return )
  ; Return true if list l contains item
  (if (and l item (= 'list (type l)))
    (progn
      (setq return nil)
      (foreach listItem l
        (if (DT:req listItem item) (setq return T))
      );END foreach
      return
    );END progn
  );END if

  ; v0.0 - 2017.10.24 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.10.24
)
(defun DT:Get3dFacesAroundPoint ( l pt / 3dFacePoints return )
  ; Return the entity names of the 3D faces which contain pt
  (if (DT:Arg 'DT:Get3dFacesAroundPoint '((l 'list)(pt 'list)))
    ; Get ename and points of each 3dface in l
    (progn
      (foreach 3dFaceEname l
        (setq 3dFacePoints (DT:Get3dFacePoints 3dFaceEname))
        (if (DT:ListContains 3dFacePoints pt)
          (setq return (append return (list 3dFaceEname)))
        );END if
      );END foreach
      return
    );END progn
  );END if

  ; v0.0 - 2017.10.24 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.10.24
)
(defun DT:GetNearBoundaryPoint ( 3dFaceList pt innerPt / surrounding3dFaces surrounding3dFacesPoints referenceAngle relativeAngle angleList angleListData smallestAngle smallestAnglePoint )
  (if (DT:Arg 'DT:GetNextBoundaryPoint '((3dFaceList 'list)(pt 'list)(innerPt 'list)))
    (progn
      ; Get pt surrounding 3dFaces
      (setq surrounding3dFaces (DT:Get3dFacesAroundPoint 3dFaceList pt))
      ; (DT:PrintVar 'surrounding3dFaces)

      ; Get surrounding 3dFace points
      (foreach 3dFaceEname surrounding3dFaces
        (setq surrounding3dFacesPoints (append surrounding3dFacesPoints (DT:Get3dFacePoints 3dFaceEname)))
      );END foreach
      ; (DT:PrintVar 'surrounding3dFacesPoints)

      ; Get angles between pt and those 3D Faces points
      (setq referenceAngle (angle pt innerPt))
      (foreach surroundingPt surrounding3dFacesPoints
        (if (not (DT:Req pt surroundingPt))
          (setq
            relativeAngle (DT:GetRelativeAngle referenceAngle (angle pt surroundingPt))
            angleList (append angleList (list relativeAngle))
            angleListData (append angleListData (list (list relativeAngle surroundingPt)))
          )
        );END if
      );END foreach
      ; (DT:PrintVar 'angleListData)

      ; Get the smallest angle
      (setq smallestAngle (apply 'min angleList))
      ; (DT:PrintVar 'smallestAngle)

      ; Get the point associated with the smallest angle
      (foreach dataEntry angleListData
        (if (= (nth 0 dataEntry) smallestAngle)
          (setq smallestAnglePoint (nth 1 dataEntry))
        );END if
      );END foreach
      ; (DT:PrintVar 'smallestAnglePoint)

      ; Return smallestAnglePoint
      smallestAnglePoint
    );END progn
  );END if

  ; v0.0 - 2017.10.24 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.10.24
)
(defun DT:ExportData ( ss pt innerPt filePath / 3dFaceList 3dFacePoints 3dFaceDataString 3dFacePointCoordinatesString fileContent contentList )
  (if (DT:Arg 'DT:ExportData '((ss 'pickset)(pt 'list)(innerPt 'list)(filePath 'str)))
    (progn
      (DT:AutoLoadFileFromCivilTemp "FileHandling.lsp")
      (setq fileContent "")

      ; Add start point
      (setq fileContent (strcat fileContent (DT:PointToString pt ";")))
      (setq fileContent (strcat fileContent ";"))
      ; Add inner point
      (setq fileContent (strcat fileContent (DT:PointToString innerPt ";")))
      (setq fileContent (strcat fileContent "\n"))

      (DT:WriteFile fileContent filePath)

      ; Get 3d faces enames from pickset
      (foreach a (ssnamex ss)
        (if (= 'ename (type (cadr a)))
          (setq 3dFaceList (append 3dFaceList (list (cadr a))))
        );END if
      );END foreach

      ; Get 3d faces data
      (foreach 3DfaceEname 3dFaceList
        (if (setq 3dFacePoints (DT:Get3dFacePoints 3DfaceEname))
          (progn
            (setq 3dFaceDataString (vl-princ-to-string 3DfaceEname))
            (setq 3dFaceDataString (strcat 3dFaceDataString ";"))

            ; Get list with points as strings
            (setq 3dFacePointCoordinatesString
              (mapcar
                '(lambda (pt) (DT:PointToString pt ";"))
                3dFacePoints
              );END mapcar
            );END setq

            ; Stringify the list
            (foreach 3dPointString 3dFacePointCoordinatesString
              (setq 3dFaceDataString (strcat 3dFaceDataString 3dPointString ";"))
            );END foreach

            (setq contentList (append contentList (list 3dFaceDataString)))
          );END progn
        );END if
      );END foreach

      (setq chunkId 0)
      (while (> (length contentList) 0)
        (setq chunk "")
        (setq i 0)
        (repeat 1000
          (setq chunk (strcat (nth i contentList) "\n"))
          (setq i (+1 i))
        );END repeat
        (DT:PrintVar 'chunk)
        (DT:WriteFile chunk (strcat filePath (itoa chunkId)))
        (setq chunkId (+chunkId i))
        (setq contentList nil)
      );END while
      (princ)
    );END progn
  );END if

  ; v0.0 - 2017.10.25 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.10.25
)
(defun DT:PointToString ( pt delimiter / strList )
  ; Returns a point as a string separated by spaces
  (setq strList (mapcar '(lambda (x) (LM:rtos x 2 5)) pt ))
  (strcat (nth 0 strList) delimiter (nth 1 strList) delimiter (nth 2 strList) )

  ; v0.0 - 2017.10.25 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.10.25
)
