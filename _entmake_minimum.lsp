; Minimum elements necessary to create an object with (entmakex)
(defun MinDxfDef ( ent_name / result temp )
  ; Return minimum DXF definition of provided ent_name
  ; Credit to T.Willey and MP (The Swamp)
  ;;  will create crap if the current or parent
  ;;  layer of the candidate entity is locked
  (foreach pair (setq result (entget ent_name))
    (if (entmake (setq temp (vl-remove pair result)))
      (progn (entdel (entlast))(setq result temp))
      result
    )
  )
)
; LWPOLYLINE: lightweight polyline
(entmakex
  (append
    (list
      (cons   0 "LWPOLYLINE")         ; Object type
      (cons 100 "AcDbEntity")
      (cons 100 "AcDbPolyline")
      (cons  70 0)                  ; Open(0)/Closed(1)
      (cons  90 2)                  ; Number of vertices
    )
    (list
      (cons 10 p1)                  ; Vertex 1
      (cons 10 p2)                  ; Vertex 2
;      ...                          ; Vertex ...
;      (cons 10 pn)                 ; Vertex n
    )
  );END append
)
(defun DrawLWPOLYLINE ( pointList )
  ; pointList [list] - List of 3d or 2D points
  (entmakex
    (append
      (list
        (cons   0 "LWPOLYLINE")         ; Object type
        (cons 100 "AcDbEntity")
        (cons 100 "AcDbPolyline")
        (cons  70 0)                  ; Open(0)/Closed(1)
        (cons  90 (length pointList)) ; Number of vertices
      )
      (mapcar
        '(lambda (pt) (cons 10 pt) )
        pointList
      );END mapcar
    );END append
  )

  ; v0.0 - 2017.10.03 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.10.03
)
(defun Draw-Polyline (lst)
  ; IS NOT LWPOLYLINE!!!
  (entmakex (list (cons 0 "POLYLINE") (cons 10 '(0 0 0)) ))
  (mapcar '(lambda (pt) (entmake (list (cons 0 "VERTEX") (cons 10 pt))) ) lst )
  (entmakex (list (cons 0 "SEQEND"))
  );END mapcar
)

; INSERT
(entmakex
  (list
    (cons  0 "INSERT")
    (cons  2 blockName)           ; Block name
    (cons 10 insertionPoint)      ; Insertion point coordinates
  )
)

; ATTDEF
(entmakex
  (list
    (cons  0 "ATTDEF")  ; Entity type [ename]
    (cons  1 def)       ; Value [str], spaces allowed
    (cons  2 tag)       ; Tag [str], spaces not allowed
    (cons  3 pr)        ; Prompt [str], spaces allowed
    (cons 10 pt)        ; Text first base point [pt]
    (cons 40 h)         ; Text height [real]
    (cons 70 f)         ; Attribute flag [int]: 0=No flags, 1=Invisible, 2=Constant, 4=Verification is Required, 8=Preset (no prompt)
  )
)
(entmakex
  (list
    (cons 0 "CIRCLE") ; Entity type [ename]
    (cons 10 c)       ; Circle centre point point [pt]
    (cons 40 r)       ; Circle radius
  )
)
(entmakex
  (list
    (cons 0 "LINE")   ; Entity type [ename]
    (cons 10 p1)      ; Line point 1 [pt]
    (cons 11 p2)      ; Line point 2 [pt]
  )
)
(entmakex
  (list
    (cons 0 "POINT")  ; Entity type [ename]
    (cons 10 pt)      ; Point coordinate [pt]
  )
)
