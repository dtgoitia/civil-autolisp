(defun c:TieSurvey(
                    /
                    ssp pt reftext layerName dist
                    sst pairList ent_name pt3D
                    ptList textFound noTextFound noTextFoundList
                    markLayer
                    startTime endTime msg
                  )
  ; This is the main function. It finds the closest text to each
  ; point and creates a 3D point with the 2D point X and Y coordinates,
  ; and with the text level as Z value. It marks up as well not found
  ; points.
  ; TODO check if the same text has been used twice and solve conflicts.

  (princ "\nSelect POINT objects to get XY values:")
  (setq
    ssp (ssget '(( 0 . "POINT")) )  ; Get points
    textFound 0                     ; Found text counter
    noTextFound 0                   ; No found text counter
  )
  ; Force user to select a text of reference
  (while (not reftext)
    (setq reftext (entsel "\nSelect an example text: ") )
    (if (/= reftext nil)
      (if (= "TEXT" (cdr (assoc 0 (entget (car reftext)) )) )
        (progn
          (setq
            layerName (cdr (assoc 8 (entget (car reftext)) ))
            dist (* 8 (cdr (assoc 40 (entget (car reftext)) )))
          )
          (princ "select object accepted.")
        );END progn
        (progn
          (setq reftext nil)
          (princ "selected object is not a text.")
        );END progn
      );END if2
      (princ "nothing selected.")
    );END if1
  );END while

  ; Run through each point
  (setq
    startTime (PrintDateTime)
    i 0
  )
  (foreach x (ssnamex ssp)
    (if (= 'ename (type (cadr x)))
      (progn
        (setq
          i (+ i 1)
          pt (cdr (assoc 10 (entget (cadr x))))           ; Get point coordinates
        )
        ; if there is any close text selected...
        (if (setq sst (GetTextsByDistanceAndLayer pt dist layerName) )
          (setq
            pairList (GetTextSetLocation sst)           ; Text selection set pairlist
            ent_name (GetClosestElement pairList pt)    ; Closest text entity name
            pt3D (list (car pt) (cadr pt) (atof (DT:GetText ent_name)) ) ; Real 3D point
            ptList (append ptList (list pt3D))          ; 3D point list
            textFound (+ textFound 1)                   ; Text found for point
          )
          (setq
            noTextFound (+ noTextFound 1)
            noTextFoundList (append noTextFoundList (list pt))
          )
        );END if
      );END progn
    );END if1
  );END foreach

  ; Build 3D points
  (foreach a ptList
    (entmakex
      (list
          (cons 0 "POINT")
          (cons 8 "_3Dpoints")
          (cons 10 a)
      );END list
    );END entmake
  );END foreach

  ; Mark point without found text
  (setq markLayer "_PointsWithoutLevels")
  (CreateBlock
    "Points with no level found"
    markLayer
    (MarkNoLinked
      noTextFoundList
      (* 2 (cdr (assoc 40 (entget (car reftext)))))
      markLayer
    );END MarkNoLinked
  );END CreateBlock

  ; Enclosure message
  (setq
    endTime (PrintDateTime)
    msg (strcat
      "\nStart time: " startTime
      "\nEnd time:   " endTime
      "\n" (itoa i) " iterations executed"
      "\n >> " (itoa textFound) " points and texts linked"
      "\n >> " (itoa noTextFound) " points without a text (marked*)"
      "\n* See layer \"_PointsWithoutLevels\" "
      );END strcat
  );END setq
  (princ msg)
  (alert msg)
  (princ)

  ; v0.0 - 2017.06.20 - Message added for clarification purpose
  ; v0.0 - 2016.??.?? - First issue
  ; Author: David Torralba
  ; Last revision: 2017.06.20
)
(defun GetTextCoordinates (ent_name)
  ; Return text coordiantes
  ; ent_name [ename] - Entity name of the text
  (cdr (assoc 10 (entget ent_name) ) )
)
(defun GetTextRotation (ent_name)
  ; Return text rotation angle (0 at the east, and grows anticlockwise)
  ; ent_name [ename] - Entity name of the text
  (cdr (assoc 50 (entget ent_name) ) )
)
(defun GetTextBoxPoints (ent_name / coords ang boxPoints ang01 ang13 d01 d13 d12 d23 p1 p2 p3 p4 p5 p6 )
  ; Return text box vertex coordinates and pseudocentre coordinates in a list
  ; ent_name [ename] - Entity name of the text
  (setq
    coords (GetTextCoordinates ent_name)
    ang (GetTextRotation ent_name)
    boxPoints (textbox (entget ent_name))
    ang01 (angle (list 0.0 0.0 0.0) (nth 0 boxPoints))
    ang13 (angle (nth 0 boxPoints) (nth 1 boxPoints))
    d01 (distance (list 0.0 0.0 0.0) (nth 0 boxPoints))
    d13 (distance (nth 0 boxPoints) (nth 1 boxPoints))
    d12 (* d13 (cos ang13))
    d23 (- (nth 1 (nth 0 boxPoints)) (nth 1 (nth 1 boxPoints)) )
    p1 (polar coords (+ ang ang01) d01)
    p2 (polar p1 ang d12)
    p3 (polar p2 (+ ang (* -0.5 pi)) d23)
    p4 (polar p3 (+ ang pi) d12)
    p5 (polar p1 (+ ang (* -0.75 pi)) (/ d23 (sqrt 2)) )
    p6 (polar p3 (+ ang (* +0.25 pi)) (/ d23 (sqrt 2)) )
  )
  (list p1 p2 p3 p4 p5 p6) ; return text box and pseudocentre point list
)
(defun GetTextCentroid (ent_name / coords ang boxPoints ang01 ang03 d01 d03 p1 p3 pC)
  ; Return text box centre coordinates
  ; ent_name [ename] - Entity name of the text
  (setq
    coords (GetTextCoordinates ent_name)
    ang (GetTextRotation ent_name)
    boxPoints (textbox (entget ent_name))
    ang01 (angle (list 0.0 0.0 0.0) (nth 0 boxPoints))
    ang03 (angle (list 0.0 0.0 0.0) (nth 1 boxPoints))
    d01 (distance (list 0.0 0.0 0.0) (nth 0 boxPoints))
    d03 (distance (list 0.0 0.0 0.0) (nth 1 boxPoints))
    p1 (polar coords (+ ang ang01) d01)
    p3 (polar coords (+ ang ang03) d03)
    pC (polar p1 (angle p1 p3) (* 0.5 (distance p1 p3)))
  )
  pC ; return text centroid
)
(defun GetTextsByDistanceAndLayer (pC dist layerName / pn pt pList ang ss )
  ; Return a selection set of all the texts within "dist" distance from
  ; "pC" point and within the layer "layerName"; or nil if nothing selected.
  ; pC [pt] - Center point coordinates
  ; dist [real] - Radial distance from pC within objects will be selected
  ; layerName [str] - Layer name where selected objects
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
  (ssget "WP" pList (list (cons 8 layerName)) )  ; return selection set
)
(defun GetTextSetLocation (ss / ls)
  ; Returns a pair list with a pseudocentre and the entity name of the text
  ; ss [sel] - Selection set with all the text entity name
  (foreach a (ssnamex ss)
    (if (= 'ename (type (cadr a)) )
      (if (= "TEXT" (cdr (assoc 0 (entget (cadr a)))))
        (setq
          ls (append ls (list (list (nth 4 (GetTextBoxPoints (cadr a))) (cadr a) ) ))
          ls (append ls (list (list (nth 5 (GetTextBoxPoints (cadr a))) (cadr a) ) ))
        )
      ); END if2
    );END if1
  );END foreach
  ls ; return pseudocentre-entity name pair list
)
(defun GetClosestElement (pairList pt / closestElement dist)
  ; Returns entity name of the closest element of a group to the point "pt".
  ; pairList [lst] - Pair list with coordinate and entity name
  ; pt [pt] - Reference point coordinates
  (setq
    closestElement (car pairList))
    dist (distance pt (car closestElement))
  (foreach element pairList
    (if
      (>
        (distance pt (car closestElement) )
        (distance pt (car element) )
      )
      (setq closestElement element)
    );END if
  );END foreach
  (cadr closestElement) ; return closestElement entity name
)
(defun MarkNoLinked (pointList markSize layerName / markList aNewLayer)
  ; Create circles to mark pointList points and return a list with all ent_names
  ; pointList [lst] - list with point coordinates to mark
  ; markSize [real] - Diameter of the circle used to mark
  ; layerName [str] - Layer where to create the circles
  (setq
    aNewLayer (vla-add (vla-get-layers (vla-get-activedocument (vlax-get-Acad-Object))) layerName )
  )
  (foreach a pointList
    (setq
      markList
        (append
          markList
          (list (entmakex (list (cons 0 "CIRCLE") (cons 8 layerName) (cons 10 a) (cons 40 markSize) ) ) )
        ); END append
    );END setq
  );END foreach
  markList ; return created entity name list
)
(defun CreateBlock (blockName layerName entList)
  ; Create a block in layer layerName with entList entities and named blockName
  ; blockName [str] - Name to assign the blog
  ; layerName [str] - Layer where to create the block
  ; entList [lst] - List of entities to add to the block

  ; Open block creation
  (entmakex (list (cons 0 "BLOCK") (cons 10 (list 0 0 0)) (cons 2 blockName) (cons 70 0)))

  ; Add entities on entList to block definition and erase them after from model space
  (foreach a entList
    (entmakex (entget a))
    (entdel a)
  );END foreach

  ; Close block creation
  (entmakex (list (cons 0 "ENDBLK") (cons 8 layerName) ))

  ; Create an INSERT of the block (instantiation)
  (entmakex (list (cons 0 "INSERT") (cons 8 layerName) (cons 2 blockName) (cons 10 (list 0 0 0)) ))
)
(defun c:TieSinglePoint ( / pXY p )
  ; Draw a point with in the selected XY coordinates with the selected level
  (setq pXY (DT:flatPoint (getpoint "\nSelect point to get XY coordinates: ")) )
  (entmakex
    (list
      (cons 0 "POINT" )
      (cons 8 "_3Dpoints" )
      (cons 10 (list (nth 0 pXY) (nth 1 pXY) (DT:clic_or_type_level)) )
    )
  )
  ; v0.0 - 2017.03.28 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.03.28
)
(defun c:BlockToPoint ()
  ; Command to replace blocks for points
  (if (setq ss (ssget '((0 . "INSERT"))))
    (foreach a (ssnamex ss)
      (if (= 'ename (type (cadr a)))
        (DT:BlockToPoint (cadr a))
      );END if
    );END foreach
  );END if

  ; v0.0 - 2017.08.10 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.10
)
(defun DT:BlockToPoint ( ename / object insertionPoint newEname )
  ; Replace ename block for a point in ename's insertion point

  (if (DT:Arg 'DT:BlockToPoint '((ename 'ename)))
    (if (= "INSERT" (cdr (assoc 0 (entget ename))))
      (progn
        (setq object (vlax-ename->vla-object ename))
        (setq insertionPoint (vlax-safearray->list (vlax-variant-value (vlax-get-property object 'InsertionPoint))))
        (setq newEname (entmakex (list (cons 0 "POINT") (cons 10 insertionPoint))))
        (if newEname
          (progn
            (vla-delete object)
            newEname
          );END progn
        );END if
      );END progn
    );END if
  );END if

  ; v0.0 - 2017.08.10 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.10
)
