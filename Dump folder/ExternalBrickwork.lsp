(defun c:22 ( / ent_name pt pointList ent_nameRef doNotEscapeVariable ent_nameOff )
  ; Draw external brickwall from workblock

  ;(setq testmode nil)
  ;(setq testmode 1)

  ; Check if layer exist, if not, create with color 80
  (if (not (tblsearch "layer" "e-exbwk"))
    (DT:AddLayer "e-exbwk" 80 "")
  );END if

  ; Select points to show external brick work
  (if (= testmode 1)
    (progn
      (princ "\nExternal brickwall:\n")
      ; SAVE SETTINGS
      (save_environment (list "osmode"))

      (if (not (tblsearch "layer" "e-exbwk"))
        (DT:AddLayer "e-exbwk" 80 "DASHED2")
      );END if

      ; CHANGE SETTINGS
      (setvar "osmode" 545) ; end + int + nea
      (setq doNotEscapeVariable T)
      ; Get points
      (if (setq pointList (DT:GetPointList "\nSelect point (press ENTER to finish): "))
        (progn
          ; Draw polyline
          (if (setq ent_nameRef (DT:DrawPolyline pointList 256 "e-exbwk" 0.150 nil 1.0))
            ; Offset it
            ; while
            (while doNotEscapeVariable
              (if (setq ent_nameOff (DT:OffSet ent_name 0.451 pt ) )
                (progn
                  ; Erase referenc object
                  (vla-delete (vlax-ename->vla-object ent_nameRef))
                  ; Create label


                  ; Align the label and set doNotEscapeVariable=nil when finished to breakloop

                );END progn
                (princ "\nERROR! ent_nameRef=nil")
              );END if
            );END while
          );END if

          ;(DT:OffSet ent_name pt)
          ; Return created entity name if successful, if not nil
          ; External BrickWork
          ;   0.451 = Offset from building outline
        );END progn
      );END if

      ; RESTORE SETTINGS
    	(restore_environment)
    );END progn

    ;TESTMODE = 0
    (progn
      (vla-offset (vlax-ename->vla-object (car (entsel))) 0.3)
      (vlax-put-property (vlax-ename->vla-object (entlast)) 'Layer "e-exbwk")
      (vlax-put-property (vlax-ename->vla-object (entlast)) 'ConstantWidth 0.15)
      (vlax-put-property (vlax-ename->vla-object (entlast)) 'Color 256)
      (vla-offset (vlax-ename->vla-object (entlast)) -0.6)
      (vlax-put-property (vlax-ename->vla-object (entlast)) 'Layer "e-exbwk")
      (vlax-put-property (vlax-ename->vla-object (entlast)) 'ConstantWidth 0.15)
      (vlax-put-property (vlax-ename->vla-object (entlast)) 'Color 256)
      (command
        "_.erase"
          pause ""
        "_.break"
          (getpoint "\nBREAK\nClick to start: ")
          pause
        "_.break"
          (getpoint "\nBREAK\nClick to start: ")
          pause
      )
      (setq pt (cadr (grread 't)))
      (entmakex
        (list
          (cons 0 "TEXT")
          (if (tblsearch "style" "ROMANS") (cons 7 "ROMANS"))
          (cons 8 "e-exbwk") ; layer
          (cons 10 (polar pt (* 1.5 pi) 2.2))
          (cons 11 pt) ; needed for text justification
          (cons 40 0.3 ) ; Height
          (cons 1 "EX BWK") ; content
          (cons 62 256) ; color ByLayer
          (cons 71 0) ; needed for text justification
          (cons 72 1) ; needed for text justification
        );END list
      );END entmakex
      (c:R)
    );END progn
  );END if
)
(defun DT:CreateText ( content pt rotation layer     style    height color )
  ; Return entity created if successful, if not nil
  (entmakex
    (append
      (list
        (cons 0 "TEXT")
        (cons 1 content)
        (cons 10 (polar pt (* 1.5 pi) 2.2))
        (cons 11 pt) ; needed for text justification
        (cons 40 height)
        (cons 71 0)
        (cons 72 1)
        (cons 73 2)
      );END list
      (if (tblsearch "style" style) (list (cons 7 style)))
      (if layer (list (cons 8 layer)))
      (if rotation (list (cons 50 rotation)))
      (if color (list (cons 62 color)) (list (cons 62 256)))
    );END append
      ; 71=0, 72=1, 73=1 Bottom centre
      ; 71=0, 72=1, 73=2 Middle centre
  );END entmakex
)
(defun c:DrawPolylineLabel (  )
  (DT:DrawPolylineLabel (car (entsel "\nSelect polyline to label: ")) "TEXT OF EXAMPLE")
)
(defun c:1() (c:ExternalBrickwork) )
(defun c:ExternalBrickwork ( / ent_name )
  ;(DT:DrawOffsetPolyline layer offset width)
  ;  Returns create ent_name if successful

  ;(setq ent_name (DT:DrawOffsetPolyline "e-exbwk" 0.45 0.15) )
  ;(DT:DrawPolylineLabel ent_name "EX BWK" 0.825 0.3)
  (DT:DrawPolylineLabel (car (entsel)) "EX BWK" 0.825 0.3)
)
(defun DT:DrawPolylineLabel ( ent_name textString textOffset textSize /
  gr pt closestPoint aText pText auxiliaryLine auxiliaryCircle auxiliaryText )
  ; Insert a text aligned to and along selected polylne
  (if (and ent_name textString textOffset textSize )
    (if
      (and
        (= 'ename (type ent_name))
        (= 'str (type textString))
        (numberp textOffset)
        (numberp textSize)
      );END and
      (while (setq gr (grread T 8))
        (if (and auxiliaryLine (entget auxiliaryLine)) (vla-delete (vlax-ename->vla-object auxiliaryLine)) )
        (if (and auxiliaryCircle (entget auxiliaryCircle)) (vla-delete (vlax-ename->vla-object auxiliaryCircle)) )
        (if (and auxiliaryText (entget auxiliaryText)) (vla-delete (vlax-ename->vla-object auxiliaryText)) )
        (setq
          pt (cadr gr)
          closestPoint (vlax-curve-getClosestPointTo (vlax-ename->vla-object ent_name) pt)
          auxiliaryLine (entmakex (list (cons 0 "LINE") (cons 10 closestPoint) (cons 11 pt)))
          aText (angle closestPoint pt)
          pText (polar closestPoint aText textOffset)
          auxiliaryCircle (entmakex (list (cons 0 "CIRCLE") (cons 10 pText) (cons 40 textSize)))
          auxiliaryText
            (entmakex
              (list
                (cons 0 "TEXT")
                (cons 10 (polar pText (* 1.5 pi) 2.2))
                (cons 11 pText) ; needed for text justification
                (cons 40 4)
                (cons 1 "textString")
                (cons 50 aText)
                (cons 71 0) ; needed for text justification
                (cons 72 1) ; needed for text justification
                (cons 73 2) ; needed for text justification
              );END list
            );END entmakex
        );END setq
      );END while
      (cond
        ((/= 'ename (type ent_name)) (princ "\nERROR @ DT:DrawPolylineLabel : ent_name is not a ename\n")    (princ) )
        ((/= 'str (type textString)) (princ "\nERROR @ DT:DrawPolylineLabel : textString is not a string\n")       (princ) )
        ((not (numberp textOffset )) (princ "\nERROR @ DT:DrawPolylineLabel : textOffset is not a number\n") (princ) )
        ((not (numberp textSize   )) (princ "\nERROR @ DT:DrawPolylineLabel : textSize is not a number\n")   (princ) )
      );END cond
    );END if
    (cond
      ((not ent_name)   (princ "\nERROR @ DT:DrawPolylineLabel : ent_name=nil\n")   (princ) )
      ((not textString) (princ "\nERROR @ DT:DrawPolylineLabel : textString=nil\n")       (princ) )
      ((not textOffset) (princ "\nERROR @ DT:DrawPolylineLabel : textOffset=nil\n") (princ) )
      ((not textSize)   (princ "\nERROR @ DT:DrawPolylineLabel : textSize=nil\n")   (princ) )
    );END cond
  );END if

  (princ)

  ; v0.0 - 2017.03.10 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.03.10
)
(defun DT:DrawPolyline ( pointList color layer globalWidth lineType lineTypeScale )
  ; Create a LWPOLYLINE
  (if pointList
    (entmakex
      (append
        (list
          (cons   0 "LWPOLYLINE")         ; Object type
          (cons 100 "AcDbEntity")
          (cons 100 "AcDbPolyline")
          (cons  70 0)                  ; Open(0)/Closed(1)
          (cons  90 (length pointList)) ; Number of vertices
        )
        (if lineType (if (tblsearch "ltype" lineType) (list (cons 6 lineType)) ))
        (if layer (list (cons 8 layer)))
        (if globalWidth (list (cons 43 globalWidth)))
        (if lineTypeScale (list (cons 48 lineTypeScale)))
        (if color (list (cons 62 color)) (list (cons 62 256)))
        (mapcar
          '(lambda (pt) (cons 10 pt) )
          pointList
        );END mapcar
      );END append
    );END entmakex
  );END if
)
(defun DT:OffSet ( ent_name d p / p1 p2 pC ang1 an2 )
  ; Offset ent_name entity a "d" distance towards "p" point
  (if (and ent_name d p)
    (if
      (and
        (= 'ename (type ent_name))
        (numberp d)
        (= 'list (type p))
      );END and
      (progn
        ; ang1 = Get angle between "p" and closest point
        (setq
          pC (vlax-curve-getClosestPointTo (vlax-ename->vla-object ent_name) p)
          ang1 (angle p pC)
        );END setq
        ; ang2 = Angle between vertex 1 and vertex 2
        (setq
          p1 (vlax-curve-getPointAtParam (vlax-ename->vla-object ent_name) 0)
          p2 (vlax-curve-getPointAtParam (vlax-ename->vla-object ent_name) 1)
          ang2 (angle p1 p2)
        );END setq
        (cond
          ((> ang1 ang2)
            (princ "\nCASE 1")
          );END subcond
          ((< ang1 ang2)
            (princ "\nCASE 2")
          );END subcond
          (t
            (princ "\np is aligned with selected segment")
          );END subcond
        );END cond
        ; ang1 > ang2 ,
      );END progn
      (cond
        ((/= 'ename (type ent_name)) (princ "\nERROR @ DT:OffSet : ent_name is not a ename\n")(princ) )
        ((= nil (numberp d))         (princ "\nERROR @ DT:OffSet : d is not a number\n")      (princ) )
        ((/= 'list (type p))         (princ "\nERROR @ DT:OffSet : p is not a point\n")       (princ) )
      );END cond
    );END if
    (cond
      ((not ent_name) (princ "\nERROR @ DT:OffSet : ent_name=nil\n")(princ) )
      ((not d)        (princ "\nERROR @ DT:OffSet : d=nil\n")       (princ) )
      ((not p)        (princ "\nERROR @ DT:OffSet : p=nil\n")       (princ) )
    );END cond
  );END if
)
(DT:OffSet (car (entsel)) 1 (getpoint))
; Working on DT:Offset
