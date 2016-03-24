(defun c:rrt (/
               ; local functions
               getSegment get-opp-ang undobegin undoend
               ; local variables
               ent txt_ent obj txt_obj obj_typ ang ans
               )

  ;;; FUNCTION
  ;;; rotates the user selected (M)TEXT to the user selected
  ;;; entity. valid entites are light weight plines, lines
  ;;; and (m)text. you are given the chance to rotate the
  ;;; by 180 degrees after intial rotation.
  
  (vl-load-com)
  
  (defun getSegment (obj pt / cpt eParam stParam)
    (cond ((setq cpt (vlax-curve-getClosestPointTo obj pt))
           (setq eParam (fix (vlax-curve-getEndParam obj)))
           (if (= eParam (setq stParam (fix (vlax-curve-getParamAtPoint obj cpt))))
             (setq stParam (1- stParam))
             (setq eParam (1+ stParam))
             )
           (list eParam (vlax-curve-getPointAtParam obj stParam)
                 (vlax-curve-getPointAtParam obj eParam))
           )
          )
    )

  ;; undo functions
  (defun undobegin ()
    (vla-EndUndoMark
      (vlax-get-property
        (vlax-get-acad-object)
        'ActiveDocument
        )
      )
    (vla-StartUndoMark
      (vlax-get-property
        (vlax-get-acad-object)
        'ActiveDocument
        )
      )
    )

  (defun undoend ()
    (vla-EndUndoMark
      (vlax-get-property
        (vlax-get-acad-object)
        'ActiveDocument
        )
      )
    )

  ;; returns the oppsite of an angle define in radians
  (defun get-opp-ang (ang)
    (cond ((< ang pi)(+ ang pi))
          ((> ang pi)(- ang pi))
          ((equal ang pi) 0.0)
          ((equal ang 0.0) pi)
          )
    )

  ;; ================= body of main function starts here ======================
 
  ;;  -----------   Get the Text to Align  -----------------
  (cond
    ((setq txt_ent (car (entsel "\nSelect text to align")))
     (setq txt_obj (vlax-ename->vla-object txt_ent)
           obj_typ (vlax-get-property txt_obj 'ObjectName)
           )
     (cond
       ((or (= obj_typ "AcDbMText") (= obj_typ "AcDbText") (= obj_typ "AcDbBlockReference")))
       (T
         (setq txt_ent nil)
         (alert "I only know how to align (M)TEXT, sorry! "))
      )
    )
  )
 
  ;;  -----------   Get the Object to Align To  -----------------
  (cond
    ((and txt_ent
          (setq ent (entsel "\nSelect entity for alignment: ")))
       (setq obj (vlax-ename->vla-object (car ent))
             obj_typ (vlax-get-property obj 'ObjectName)
       )
       (cond
         ((= obj_typ "AcDbPolyline")
          (if (setq pt_lst (getSegment obj (last ent)))
            (setq ang (angle (cadr pt_lst)(caddr pt_lst)))
            )
          )
         ((= obj_typ "AcDbLine")
          (setq ang (vlax-get-property obj 'Angle))
          )
         ((= obj_typ "AcDbText")
          (setq ang (vlax-get-property obj 'Rotation))
          )
         ((= obj_typ "AcDbMText")
          (setq ang (vlax-get-property obj 'Rotation))
          )
         ((= obj_typ "AcDbBlockReference")
          (setq ang (vlax-get-property obj 'Rotation))
          )
         ((= obj_typ "AcDbArc")
          (setq ang (angle
                      (vlax-safearray->list
                        (vlax-variant-value
                          (vla-get-StartPoint obj)))
                      (vlax-safearray->list
                        (vlax-variant-value
                          (vla-get-EndPoint obj)))
                    )
           )
          )
         
         (T (alert "That's not an entity I deal with"))
       )
     )
  )
 
  ;;  -----------   Align the Text   -----------------
  (cond
    ((null ang)) ; do nothing
    ((null txt_ent)) ; do nothing
    (T
      (undobegin)
      (vlax-put-property txt_obj 'Rotation ang)
      (setq ans (getstring "\nRotate 180 [Y/N]<N>: "))
      (if (= (strcase ans) "Y")
        (vlax-put-property txt_obj 'Rotation (get-opp-ang ang))
        )
      (vlax-release-object txt_obj)
      (undoend)
     )
   )
  (princ)
  
  ; NOTE: Wayne - RRT.lsp modificado para poder trabajar con bloques también
  ; Author: David Torralba
  ; Last revision: 2016.02.16
)