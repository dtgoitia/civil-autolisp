(defun c:rtt (/
               ; local functions
               ;getSegment get-opp-ang undobegin undoend DT:matrix_calc
               ; local variables
               ;ent txt_ent obj txt_obj obj_typ ang ans
               )

  ;;; FUNCTION
  ;;; rotates the user selected (M)TEXT to the user selected
  ;;; entity. valid entites are light weight plines, lines
  ;;; and (m)text. you are given the chance to rotate the
  ;;; by 180 degrees after intial rotation.
  
  (vl-load-com)
  
  (defun DT:matrix_calc ( nent p )
    ; nent (nentsel)
    (setq
      v1 (car    (caddr nent))
      v2 (cadr   (caddr nent))
      v3 (caddr  (caddr nent))
      P0 (cadddr (caddr nent))
      px (car   p) ; coord X en el dibujo hijo
      py (cadr  p) ; coord Y en el dibujo hijo
      pz (caddr p) ; coord Z en el dibujo hijo
    )
    (setq
      vx (list (car   v1) (car   v2) (car   v3))
      vy (list (cadr  v1) (cadr  v2) (cadr  v3))
      vz (list (caddr v1) (caddr v2) (caddr v3))
      e1 (mapcar '* vx p)
      e2 (mapcar '* vy p)
      e3 (mapcar '* vz p)
      x (+ (+ (+ (car e1) (cadr e1)) (caddr e1)) (car   P0)) ; coord X en el dibujo padre
      y (+ (+ (+ (car e2) (cadr e2)) (caddr e2)) (cadr  P0)) ; coord Y en el dibujo padre
      z (+ (+ (+ (car e3) (cadr e3)) (caddr e3)) (caddr P0)) ; coord Z en el dibujo padre
      p_padre (list x y z)
    )
  )
  
  (defun getSegment (obj pt / cpt eParam stParam)
    (princ "\nobj = ")(princ obj)
    (princ "\npt  = ")(princ pt) ; punto clicado (coordenadas dibujo padre)
    (cond
      ( (setq cpt (vlax-curve-getClosestPointTo obj pt))
        (princ "\ncpt   = ")(princ cpt)(princ)
        
        (setq eParam (fix (vlax-curve-getEndParam obj)))
        (princ "\neParam   = ")(princ eParam)(princ)
        (if (= eParam (setq stParam (fix (vlax-curve-getParamAtPoint obj cpt))))
          (setq stParam (1- stParam))
          (setq eParam (1+ stParam))
        )
        (princ "\nafter if:")
        (princ "\nstParam   = ")(princ stParam)(princ)
        (princ "\neParam   = ")(princ eParam)(princ)
        (list
          eParam
          (vlax-curve-getPointAtParam obj stParam)
          (vlax-curve-getPointAtParam obj eParam)
        )
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
          (setq ent (nentsel "\nSelect entity for alignment: "))
          ;(setq ent (entsel "\nSelect entity for alignment: "))
     )
      (setq obj (vlax-ename->vla-object (car ent))
            obj_typ (vlax-get-property obj 'ObjectName)
      )
      
      (cond 
        ( (= obj_typ "AcDbPolyline")
          (if (setq pt_lst (getSegment obj (cadr ent)))
            (progn
              (princ "\npt_lst = ")(princ pt_lst)(princ)
              (setq ang (angle (cadr pt_lst) (caddr pt_lst)))
              
            )
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
      ;(setq ans (getstring "\nRotate 180 [Y/N]<N>: "))
      ;(if (= (strcase ans) "Y")
      ;  (vlax-put-property txt_obj 'Rotation (get-opp-ang ang))
      ;  )
      (vlax-release-object txt_obj)
      (undoend)
     )
   )
  (princ)
  
  ; NOTE: Wayne - RRT.lsp modificado para poder trabajar con bloques también
  ; Author: David Torralba
  ; Last revision: 2016.02.16
)