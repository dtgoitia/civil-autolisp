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
  (defun DT:TRANSFORM_WITH_NENTSELP_MATRIX (
                                  mtx p
                                  /
                                  m00 m01 m02 m03
                                  m10 m11 m12 m13
                                  m20 m21 m22 m23
                                  m30 m31 m32 m33
                                  ptx pty ptz
                                  )
    ; transform with matrix

    (cond
      ((/= mtx nil)
        ;(princ "\nThis is a nested object.")
        (setq
          m00 (car (car    mtx)) m01 (cadr (car    mtx)) m02 (caddr (car    mtx)) m03 (cadddr (car    mtx))
          m10 (car (cadr   mtx)) m11 (cadr (cadr   mtx)) m12 (caddr (cadr   mtx)) m13 (cadddr (cadr   mtx))
          m20 (car (caddr  mtx)) m21 (cadr (caddr  mtx)) m22 (caddr (caddr  mtx)) m23 (cadddr (caddr  mtx))
          m30 (car (cadddr mtx)) m31 (cadr (cadddr mtx)) m32 (caddr (cadddr mtx)) m33 (cadddr (cadddr mtx))

          ptx (+ (* (car p) m00) (* (cadr p) m01) (* (caddr p) m02) m03 )
          pty (+ (* (car p) m10) (* (cadr p) m11) (* (caddr p) m12) m13 )
          ptz (+ (* (car p) m20) (* (cadr p) m21) (* (caddr p) m22) m23 )
        )
        (list ptx pty ptz) ; return new point
      ); END cond nested object (mtx /= nil)
      ((= mtx nil)
        (princ "\nThis is a no-nested object.")
        (list (car p) (cadr p) (caddr p))
      ); END cond no nested object (mtx = nil)
    ); END cond
  )
  ;(defun getSegment (obj pt / cpt eParam stParam)
  ;  (cond
  ;    ( (setq cpt (vlax-curve-getClosestPointTo obj pt))
  ;      (setq eParam (fix (vlax-curve-getEndParam obj)))
  ;      (if (= eParam (setq stParam (fix (vlax-curve-getParamAtPoint obj cpt))))
  ;        (setq stParam (1- stParam))
  ;        (setq eParam (1+ stParam))
  ;      ); END if
  ;      (list
  ;        eParam
  ;        (vlax-curve-getPointAtParam obj stParam)
  ;        (vlax-curve-getPointAtParam obj eParam)
  ;      ); END list
  ;    ); END cond1
  ;  ); END cond
  ;); END defun
  (defun DT:getSegment (obj pt / cpt eParam stParam)
    (if (/= matrix nil)
      (setq pt (DT:TRANSFORM_WITH_NENTSELP_MATRIX (invm matrix) pt))
    ); END if

    (if (setq cpt (vlax-curve-getClosestPointTo obj pt))
      (progn
        (setq eParam (fix (vlax-curve-getEndParam obj)))
        (if (= eParam (setq stParam (fix (vlax-curve-getParamAtPoint obj cpt))))
          (setq stParam (1- stParam))
          (setq eParam (1+ stParam))
        ); END if
        (setq
          p1 (vlax-curve-getPointAtParam obj stParam)         ; xref coords, not current coords
          p2 (vlax-curve-getPointAtParam obj eParam)          ; xref coords, not current coords
          ;p1 (DT:TRANSFORM_WITH_NENTSELP_MATRIX matrix p1)    ; current coords
          ;p2 (DT:TRANSFORM_WITH_NENTSELP_MATRIX matrix p2)    ; current coords
        ); END seqt
        (princ "\np1 = ")(princ p1)
        (princ "\np2 = ")(princ p2)
        (list
          eParam
          (DT:TRANSFORM_WITH_NENTSELP_MATRIX matrix p1)
          (DT:TRANSFORM_WITH_NENTSELP_MATRIX matrix p2)
        )
      ); END progn
    ); END if
    ;(command "circle" p1 "0.5")
    ;(command "circle" p2 "0.5")
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
         (alert "I only know how to align (M)TEXT or blocks, sorry! "))
      )
    )
  )

  ;;  -----------   Get the Object to Align To  -----------------
  (cond
    ((and txt_ent
          (setq
            ent (nentselp "\nSelect entity for alignment: ")
            matrix (caddr ent)
          )
          ;(setq ent (entsel "\nSelect entity for alignment: "))
     )
      (setq obj (vlax-ename->vla-object (car ent))
            obj_typ (vlax-get-property obj 'ObjectName)
      )

      (cond
        ( (= obj_typ "AcDbPolyline")
          (if (setq pt_lst (DT:getSegment obj (cadr ent)))
            (setq ang (angle (cadr pt_lst) (caddr pt_lst)))
          )
        )
        ( (= obj_typ "AcDbLine")
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

  ; v0.1 - Nested polylines added as objects to align to.
  ;      - Function created to apply matrix transformation.
  ; v0.0 - Original routine backed up and renamed as "Wayne - RTT.lsp"
  ;      - Modificated to be able to rotate blocks too.
  ; Author: David Torralba
  ; Last revision: 2016.02.16
)
