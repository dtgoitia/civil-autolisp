;;;======================== VARIANTS & SAFEARRAYS ========================;;;

;; Variant -> LISP

;; gc:VariantToLispData
;; Converts a variant or a safearray into LISP data (list)
;;
;; Argument: var variant or safearray

(defun gc:VariantToLispData (var)
  (cond
    ((= (type var) 'variant)
     (gc:VariantToLispData (vlax-variant-value var)))
    ((= (type var) 'safearray)
     (mapcar 'gc:VariantToLispData (vlax-safearray->list var))
    )
    (T var)
  )
)

;; gc:2dVariantToPointList
;; Converts a variant of 2D coordinates into a 2d points list
;; LightweightPolyline: OCS coordinates
;;
;; Argument
;; var: a variant (array of doubles) as returned by vla-get-Coordinates

(defun gc:2dVariantToPointList (var / foo)
  (defun foo (lst)
    (if lst
      (cons (list (car lst) (cadr lst)) (foo (cddr lst)))
    )
  )
  (foo (vlax-safearray->list (vlax-variant-value var)))
)

;; gc:3dVariantToPointList
;; Converts a variant of 3D coordinates into a 3d points list
;; 2d Polyline: OCS coordinates (Z = 0)
;; 3DFace, 3DPolyline, Leader, MLine, PolyfaceMesh,
;; PolygonMesh, Solid, Trace: WCS coordinates
;;
;; Argument
;; var: a variant (array of doubles) as returned by vla-get-Coordinates

(defun gc:3dVariantToPointList (var / foo)
  (defun foo (lst)
    (if lst
      (cons (list (car lst) (cadr lst) (caddr lst)) (foo (cdddr lst)))
    )
  )
  (foo (vlax-safearray->list (vlax-variant-value var)))
)

;; gc:VariantsToDxfList
;; Returns an assoc list (DXF list type)
;;
;; Arguments
;; xtyp: variant (array of integers)
;; xval: varinat (array of variants)

(defun gc:VariantsToDxfList (xtyp xval)
  (mapcar 'cons (gc:VariantToLispData xtyp) (gc:VariantToLispData xval))
)

;; gc:GetXdata
;; Returns the object xadta list
;;
;; Arguments
;; obj: (vla-object) the object containing xdata
;; app: (string) the registred application name ("" for all)

(defun gc:GetXdata (obj app / xtyp xval)
  (vla-GetXdata obj app 'xtyp 'xval)
  (gc:VariantsToDxfList xtyp xval)
)

;; gc:GetXrecordData
;; Returns the xrecord object DXF data list
;;
;; Arguments
;; xrec: (vla-object) thet XRECORD object

(defun gc:GetXrecordData (xrec / xtyp xval)
  (vla-GetXrecordData xrec 'xtyp 'xval)
  (gc:VariantsToDxfList xtyp xval)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; LISP -> variant

;; gc:2dPointListToVariant (gile)
;; Return a variant of 2d coordinates
;;
;; Argument: a 2d points list -type (x y)-

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

;; gc:3dPointListToVariant (gile)
;; Return a variant of 3d coordinates
;;
;; Argument: a 3d points list -type (x y z)-

(defun gc:3dPointListToVariant (lst)
  (vlax-make-variant
    (vlax-safearray-fill
      (vlax-make-safearray
        vlax-VbDouble
        (cons 0 (1- (* 3 (length lst))))
      )
      (apply 'append lst)
    )
  )
)

;; gc:ObjectListToVariant
;; returns a variant (array of objects)
;;
;; Argument
;; lst: a vla-object list

(defun gc:ObjectListToVariant (lst)
  (vlax-make-variant
    (vlax-safearray-fill
      (vlax-make-safearray
        vlax-vbObject
        (cons 0 (1- (length lst)))
      )
      lst
    )
  )
)

;; gc:DxfListToVariants
;; Defines 2 variables and bounds a variant to each
;;
;; Arguments
;; lst: a DXF list
;; typeSymbol: a quoted symbol (other than 'typeSymbol)
;; valueSymbol: a quoted symbol (other than 'valueSymbol)

(defun gc:DxfListToVariants (lst typeSymbol valueSymbol)
  (set typeSymbol
       (vlax-make-variant
         (vlax-safearray-fill
           (vlax-make-safearray
             vlax-vbInteger
             (cons 0 (1- (length lst)))
           )
           (mapcar 'car lst)
         )
       )
  )
  (set valueSymbol
       (vlax-make-variant
         (vlax-safearray-fill
           (vlax-make-safearray
             vlax-vbVariant
             (cons 0 (1- (length lst)))
           )
           (mapcar '(lambda (x)
                      (if (listp (setq x (cdr x)))
                        (vlax-3d-point x)
                        (vlax-make-variant x)
                      )
                    )
                   lst
           )
         )
       )
  )
)


;; gc:SetXdata
;; Set xdatas to an object
;;
;; Arguments
;; obj: (vla-object) the object to set xdatas
;; lst: (liste DXF) the xdatas as:
;; '((1001 . "App_Name") (1002 . "{") (1000 . "string") (1070 . 1) (1002 . "}"))

(defun gc:SetXdata (obj lst / xtyp xval)
  (gc:DxfListToVariants lst 'xtyp 'xval)
  (vla-SetXdata obj xtyp xval)
)

;; gc:SetXrecordData
;; Set datas to an xrecord
;;
;; Arguments
;; xrec: (vla-object) the Xrecord object
;; lst : (liste DXF) the datas as:
;; '((1 . "string") (70 . 1) (10 1.0 2.0 0.0))

(defun gc:SetXrecordData (xrec lst / xtyp xval)
  (gc:DxfListToVariants lst 'xtyp 'xval)
  (vla-SetXrecordData xrec xtyp xval)
)
