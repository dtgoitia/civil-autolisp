; Offset a 3dPoly with horizontal and vertical distances
; 15 August 2014 - Gian Paolo Cattaneo

(defun c:o3 ( / *error* 3dp p ogt)
  (defun *error* ( msg )
    (setvar 'offsetgaptype ogt)
    (vla-endundomark (vla-get-activedocument (vlax-get-acad-object)))
    (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )
  (vla-startundomark (vla-get-activedocument (vlax-get-acad-object)))
  (setq ogt (getvar 'offsetgaptype))
  (setvar 'offsetgaptype 0)
  (or op (setq op 0.00))
  (or oz (setq oz 0.00))
  (while
      (progn
          (setq 3dp (car (entsel "\nSelect the 3D polyline to offset")))
          (if (not (eq "AcDb3dPolyline" (vlax-get (vlax-ename->vla-object 3dp) 'ObjectName)))
              (progn
                  (alert "This is not a 3D Polyline")
                  t
              )
          )
      )
  )
  (if 3dp
      (progn
          (while
              (progn
                  (initget (+ 2 4))
                  (setq op
                      (cond
                          ( (getdist (strcat "\nOffset Distance (horizontal)  <" (rtos op 2 2)">: ")) )
                          ( op )
                      )
                  )
                  (if (= op 0.)
                      (progn
                          (alert "The horizontal offset distance must be greater than zero")
                          t
                      )
                  )
              )
          )
          (setq oz
              (cond
                  ( (getdist (strcat "\nOffset Distance (vertical)    <" (rtos oz 2 2)">: ")) )
                  ( oz )
              )
          )
          (setq p (getpoint "\nSpecify point on side to offset "))
          (off3DP 3dp op oz p)
      )
  )
  (setvar 'offsetgaptype ogt)
  (vla-endundomark (vla-get-activedocument (vlax-get-acad-object)))
  (princ)
)

;******************* Offset 3D Polyline ********************;
;                                                           ;
;  obj   [ENAME] - entity name                              ;
;  d_o   [REAL]  - horizontal offset distance               ;
;  d_z   [REAL]  - vertical offset distance                 ;
;  p_of  [LIST]  - point on side to offset                  ;
;                                                           ;
; ----------------------------------------------------------;
; (off3DP (car (entsel)) 2.15 -2.00 '(3 4 0))               ;
; ----------------------------------------------------------;
; author: Gian Paolo Cattaneo - 30.03.2013                  ;
;***********************************************************;
(defun off3DP ( obj d_o d_z p_of / EL Lv PL2 PL3 Lv Lv1 Lv2 n_of obj_of)
    (if
        (and
            (eq "AcDb3dPolyline" (vlax-get (vlax-ename->vla-object obj) 'ObjectName))
            (setq Lv (pl_coord obj))
            (setq PL2
                (entmakex
                    (append
                        (list
                            (cons 0 "LWPOLYLINE")
                            (cons 100 "AcDbEntity")
                            (cons 100 "AcDbPolyline")
                            (cons 90 (length Lv))
                            (cons 70 (if (vlax-curve-IsClosed obj) 1 0))
                            (cons 43 0.0)
                        )
                        (mapcar '(lambda (x) (cons 10 x)) Lv)
                    )
                )
            )
        )
        (progn
            (setq EL (entlast))
            (vl-cmdf "_offset" d_o PL2 p_of "")
            (if (and
                    (/= EL (setq PL3 (entlast)))
                    (= (setq n_of (length (setq obj_of (e_next EL "LS")))) 1)
                )
                (progn
                    (setq Lv2 nil)
                    (setq Lv1 (pl_coord PL3))
                    (entdel PL2)
                    (entdel PL3)
                    (mapcar
                        '(lambda ( a b )
                             (setq Lv2 (cons (list (car b) (cadr b) (+ d_z (caddr a))) Lv2))
                         )
                         Lv Lv1
                    )
                    (setq Lv2 (reverse Lv2))
                    (entmake
                        (list
                            '(0 . "POLYLINE")
                            '(10 0.0 0.0 0.0)
                            (assoc 8 (entget obj))
                            (assoc 70 (entget obj))
                        )
                    )
                    (repeat (length Lv2)
                        (entmake
                            (list
                                (cons 0 "VERTEX")
                                (cons 10 (CAR Lv2))
                                (cons 70 32)
                            )
                        )
                        (setq Lv2 (cdr Lv2))
                    )
                    (entmake '((0 . "SEQEND")))
                    t
                )
                (progn
                    (entdel PL2)
                    (if (> n_of 1)
                        (repeat n_of
                            (entdel (car obj_of))
                            (setq obj_of (cdr obj_of))
                        )
                    )
                    nil
                )
            )
        )
    )
)
;***********************************************************;

(defun pl_coord (# / p m)
    (setq p (if (vlax-curve-IsClosed #)
                (fix (vlax-curve-getEndParam #))
                (1+ (fix (vlax-curve-getEndParam #)))
            )
    )
    (while (/= 0 p)
        (setq m (cons (vlax-curve-getPointAtParam # (setq p (1- p))) m))
    )
)
;***********************************************************;

(defun e_next (entL mode / next)
    (if (= mode "SS") (setq next (ssadd)))
    (if (/= entL (entlast))
        (while (setq entL (entnext entL))
            (if (entget entL)
                (cond
                    ( (= mode "LS") (setq next (cons entL next)) )
                    ( (= mode "SS") (setq next (ssadd entL next)) )
                )
            )
        )
    )
    next
)

(vl-load-com)
;***********************************************************;
(princ)
