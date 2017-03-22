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
            (princ (strcat "\nCASE 1: ang1 > ang2" "\nang1 = " (LM:rtos ang1 2 2) "\nang2 = " (LM:rtos ang2 2 2) ))
            (princ)
          );END subcond
          ((< ang1 ang2)
            (princ (strcat "\nCASE 2: ang1 < ang2" "\nang1 = " (LM:rtos ang1 2 2) "\nang2 = " (LM:rtos ang2 2 2) ))
            (princ)
          );END subcond
          (t
            (princ "\np is aligned with selected segment")
          );END subcond
        );END cond
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
; Working on DT:SegmentSide
