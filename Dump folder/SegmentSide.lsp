(defun DT:SegmentSide ( pRef1 pRef2 p / angP angReference angRelative )
  ; Returns 0 or 1 depending on the side of p
  ;   - 0   if "p" is on the RIGHT of the segment created by pRef1 and pRef2
  ;   - 1   if "p" is on the LEFT  of the segment created by pRef1 and pRef2
  ;   - nil if "p" is IN LINE with    the segment created by pRef1 and pRef2
  ; pRef1 [pt] - Point 1 of the segment of reference
  ; pRef2 [pt] - Point 2 of the segment of reference
  ; p [pt]     - Point to determine side

  ;|
  NOTE: "pRef1" and pRef2 will be provided by the parent function containing (DT:SegmentSide).
  in order to account for curved segments take "pRef1" as closest point to "p", and "pRef2"
  is the point with the curve parameter inmediatly after "pRef2". This means that:
                       param "pRef2" = param "pRef1" + 0.001
  |;
  (if (and pRef1 pRef2 p)
    (if
      (and
        (= 'list (type pRef1))
        (= 'list (type pRef2))
        (= 'list (type p    ))
      );END and
      (progn
        ; Get angP =  angle between "p" and closest point
        (setq angP (angle pRef1 p) )
        ; Get angReference =  angle of reference
        (setq angReference (angle pRef1 pRef2) )
        ; Get angRelative = relative angle between pRef1-P and pRef1-pRef2
        (setq angRelative (- angP angReference) )
        (if
          (or
            (= angRelative -pi)
            (= angRelative 0)
            (= angRelative pi)
          );END or
          ; "p" is aligned with reference
          nil
        );END if
        (cond
          ;
          ( ; Reference segment is horizontal, pointing to the right
            ; angReference = 0
            (= angReference 0)
            (princ (strcat "\nReference is horizontal and pointing to the right"))
            (princ)
          );END subcond
          ( ; Reference segment is horizontal, pointing to the left
            ; angReference = π
            (= angReference pi)
            (princ (strcat "\nReference is horizontal and pointing to the left"))
            (princ)
          );END subcond
          ( ; Reference segment is not horizontal
            ; angReference = 0
            (and
              (/= angReference  0)
              (/= angReference pi)
            );END and
            (princ (strcat "\nReference is not horizontal"))
            (princ)
          );END subcond
        );END cond
        ;|
        (cond
          ((> angP angReference)
            (princ (strcat "\nCASE 1: angP > angReference" "\nangP = " (LM:rtos angP 2 2) "\nangReference = " (LM:rtos angReference 2 2) ))
            (princ)
          );END subcond
          ((< angP angReference)
            (princ (strcat "\nCASE 2: angP < angReference" "\nangP = " (LM:rtos angP 2 2) "\nangReference = " (LM:rtos angReference 2 2) ))
            (princ)
          );END subcond
          (t
            (princ "\np is aligned with selected segment")
            (princ)
          );END subcond
          |;
        );END cond
      );END progn
      (cond
        ((/= 'list (type pRef1)) (princ "\nERROR @ DT:SegmentSide : pRef1 is not a point\n") (princ) )
        ((/= 'list (type pRef2)) (princ "\nERROR @ DT:SegmentSide : pRef2 is not a point\n") (princ) )
        ((/= 'list (type p    )) (princ "\nERROR @ DT:SegmentSide : p is not a point\n")     (princ) )
      );END cond
    );END if
    (cond
      ((not pRef1) (princ "\nERROR @ DT:SegmentSide : pRef1=nil\n")(princ) )
      ((not pRef2) (princ "\nERROR @ DT:SegmentSide : pRef2=nil\n")(princ) )
      ((not p)     (princ "\nERROR @ DT:SegmentSide : p=nil\n")    (princ) )
    );END cond
  );END if
)
(defun c:SegmentSide ( / a )
  (setq a (car (entsel)))
  (DT:SegmentSide
    (vlax-curve-getPointAtParam (vlax-ename->vla-object a) 0)
    (vlax-curve-getPointAtParam (vlax-ename->vla-object a) 1)
    (getpoint "\nSelect point to tell side: ")
  )
)
