(defun DT:InnerPoint ( ent_name / pointList absoluteAngleList relativeAngleList totalAngle point0 angle0 innerAngle innerPoint )
  ; Return inner point of passed polyline
  (if (DT:Arg 'DT:InnerPoint '((ent_name 'ename)))
    (if (= "LWPOLYLINE" (cdr (assoc 0 (entget ent_name))))
      (progn
        ; Get absolute angle list
        (setq pointList (DT:GetLwpolyPoints ent_name))
        (setq absoluteAngleList (DT:GetAbsoluteAngleList pointList))
        (setq relativeAngleList (DT:GetRelativeAngleList absoluteAngleList))

        (setq totalAngle 0)
        (foreach ang relativeAngleList (setq totalAngle (+ totalAngle ang)))

        (setq point0 (nth 0 pointList))
        (setq angle0 (nth 0 absoluteAngleList))
        (if (< totalAngle 0)
          (setq innerAngle (+ angle0 0.1) )
          (setq innerAngle (- angle0 0.1) )
        );END if
        (setq innerPoint (polar point0 innerAngle (* 0.5 (distance point0 (nth 1 pointList))) ))
      );END progn
    );END if
  );END if

  ; v0.0 - 2017.05.26 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.05.26
)
(defun DT:GetAbsoluteAngleList ( pointList / i p0 p1 absoluteAngleList )
  ; Return absolute angles formed by point in pointList list
  (if (DT:Arg 'DT:GetAbsoluteAngleList '((pointList 'list)))
    (progn
      (setq i 0)
      (foreach point pointList
        (setq i (+ i 1))
        (if (> i 1)
          (progn ; normal iteration
            (setq
              p0 p1     ; set old point as new point
              p1 point  ; get new point
              absoluteAngleList (append absoluteAngleList (list (angle p0 p1)))
            );END setq
          );END progn
          (setq p1 point) ; get new point
        );END if
      );END foreach
      (setq
        p0 p1     ; set old point as new point
        p1 (nth 0 pointList)  ; get new point
        absoluteAngleList (append absoluteAngleList (list (angle p0 p1)))
      );END setq
    );END progn
  );END if

  ; v0.0 - 2017.05.26 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.05.26
)
(defun DT:GetRelativeAngleList ( absoluteAngleList / i ang0 ang1 angleDifference spin relativeAngle relativeAngleList )
  (if (DT:Arg 'DT:GetRelativeAngleList '((absoluteAngleList 'list)))
    (progn
      (setq i 0)
      (foreach ang absoluteAngleList
        (setq i (+ i 1))
        (if (> i 1)
          (progn ; normal iteration
            (setq
              ang0 ang1  ; set old angle as new angle
              ang1 ang   ; get new angle
              relativeAngle (DT:GetRelativeAngle ang0 ang1)
              relativeAngleList (append relativeAngleList (list relativeAngle))
            );END setq
          );END progn
          (setq ang1 ang ) ; get new angle
        );END if
      );END foreach
      (setq
        ang0 ang1                       ; set old angle as new angle
        ang1 (nth 0 absoluteAngleList)  ; get new angle
        relativeAngle (DT:GetRelativeAngle ang0 ang1)
        relativeAngleList (append relativeAngleList (list relativeAngle))
      );END setq
      relativeAngleList
    );END progn
  );END if

  ; v0.0 - 2017.05.26 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.05.26
)
(defun DT:GetRelativeAngle ( ang0 ang1 / angleDifference spin return )
  ; Return relative angle
  ; > 0 if clockwise
  ; < 0 if anticlockwise
  (if (DT:Arg 'DT:GetRelativeAngle '((ang0 'real)(ang1 'real)))
    (progn
      (setq
        angleDifference (- ang1 ang0)
        spin (DT:Spin ang0 ang1)
      )
  ;  - 0 : clockwise
  ;  - 1 : anticlockwise
  ;  - 2 : inline
      (cond
        ((= 2 spin)                             (setq return 0)                           )
        ((< angleDifference 0)                  (setq return (- (+ ang1 (* 2 pi)) ang0))  )
        ((and (= spin 1) (< angleDifference 0)) (setq return (- (+ ang1 (* 2 pi)) ang0))  )
        ((and (= spin 0) (< angleDifference 0)) (setq return (- (+ ang1 (* 2 pi)) ang0))  )
        (t                                      (setq return angleDifference)             )
      );END cond
    );END progn
  );END if

  ; Reverse angle value from pi to 2pi
  (if (and (= spin 0) (> return pi) ) (setq return (- (* 2 pi) return)) )

  ; Reverse the sign for anticlockwise relative angle
  (if (= spin 1) (setq return (- 0 return)) )

  ; Return value
  return

  ; v0.0 - 2017.05.26 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.05.26
)
