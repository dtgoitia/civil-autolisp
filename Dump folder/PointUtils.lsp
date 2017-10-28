(defun DT:MidPoint ( pA pB )
  ; Return the average point between
  (mapcar
    '(lambda (a b) (* 0.5 (+ a b)))
    pA pB
  );END mapcar

  ; v0.0 - 2017.07.27 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.07.27
)
