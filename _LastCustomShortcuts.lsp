; DO NOT REMOVE THIS LINE. It's a checking.
(defun c:1() (c:BYC))
(defun c:2( / p1 p1a p1b)
  ; Insert rear door block between two points, and rotate 90 degree.
  (setvar "osmode" 513)
  (setq
   p1a (getpoint "\nPoint 1a:")
   p1b (getpoint "\nPoint 1b:")
   p1 (polar p1a (angle p1a p1b) (* 0.5 (distance p1a p1b)) )
  )
  (setvar "osmode" 0)
  ;(command "-insert" "Part-m-secondary" p1 1 1 (* (/ 180 pi) (angle p1a p1b)) )
  (command "-insert" "Part-m-secondary" p1 1 1 (* (/ 180 pi) (+ (angle p1a p1b) (* -0.5 pi) ) -1 ) )
  (command "rotate" (entlast) "" p1 "-90")
  (setvar "osmode" 513)
)
