(defun LM:RGB->OLE ( r g b )
  ;; RGB -> OLE  -  Lee Mac
  ;; Args: r,g,b - [int] Red, Green, Blue values
    (logior (fix r) (lsh (fix g) 8) (lsh (fix b) 16))
)
(defun LM:OLE->RGB ( c )
  ;; OLE -> RGB  -  Lee Mac
  ;; Args: c - [int] OLE Colour
    (mapcar '(lambda ( x ) (lsh (lsh (fix c) x) -24)) '(24 16 8))
)
(defun LM:RGB->True ( r g b )
  ;; RGB -> True  -  Lee Mac
  ;; Args: r,g,b - [int] Red, Green, Blue values
    (logior (lsh (fix r) 16) (lsh (fix g) 8) (fix b))
)
(defun LM:True->RGB ( c )
  ;; True -> RGB  -  Lee Mac
  ;; Args: c - [int] True Colour
    (mapcar '(lambda ( x ) (lsh (lsh (fix c) x) -24)) '(8 16 24))
)
(defun LM:RGB->ACI ( r g b / c o )
  ;; RGB -> ACI  -  Lee Mac
  ;; Args: r,g,b - [int] Red, Green, Blue values
    (if (setq o (vla-getinterfaceobject (LM:acapp) (strcat "autocad.accmcolor." (substr (getvar 'acadver) 1 2))))
        (progn
            (setq c (vl-catch-all-apply '(lambda ( ) (vla-setrgb o r g b) (vla-get-colorindex o))))
            (vlax-release-object o)
            (if (vl-catch-all-error-p c)
                (prompt (strcat "\nError: " (vl-catch-all-error-message c)))
                c
            )
        )
    )
)
(defun LM:ACI->RGB ( c / o r )
  ;; ACI -> RGB  -  Lee Mac
  ;; Args: c - [int] ACI (AutoCAD Colour Index) Colour (1<=c<=255)
    (if (setq o (vla-getinterfaceobject (LM:acapp) (strcat "autocad.accmcolor." (substr (getvar 'acadver) 1 2))))
        (progn
            (setq r
                (vl-catch-all-apply
                   '(lambda ( )
                        (vla-put-colorindex o c)
                        (list (vla-get-red o) (vla-get-green o) (vla-get-blue o))
                    )
                )
            )
            (vlax-release-object o)
            (if (vl-catch-all-error-p r)
                (prompt (strcat "\nError: " (vl-catch-all-error-message r)))
                r
            )
        )
    )
)
(defun LM:acapp nil
  ;; Application Object  -  Lee Mac
  ;; Returns the VLA Application Object
    (eval (list 'defun 'LM:acapp 'nil (vlax-get-acad-object)))
    (LM:acapp)
)
(defun LM:RGB->HSL ( r g b / d h l m n s )
  ;; RGB -> HSL  -  Lee Mac
  ;; Args: r,g,b - [int] Red, Green, Blue values
    (setq r (/ r 255.0)
          g (/ g 255.0)
          b (/ b 255.0)
          n (min r g b)
          m (max r g b)
    )
    (if (zerop (setq d (- m n)))
        (list 0 0 (fix (+ 0.5 (* m 100))))
        (progn
            (if (< (setq l (/ (+ m n) 2.0)) 0.5)
                (setq s (/ d (+ m n)))
                (setq s (/ d (- 2.0 m n)))
            )
            (cond
                (   (= g m) (setq h (+ (/ (- b r) d) 2)))
                (   (= b m) (setq h (+ (/ (- r g) d) 4)))
                (   (setq h (/ (- g b) d)))
            )
            (list
                (fix (+ 0.5 (rem (+ 360 (* h 60)) 360)))
                (fix (+ 0.5 (* s 100)))
                (fix (+ 0.5 (* l 100)))
            )
        )
    )
)
(defun LM:HSL->RGB ( h s l / u v )
  ;; HSL -> RGB  -  Lee Mac
  ;; Args: [int] 0<=h<=360, 0<=s<=100, 0<=l<=100
    (setq h (/ h 360.0)
          s (/ s 100.0)
          l (/ l 100.0)
    )
    (mapcar '(lambda ( x ) (fix (+ 0.5 (* 255 x))))
        (cond
            (   (zerop s) (list l l l))
            (   (zerop l)'(0 0 0))
            (   (setq v (if (< l 0.5) (* l (1+ s)) (- (+ l s) (* l s)))
                      u (-  (* 2.0 l) v)
                )
                (mapcar
                   '(lambda ( h )
                        (setq h (rem (1+ h) 1))
                        (cond
                            (   (< (* 6.0 h) 1.0) (+ u (* 6.0 h (- v u))))
                            (   (< (* 2.0 h) 1.0) v)
                            (   (< (* 3.0 h) 2.0) (+ u (* 6.0 (- (/ 2.0 3.0) h) (- v u))))
                            (   u   )
                        )
                    )
                    (list (+ h (/ 1.0 3.0)) h (- h (/ 1.0 3.0)))
                )
            )
        )
    )
)
(defun LM:OLE->True ( c )
  ;; OLE -> True  -  Lee Mac
  ;; Args: c - [int] OLE Colour
    (apply 'logior
        (mapcar
           '(lambda ( x ) (lsh (lsh (lsh (fix c) x) -24) (- x 8)))
           '(24 16 08)
        )
    )
)
(defun LM:True->OLE ( c )
  ;; True -> OLE  -  Lee Mac
  ;; Args: c - [int] True Colour
    (apply 'logior
        (mapcar
           '(lambda ( x ) (lsh (lsh (lsh (fix c) x) -24) (- x 8)))
           '(08 16 24)
        )
    )
)
(defun LM:OLE->ACI ( c )
  ;; OLE -> ACI  -  Lee Mac
  ;; Args: c - [int] OLE Colour
    (apply 'LM:RGB->ACI (LM:OLE->RGB c))
)
(defun LM:ACI->OLE ( c )
  ;; ACI -> OLE  -  Lee Mac
  ;; Args: c - [int] ACI (AutoCAD Colour Index) Colour (1<=c<=255)
    (apply 'LM:RGB->OLE (LM:ACI->RGB c))
)
(defun LM:OLE->HSL ( c )
  ;; OLE -> HSL  -  Lee Mac
  ;; Args: c - [int] OLE Colour
    (apply 'LM:RGB->HSL (LM:OLE->RGB c))
)
(defun LM:HSL->OLE ( h s l )
  ;; HSL -> OLE  -  Lee Mac
  ;; Args: [int] 0<=h<=360, 0<=s<=100, 0<=l<=100
    (apply 'LM:RGB->OLE (LM:HSL->RGB h s l))
)
(defun LM:True->ACI ( c / o r )
  ;; True -> ACI  -  Lee Mac
  ;; Args: c - [int] True Colour
    (apply 'LM:RGB->ACI (LM:True->RGB c))
)
(defun LM:ACI->True ( c / o r )
  ;; ACI -> True  -  Lee Mac
  ;; Args: c - [int] ACI (AutoCAD Colour Index) Colour (1<=c<=255)
    (apply 'LM:RGB->True (LM:ACI->RGB c))
)
(defun LM:True->HSL ( c )
  ;; True -> HSL  -  Lee Mac
  ;; Args: c - [int] True Colour
    (apply 'LM:RGB->HSL (LM:True->RGB c))
)
(defun LM:HSL->True ( h s l )
  ;; HSL -> True  -  Lee Mac
  ;; Args: [int] 0<=h<=360, 0<=s<=100, 0<=l<=100
    (apply 'LM:RGB->True (LM:HSL->RGB h s l))
)
(defun LM:ACI->HSL ( c )
  ;; ACI -> HSL  -  Lee Mac
  ;; Args: c - [int] ACI (AutoCAD Colour Index) Colour (1<=c<=255)
    (apply 'LM:RGB->HSL (LM:ACI->RGB c))
)
(defun LM:HSL->ACI ( h s l )
  ;; HSL -> ACI  -  Lee Mac
  ;; Args: [int] 0<=h<=360, 0<=s<=100, 0<=l<=100
    (apply 'LM:RGB->ACI (LM:HSL->RGB h s l))
)
