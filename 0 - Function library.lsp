(vl-load-com)
(defun _Set_Line ( pa pb )
  ; pa - Line initial point
  ; pb - Line final point
  (entmakex
    (list
      (cons 0 "LINE")                                         ; Element type: line
      (cons 10 pa)                                            ; Object primary point: line initial point
      (cons 11 pb)                                            ; Line final point
      (cons 62 8)                                             ; Line color
      (cons 6 "DASHED")                                       ; Line type: dashed
      (cons 48 0.06)                                          ; Line type scale
    )
  )
  ; v0.1 - 2016.03.21 - Translate into English
  ; v0.0 - 2016.03.18 - First issue
  ; Author: David Torralba
  ; Last revision: 2016.03.25
)
(defun _Reference_Circle ( pc rd )
  ; pc - Circle center point
  ; rd - Circle radius
  (entmakex
    (list
      (cons 0 "CIRCLE")                                       ; Element type: circle
      (cons 10 pc)                                            ; Object primary point: circle center point
      (cons 40 rd)                                            ; Circle radius
      (cons 62 8)                                             ; Line color
      (cons 6 "CONTINUOUS")                                   ; Line type: dashed
    )
  )
  ; v0.1 - 2016.03.21 - Translate into English
  ; v0.0 - 2016.03.18 - First issue
  ; Author: David Torralba
  ; Last revision: 2016.03.25
)
(defun _CursorText_L1 ( p txt1 textsize )
  (entmakex
    (list (cons 0 "TEXT")
      (cons 40 (setq dis textsize))                           ; Text height
      (cons 10 (polar (polar p 0 dis) (* -0.5 pi) (* 2 dis))) ; Text insertion point
      (cons 50 0)                                             ; Text rotation
      (cons 62 251)                                           ; Text color
      (cons 1 txt1)                                           ; Text content
      (cons 7 "ARIAL")                                        ; Text style
    )
  )
)
(defun _CursorText_L2 ( p txt2 textsize )
  (entmakex
    (list (cons 0 "TEXT")
      (cons 40 (setq dis textsize))                           ; Text height
      (cons 10 (polar (polar p 0 dis) (* -0.5 pi) (* 4 dis))) ; Text insertion point
      (cons 50 0)                                             ; Text rotation
      (cons 62 251)                                           ; Text color
      (cons 1 txt2)                                           ; Text content
      (cons 7 "ARIAL")                                        ; Text style
    )
  )
)
(defun DT:input_string_or_point ( / in number ch pt)
  (princ "\nSelect a level or type it: ")
  (setq
    in (grread)
    number ""
  )
  (cond
    ((= 3 (car in)) ; Point input
      (cadr in)
    )
    ((= 2 (car in)) ; String input
      (while (and (= 2 (car in))  (and (/= 13 (cadr in)) (/= 32 (cadr in))))
        (if (/= 8 (cadr in))
          (progn ; if the key is not "delete"
            (setq
              ch (chr (cadr in))        ; convert input key (chr) in to a character (ch)
              number (strcat number ch) ; join it with the previous string and store it
            )
            (princ ch)
          )
          (progn ; if the key is "delete"
            (setq
              ch (chr (cadr in))                              ; convert input key (chr) in to a character (ch)
              number (substr number 1 (- (strlen number) 1 )) ; remove last character from the string
            )
            (princ ch)
          )
        )
        (setq in (grread))
      )
      (setq number number)
    )
    ((= 25 (car in))
      (princ "noting selected.")
      (c:INPUT)
    )
    (t (alert "IMPORTANT!\nNew case detected. Please, report to David inmediatly what did you do before this pop up appeared to improve the routine.")(princ))
  )
  ; v0.1 - 2016.03.31 - Add possibily to remove typed characters.
  ; v0.0 - 2016.03.30 - First issue
  ; Author: David Torralba
  ; Last revision: 2016.03.30
)
(defun DT:destripar_txt ( / nent VL_ent_name )
  ; Return the string of any atribute or text, regardless of how deep nested is it
  (setq nent nil)
  (while (not nent)
    (setq nent (nentsel))
    (if (not nent)
      (princ "nothing selected.\n")
      (progn
        (setq VL_ent_name (vlax-ename->vla-object (car nent)))
        (if (vlax-property-available-p VL_ent_name 'TextString)
          (vlax-get-property VL_ent_name 'TextString)
          (progn
            (setq nent nil)
            (princ "no text found.")
          ); END progn2
        ); END if2
      ); END progn1
    ); END if1
  )
)
(defun DT:clic_or_type_level(/ in nent txt VL_ent_name)
  ; Clic on any atribute or text with a level and return its text, or type it in
  (setq in (DT:input_string_or_point))
  (cond
    ((= 'LIST (type in)) ; it's a point
      (setq nent (nentselp in))
      (if (not nent)
        (princ "nothing selected.\n")
        (progn
          (setq VL_ent_name (vlax-ename->vla-object (car nent)))
          (if (vlax-property-available-p VL_ent_name 'TextString)
            (setq txt (vlax-get-property VL_ent_name 'TextString))
            (progn
              (setq nent nil)
              (princ "no text found.")
            ); END progn2
          ); END if2
        ); END progn1
      ); END if1
    )
    ((= 'STR (type in)) ; it's a string
    (setq txt in)
    )
  )
  ; Analize the input text
  (cond
    ; Normal number: adoptable manholes, PI_DAVID block
    ( (and (< (strlen txt) 8) (> (strlen txt) 4) (/= "S" (substr txt 1 1)) (/= "F" (substr txt 1 1)))
      ;(alert "case 1")
      (atof txt)
    )
    ; FFL
    ( (and (= "FFL " (substr txt 1 4)) (= 4 (- (strlen txt) (vl-string-search "." txt))) )
      ;(alert "case 2")
      (atof (substr txt 5))
    )
    ; Road level
    ( (and (= "%%U" (substr txt 1 3)) (= 3 (- (strlen txt) (vl-string-search "." txt))))
      ;(alert "case 3")
      (atof (substr txt 4 10))
    )
    ; Plot level
    ( (and (= "%%U" (substr txt 1 3)) (= 4 (- (strlen txt) (vl-string-search "." txt))))
      ;(alert "case 4")
      (atof (substr txt 4 9))
    )
    ; Private mahole
    ( (and
        (or (= "S" (substr txt 1 1)) (= "F" (substr txt 1 1)))
        (and (>= (ascii (substr txt 2 1)) 48) (<= (ascii (substr txt 2 1)) 57))
        (> (strlen txt) 5)
      )
      ;(setq cota (atof (substr txt 4 9)))
      ;(alert "case 5")
      (atof (substr txt 2))
    )
    ; Non number
    ( (and
        (= (atof txt) 0)
        (or (< (ascii (substr txt 2 1)) 97) (> (ascii (substr txt 2 1)) 122))
        (/= (ascii (substr txt 1 1)) 48)
      )
      ;(alert "case 6")
      (getreal "\nNumber format not understood. Please, introduce level: ")
    )
    ; Other
    (t
      ;(alert "case 7")
      (initget "Yes No")
      (setq ans (getkword (strcat "\nNo standard format. Verify " txt "m level. [Yes/No] <Yes>:")))
      (if (or (not ans) (= ans "Yes"))
        (atof txt)
        (exit)
      )
    );END cond4
  ); END cond
)
(defun DT:level_detection ( / txt )
  (setq txt (DT:destripar_txt))
  (cond
    ; Normal number: adoptable manholes, PI_DAVID block
    ( (and (< (strlen txt) 8) (> (strlen txt) 4) (/= "S" (substr txt 1 1)) (/= "F" (substr txt 1 1)))
      (atof txt)
    )
    ; FFL
    ( (and (= "FFL " (substr txt 1 4)) (= 4 (- (strlen txt) (vl-string-search "." txt))) )
      (atof (substr txt 5))
    )
    ; Road level
    ( (and (= "%%U" (substr txt 1 3)) (= 3 (- (strlen txt) (vl-string-search "." txt))))
      (atof (substr txt 4 10))
    )
    ; Plot level
    ( (and (= "%%U" (substr txt 1 3)) (= 4 (- (strlen txt) (vl-string-search "." txt))))
      (atof (substr txt 4 9))
    )
    ; Private mahole
    ( (and (or (= "S" (substr txt 1 1)) (= "F" (substr txt 1 1))) (> (strlen txt) 5))
      ;(setq cota (atof (substr txt 4 9)))
      (atof (substr txt 2))
    )
    ; Other
    (t
      (if (= (setq cota (atof txt)) 0)
        (getreal "\nI don't understand this format. Please, introduce level: ")
      ); END if
    );END cond4
  ); END cond
)
(defun DT:matrix_calc ( nent p )
  (setq
    nent (nentsel)
    v1 (car    (caddr nent))
    v2 (cadr   (caddr nent))
    v3 (caddr  (caddr nent))
    P0 (cadddr (caddr nent))
    px (car   p) ; coord X in child drawing (nested drawing)
    py (cadr  p) ; coord Y in child drawing (nested drawing)
    pz (caddr p) ; coord Z in child drawing (nested drawing)
  )
  (setq
    vx (list (car   v1) (car   v2) (car   v3))
    vy (list (cadr  v1) (cadr  v2) (cadr  v3))
    vz (list (caddr v1) (caddr v2) (caddr v3))
    e1 (mapcar '* vx p)
    e2 (mapcar '* vy p)
    e3 (mapcar '* vz p)
    x (+ (+ (+ (car e1) (cadr e1)) (caddr e1)) (car   P0)) ; coord X in parent drawing
    y (+ (+ (+ (car e2) (cadr e2)) (caddr e2)) (cadr  P0)) ; coord Y in parent drawing
    z (+ (+ (+ (car e3) (cadr e3)) (caddr e3)) (caddr P0)) ; coord Z in parent drawing
    p_padre (list x y z)
  )
  (princ)
)
(defun LM:Round ( n ) (fix (+ n (if (minusp n) -0.5 0.5))))
(defun LM:getvisibilitystate ( blk )
;; Get Dynamic Block Visibility State  -  Lee Mac
;; Returns the value of the Visibility Parameter of a Dynamic Block (if present)
;; blk - [vla] VLA Dynamic Block Reference object
;; Returns: [str] Value of Visibility Parameter, else nil
    (LM:getdynpropvalue blk (LM:getvisibilityparametername blk))
)
(defun LM:getvisibilityparametername ( blk / vis )
;; Get Visibility Parameter Name  -  Lee Mac
;; Returns the name of the Visibility Parameter of a Dynamic Block (if present)
;; blk - [vla] VLA Dynamic Block Reference object
;; Returns: [str] Name of Visibility Parameter, else nil
    (if
        (and
            (vlax-property-available-p blk 'effectivename)
            (setq blk
                (vla-item
                    (vla-get-blocks (vla-get-document blk))
                    (vla-get-effectivename blk)
                )
            )
            (= :vlax-true (vla-get-isdynamicblock blk))
            (= :vlax-true (vla-get-hasextensiondictionary blk))
            (setq vis
                (vl-some
                   '(lambda ( pair )
                        (if
                            (and
                                (= 360 (car pair))
                                (= "BLOCKVISIBILITYPARAMETER" (cdr (assoc 0 (entget (cdr pair)))))
                            )
                            (cdr pair)
                        )
                    )
                    (dictsearch
                        (vlax-vla-object->ename (vla-getextensiondictionary blk))
                        "ACAD_ENHANCEDBLOCK"
                    )
                )
            )
        )
        (cdr (assoc 301 (entget vis)))
    )
)
(defun LM:getdynpropvalue ( blk prp )
;; Get Dynamic Block Property Value  -  Lee Mac
;; Returns the value of a Dynamic Block property (if present)
;; blk - [vla] VLA Dynamic Block Reference object
;; prp - [str] Dynamic Block property name (case-insensitive)
    (setq prp (strcase prp))
    (vl-some '(lambda ( x ) (if (= prp (strcase (vla-get-propertyname x))) (vlax-get x 'value)))
        (vlax-invoke blk 'getdynamicblockproperties)
    )
)
(defun LM:vl-setattributevalue ( blk tag val )
  ;; Set Attribute Value  -  Lee Mac
  ;; Sets the value of the first attribute with the given tag found within the block, if present.
  ;; blk - [vla] VLA Block Reference Object
  ;; tag - [str] Attribute TagString
  ;; val - [str] Attribute Value
  ;; Returns: [str] Attribute   value if successful, else nil.
    (setq tag (strcase tag))
    (vl-some
       '(lambda ( att )
            (if (= tag (strcase (vla-get-tagstring att)))
                (progn (vla-put-textstring att val) val)
            )
        )
        (vlax-invoke blk 'getattributes)
    )
)
(defun get_block_att ( obj attribute )
  (vl-some '(lambda ( att )
              (if (= attribute (strcase (vla-get-tagstring att)))
                (vla-get-textstring att)
              ) ; END if
            )
          (vlax-invoke obj 'getattributes)
        )
)
(defun LM:grsnap:snapfunction ( )
  ;; Object Snap for grread: Snap Function  -  Lee Mac
  ;; Returns: [fun] A function requiring two arguments:
  ;; p - [lst] UCS Point to be snapped
  ;; o - [int] Object Snap bit code
  ;; The returned function returns either the snapped point (displaying an appropriate snap symbol)
  ;; or the supplied point if the snap failed for the given Object Snap bit code.
  (eval
    (list 'lambda '( p o / q )
      (list 'if '(zerop (logand 16384 o))
        (list 'if
         '(setq q
            (cdar
              (vl-sort
                (vl-remove-if 'null
                  (mapcar
                    (function
                      (lambda ( a / b )
                        (if (and (= (car a) (logand (car a) o)) (setq b (osnap p (cdr a))))
                          (list (distance p b) b (car a))
                        )
                      )
                    )
                   '(
                      (0001 . "_end")
                      (0002 . "_mid")
                      (0004 . "_cen")
                      (0008 . "_nod")
                      (0016 . "_qua")
                      (0032 . "_int")
                      (0064 . "_ins")
                      (0128 . "_per")
                      (0256 . "_tan")
                      (0512 . "_nea")
                      (2048 . "_app")
                      (8192 . "_par")
                    )
                  )
                )
               '(lambda ( a b ) (< (car a) (car b)))
              )
            )
          )
          (list 'LM:grsnap:displaysnap '(car q)
            (list 'cdr
              (list 'assoc '(cadr q)
                (list 'quote
                  (LM:grsnap:snapsymbols
                    (atoi (cond ((getenv "AutoSnapSize")) ("5")))
                  )
                )
              )
            )
            (LM:OLE->ACI
              (if (= 1 (getvar 'cvport))
                (atoi (cond ((getenv "Layout AutoSnap Color")) ("117761")))
                (atoi (cond ((getenv  "Model AutoSnap Color")) ("104193")))
              )
            )
          )
        )
      )
     '(cond ((car q)) (p))
    )
  )
)
(defun LM:grsnap:displaysnap ( pnt lst col / scl )
  ;; Object Snap for grread: Display Snap  -  Lee Mac
  ;; pnt - [lst] UCS point at which to display the symbol
  ;; lst - [lst] grvecs vector list
  ;; col - [int] ACI colour for displayed symbol
  ;; Returns nil
    (setq scl (/ (getvar 'viewsize) (cadr (getvar 'screensize)))
          pnt (trans pnt 1 2)
    )
    (grvecs (cons col lst)
        (list
            (list scl 0.0 0.0 (car  pnt))
            (list 0.0 scl 0.0 (cadr pnt))
            (list 0.0 0.0 scl 0.0)
           '(0.0 0.0 0.0 1.0)
        )
    )
)
(defun LM:grsnap:snapsymbols ( p / -p -q -r a c i l q r )
  ;; Object Snap for grread: Snap Symbols  -  Lee Mac
  ;; p - [int] Size of snap symbol in pixels
  ;; Returns: [lst] List of vector lists describing each Object Snap symbol
    (setq -p (- p) q (1+  p)
          -q (- q) r (+ 2 p)
          -r (- r) i (/ pi 6.0)
           a 0.0
    )
    (repeat 12
        (setq l (cons (list (* r (cos a)) (* r (sin a))) l)
              a (- a i)
        )
    )
    (setq c (apply 'append (mapcar 'list (cons (last l) l) l)))
    (list
        (list 1
            (list -p -p) (list p -p) (list p -p) (list p p) (list p p) (list -p p) (list -p p) (list -p -p)
            (list -q -q) (list q -q) (list q -q) (list q q) (list q q) (list -q q) (list -q q) (list -q -q)
        )
        (list 2
            (list -r -q) (list 0  r) (list 0  r) (list r -q)
            (list -p -p) (list p -p) (list p -p) (list 0  p) (list 0  p) (list -p -p)
            (list -q -q) (list q -q) (list q -q) (list 0  q) (list 0  q) (list -q -q)
        )
        (cons 4 c)
        (vl-list* 8 (list -r -r) (list r r) (list r -r) (list -r r) c)
        (list 16
            (list p 0) (list 0 p) (list 0 p) (list -p 0) (list -p 0) (list 0 -p) (list 0 -p) (list p 0)
            (list q 0) (list 0 q) (list 0 q) (list -q 0) (list -q 0) (list 0 -q) (list 0 -q) (list q 0)
            (list r 0) (list 0 r) (list 0 r) (list -r 0) (list -r 0) (list 0 -r) (list 0 -r) (list r 0)
        )
        (list 32
            (list  r r) (list -r -r) (list  r q) (list -q -r) (list  q r) (list -r -q)
            (list -r r) (list  r -r) (list -q r) (list  r -q) (list -r q) (list  q -r)
        )
        (list 64
            '( 0  1) (list  0  p) (list  0  p) (list -p  p) (list -p  p) (list -p -1) (list -p -1) '( 0 -1)
            '( 0 -1) (list  0 -p) (list  0 -p) (list  p -p) (list  p -p) (list  p  1) (list  p  1) '( 0  1)
            '( 1  2) (list  1  q) (list  1  q) (list -q  q) (list -q  q) (list -q -2) (list -q -2) '(-1 -2)
            '(-1 -2) (list -1 -q) (list -1 -q) (list  q -q) (list  q -q) (list  q  2) (list  q  2) '( 1  2)
        )
        (list 128
            (list (1+ -p) 0) '(0 0) '(0 0) (list 0 (1+ -p))
            (list (1+ -p) 1) '(1 1) '(1 1) (list 1 (1+ -p))
            (list -p q) (list -p -p) (list -p -p) (list q -p)
            (list -q q) (list -q -q) (list -q -q) (list q -q)
        )
        (vl-list* 256 (list -r r)  (list r r) (list -r (1+ r)) (list r (1+ r)) c)
        (list 512
            (list -p -p) (list  p -p) (list -p  p) (list p p) (list -q -q) (list  q -q)
            (list  q -q) (list -q  q) (list -q  q) (list q q) (list  q  q) (list -q -q)
        )
        (list 2048
            (list   -p     -p) (list    p      p) (list   -p      p) (list    p     -p)
            (list (+ p 05) -p) (list (+ p 06) -p) (list (+ p 05) -q) (list (+ p 06) -q)
            (list (+ p 09) -p) (list (+ p 10) -p) (list (+ p 09) -q) (list (+ p 10) -q)
            (list (+ p 13) -p) (list (+ p 14) -p) (list (+ p 13) -q) (list (+ p 14) -q)
            (list -p -p) (list p -p) (list p -p) (list p p) (list p p) (list -p p) (list -p p) (list -p -p)
            (list -q -q) (list q -q) (list q -q) (list q q) (list q q) (list -q q) (list -q q) (list -q -q)
        )
        (list 8192 (list r 1) (list -r -q) (list r 0) (list -r -r) (list r q) (list -r -1) (list r r) (list -r 0))
    )
)
(defun LM:grsnap:parsepoint ( bpt str / str->lst lst )
  ;; Object Snap for grread: Parse Point  -  Lee Mac
  ;; bpt - [lst] Basepoint for relative point input, e.g. @5,5
  ;; str - [str] String representing point input
  ;; Returns: [lst] Point represented by the given string, else nil

    (defun str->lst ( str / pos )
        (if (setq pos (vl-string-position 44 str))
            (cons (substr str 1 pos) (str->lst (substr str (+ pos 2))))
            (list str)
        )
    )

    (if (wcmatch str "`@*")
        (setq str (substr str 2))
        (setq bpt '(0.0 0.0 0.0))
    )

    (if
        (and
            (setq lst (mapcar 'distof (str->lst str)))
            (vl-every 'numberp lst)
            (< 1 (length lst) 4)
        )
        (mapcar '+ bpt lst)
    )
)
(defun LM:grsnap:snapmode ( str )
  ;; Object Snap for grread: Snap Mode  -  Lee Mac
  ;; str - [str] Object Snap modifier
  ;; Returns: [int] Object Snap bit code for the given modifier, else nil
    (vl-some
        (function
            (lambda ( x )
                (if (wcmatch (car x) (strcat (strcase str t) "*"))
                    (progn
                        (princ (cadr x)) (caddr x)
                    )
                )
            )
        )
       '(
            ("endpoint"      " of " 00001)
            ("midpoint"      " of " 00002)
            ("center"        " of " 00004)
            ("node"          " of " 00008)
            ("quadrant"      " of " 00016)
            ("intersection"  " of " 00032)
            ("insert"        " of " 00064)
            ("perpendicular" " to " 00128)
            ("tangent"       " to " 00256)
            ("nearest"       " to " 00512)
            ("appint"        " of " 02048)
            ("parallel"      " to " 08192)
            ("none"          ""     16384)
        )
    )
)
(defun LM:OLE->ACI ( c )
  ;; OLE -> ACI  -  Lee Mac
  ;; Args: c - [int] OLE Colour
  (apply 'LM:RGB->ACI (LM:OLE->RGB c))
)
(defun LM:OLE->RGB ( c )
  ;; OLE -> RGB  -  Lee Mac
  ;; Args: c - [int] OLE Colour
  (mapcar '(lambda ( x ) (lsh (lsh (fix c) x) -24)) '(24 16 8))
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
(defun LM:acapp nil
  ;; Application Object  -  Lee Mac
  ;; Returns the VLA Application Object
  (eval (list 'defun 'LM:acapp 'nil (vlax-get-acad-object)))
  (LM:acapp)
)
(defun invm ( m / c f p r )
;; Matrix Inverse  -  gile & Lee Mac
;; Uses Gauss-Jordan Elimination to return the inverse of a non-singular nxn matrix.
;; Args: m - nxn matrix

    (defun f ( p m )
        (mapcar '(lambda ( x ) (mapcar '(lambda ( a b ) (- a (* (car x) b))) (cdr x) p)) m)
    )
    (setq  m (mapcar 'append m (imat (length m))))
    (while m
        (setq c (mapcar '(lambda ( x ) (abs (car x))) m))
        (repeat (vl-position (apply 'max c) c)
            (setq m (append (cdr m) (list (car m))))
        )
        (if (equal 0.0 (caar m) 1e-14)
            (setq m nil
                  r nil
            )
            (setq p (mapcar '(lambda ( x ) (/ (float x) (caar m))) (cdar m))
                  m (f p (cdr m))
                  r (cons p (f p r))
            )
        )
    )
    (reverse r)
)
(defun imat ( n / i j l m )
;; Identity Matrix  -  Lee Mac
;; Args: n - matrix dimension
    (repeat (setq i n)
        (repeat (setq j n)
            (setq l (cons (if (= i j) 1.0 0.0) l)
                  j (1- j)
            )
        )
        (setq m (cons l m)
              l nil
              i (1- i)
        )
    )
    m
)
(defun TODAY ( / d yr mo day)
  (setq
    d (rtos (getvar "CDATE") 2 6)
    yr (substr d 1 4)
	  mo (substr d 5 2)
	  day (substr d 7 2)
	); END setq
  (strcat day "." mo "." yr)
)
(defun fbi ( blk )
;Fast Block Insert: point + rotation
	(command "-insert" blk pause 1 1 pause)
	(princ)
  ; v0.0 - 2016.03.29 - First issue
  ; Author: David Torralba
  ; Last revision: 2016.03.29
)
(defun fbi2 ( blk )
;Fast Block Insert: point
	(command "-insert" blk pause 1 1 0)
	(princ)
  ; v0.0 - 2016.03.30 - First issue
  ; Author: David Torralba
  ; Last revision: 2016.03.30
)
(princ)
