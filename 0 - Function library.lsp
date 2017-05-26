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
      (if (not (tblsearch "LTYPE" "DASHED")) (cons 6 "CONTINUOUS") (cons 6 "DASHED"))  ; Line type: dashed, if possible
      (cons 48 0.06)                                          ; Line type scale
    )
  )
  ; v0.2 - 2016.04.10 - Linetype style check.
  ; v0.1 - 2016.03.21 - Translate into English
  ; v0.0 - 2016.03.18 - First issue
  ; Author: David Torralba
  ; Last revision: 2016.04.10
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
      (if (not (tblsearch "style" "ARIAL")) (cons 7 "standard") (cons 7 "ARIAL")) ; Text style: ARIAL, if possible
    )
  )
  ; v0.2 - 2016.04.10 - Text style check.
  ; v0.1 - 2016.03.21 - Translate into English
  ; v0.0 - 2016.03.18 - First issue
  ; Author: David Torralba
  ; Last revision: 2016.04.10
)
(defun _CursorText_L2 ( p txt2 textsize )
  (entmakex
    (list (cons 0 "TEXT")
      (cons 40 (setq dis textsize))                           ; Text height
      (cons 10 (polar (polar p 0 dis) (* -0.5 pi) (* 4 dis))) ; Text insertion point
      (cons 50 0)                                             ; Text rotation
      (cons 62 251)                                           ; Text color
      (cons 1 txt2)                                           ; Text content
      (if (not (tblsearch "style" "ARIAL")) (cons 7 "standard") (cons 7 "ARIAL")) ; Text style: ARIAL, if possible
    )
  )
  ; v0.2 - 2016.04.10 - Text style check.
  ; v0.1 - 2016.03.21 - Translate into English
  ; v0.0 - 2016.03.18 - First issue
  ; Author: David Torralba
  ; Last revision: 2016.04.10
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
(defun DT:clic_or_type_level (/ in nent txt VL_ent_name)
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
      (atof (substr txt 5))
    )
    ; Road level
    ( (and (or (= "%%U" (substr txt 1 3)) (= "%%u" (substr txt 1 3))) (= 3 (- (strlen txt) (vl-string-search "." txt))))
      (atof (substr txt 4 10))
    )
    ; Plot level
    ( (and (or (= "%%U" (substr txt 1 3)) (= "%%u" (substr txt 1 3))) (= 4 (- (strlen txt) (vl-string-search "." txt))))
      (atof (substr txt 4 9))
    )
    ; Private mahole
    ( (and
        (or (= "S" (substr txt 1 1)) (= "F" (substr txt 1 1)))
        (and (>= (ascii (substr txt 2 1)) 48) (<= (ascii (substr txt 2 1)) 57))
        (> (strlen txt) 5)
      )
      (atof (substr txt 2))
    )
    ; Sub-base level
    ( (and
        (or (= "SB" (substr txt 1 2)) (= "sb" (substr txt 1 2)) )
        (= 3 (- (strlen txt) (vl-string-search "." txt)) )
      )
      (atof (substr txt 3 8))
    )
    ; Non number
    ( (and
        (= (atof txt) 0)
        (or (< (ascii (substr txt 2 1)) 97) (> (ascii (substr txt 2 1)) 122))
        (/= (ascii (substr txt 1 1)) 48)
      )
      (getreal "\nNumber format not understood. Please, introduce level: ")
    )
    ; Other
    (t
      ;(alert "case 7")
      (if (= 0.0 (atof txt))
        (progn
          (initget "Yes No")
          (setq ans (getkword (strcat "\nNo standard format. Verify " txt "m level. [Yes/No] <Yes>:")))
          (if (or (not ans) (= ans "Yes"))
            (atof txt)
            (exit)
          )
        );END progn
        (atof txt)
      );END if
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
(defun LM:rtos ( real units prec / dimzin result )
  ;; rtos wrapper  -  Lee Mac
  ;; A wrapper for the rtos function to negate the effect of DIMZIN
    (setq dimzin (getvar 'dimzin))
    (setvar 'dimzin 0)
    (setq result (vl-catch-all-apply 'rtos (list real units prec)))
    (setvar 'dimzin dimzin)
    (if (not (vl-catch-all-error-p result))
        result
    )
)
(defun LM:getdynprops ( blk )
  ;; Get Dynamic Block Properties  -  Lee Mac
  ;; Returns an association list of Dynamic Block properties & values.
  ;; blk - [vla] VLA Dynamic Block Reference object
  ;; Returns: [lst] Association list of ((<prop> . <value>) ... )
    (mapcar '(lambda ( x ) (cons (vla-get-propertyname x) (vlax-get x 'value)))
        (vlax-invoke blk 'getdynamicblockproperties)
    )
)
(defun LM:setdynprops ( blk lst / itm )
  ;; Set Dynamic Block Properties  -  Lee Mac
  ;; Modifies values of Dynamic Block properties using a supplied association list.
  ;; blk - [vla] VLA Dynamic Block Reference object
  ;; lst - [lst] Association list of ((<Property> . <Value>) ... )
  ;; Returns: nil
    (setq lst (mapcar '(lambda ( x ) (cons (strcase (car x)) (cdr x))) lst))
    (foreach x (vlax-invoke blk 'getdynamicblockproperties)
        (if (setq itm (assoc (strcase (vla-get-propertyname x)) lst))
            (vla-put-value x (vlax-make-variant (cdr itm) (vlax-variant-type (vla-get-value x))))
        )
    )
)
(defun LM:getvisibilitystate ( blk )
  ;; Get Dynamic Block Visibility State  -  Lee Mac
  ;; Returns the value of the Visibility Parameter of a Dynamic Block (if present)
  ;; blk - [vla] VLA Dynamic Block Reference object
  ;; Returns: [str] Value of Visibility Parameter, else nil
    (LM:getdynpropvalue blk (LM:getvisibilityparametername blk))
)
(defun LM:SetVisibilityState ( blk val / vis )
  ;; Set Dynamic Block Visibility State  -  Lee Mac
  ;; Sets the Visibility Parameter of a Dynamic Block (if present) to a specific value (if allowed)
  ;; blk - [vla] VLA Dynamic Block Reference object
  ;; val - [str] Visibility State Parameter value
  ;; Returns: [str] New value of Visibility Parameter, else nil
    (if
        (and
            (setq vis (LM:getvisibilityparametername blk))
            (member (strcase val) (mapcar 'strcase (LM:getdynpropallowedvalues blk vis)))
        )
        (LM:setdynpropvalue blk vis val)
    )
)
(defun LM:effectivename ( obj )
  ;; Effective Block Name  -  Lee Mac
  ;; obj - [vla] VLA Block Reference object
    (vlax-get-property obj
        (if (vlax-property-available-p obj 'effectivename)
            'effectivename
            'name
        )
    )
)
(defun get_block_att ( obj IL )
  (vl-some '(lambda ( att )
              (if (= IL (strcase (vla-get-tagstring att)))
                (vla-get-textstring att)
              ) ; END if
            )
          (vlax-invoke obj 'getattributes)
        )
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
(defun LM:getdynpropallowedvalues ( blk prp )
  ;; Get Dynamic Block Property Allowed Values  -  Lee Mac
  ;; Returns the allowed values for a specific Dynamic Block property.
  ;; blk - [vla] VLA Dynamic Block Reference object
  ;; prp - [str] Dynamic Block property name (case-insensitive)
  ;; Returns: [lst] List of allowed values for property, else nil if no restrictions
    (setq prp (strcase prp))
    (vl-some '(lambda ( x ) (if (= prp (strcase (vla-get-propertyname x))) (vlax-get x 'allowedvalues)))
        (vlax-invoke blk 'getdynamicblockproperties)
    )
)
(defun LM:vl-getattributevalue ( blk tag )
  ;; Get Attribute Value  -  Lee Mac
  ;; Returns the value held by the specified tag within the supplied block, if present.
  ;; blk - [vla] VLA Block Reference Object
  ;; tag - [str] Attribute TagString
  ;; Returns: [str] Attribute value, else nil if tag is not found.
    (setq tag (strcase tag))
    (vl-some '(lambda ( att ) (if (= tag (strcase (vla-get-tagstring att))) (vla-get-textstring att)))
        (vlax-invoke blk 'getattributes)
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
(defun LM:vl-getattributes ( blk )
  ;; Get Attributes  -  Lee Mac
  ;; Returns an association list of attributes present in the supplied block.
  ;; blk - [vla] VLA Block Reference Object
  ;; Returns: [lst] Association list of ((<Tag> . <Value>) ... )
    (mapcar '(lambda ( att ) (cons (vla-get-tagstring att) (vla-get-textstring att)))
        (vlax-invoke blk 'getattributes)
    )
)
(defun LM:setdynpropvalue ( blk prp val )
  ;; Set Dynamic Block Property Value  -  Lee Mac
  ;; Modifies the value of a Dynamic Block property (if present)
  ;; blk - [vla] VLA Dynamic Block Reference object
  ;; prp - [str] Dynamic Block property name (case-insensitive)
  ;; val - [any] New value for property
  ;; Returns: [any] New value if successful, else nil
    (setq prp (strcase prp))
    (vl-some
       '(lambda ( x )
            (if (= prp (strcase (vla-get-propertyname x)))
                (progn
                    (vla-put-value x (vlax-make-variant val (vlax-variant-type (vla-get-value x))))
                    (cond (val) (t))
                )
            )
        )
        (vlax-invoke blk 'getdynamicblockproperties)
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
(defun LM:vl-getattributevalue ( blk tag )
  ;; Get Attribute Value  -  Lee Mac
  ;; Returns the value held by the specified tag within the supplied block, if present.
  ;; blk - [vla] VLA Block Reference Object
  ;; tag - [str] Attribute TagString
  ;; Returns: [str] Attribute value, else nil if tag is not found.
    (setq tag (strcase tag))
    (vl-some '(lambda ( att ) (if (= tag (strcase (vla-get-tagstring att))) (vla-get-textstring att)))
        (vlax-invoke blk 'getattributes)
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
(defun LM:SubstNth ( a n l / i )
  ;;---------------------=={ Subst Nth }==----------------------;;
  ;;                                                            ;;
  ;;  Substitutes an item at the nth position in a list.        ;;
  ;;------------------------------------------------------------;;
  ;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;
  ;;------------------------------------------------------------;;
  ;;  Arguments:                                                ;;
  ;;  a - item to substitute                                    ;;
  ;;  n - position in list to make the substitution             ;;
  ;;  l - list in which to make the substitution                ;;
  ;;------------------------------------------------------------;;
  ;;  Returns:  Resultant list following the substitution       ;;
  ;;------------------------------------------------------------;;
  (setq i -1)
  (mapcar '(lambda ( x ) (if (= (setq i (1+ i)) n) a x)) l)
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
(defun CopyToClipboard (str / html result)
  (if (= 'STR (type str))
    (progn
      (setq
        html (vlax-create-object "htmlfile")
        result (vlax-invoke (vlax-get (vlax-get html 'ParentWindow) 'ClipBoardData) 'setData "Text" str)
      ); END setq
      (vlax-release-object html)
      str
    ); END progn
   );END if
)
(defun DT:AVE_vertex ( VL_ent_name / c i x y p )
  ; Returns a point with the average coordinates of all the polyline vertexes
  (setq c (vlax-get VL_ent_name "Coordinates") i 0 x 0 y 0 )
  (repeat (/ (length c) 2)
    (setq
      x (+ x (nth i c)) i (+ i 1)
      y (+ y (nth i c)) i (+ i 1)
    )
  );END repeat
  (setq p (list (/ x (/ (length c) 2)) (/ y (/ (length c) 2)) 0.0 ) )
)
(defun c:beo () (DT:beo (car (entsel)) ) )
(defun DT:beo ( ent_name / VL_ent_name ans)
  ; Block Explode Option
  ; This routine allows to enable/disable explodable option at selected block.

  ; OPERATION - Mark selected object
  (sssetfirst nil (ssadd ent_name))
  ;OPERATION - Check if it is a block
  (if (= "INSERT" (cdr (assoc 0 (entget ent_name))))
    (progn
      ; OPERATION - Get objects VLA block name
      (setq VL_ent_name (vla-item (vla-get-blocks (vla-get-activedocument (vlax-get-acad-object))) (cdr (assoc 2 (entget ent_name)))) )
      ; OPERATION - Check if EXPLODABLE property exists at the selected object
      (if (vlax-property-available-p VL_ent_name 'explodable)
        (cond
          ((= (vlax-get-property VL_ent_name 'explodable) :vlax-true)
            (princ "\nSelected block's explode option is ON.")
            (initget "Yes No")
            (setq ans (getkword " Do you want to disable it? [Yes/No] <No>:"))
            (if (not ans) (setq ans "No"))
            (if (= ans "Yes")
              (progn
                (vlax-put-property VL_ent_name 'explodable :vlax-false)
                ; OPERATION - Check if it's been changed correctly
                (if (= (vlax-get-property VL_ent_name 'explodable) :vlax-true) (princ "\nSorry, it was impossible to disable explode option.") )
              );END progn
            );END if ans
          );END subcond
          ((= (vlax-get-property VL_ent_name 'explodable) :vlax-false)
            (princ "\nSelected block's explode option is OFF.")
            (initget "Yes No")
            (setq ans (getkword " Do you want to enable it? [Yes/No] <No>:"))
            (if (not ans) (setq ans "No"))
            (if (= ans "Yes")
              (progn
                (vlax-put-property VL_ent_name 'explodable :vlax-true)
                ; OPERATION - Check if it's been changed correctly
                (if (= (vlax-get-property VL_ent_name 'explodable) :vlax-false) (princ "\nSorry, it was impossible to disable explode option.") )
              );END progn
            );END if ans
          );END subcond
        );END cond
      );END if property available
    );END progn
    (alert "This is not a block!")
  );END if

  ; OPERATION - Ungrip selected object
  (sssetfirst nil nil)

  (princ)

  ; v0.0 - 2016.08.11 - First issue
  ; Author: David Torralba
  ; Last revision: 2016.08.11
)
(defun c:egurre( / filePath)
  ; Descarga mi libreria personal
  (setq filePath "C:/_PersonalLibrary.lsp")
  (if (= T (download "https://raw.githubusercontent.com/dtgoitia/civil-autolisp/master/_PersonalLibrary.lsp" "C:/"))
    (progn
      ; Comprobar que el archivo es correcto
      (if (= T (CheckPersonalLibraryFirstLine filePath))
        (progn
          (LoadWithoutSecureload filePath "\nEl archivo se ha descargado, contiene lo esperado, pero como no lo puedo cargar se ha eliminado.")
          (vl-file-delete filePath)                                 ; Borra el archivo
        );END progn
        (progn
          (princ "\nEl archivo descargado no contiene lo esperado y se ha eliminado. Comprueba la URL de GitHub.")
          (vl-file-delete filePath) ; Borra el archivo
        );END progn
      );END if
    );END progn
    (princ "\nNo he podido descargar el archivo.")
  );END if
  (CleanCommandLine)
  (princ)
)
(defun LoadWithoutSecureload ( filePath OnFailMessage / old_secureload URL)
  (setq old_secureload (getvar "secureload")) ; get current SECURELOAD
  (setvar "secureload" 0)                     ; set SECURELOAD to 0
  (load filePath OnFailMessage)               ; load file
  (setvar "secureload" old_secureload)        ; reset SECURELOAD
  (princ)
)
(defun CheckPersonalLibraryFirstLine( filePath / fileID )
  (if filePath
    (if (findfile filePath)
      (if (setq fileID (open (findfile filePath) "R") )
        (if (= (read-line fileID) "; DO NOT REMOVE THIS LINE. It's a checking.")
          (progn
            (close fileID)
            T
          );END progn
          (progn
            (princ "\nERROR @ CheckPersonalLibraryFirstLine: downloaded file header is incorrect.")
            (close fileID)
            nil
          );END progn
        );END if
      );END if
      (progn (princ "\nERROR @ CheckPersonalLibraryFirstLine: supplied file can't be opened.") (princ) )
    );END if
    (progn (princ "\nERROR @ CheckPersonalLibraryFirstLine: no filePath argument supplied") (princ) )
  );END if
)
(defun download (lien rep / cp ok tmp util)
  ; Credit to *BigBill
  ; Source: http://forums.autodesk.com/t5/visual-lisp-autolisp-and-general/code-to-get-remote-file-and-place-in-a-specific-directory-i-e/td-p/2657085
  ; Source last check date: 2016.11.10
  ; Message in French updated to English.
  (setq util (vla-get-Utility (vla-get-ActiveDocument (vlax-get-acad-object) ) ) )
  (if (eq (vla-isurl util lien) :vlax-true)
    (if (vl-catch-all-error-p (vl-catch-all-apply 'vla-GetRemoteFile (list util lien 'tmp :vlax-true)))
      (princ "\nDownload error.")
      (progn
        (setq cp (strcat rep (vl-filename-base lien) (vl-filename-extension lien)))
        (if (findfile cp) ; If a previos file with the same name exists, delete it.
          (vl-file-delete cp)
        );END if
        (if (vl-catch-all-error-p (vl-catch-all-apply 'vl-file-copy (list tmp cp)))
          (progn
            (princ "\nUnable to move the file \""
              (strcat (vl-filename-base cp)(vl-filename-extension cp))
              "\" since the directory \n\""
              tmp
              )
              (vl-file-delete tmp)
          );END progn
          (progn
            (vl-file-delete tmp)
            (if (vl-file-size cp)
              (if (zerop (vl-file-size cp))
                (progn
                  (vl-file-delete cp)
                  (princ "\nUnable to download the file.")
                );END progn
                (setq ok T)
              );END if
              (progn
                (princ "\nUnable to delete the file.")
              );END progn
            );END if
          );END progn
        );END if
      );END progn
    );END if
    (princ "\nThe url is not valid.")
  );END if
  ok
)
(defun CleanCommandLine()
  (repeat 400 (princ (strcat "\n" (chr 160) ) ) )
  (princ)
)
(defun FindCharRightToLeft ( path pattern / pos len i)
  ; Return the position of the first character that matches
  (if path
    (progn
      (setq i 0)
      (while (vl-string-position (ascii pattern) path)
        (setq
          pos (vl-string-position (ascii pattern) path)
          len (strlen path)
          path (strcat (substr path 1 pos) (substr path (+ pos 2)) )
          i (+ i 1)
        )
      );END while
      (+ pos i)
    );END progn
  );END if
)
(defun SplitUrlDirectoryFile ( path )
  ; Return file directory and file name in a list
  (if path
    (list
      (substr path 1 (FindCharRightToLeft path "/"))
      (substr path (+ (FindCharRightToLeft path "/") 1))
    );END list
    (progn (princ "\nERROR @ SplitUrlDirectoryFile: provided path is incorrect.") (princ) )
  );END if
)
; SIMPLE VERSION
;(defun DT:CheckIfBlockExists( blockName )
;  (if (tblsearch "block" blockName)
;    T
;    nil
;  );END if
;)
(defun DT:CheckIfBlockExists ( blockName / exists)
  ; Returns T if any block named as blockName (not Layout or Xref) exists within
  ; the Block Table, otherwise returns nil
  ; blockName [str] - Name of the block to search
  (vlax-for x (vla-get-blocks (vla-get-ActiveDocument (vlax-get-acad-object)))
    (progn
      (if
        (and
          (= (vla-get-IsLayout x) :vlax-false) ; is not a layout
          (= (vla-get-IsXref x)   :vlax-false) ; is not an xref
        );END and
        (if (= blockName (vlax-get-property x (if (vlax-property-available-p x 'EffectiveName) 'EffectiveName 'Name) ) )
          (setq exists T) ; the block exists
          nil             ; the block doesn't exist
        );END if
      );END if
    );END progn
  );END vlax-for
  exists
)
(defun DT:replaceBlock ( ent_name newBlockName / entList)
  ; Replaces the insert ent_name for a new insert of newBlockName block. If successful returnt T, otherwise nil.
  ; ent_name [ename] - entity to be replaced
  ; newBlockName [str] - new block name
  (setq
    entList (entget ent_name)
    entList (subst (cons 2 newBlockName) (assoc 2 entList) entList)
  )
  (if (entmod entList)
    T
    nil
  )
)
; ERROR HANDLING FUNCTIONS -----------------------------------------------------
(defun get_sysvars(targets)
  ; Return variable's values
  (mapcar
    '(lambda (variable) (list variable (getvar variable)))
    targets
  )
); END defun
(defun set_sysvars(variables)
  ; Given a list of pairs of var-value, it sets them
  (mapcar
    '(lambda (variable) (setvar (nth 0 variable) (nth 1 variable)))
    variables
  )
); END defun
(defun get_environment(targets)
  ; Store current *error* function and requested system variables
  ; targets [list] = system variables to store, e.g.: (list "osmode" "angdir" "angbase")
  (setq
    old_error *error*
    old_sysvars (get_sysvars targets)
  )
);END defun
(defun restore_environment()
  ; Store old environment
  (set_sysvars old_sysvars)     ; Restore old system variables
  (setq
    *error* old_error           ; Restore old error function
    old_error nil               ; Clear old error variable
  )
); END defun
(defun set_custom_error()
  (defun *error*(msg)
    (princ (strcat "\n >>> ERROR: " msg))
    (restore_environment)
  );END defun
);END defun
(defun save_environment (targets)
  ; Store environment and set custom error handler
  (get_environment targets)
  (set_custom_error)
);END defun
;| USE TEMPLATE
(defun c:test (/ old_error old_sysvars)
    (save_environment '("osmode" "angdir" "angbase"))

    WRITE_HERE_YOUR_CODE

    (restore_environment)
);END defun
|;
; ------------------------------------------------------------------------------
(defun DT:flatPoint( pt )
; Return the provided 3D point with Z=0
  (list (nth 0 pt) (nth 1 pt) 0.0)
)
(defun DT:Interpolate ( pA pB pC / dAB dAC dAD aAB aAC aABC pC pD zA zB zD )
  ; Returns a list with the reference points and the interpolated point
  ; all of them as 3D points:
  ; ( pA pB pD )
  ; pA [pt] - 3D point of reference
  ; pB [pt] - 3D point of reference
  ; pC [pt] - 3D/2D point where to calculate the level
  ; pD [pt] - 3D point with level already interpolated
  ; NOTES:
  ; - pC and pD match on plan.
  ; - if pC (and pD) are not in line with pA and pB, the algorithm
  ;   will find the closest point to the pA-pB virtual line and work
  ;   as if pC (and pC) where that point.
  (if (and pA pB pC)
    (progn ; True: 3 no-nil arguments passed
      (if (and (= 'LIST (type pA)) (= 'LIST (type pB)) (= 'LIST (type pC)) )
        (progn ; True: 3 list-type arguments passed
          (if (and (= 3 (length pA)) (= 3 (length pB)) (= 3 (length pC)) )
            (progn ; True: 3 point-list-type arguments passed
              (setq
                dAB (distance (DT:flatPoint pA) (DT:flatPoint pB))  ; AB distance on plan
                dAC (distance (DT:flatPoint pA) (DT:flatPoint pC))  ; AC distance on plan
                aAB (angle pA pB)                                   ; AB direction (angle)
                aAC (angle pA pC)                                   ; AC direction (angle)
                aABC (- aAC aAB)                                    ; angle between AB and AC
                dAD (* dAC (cos aABC))                              ; AD distance = AC distance on plan projected over AB line
                pD (polar pA aAB dAD)                               ; D point on plan = C point on plan projected over AB line
                zA (nth 2 pA)                                       ; Level on A
                zB (nth 2 pB)                                       ; Level on B
                zD (+ zA (* dAD (/ (- zB zA) dAB)))                 ; level on D = level on C projected over AB line
                pC (list (nth 0 pC) (nth 1 pC) zD)                  ; C 3D point
              )

              ; Return data
              (list pA pB pC)
            );END progn
            (cond
              ((/= 3 (length pA)) (princ "\nERROR @ DT:int : pA is not a point")(princ) )
              ((/= 3 (length pB)) (princ "\nERROR @ DT:int : pB is not a point")(princ) )
              ((/= 3 (length pC)) (princ "\nERROR @ DT:int : pC is not a point")(princ) )
            );END cond
          );END if
        );END progn
        (cond
          ((/= 'LIST (type pA)) (princ "\nERROR @ DT:int : pA is not a list")(princ) )
          ((/= 'LIST (type pB)) (princ "\nERROR @ DT:int : pB is not a list")(princ) )
          ((/= 'LIST (type pC)) (princ "\nERROR @ DT:int : pC is not a list")(princ) )
        );END cond
      );END if
    );END progn
    (cond
      ((not pA) (princ "\nERROR @ DT:int : pA=nil")(princ) )
      ((not pB) (princ "\nERROR @ DT:int : pB=nil")(princ) )
      ((not pC) (princ "\nERROR @ DT:int : pC=nil")(princ) )
    );END cond
  );END if

  ; v0.0 - 2017-01-28 - First issue
  ; Author: David Torralba
  ; Last revision: 2017-01-28
)
(defun DT:GetCharFromPosition ( str n )
  ; Return the character in the position n, 1-based numbering
  ; or return nil if n is out of range
  (if (and str n)
    (progn
      (if (= 'str (type n)) (setq n (atoi n)) ) ; convert n to integer
      (if (and (<= n (strlen str)) (> n 0) )     ; check n is not bigger than str length or les than zero
        (substr str n 1)
        nil
      );END if
    );END progn
    (progn (princ "\nERROR @ DT:GetCharPosition > str or n = nil")(princ))
  );END if

  ; v0.0 - 2017.01.27 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.01.27
)
(defun DT:GetCharPositions ( str ch / i ls )
  ; Return a list with the position of the character, 1-based numbering
  ; or return nil if nothing found
  (if (and ch str)
    (progn
      (setq i 0)
      (while (<= i (strlen str))
        (setq i (+ i 1))
        (if (= ch (DT:GetCharFromPosition str i) )
          (setq ls (append ls (list i) ))
        );END if
      );END while
      ls
    );END progn
    (progn (princ "\nERROR @ DT:GetCharPosition > ch or str = nil")(princ))
  );END if

  ; v0.0 - 2017.01.27 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.01.27
)
(defun DT:StringToList ( str delimiter / ls previousPosition )
  ; Return a list with the stirng splitted
  ; str [str]       - String to split
  ; delimiter [str] - Single character string
  (if (and str delimiter)
    (if (and (= 'str (type str)) (= 'str (type delimiter)))
      (if (= 1 (strlen delimiter))
        (progn
          (if (setq positionList (DT:GetCharPositions str delimiter) )
            (if (/= (strlen str) (length positionList))
              (progn
                (foreach currentPosition positionList
                  (setq
                    previousPosition (if previousPosition previousPosition 1)
                    ls (if (= previousPosition currentPosition)
                      ls ; don't do anything
                      (append ls (list (substr str previousPosition (- currentPosition previousPosition)) ))
                    );END if
                    previousPosition (+ currentPosition 1)
                  )
                );END foreach
                (setq
                  ls (if (= previousPosition (+ (strlen str) 1))
                      ls ; don't do anything
                      (append ls (list (substr str previousPosition (- (+ (strlen str) 1) previousPosition)) ))
                    );END if
                  );END setq
                ls
              );END progn
              nil
            );END if
            (list str)
          );END if
        );END progn
        (progn (princ "\nERROR @ DT:StringToList > delimiter can only has 1 character")(princ))
      );END if
      (cond
        ((/= 'str (type str)      ) (princ "\nERROR @ DT:StringToList > str is not a string")(princ) )
        ((/= 'str (type delimiter)) (princ "\nERROR @ DT:StringToList > delimiter is not a string")(princ) )
      );END cond
    );END if
    (progn (princ "\nERROR @ DT:StringToList > str or delimiter = nil")(princ))
  );END if

  ; v0.0 - 2017.01.27 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.01.27
)
(defun DT:CheckIfAscendantOrder ( l / r i )
  ; Return true if the list is in ascendant order, nil if not
  (if l
    (progn
      (setq i 0 r T )

      ; If
      (foreach a l
        (if (not (or (= 'int (type a)) (= 'real (type a)))) (setq r nil) )
      );END foreach

      ; If element i > next element return nil
      (while (and (< i (- (length l) 1)) r)
        (if (> (nth i l) (nth (+ i 1) l) ) (setq r nil) )
        (setq i (+ i 1))
      );END while



      ; return result
      r
    );END progn
    nil
  );END if

  ; v0.0 - 2017.01.29 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.01.29
)
(defun DT:GetSmallest ( l / smallestValue )
  ; Return the smallest element on l, being l a number list
  (if l
    (if (= 'list (type l))
      (progn
        (setq smallestValue (nth 0 l) )
        (if (> (length l) 1)
          (foreach a l
            (if (< a smallestValue) (setq smallestValue a) )
          );END foreach
        );END if
        smallestValue
      );END progn
      (progn (princ "\nERROR @ DT:GetSmallest > l is not a list\n")(princ) nil )
    );END if
    (progn (princ "\nERROR @ DT:GetSmallest > l = nil\n")(princ) nil )
  );END if

  ; v0.0 - 2017.01.29 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.01.29
)
(defun DT:GetSmallestIndex ( l / smallestValue i index)
  ; Return the index (zero-based) of the smallest element on l, being l a number list
  (if l
    (if (= 'list (type l))
      (progn
        (setq i -1 smallestValue (nth 0 l) index 0)
        (if (> (length l) 1)
          (foreach a l
            (setq i (+ i 1))
            (if (< a smallestValue) (setq smallestValue a index i) )
          );END foreach
        );END if
        index
      );END progn
      (progn (princ "\nERROR @ DT:GetSmallestIndex > l is not a list\n")(princ) nil )
    );END if
    (progn (princ "\nERROR @ DT:GetSmallestIndex > l = nil\n")(princ) nil )
  );END if

  ; v0.0 - 2017.01.29 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.01.29
)
(defun DT:SubLst ( l a b / nl i )
  (if
    (and l a b (>= a 0) (< b (length l)) )
    (progn
      ; Return list elements from index a to b
      (setq i 0)
      (while (<= i b)
        (if (>= i a)
          (setq nl (append nl (list (nth i l))))
        );END if
        (setq i (+ i 1))
      );END while

      ; Return new list
      nl
    );END progn
    (cond
      ((not l) (princ "\nERROR @ DT:SubLst > l = nil\n")(princ) nil )
      ((not a) (princ "\nERROR @ DT:SubLst > a = nil\n")(princ) nil )
      ((not b) (princ "\nERROR @ DT:SubLst > b = nil\n")(princ) nil )
      ((< a 0) (princ "\nERROR @ DT:SubLst > a out of range\n")(princ) nil )
      ((>= b (length l)) (princ "\nERROR @ DT:SubLst > b out of range\n")(princ) nil )
    );END cond
  );END if

  ; v0.0 - 2017.01.29 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.01.29
)
(defun DT:SortByNumber ( lst / notNumber smallestValueIndex sortedList )
  ; Return lst elements in numerical ascendant order
  ; lst [lst] - List of positive and/or negative integer and/or reals
  (if lst
    (if (= 'list (type lst))
      (progn
        ; Check if there is any no-number on the list lst
        (foreach a lst
          (if (not (numberp a)) (setq notNumber T))
        );END foreach

        (if (not notNumber)
          (progn
            (while lst
              (setq
                ; Get the index of the smallest value out of the list
                smallestValueIndex (DT:GetSmallestIndex lst)
                ; Add smallest value to sortedList
                sortedList (append sortedList (list (nth smallestValueIndex lst)) )
                ; Split the list
                lst
                  (append
                    (DT:SubLst lst 0                          (+ smallestValueIndex -1) )
                    (DT:SubLst lst (+ smallestValueIndex +1)  (+ (length lst) -1)       )
                  );END append
              );END setq
            );END while

            ; Return sorted list
            sortedList
          );END progn
          (progn (princ "\nERROR @ DT:SortByNumber > lst contains no numbers\n")(princ) nil )
        );END if
      );END progn
      (progn (princ "\nERROR @ DT:SortByNumber > lst is not a list\n")(princ) nil )
    );END if
    (progn (princ "\nERROR @ DT:SortByNumber > lst = nil\n")(princ) nil )
  );END if

  ; v0.0 - 2017.01.29 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.01.29
)
(defun DT:GetBlockVLAObject ( blockName / return )
  ; Return VL object name of the block named "blockName", if any
  (if blockName
    (if (= 'str (type blockName))
      (progn
        (vlax-for blk (vla-get-Blocks (vla-get-ActiveDocument (vlax-get-acad-object)))
          ;(princ "\n(vla-get-Name blk) = ")(princ (vla-get-Name blk))
          (if (= blockName (vla-get-Name blk))
            (setq return blk)
          );END if
        );END vlax-for
      );END progn
      (progn (princ "\nERROR @ DT:ib > blockName is not a string\n") nil )
    );END if
    (progn (princ "\nERROR @ DT:ib > blockName = nil\n") nil )
  );END if

  return

  ; v0.0 - 2017.04.12 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.04.12
)
(defun DT:Arg ( functionName argumentList / argumentSymbol argumentType errorList err )
  ; Return T if the passed arguments are right, otherwise print error message and return nil
  ; functionName [SYM]  - Symbol of the function to check
  ; argumentList [LIST] - Non-evaluated list of argument symbols and argument types as bellow:
  ;                     - '( (argumentSymbol argumentType) (argumentSymbol argumentType) (argumentSymbol argumentType) )
  (if (and functionName argumentList)
    (if (and (= 'sym (type functionName)) (= 'list (type argumentList)))
      (progn
        (foreach argument argumentList
          ; Save argumentSymbol and argumentType
          (setq
            argumentSymbol (nth 0 argument)
            argumentType   (nth 1 argument)
          );END setq

          ; If argumentSymbol and argumentType values are different to nil
          (if (and (/= nil (eval argumentSymbol)) (/= nil (eval argumentType)))
            (progn
              ; If argument type is correct
              (if (= (eval argumentType) (type (eval argumentSymbol)))
                nil
                (setq errorList (append errorList (list (strcat "ERROR @ " (vl-symbol-name functionName) " : " (vl-symbol-name argumentSymbol) " is not a " (vl-symbol-name (eval argumentType))))))
              );END if
            );END progn
            (cond
              ((not (eval argumentSymbol)) (setq errorList (append errorList (list (strcat "ERROR @ " (vl-symbol-name functionName) " : " (vl-symbol-name argumentSymbol) "=nil")))))
              ((not (eval argumentType))   (setq errorList (append errorList (list (strcat "ERROR @ " (vl-symbol-name functionName) " : " (vl-symbol-name (eval argumentType)) "=nil")))))
            );END cond
          );END if
        );END foreach
      );END progn
      (cond
        ((/= 'str (type functionName))  (setq errorList (append errorList (list "ERROR @ DT:Arg : functionName is not a symbol"))))
        ((/= 'list (type argumentList)) (setq errorList (append errorList (list "ERROR @ DT:Arg : argumentList is not a list"))))
      );END cond
    );END if
    (cond
      ((not functionName) (setq errorList (append errorList (list "ERROR @ DT:Arg : functionName=nil"))))
      ((not argumentList) (setq errorList (append errorList (list "ERROR @ DT:Arg : argumentList=nil"))))
    );END cond
  );END if

  ; If any error message print them and return nil, otherwise return T
  (if errorList
    (progn
      (foreach err errorList
        (princ "\n")(princ err)
      );END foreach
      nil
    );END progn
    T
  );END if

  ; v0.2 - 2017.05.26 - Error message amended
  ; v0.1 - 2017.05.13 - Update variable names for clarity purporse
  ;                   - Update error printing system
  ; v0.0 - 2017.05.12 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.05.26
)
(defun DT:GetLwpolyPoints ( ent_name / alternator Xcoord pointList )
  ; Return a list with LWPOLYLINE coordinates
  (if (DT:Arg 'DT:GetLwpolyPoints '((ent_name 'ename)))
    (if (= "LWPOLYLINE" (cdr (assoc 0 (entget ent_name))))
      (progn
        (setq alternator T)
        (foreach a (vlax-safearray->list (vlax-variant-value (vlax-get-property (vlax-ename->vla-object ent_name) 'Coordinates)))
          (if alternator
            (progn ; X value
              (setq
                alternator nil
                Xcoord (list a)
              );END setq
            );END progn
            (progn ; Y value
              (setq
                alternator T
                pointList (append pointList (list (append Xcoord (list a))))
              );END setq
            );END progn
          );END if
        );END foreach

        ; Return coordinate list
        pointList

      );END progn
      (progn (princ "\nERROR @ DT:GetLwpolyPoints > ent_name is not LWPOLYLINE")(princ) nil)
    );END if
  );END if

  ; v0.0 - 2017.05.26 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.05.26
)
(defun DT:Spin ( ang0 ang1 )
  ; Return spin direction from ang0 to ang1
  ;  - 0 : clockwise
  ;  - 1 : anticlockwise
  ;  - 2 : inline
  ; or return nil if error
  (if (DT:Arg 'DT:Spin '((ang0 'real)(ang1 'real)))
    (if (> ang0 pi)
      (cond ; ang0 > pi
        ((> ang0 ang1) ; ANG0 > ang1
          ; ang1-ang0 < 0
          (if (> (- (+ ang1 (* 2 pi)) ang0) pi)
            0 ; right
            1 ; left  *
          );END if
        );END subcond
        ((< ang0 ang1) 1)                             ; left    ang0 < ANG1
        ((= ang0 ang1) 2)                             ; inline  ang0 = ang1
      );END cond
      (cond ; ang0 <= pi
        ((and (> ang1 ang0) (< ang1 (+ ang0 pi))) 1)  ; left    ANG0 > ang1  &&  ang1 > ang0+pi
        ((= ang0 ang1) 2)                             ; inline  ang0 = ang1
        (t 0)                                         ; right   ang0 < ANG1
      );END cond
    );END if
  );END if

  ; v0.0 - 2017.05.26 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.05.26
)
(defun DT:GetRelativeAngle ( ang0 ang1 / angleDifference spin return )
  ; Return relative angle between angles ang0 and ang1
  ;  - > 0  if clockwise anlge
  ;  - < 0  if anticlockwise angle
  (if (DT:Arg 'DT:GetRelativeAngle '((ang0 'real)(ang1 'real)))
    (progn
      (setq
        angleDifference (- ang1 ang0)
        spin (DT:Spin ang0 ang1)
      )
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
  ; Return absolute angles formed by points in pointList list
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
(defun DT:GetRelativeAngleList ( absoluteAngleList / i ang0 ang1 relativeAngle relativeAngleList )
  ; Return relative angles formed by absolute angles in absoluteAngleList list
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
