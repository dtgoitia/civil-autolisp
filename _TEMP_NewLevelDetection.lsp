(defun c:1()
  ;(DT:ParseLevelString (vlax-get-property (vlax-ename->vla-object (car (entsel))) 'TextString ) )
);END defun
(defun DT:ParseLevelString ( levelString )
  (if levelString
    (if (= 'str (type levelString))
      (progn
        (princ "\nlevelString = ")(princ levelString)(princ "\n")
        (cond
          ; Real number: adoptable manholes, PI_DAVID block
          ( (and (< (strlen levelString) 8) (> (strlen levelString) 4) (/= "S" (substr levelString 1 1)) (/= "F" (substr levelString 1 1)))
            (list 1 (atof levelString))
          )
          ; FFL
          ( (and (= "FFL " (substr levelString 1 4)) (= 4 (- (strlen levelString) (vl-string-search "." levelString))) )
            (list 2 (atof (substr levelString 5)))
          )
          ; Road level: underlined and 3 decimals
          ( (and (or (= "%%U" (substr levelString 1 3)) (= "%%u" (substr levelString 1 3))) (= 3 (- (strlen levelString) (vl-string-search "." levelString))))
            (list 3 (atof (substr levelString 4 10)))
          )
          ; Plot level: underlined and 2 decimals
          ( (and (or (= "%%U" (substr levelString 1 3)) (= "%%u" (substr levelString 1 3))) (= 4 (- (strlen levelString) (vl-string-search "." levelString))))
            (list 4 (atof (substr levelString 4 9)))
          )
          ; Private Foul Mahole: start with "F" and 2 decimals
          ( (and
              (= "F" (substr levelString 1 1))
              (and (>= (ascii (substr levelString 2 1)) 48) (<= (ascii (substr levelString 2 1)) 57))
              (> (strlen levelString) 5)
            )
            (list 5 (atof (substr levelString 2)))
          )
          ; Private Storm Mahole: start with "S" and 2 decimals
          ( (and
              (= "S" (substr levelString 1 1))
              (and (>= (ascii (substr levelString 2 1)) 48) (<= (ascii (substr levelString 2 1)) 57))
              (> (strlen levelString) 5)
            )
            (list 6 (atof (substr levelString 2)))
          )
          ; Sub-base level: start with "SB" and 2 decimals
          ( (and
              (or (= "SB" (substr levelString 1 2)) (= "sb" (substr levelString 1 2)) )
              (= 3 (- (strlen levelString) (vl-string-search "." levelString)) )
            )
            (list 7 (atof (substr levelString 3 8)))
          )
          ; Other
          (t
            (list 0)
          )
        );END cond
      );END progn
      (progn (princ "\nERROR @ DT:ParseLevel : levelString is not a string\n") (princ) )
    );END if
    (progn (princ "\nERROR @ DT:ParseLevel : levelString=nil\n") (princ) )
  );END if
)
(defun DT:FormatLevelToText ( levelFloat class )
  ; Return a string with the levelFloat number formated according to the specified class
  ; 1 - Real number
  ; 2 - FFL
  ; 3 - Road level
  ; 4 - Plot level
  ; 5 - Foul private manhole level
  ; 6 - Storm private manhole level
  ; 7 - Subbase level
  (if (and levelFloat class )
    (if (= 'real (type levelFloat))
      (progn
        (princ "\nlevelFloat = ")(princ levelFloat)(princ "\n")
        ()
      );END progn
      (cond
        ((/= 'real (type levelFloat))
          (progn (princ "\nERROR @ DT:FormatLevelToText : levelFloat is not a real\n") (princ) )
        );END subcond
        ((and (/= 'int (type class)) (if (= 'int (type class))  (> class 0)))
          (progn (princ "\nERROR @ DT:FormatLevelToText : levelFloat is not an integer\n") (princ) )
        );END subcond
      );END cond
    );END if
    (cond
      ((not levelFloat) (progn (princ "\nERROR @ DT:FormatLevelToText : levelFloat=nil\n") (princ) ))
      ((not class     ) (progn (princ "\nERROR @ DT:FormatLevelToText : class=nil\n") (princ) ))
    );END cond
  );END if
)
