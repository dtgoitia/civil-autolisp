(defun c:xx ()
  ; Trigger
  (DT:AutoLoadFileFromCivilTemp "LevelDetection.lsp")
  (DT:SelectOrTypeLevel nil)

  ; v0.0 - 2017.08.14 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.14
)
(defun DT:SelectOrTypeLevel ( silentOption / input nename content )
  ; Return real number if sucessful, otherwise return nil
  ; silentOption [bool] - If T: no messages will be prompted
  ;                     - If nil: messages will be prompted

  (setq input (DT:input_string_or_point))
  (cond
    ; If point input:
    ((= 'LIST (type input))
      (if (setq nename (car (nentselp input)))
        (if (setq content (DT:GetText nename))
          (if (setq level (DT:ParseLevelString content))
            (cadr level) ; return level as a real number
          );END if
          (progn
            (if (not silentOption) (princ "no text found") )
            nil ; return nil
          ); END progn
        ); END if
        (progn ; if no entity selected
          (if (not silentOption) (princ "nothing selected") )
          nil ; return nil
        );END progn
      ); END if
    )
    ; If string input:
    ((= 'STR (type input))
      input
    )
  )

  ; v0.0 - 2017.08.14 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.14
)
(defun DT:ParseLevelString ( levelString )
  (if levelString
    (if (= 'str (type levelString))
      (progn
        (cond
          ; Real number: adoptable manholes, PI_DAVID block
          ( (and (< (strlen levelString) 8) (> (strlen levelString) 4) (/= "S" (substr levelString 1 1)) (/= "F" (substr levelString 1 1)))
            (list 1 (atof levelString))
          )
          ( (and
              (= "FFL" (substr levelString 1 3))                                ; starts with "FFL"
              (= 4 (- (strlen levelString) (vl-string-search "." levelString))) ; has 3 decimals ("." is in 4th position from the right)
            );END and
            (list 2 (atof (substr levelString 4)))
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

  ; v0.1 - 2017.07.27 - Update FFL detection case to accept optional spaces between "FFL" and the numbers
  ; v0.0 - 2016.??.?? - First issue
  ; Author: David Torralba
  ; Last revision: 2017.07.27
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
