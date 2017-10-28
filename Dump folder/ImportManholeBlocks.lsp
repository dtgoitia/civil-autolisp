; (defun c:xx ()
;   ; Trigger
;   (DT:AutoLoadFileFromCivilTemp "ImportManholeBlocks.lsp")
;   (c:ImportManholeBlock)
;
;   ; v0.0 - 2017.07.28 - First issue
;   ; Author: David Torralba
;   ; Last revision: 2017.07.28
; )
(DT:AutoLoadFileFromCivilTemp "SplitString.lsp")
(DT:AutoLoadFileFromCivilTemp "LmAttributeFunctions.lsp")
(defun c:ImportManholeBlock ( / attributeList blockName blockLayer )
  ; This program was written to pass non standard manholes (with ID, CL and ILs in a MTEXT) to standard mahole block

  (if (setq attributeList (DT:ParseSewerBoxString (DT:GetText (car (entsel "\nSelect label: ")))))
    (progn
      ; Select foul or storm settings
      (cond
        ((= "F" (substr (nth 0 attributeList) 1 1) )
          (setq blockName   "FW-Manhole")
          (setq blockLayer  "e-afd")
        );END subcond
        ((= "S" (substr (nth 0 attributeList) 1 1) )
          (setq blockName   "SW-Manhole")
          (setq blockLayer  "e-asd")
        );END subcond
        ; If not storm or foul, abort function
        (t (exit) )
      );END cond

      ; Insert block
      (setvar "clayer" blockLayer)
      (setvar "osmode" 4)

      ; Insert block with default values
      (command "-insert" blockName pause 1 1 0)(while (> (getvar "CMDACTIVE") 0) (command ""))

      ; Format correctly the manhole box properties
      (LM:SetDynProps (vlax-ename->vla-object (entlast))
        (append
          (list
            (cons "IL amount" (- (length attributeList) 2))
            (cons "Manhole size" "DN1200")
          );END list
          (if (> (- (length attributeList) 2) 1)
            (list (cons "Box width" 5.0))
          );END if
        );END append
      )

      ; Pass attributes to manhole box
      (LM:vl-setattributevalues (vlax-ename->vla-object (entlast))
        (list
          (cons "ID" (nth 0 attributeList))
          (cons "CL" (nth 1 attributeList))
          (cons "IL1" (nth 2 attributeList))
          (cons "IL2" (nth 3 attributeList))
          (cons "IL3" (nth 4 attributeList))
          (cons "IL4" (nth 5 attributeList))
        );END list
      )
    );END progn
  );END if

  ; v0.1 - 2017.07.13 - Block insertion instruction amended, it was inserting only "SW-Manhole" blocks
  ; v0.0 - 2017.06.29 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.07.13
)
(defun DT:ParseSewerBoxString ( sewerBoxString / manholeIlListString manholeIlList idPosition manholeId patternCl clPosition manholeCl manholeClString patternIl ilPosition manholeIl manholeClString)
  ; Parse sewer box text and return a list with the attribute values
  (if (DT:Arg 'DT:ParseSewerBoxString '((sewerBoxString 'str)))
    (progn
      ; Get manhole name: (first line)
      (setq idPosition (vl-string-search "\\P" sewerBoxString))
      (setq manholeId (substr sewerBoxString 1 idPosition) )

      ; Get manhole CL: get first number on the right of CL
      (setq patternCl "CL ")
      (setq clPosition (vl-string-search patternCl sewerBoxString))
      (setq manholeCl (substr sewerBoxString (+ clPosition (strlen patternCl) 1)) )
      (setq manholeClString (LM:rtos (atof manholeCl) 2 3))

      ; Get manhole IL: get first number on the right of IL
      (setq patternIl "IL ")
      (setq ilPosition (vl-string-search patternIl sewerBoxString))
      (setq manholeIl (substr sewerBoxString (+ ilPosition (strlen patternIl) 1)) )
      (setq manholeIlString (LM:rtos (atof manholeIl) 2 3))

      (setq manholeIlListString (DT:SplitString (substr sewerBoxString (+ ilPosition (strlen patternIl) 1)) "\\PIL "))
      (setq
        manholeIlList
        (mapcar
          '(lambda (il) (LM:rtos (atof il) 2 3) )
          manholeIlListString
        );END mapcar
      );END setq

      ; Return values
      (append
        (list manholeId manholeClString)
        manholeIlList
      );END append
    );END progn
  );END if

  ; v0.0 - 2017.06.29 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.06.29
)
