(defun c:1() (princ "\nGET MANHOLE DATA ACCORDING TO CENTRELINE: \n") (DT:ExtractManholeDataAlongCentrelines) )
(defun c:2() (princ "\nDRAW MANHOLES ONTO LONGITUDINAL SECTION: \n") (DT:DrawExtractedManholesOnLongSection) )
(defun DT:DrawExtractedManholesOnLongSection ( / ent_name startPoint datum verticalExageration)
  ; Draw manholes stored in globalVariableManholesData on a longitudinal section

  ; SAVE SETTINGS
  (save_environment (list "osmode" "attdia" "attreq"))

  ; CHANGE SETTINGS
  (setvar "osmode" 0)
  (setvar "attdia" 0)
  (setvar "attreq" 1)

  (setq
     layFoul  "e-afd-PH2"
     layStorm "e-asd-PH2"
  )
  ; INPUT - Ask user datum and verticalExageration
  (while (not ent_name)
    (setq ent_name (car (entsel "\nSelect datum line: ")))
  );END while
  (if ent_name
    (if (setq startPoint (cdr (assoc 10 (entget ent_name))))
      (progn
        (while (not datum)
          (setq datum (getreal "\nIntroduce datum: "))
        );END while
        (while (not verticalExageration)
          (setq verticalExageration (getreal "\nVertical exageration: "))
        );END while

        ; OPERATION - Execute the function
        (DT:DrawExtractedManholes globalVariableManholesData startPoint datum verticalExageration layFoul layStorm )
      );END progn
      (progn (princ "\nNo startPoint introduced.")(princ))
    );END if
  );END if


  ; RESTORE SETTINGS
  (restore_environment)

  (princ)

  ; v0.1 - 2017.01.25 - Function full restructuration
  ; v0.0 - 2016.08.08 - First issue
  ; Author: David Torralba
  ; Last revision: 2016.08.08
)
(defun DT:PK( VL_ent_name pt )
  (vlax-curve-getDistAtPoint VL_ent_name (vlax-curve-getClosestPointTo VL_ent_name pt))
)
(defun DT:GetManholeDatabaseForLongSection ( centrelineEnt_name
                                        /
                                        ss
                                        manholeDatabase manholeData manholeChainage manholeChainageList
                                        manholeDatabaseSorted manholeChainageListSorted
                                      )
  ; Returns a sorted list (by chainage) with the selected manhole data
  ;
  ; INPUT - Ask user to select manholes
  (setq
    ss (ssget '((0 . "INSERT")))
    manholeDatabase nil
  )
  (foreach a (ssnamex ss)
    (if (= 'ename (type (cadr a)))
      (progn
        (if (setq manholeData (DT:GetManholeDataForLongSection (cadr a)) )
          (progn
            (setq
              manholeDatabase (append manholeDatabase (list manholeData))
              manholeChainage (car manholeData)
              manholeChainageList (append manholeChainageList (list manholeChainage ))
            );END setq
          );END progn
        );END if
      );END progn
    );END if1
  );END foreach

  ; OPERATION - Return list of manhole chainages in correct order
  (setq manholeChainageListSorted (DT:SortByNumber manholeChainageList))

  ; OPERATION - Rebuild data lists shorted by chainages
  (foreach ch manholeChainageListSorted
    (cond
      ((/= "" ch)
        (setq
          ; get position of the chainage at manholeChainageList (non-sorted list of chainages)
          position (vl-position ch manholeChainageList)
          ; copy data associated to the current chainage to manholeDatabaseSorted
          manholeDatabaseSorted (append manholeDatabaseSorted (list (nth position manholeDatabase)))
        );END setq
      );END subcond
    );END cond
  );END foreach

  ; v0.2 - 2107.01.29 - Tidy up comments
  ;                   - Add layer to de manholeData
  ;                   - Split code into simpler functions
  ;                   - Wrap long section manhole entities in separate groups
  ; v0.1 - 2017.01.25 - Function fully rewritten
  ; v0.0 - 2016.08.08 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.01.29
)
(defun DT:GetManholeDataForLongSection ( manholeEnt_name )
  ; Return manhole data list as so:
  ; (list
  ;   Manhole Block Chainage            = (nth 0 man_data)
  ;   Manhole Block "ID" attribute      = (nth 1 man_data)
  ;   Manhole Block "IL1" attribute     = (nth 2 man_data)
  ;   Manhole Block "IL2" attribute     = (nth 3 man_data)
  ;   Manhole Block "IL3" attribute     = (nth 4 man_data)
  ;   Manhole Block "IL4" attribute     = (nth 5 man_data)
  ;   Manhole Block "visibility state"  = (nth 6 man_data)   para tener el tamano de la arqueta (DN1200, etc.)
  ;   Manhole Block "manhole size"      = (nth 7 man_data)
  ;   Manhole Block layer               = (nth 8 man_data)  para luego poder dibujarlo en la misma capa en el long section
  ; );END list

  (if manholeEnt_name
    (if (= "INSERT" (cdr (assoc 0 (entget manholeEnt_name))))
      ; Check if is a Manhole Block
      (if (= "W-Manhole" (substr (LM:effectivename (vlax-ename->vla-object manholeEnt_name)) 2 9))
        ; Return manhole data
        (list
          (DT:PK (vlax-ename->vla-object centrelineEnt_name) (cdr (assoc 10 (entget (cadr a)))))        ; Get manhole chainage
          (LM:vl-getattributevalue (vlax-ename->vla-object (cadr a)) "ID")                ; Extract Manhole Block "ID" attribute
          (LM:vl-getattributevalue (vlax-ename->vla-object (cadr a)) "IL1")               ; Extract Manhole Block "IL1" attribute
          (LM:vl-getattributevalue (vlax-ename->vla-object (cadr a)) "IL2")               ; Extract Manhole Block "IL2" attribute
          (LM:vl-getattributevalue (vlax-ename->vla-object (cadr a)) "IL3")               ; Extract Manhole Block "IL3" attribute
          (LM:vl-getattributevalue (vlax-ename->vla-object (cadr a)) "IL4")               ; Extract Manhole Block "IL4" attribute
          (LM:getvisibilitystate (vlax-ename->vla-object (cadr a)))                       ; Extract Manhole Block "visibility state"
          (cdr (assoc "Manhole size" (LM:getdynprops (vlax-ename->vla-object (cadr a))))) ; Extract Manhole Block "Manhole size"
          (cdr (assoc 8 (entget (cadr a))))                                               ; Extract Manhole Block layer
        );END list
        (progn (princ "\nERROR @ DT:GetManholeDataForLongSection > manholeEnt_name is not a manhole block\n")(princ) nil )
      );END if
      (progn (princ "\nERROR @ DT:GetManholeDataForLongSection > manholeEnt_name is not a block\n")(princ) nil )
    );END if
    (progn (princ "\nERROR @ DT:GetManholeDataForLongSection > manholeEnt_name = nil\n")(princ) nil )
  );END if

  ; v0.0 - 2017.01.29 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.01.29
)
(defun DT:ExtractManholeDataAlongCentrelines ( / centrelineEnt_name ans )
  ; ALMOST FINISHED - Purpose: store the chainages of selected manholes, to later draw them in the long section with another function
  ; PRECAUTION - If any manhole is not within the line, the routine will assign them the chainage at the beginning or end of the centreline.
  ;
  ; INPUT - Ask user to select a centreline
  (while (not centrelineEnt_name)
    (if (not (setq centrelineEnt_name (car (entsel "\nSelect centreline: "))))
      (princ "missed. Try again.")
      (progn
        (if
          (or
            (= "POLYLINE"   (cdr (assoc 0 (entget centrelineEnt_name))))
            (= "LWPOLYLINE" (cdr (assoc 0 (entget centrelineEnt_name))))
          );END or
          (princ "object selected.")
          (progn
            (princ "selected object is not a polyline. Try again.")
            (setq centrelineEnt_name nil)
          );END progn
        );END if
      );END progn
    ); END if
  );END while

  ; OPERATION - Check if polyline looks like a centreline
  (if (/= "e-centreline" (cdr (assoc 8 (entget centrelineEnt_name))))
    (progn
      (initget "Yes No")
      (setq ans (getkword "\nSelected object is not on layer \"e-centreline\".\nAre you sure you want to use this object as centreline? [Yes/No]: <No>"))
      (if (not ans) (setq ans "No"))
      (if (= ans "Yes")
        (setq *manholeDatabaseForLongitudinalSection* (DT:GetManholeDatabaseForLongSection centrelineEnt_name))
      );END if
    );END progn
    ; Save globally manhole database
    (setq *manholeDatabaseForLongitudinalSection* (DT:GetManholeDatabaseForLongSection centrelineEnt_name))
  );END if

  ; v0.1 - 2017.01.29 - Centreline selection warning message fixed
  ;                   - Code tidy up
  ; v0.0 - 2017.01.25 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.01.29
)
(defun DT:DrawExtractedManholes ( globalVariableManholesData startPoint datum verticalExageration layFoul layStorm )
  (if *manholeDatabaseForLongitudinalSection*
    (progn
      (foreach manholeData *manholeDatabaseForLongitudinalSection*
        (DT:DrawManhole manholeData startPoint datum verticalExageration layFoul layStorm )
      );END foreach
    );END progn
    (progn (princ "\nERROR @ DT:DrawExtractedManholes > *manholeDatabaseForLongitudinalSection* = nil")(princ))
  );END if

  ; v0.0 - 2017.01.25 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.01.25
)
(defun DT:DrawInvertLevelMark ( c lay )
  (entmakex
    (list
      (cons 0 "CIRCLE")
      (cons 8 lay)
      (cons 10 c)
      (cons 40 0.3)
    );END list
  );END entmakex

  ; v0.1 - 2017.01.29 - Pass layer as argument
  ; v0.0 - 2017.01.25 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.01.29
)
(defun DT:DrawStormInvertLevelBoxLine ( pBase lay )
  (entmakex
    (list
      (cons 0 "LINE")
      (if (tblsearch "LTYPE" "CONTINUOUS") (cons 6 "CONTINUOUS"))  ; Linetype CONTINUOUS, if possible
      (cons 8 lay)
      (cons 10 (polar pBase (* 1.5 pi) 42) )
      (cons 11 (polar pBase (* 1.5 pi) 48) )
      (cons 62 256)
    );END list
  );END entmakex

  ; v0.1 - 2017.03.02 - Set CONTINUOUS linetype, if possible
  ;                   - Set color ByLayer to avoid problems with current color settings
  ; v0.0 - 2017.01.25 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.03.02
)
(defun DT:DrawFoulInvertLevelBoxLine ( pBase lay )
  (entmakex
    (list
      (cons 0 "LINE")
      (if (tblsearch "LTYPE" "CONTINUOUS") (cons 6 "CONTINUOUS"))  ; Linetype CONTINUOUS, if possible
      (cons 8 lay)
      (cons 10 (polar pBase (* 1.5 pi) 48) )
      (cons 11 (polar pBase (* 1.5 pi) 54) )
      (cons 62 256)
    );END list
  );END entmakex

  ; v0.1 - 2017.03.02 - Set CONTINUOUS linetype, if possible
  ;                   - Set color ByLayer to avoid problems with current color settings
  ; v0.0 - 2017.01.25 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.03.02
)
(defun DT:DrawInverLevelText (pt lay str)
  (entmakex
    (list
      (cons 0 "TEXT")
      (if (tblsearch "style" "ROMANS") (cons 7 "ROMANS"))
      (cons 8 lay)
      (cons 10 (polar pt (* 1.5 pi) 2.2))
      (cons 11 pt) ; needed for text justification
      (cons 40 0.84 )
      (cons 1  str)
      (cons 62 7) ; white color
      (cons 50 (* 0.5 pi))
      (cons 71 0) ; needed for text justification
      (cons 72 1) ; needed for text justification
    );END list
  );END entmakex

  ; v0.1 - 2017.03.02 - Align text to center
  ; v0.0 - 2017.01.25 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.03.02
)
(defun DT:DrawManholeInvertLevelTexts ( p0 lay IL1 IL2 IL3 IL4 / p0 ent_nameList )
  ; Return list with entity name of all created entities
  ; p0 [pt]    - Point of reference to calculate texts positions (datum)
  ; lay [str]  - layer where to draw invert levels
  ; IL1 [real] - Invert level 1 value
  ; IL2 [real] - Invert level 2 value
  ; IL3 [real] - Invert level 3 value
  ; IL4 [real] - Invert level 4 value
  (if (< 0 (atof IL1)) (setq ent_nameList (append ent_nameList (list (DT:DrawInverLevelText (polar p0 pi 0.4) lay IL1) ))) )
  (if (< 0 (atof IL2)) (setq ent_nameList (append ent_nameList (list (DT:DrawInverLevelText (polar p0 0 1.25) lay IL2) ))) )
  (if (< 0 (atof IL3)) (setq ent_nameList (append ent_nameList (list (DT:DrawInverLevelText (polar p0 0 2.50) lay IL3) ))) )
  (if (< 0 (atof IL4)) (setq ent_nameList (append ent_nameList (list (DT:DrawInverLevelText (polar p0 0 3.75) lay IL4) ))) )

  ; Return drawn entity name list
  ent_nameList

  ; v0.1 - 2017.01.29 - Return list with drawn invert tevel text entity names
  ; v0.0 - 2017.01.25 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.01.29
)
(defun DT:DrawVerticalAxis ( p1 p2 lay )
  (entmakex
    (list
      (cons 0 "LINE")
      (cons 8 lay)
      (cons 10 p1)
      (cons 11 p2)
      (if (tblsearch "LTYPE" "CENTER") (cons 6 "CENTER"))  ; Linetype CENTER, if possible
      (cons 48 0.5) ; Linetype scale
      (cons 62 7) ; white color
    );END list
  );END entmakex

  ; v0.1 - 2017.03.02 - Correct linetype scale
  ; v0.0 - 2017.01.25 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.03.02
)
(defun DT:WriteVerticalAxisLabel (pt lay manholeName)
  (entmakex
    (list
      (cons 0 "TEXT")
      (if (tblsearch "style" "ROMANS") (cons 7 "ROMANS"))
      (cons 8 lay)
      (cons 10 (polar pt (* 1.5 pi) 2.2))
      (cons 11 pt) ; needed for text justification
      (cons 40 1.260 )
      (cons 1 manholeName)
      (cons 62 2) ; yellow color
      (cons 50 (* 0.5 pi))
      (cons 71 0) ; needed for text justification
      (cons 72 0) ; needed for text justification
      (cons 73 2) ; needed for text justification
    );END list
  );END entmakex

  ; v0.0 - 2017.01.25 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.01.25
)
(defun DT:MarkInvertLevel ( pBase ILs datum verticalExageration lay / ent_nameList )
  (foreach a ILs
    (if (> a 0)
      (setq ent_nameList (append ent_nameList (list
        (DT:DrawInvertLevelMark (polar pBase (* 0.5 pi) (* (- a datum) verticalExageration)) lay )
      )))
    );END if
  );END foreach

  ; Return drawn entity name list
  ent_nameList

  ; v0.1 - 2017.01.29 - Return list with drawn invert tevel makk entity names
  ; v0.0 - 2017.01.25 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.01.25
)
(defun DT:GetILFromManholeData ( manholeData )
  ; Return a list with ILs from manholeData in real format
  (if manholeData
    (list
      (atof (nth 2 manholeData))
      (atof (nth 3 manholeData))
      (atof (nth 4 manholeData))
      (atof (nth 5 manholeData))
    );END list
  );END if

  ; v0.0 - 2017.01.25 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.01.25
)
(defun DT:DrawManhole ( manholeData startPoint datum verticalExageration layFoul layStorm / pBase lay )
  ; Draw manhole data on the long section
  ; Return list of drawn entities if successful
  ; Return nil if something wrong
  (if (and manholeData datum startPoint)
    (if (= 'list (type manholeData))
      (if (= 9 (length manholeData))
        (if (or (= (substr (nth 1 manholeData) 1 1) "S") (= (substr (nth 1 manholeData) 1 1) "F") )
          (progn
            (setq
              ; Get manhole position in long section according to its chainage
              pBase (polar startPoint 0 (nth 0 manholeData))
              ; Get manhole layer
              lay (nth 8 manholeData)
            )
            ; Draw manhole data on long section
            (cond
              ((= (substr (nth 1 manholeData) 1 1) "S")
                ; Return all drawn entity names, and append them to later wrap them in a group (name the group with the manhole name)
                (DT:SetGroup
                  (append
                    (list (DT:DrawStormInvertLevelBoxLine pBase lay))
                    (DT:DrawManholeInvertLevelTexts (polar pBase (* 1.5 pi) 45) lay (nth 2 manholeData) (nth 3 manholeData) (nth 4 manholeData) (nth 5 manholeData))
                    (list
                      (DT:DrawVerticalAxis pBase (polar pBase (* 0.5 pi) 110) lay)
                      (DT:WriteVerticalAxisLabel (polar pBase (* 0.5 pi) 110.5) lay (nth 1 manholeData))
                    );END list
                    (DT:MarkInvertLevel pBase (DT:GetILFromManholeData manholeData) datum verticalExageration lay)
                  );END append
                );END DT:SetGroup
              );END subcond
              ((= (substr (nth 1 manholeData) 1 1) "F")
                (DT:SetGroup
                  (append
                    (list
                      (DT:DrawFoulInvertLevelBoxLine pBase lay)
                    );END list
                    (DT:DrawManholeInvertLevelTexts (polar pBase (* 1.5 pi) 51) lay (nth 2 manholeData) (nth 3 manholeData) (nth 4 manholeData) (nth 5 manholeData))
                    (list
                      (DT:DrawVerticalAxis pBase (polar pBase (* 0.5 pi) 110) lay)
                      (DT:WriteVerticalAxisLabel (polar pBase (* 0.5 pi) 110.5) lay (nth 1 manholeData))
                    );END list
                    (DT:MarkInvertLevel pBase (DT:GetILFromManholeData manholeData) datum verticalExageration lay)
                  );END append
                );END DT:SetGroup
              );END subcond
            );END cond
          );END progn
          (progn (princ "\nERROR @ DT:DrawManhole > manhole name doesn't start with \"S\" or \"F\"")(princ) nil )
        );END if
        (progn (princ "\nERROR @ DT:DrawManhole > provided manholeData list has wrong number of items")(princ) nil )
      );END if
      (progn (princ "\nERROR @ DT:DrawManhole > provided manholeData has wrong data type")(princ) nil )
    );END if
    (progn (princ "\nERROR @ DT:DrawManhole > manholeData/datum/startPoint = nil")(princ) nil )
  );END if

  ; v0.0 - 2017.01.25 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.01.25
)
