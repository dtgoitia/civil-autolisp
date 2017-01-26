(defun c:1() (DT:ExtarctManholeChainages))
(defun c:2 ( / old_error old_sysvars ent_name startPoint datum verticalExageration)
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

  ; v0.0 - 2017.01.25 - Function full restructuration
  ; v0.0 - 2016.08.08 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.01.25
)
(defun DT:PK( VL_ent_name pt )
  (vlax-curve-getDistAtPoint VL_ent_name (vlax-curve-getClosestPointTo VL_ent_name pt))
)
(defun DT:SelectManholes( cntl
                        /
                        ss
                        man_db man_entry man_ch_list
                        sorted_man_db sorted_man_ch_list
                        pt ch id il1 il2 il3 il4 vis_sta
                        )
  ; Returns a sorted list (by chaiange) with the selected manhole data
  ;
  ; INPUT - Ask user to select manholes
  (setq
    ss (ssget '((0 . "INSERT")))
    man_db nil
  )
  (foreach a (ssnamex ss)
    (if (= 'ename (type (cadr a)))
      (progn
        ; Check if is a Manhole Block
        (if (= "W-Manhole" (substr (LM:effectivename (vlax-ename->vla-object (cadr a))) 2 9))
          (progn
            (setq
              man_entry (list
                          (DT:PK (vlax-ename->vla-object cntl) (cdr (assoc 10 (entget (cadr a)))))        ; Get manhole chainage
                          (LM:vl-getattributevalue (vlax-ename->vla-object (cadr a)) "ID")                ; Extract Manhole Block "ID" attribute
                          (LM:vl-getattributevalue (vlax-ename->vla-object (cadr a)) "IL1")               ; Extract Manhole Block "IL1" attribute
                          (LM:vl-getattributevalue (vlax-ename->vla-object (cadr a)) "IL2")               ; Extract Manhole Block "IL2" attribute
                          (LM:vl-getattributevalue (vlax-ename->vla-object (cadr a)) "IL3")               ; Extract Manhole Block "IL3" attribute
                          (LM:vl-getattributevalue (vlax-ename->vla-object (cadr a)) "IL4")               ; Extract Manhole Block "IL4" attribute
                          (LM:getvisibilitystate (vlax-ename->vla-object (cadr a)))                       ; Extract Manhole Block "visibility state"
                          (cdr (assoc "Manhole size" (LM:getdynprops (vlax-ename->vla-object (cadr a))))) ; Extract Manhole Block "Manhole size"
                        )

              man_db (append man_db (list man_entry))
              man_ch (LM:rtos (car man_entry) 2 5)
            );END setq
            ;(princ "\nman_db = ")(princ man_db)
            ; Manhole Block Chainage            = (nth 0 man_data)
            ; Manhole Block "ID" attribute      = (nth 1 man_data)
            ; Manhole Block "IL1" attribute     = (nth 2 man_data)
            ; Manhole Block "IL2" attribute     = (nth 3 man_data)
            ; Manhole Block "IL3" attribute     = (nth 4 man_data)
            ; Manhole Block "IL4" attribute     = (nth 5 man_data)
            ; Manhole Block "visibility state"  = (nth 6 man_data)   para tener el tamano de la arqueta (DN1200, etc.)
            ; Manhole Block "manhole size"      = (nth 7 man_data)

            ; OPERATION - Add zeros till have minimum 5 digits before dot "." to be able to sort them as strings later
            (cond
              ((= 1 (vl-string-search "." man_ch)) (setq man_ch (strcat "0000" man_ch)) )
              ((= 2 (vl-string-search "." man_ch)) (setq man_ch (strcat "000"  man_ch)) )
              ((= 3 (vl-string-search "." man_ch)) (setq man_ch (strcat "00"   man_ch)) )
              ((= 4 (vl-string-search "." man_ch)) (setq man_ch (strcat "0"    man_ch)) )
            );END cond

            ; OPERATION - Create list of chainages as strings
            (setq man_ch_list (append man_ch_list (list man_ch )) )
          );END progn
        );END if
      );END progn
    );END if1
  );END foreach

  ; OPERATION - Return list of manholes in correct order
  (setq sorted_man_ch_list (acad_strlsort man_ch_list))

  ; OPERATION - Rebuild data lists shorted by chainages
  (foreach ch sorted_man_ch_list
    (cond
      ((/= "" ch)
        (setq
          position (vl-position ch man_ch_list) 		                        ; get position of the chainage at man_ch_list (non-sorted list of chainages)
          sorted_man_db (append sorted_man_db (list (nth position man_db)))	; copy data associated to the current chainage to sorted_man_db
        );END setq
      )
    );END cond
  );END foreach
  ;(princ "\nsorted_man_db = ")(princ sorted_man_db)
)
(defun DT:ExtarctManholeChainages ( / cntl ans )

  ; ALMOST FINISHED - Purpose: store the chainages of selected manholes, to later draw them in the long section with another function
  ; PRECAUTION - If any manhole is not within the line, the routine will assign them the chainage at the beginning or end of the centreline.
  ;
  ; INPUT - Ask user to select a centreline
  (while (not cntl)
    (if (not (setq cntl (car (entsel "\nSelect centreline: "))))
      (princ "missed. Try again.")
      (progn
        (if
          (or
            (= "POLYLINE"   (cdr (assoc 0 (entget cntl))))
            (= "LWPOLYLINE" (cdr (assoc 0 (entget cntl))))
          );END or
          (princ "object selected.")
          (progn
            (princ "selected object is not a polyline. Try again.")
            (setq cntl nil)
          );END progn
        );END if
      );END progn
    ); END if
  );END while

  ; OPERATION - Check if polyline looks like a centreline
  (if (/= "e-centreline" (cdr (assoc 8 (entget cntl))))
    (progn
      (initget "Yes No")
      (setq ans (getkword "\nSelected object is not on layer \"e-centreline\".\nAre you sure you want to use this object as centreline? <Yes/No>: [No]"))
      (if (not ans) (setq ans "No"))
      (if (= ans "Yes")
        (setq global_variable_manhole_db (DT:SelectManholes cntl))
      );END if
    );END progn
    (progn
      (setq global_variable_manhole_db (DT:SelectManholes cntl))
    )
  );END if
  ;(princ)
)
(defun DT:DrawExtractedManholes ( globalVariableManholesData startPoint datum verticalExageration layFoul layStorm )
  (if global_variable_manhole_db
    (progn
      (foreach manholeData global_variable_manhole_db
        (DT:DrawManhole manholeData startPoint datum verticalExageration layFoul layStorm )
      );END foreach
    );END progn
    (progn (princ "\nERROR @ DT:DrawExtractedManholes > global_variable_manhole_db = nil")(princ))
  );END if
)
(defun DT:DrawInvertLevelMark ( c )
  (entmakex (list (cons 0 "CIRCLE")(cons 8 "0")(cons 10 c)(cons 40 0.3)))
)
(defun DT:DrawStormInvertLevelBoxLine ( pBase lay )
  (entmakex (list
    (cons 0 "LINE")
    (cons 8 lay)
    (cons 10 (polar pBase (* 1.5 pi) 42) )
    (cons 11 (polar pBase (* 1.5 pi) 48))))
)
(defun DT:DrawFoulInvertLevelBoxLine ( pBase lay )
  (entmakex (list
    (cons 0 "LINE")
    (cons 8 lay)
    (cons 10 (polar pBase (* 1.5 pi) 48) )
    (cons 11 (polar pBase (* 1.5 pi) 54))))
)
(defun DT:InverLevelText (pt lay str)
  (entmakex (list
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
    (cons 72 0) ; needed for text justification
  ))
)
(defun DT:WriteInvertLevels ( p0 lay IL1 IL2 IL3 IL4 / p0 )
  (if (< 0 (atof IL1)) (DT:InverLevelText (polar p0 pi 0.4) lay IL1) )
  (if (< 0 (atof IL2)) (DT:InverLevelText (polar p0 0 1.25) lay IL2) )
  (if (< 0 (atof IL3)) (DT:InverLevelText (polar p0 0 2.50) lay IL3) )
  (if (< 0 (atof IL4)) (DT:InverLevelText (polar p0 0 3.75) lay IL4) )
)
(defun DT:DrawVerticalAxis ( p1 p2 lay )
  (entmakex (list
    (cons 0 "LINE")
    (cons 8 lay)
    (cons 10 p1)
    (cons 11 p2)
    (if (tblsearch "LTYPE" "CENTER") (cons 6 "CENTER"))  ; Line type CENTER, if possible
    (cons 48 0.06) ; Line type scale
    (cons 62 7) ; white color
  ))
)
(defun DT:WriteVerticalAxisLabel (pt lay manholeName)
  (entmakex (list
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
    ))
)
(defun DT:MarkInvertLevel ( pBase ILs datum verticalExageration)
  (foreach a ILs
    (if (> a 0)
      (DT:DrawInvertLevelMark (polar pBase (* 0.5 pi) (* (- a datum) verticalExageration)) )
    );END if
  );END foreach
)
(defun DT:GetILFromManholeData ( manholeData )
  ; Return a list with ILs from manholeData in real format
  (if manholeData
    (list
      (atof (nth 2 manholeData))
      (atof (nth 3 manholeData))
      (atof (nth 4 manholeData))
      (atof (nth 5 manholeData))
    )
  );END if
)
(defun DT:DrawManhole ( manholeData startPoint datum verticalExageration layFoul layStorm / pBase lay )
  (princ "\nmanholeData = ")(princ manholeData)
  (if (and manholeData datum startPoint)
    (if (= 'list (type manholeData))
      (if (= 8 (length manholeData))
        (if (or (= (substr (nth 1 manholeData) 1 1) "S") (= (substr (nth 1 manholeData) 1 1) "F") )
          (progn
            ; Find manhole position (chainage wise)
            (setq pBase (polar startPoint 0 (nth 0 manholeData)) )

            ; Select layer
            (cond
              ((= (substr (nth 1 manholeData) 1 1) "S") (setq lay "e-asd-PhH2") )
              ((= (substr (nth 1 manholeData) 1 1) "F") (setq lay "e-afd-PH2") )
            );END cond

            (if lay
              (progn
                ; Draw invert level box line and ILs
                (cond
                  ((= (substr (nth 1 manholeData) 1 1) "S")
                    (DT:DrawStormInvertLevelBoxLine pBase lay)
                    (DT:WriteInvertLevels (polar pBase (* 1.5 pi) 45) lay (nth 2 manholeData) (nth 3 manholeData) (nth 4 manholeData) (nth 5 manholeData))
                    (DT:DrawVerticalAxis pBase (polar pBase (* 0.5 pi) 110) lay)
                    (DT:WriteVerticalAxisLabel (polar pBase (* 0.5 pi) 110.5) lay (nth 1 manholeData))
                    (DT:MarkInvertLevel pBase (DT:GetILFromManholeData manholeData) datum verticalExageration)
                  );END subcond
                  ((= (substr (nth 1 manholeData) 1 1) "F")
                    (DT:DrawFoulInvertLevelBoxLine pBase lay)
                    (DT:WriteInvertLevels (polar pBase (* 1.5 pi) 51) lay (nth 2 manholeData) (nth 3 manholeData) (nth 4 manholeData) (nth 5 manholeData))
                    (DT:DrawVerticalAxis pBase (polar pBase (* 0.5 pi) 110) lay)
                    (DT:WriteVerticalAxisLabel (polar pBase (* 0.5 pi) 110.5) lay (nth 1 manholeData))
                    (DT:MarkInvertLevel pBase (DT:GetILFromManholeData manholeData) datum verticalExageration)
                  );END subcond
                );END cond
              );END progn
              (progn (princ "\nERROR @ DT:DrawManhole > impossible to choose layer")(princ))
            );END if
          );END progn
          (progn (princ "\nERROR @ DT:DrawManhole > manhole name doesn't start with \"S\" or \"F\"")(princ))
        );END if
        (progn (princ "\nERROR @ DT:DrawManhole > provided manholeData list has wrong number of items")(princ))
      );END if
      (progn (princ "\nERROR @ DT:DrawManhole > provided manholeData has wrong data type")(princ))
    );END if
    (progn (princ "\nERROR @ DT:DrawManhole > manholeData/datum/startPoint = nil")(princ))
  );END if
)
