(defun c:1(
;defun c:ExtarctManholeChainages (
                                /
                                cntl
                                ans
                                )

  ; ALMOST FINISHED - Purpose: store the chainages of selected manholes, to later draw them in the long section with another function
  ; PRECAUTION - If any manhole is not in the line, the routine will assign them the chainage at the beginning or end of the centreline.
  ;

  ; AUXILIARY FUNCTIONS
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
  );END defun DT:SelectManholes()

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
;  (princ)
)
(defun c:2()
  ; ERROR HANDLING FUNCTION ----------------------------------------------------
  (defun *error* ( msg )
    (if (not (member msg '("Function cancelled" "quit / exit abort")))
      (princ (strcat "\nError: " msg))
    )
    ; RESTORE PREVIOUS SETTINGS
    (setvar "osmode" oldosmode)
    (setvar "attdia" oldattdia)
    (setvar "attreq" oldattreq)
    (princ)
  );END defun *error*()

  ; SAVE SETTINGS
  (setq oldosmode (getvar "osmode"))
  (setq oldattdia (getvar "attdia"))
  (setq oldattreq (getvar "attreq"))

  ; CHANGE SETTINGS
  (setvar "osmode" 0)
  (setvar "attdia" 0)
  (setvar "attreq" 1)

  ; AUXILIARY FUNCTIONS
  (defun DT:MarkManholeIL( ls
                          /
                          p0 p_ins
                          ch ID IL1 IL2 IL3 IL4 vis Chamber
                          )
    ; CHECK - Imported list
    (princ "\nimported_list = ")(princ ls)

    ; INPUT - Select datum line with XDATA
    (setq
      p0 (cdr (assoc 10 (entget (car (entsel "\nSelect datum line: ") ))))
      datum (getreal "\nIntroduce datum level:")
    )
    (foreach manhole ls
      (princ "\n")(princ manhole)
      (if
        (or
          (= "SW" (substr (nth 1 manhole) 1 2))
          (= "FW" (substr (nth 1 manhole) 1 2))
          (= "WW" (substr (nth 1 manhole) 1 2))
        );END or
        (progn
          ; OPERATION - Collect list data
          (setq
            p_ins (polar p0 0 (nth 0 manhole))
            ch (nth 0 manhole)
            ID (nth 1 manhole)
            IL1 (nth 2 manhole)
            IL2 (nth 3 manhole)
            IL3 (nth 4 manhole)
            IL4 (nth 5 manhole)
            vis (nth 6 manhole)
            Chamber (nth 7 manhole)
          );END setq

          (princ "\nOK1")

          ; OPERATION - Choose correct block and layer (Storm/Foul)
          (cond
            ((= "SW" (substr (nth 1 manhole) 1 2))
              (command "-insert" "LongSectionSWM" p_ins 1 1 0 IL1 IL2 IL3 IL4 ID (strcat Chamber " TYPE ?"))
              (while (> (getvar "CMDACTIVE") 0) (command ""))
              (vla-put-layer (vlax-ename->vla-object (entlast)) "e-asd")
            )
            ((= "FW" (substr (nth 1 manhole) 1 2))
              (command "-insert" "LongSectionFWM" p_ins 1 1 0 IL1 IL2 IL3 IL4 ID (strcat Chamber " TYPE ?"))
              (while (> (getvar "CMDACTIVE") 0) (command ""))
              (vla-put-layer (vlax-ename->vla-object (entlast)) "e-afd")
            )
            ((= "WW" (substr (nth 1 manhole) 1 2))
              (command "-insert" "LongSectionFWM" p_ins 1 1 0 IL1 IL2 IL3 IL4 ID (strcat Chamber " TYPE ?"))
              (while (> (getvar "CMDACTIVE") 0) (command ""))
              (vla-put-layer (vlax-ename->vla-object (entlast)) "e-afd")
            )
          );END cond

          ; OPERATION - Translate visibility state
          (cond
            ((= vis "1") (setq vis "1IL"))
            ((= vis "2") (setq vis "2IL"))
            ((= vis "3") (setq vis "3IL"))
            ((= vis "4") (setq vis "4IL"))
          );END cond
(princ "\nOK2")
          ; OPERATION - Update inserted block "Chamber size" lookup parameter
          (princ "\n(entlast) = ")(princ (entlast))
          (princ "\nChamber = ")(princ Chamber)
          (LM:setdynprops (vlax-ename->vla-object (entlast)) (list (cons "Chamber size" Chamber)))
          ; OPERATION - Update inserted block visibility state
          (LM:SetVisibilityState (vlax-ename->vla-object (entlast)) vis)
(princ "\nOK3")
          ; OPERATION - Convert IL (string to real)
          (setq
            IL1 (* 10 (- (atof IL1) datum))
            IL2 (* 10 (- (atof IL2) datum))
            IL3 (* 10 (- (atof IL3) datum))
            IL4 (* 10 (- (atof IL4) datum))
          )
          ; OPERATION - Update inserted block IL and IL marker witdh
          (cond
            ((= vis "1IL")
              (LM:setdynprops (vlax-ename->vla-object (entlast))
                (list
                  (cons "Dist_IL1" IL1) (cons "Width_IL1" (* 0.001 (atof (substr Chamber 3))) )
                );END list
              );END LM:setdynprops
            )
            ((= vis "2IL")
              (LM:setdynprops (vlax-ename->vla-object (entlast))
                (list
                  (cons "Dist_IL1" IL1) (cons "Width_IL1" (* 0.001 (atof (substr Chamber 3))) )
                  (cons "Dist_IL2" IL2) (cons "Width_IL2" (* 0.001 (atof (substr Chamber 3))) )
                );END list
              );END LM:setdynprops
            )
            ((= vis "3IL")
              (LM:setdynprops (vlax-ename->vla-object (entlast))
                (list
                  (cons "Dist_IL1" IL1) (cons "Width_IL1" (* 0.001 (atof (substr Chamber 3))) )
                  (cons "Dist_IL2" IL2) (cons "Width_IL2" (* 0.001 (atof (substr Chamber 3))) )
                  (cons "Dist_IL3" IL3) (cons "Width_IL3" (* 0.001 (atof (substr Chamber 3))) )
                );END list
              );END LM:setdynprops
            )
            ((= vis "4IL")
              (LM:setdynprops (vlax-ename->vla-object (entlast))
                (list
                  (cons "Dist_IL1" IL1) (cons "Width_IL1" (* 0.001 (atof (substr Chamber 3))) )
                  (cons "Dist_IL2" IL2) (cons "Width_IL2" (* 0.001 (atof (substr Chamber 3))) )
                  (cons "Dist_IL3" IL3) (cons "Width_IL3" (* 0.001 (atof (substr Chamber 3))) )
                  (cons "Dist_IL4" IL4) (cons "Width_IL4" (* 0.001 (atof (substr Chamber 3))) )
                );END list
              );END LM:setdynprops
            )
          );END cond

          ; mira como introducir el tema de las escalas, para el tamano del texto: pregunta cual es la escala comun, y si quieren cambiarla..que lo exploten
        );END progn
      );END if
    );END foreach
    ;(princ "\nentget = ")(princ datum)
  );END defun DT:MarkManholeIL()

  ; OPERATION - Execute the function
  (DT:MarkManholeIL global_variable_manhole_db)

  ; RESTORE PREVIOUS SETTINGS
  (setvar "osmode" oldosmode)
  (setvar "attdia" oldattdia)
  (setvar "attreq" oldattreq)

  (princ)

  ; v0.0 - 2016.08.08 - First issue
  ; Author: David Torralba
  ; Last revision: 2016.08.08
)
