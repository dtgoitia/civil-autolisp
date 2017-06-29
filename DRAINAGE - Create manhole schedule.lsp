;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Shortcuts:                                                                ;;
;;    MSC: creates manhole schedule                                           ;;
;;    MSCS: calculates soffit, chamber size and manhole type                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get_ID ( VL_ent_name )
  (vl-some '(lambda ( att )
              (if (= "ID" (strcase (vla-get-tagstring att)))
                (vla-get-textstring att)
              ) ; END if
            )
          (vlax-invoke VL_ent_name 'getattributes)
        )
  ; v0.0 - 2016.04.10 - First issue
  ; Author: David Torralba
  ; Last revision: 2016.04.10
)
(defun get_manhole_att ( txt_msg )
  ; (Re)load VLISP
  (vl-load-com)

  ; INPUT - Select object
  (setq block_effective_name nil)
  (while  (and
            (/= block_effective_name "FW-Manhole")
            (/= block_effective_name "FW-Manhole-NoAdoptable")
            (/= block_effective_name "FW-Manhole-NoWipeout")
            (/= block_effective_name "FW-Manhole-NoWipeout-NoAdoptable")
            (/= block_effective_name "SW-Manhole")
            (/= block_effective_name "SW-Manhole-NoAdoptable")
            (/= block_effective_name "SW-Manhole-NoWipeout")
            (/= block_effective_name "SW-Manhole-NoWipeout-NoAdoptable")
          )
    (setq ent (entsel txt_msg))
    (if (not ent)
      (princ "\nNothing selected. Try again, please.\n") ; True
      (progn
        ; OPERATION - get the entity and entity name
        (setq ent_name (car ent))
        (if (= (cdr (assoc 0 (entget ent_name))) "INSERT")
          (progn
            ; OPERATION - Convert to name to VL object
            (setq VL_ent_name (vlax-ename->vla-object ent_name))
            ; OPERATION - Comprobar si es un manhole, y si no lo es abortar la rutina.
            (setq block_effective_name (LM:effectivename VL_ent_name))
            (if
              (or
                (/= block_effective_name "FW-Manhole")
                (/= block_effective_name "FW-Manhole-NoAdoptable")
                (/= block_effective_name "FW-Manhole-NoWipeout")
                (/= block_effective_name "FW-Manhole-NoWipeout-NoAdoptable")
                (/= block_effective_name "SW-Manhole")
                (/= block_effective_name "SW-Manhole-NoAdoptable")
                (/= block_effective_name "SW-Manhole-NoWipeout")
                (/= block_effective_name "SW-Manhole-NoWipeout-NoAdoptable")
              );END or
              (princ "\nManhole selected.\n")                               ; True
              (princ "\nNot manhole block selected. Please, try again.\n")  ; False
            ); END if
          )
          (princ "\nNot manhole block selected. Please, try again.\n") ; False
        ); END if
      ); END progn
    ); END if
  ); END while

  ; get visibility state
  (setq vis_sta (LM:getvisibilitystate VL_ent_name))

  ; OPERATION - Check if 'getattributes method is applicable
  (if (vlax-method-applicable-p VL_ent_name 'getattributes) ; Condition
    (progn                                                ; True. Se puede aplicar el método
      ; Extract ID and CL
      (setq ID (get_block_att VL_ent_name "ID")
            CL (get_block_att VL_ent_name "CL")
      )
      ; OPERATION - Extract coordinates
      (setq coord (cdr (assoc 10 (entget ent_name))))
      (setq
        E_coord (car coord)  txt_E_coord (rtos E_coord 2 3)
        N_coord (cadr coord) txt_N_coord (rtos N_coord 2 3)
      )
      (princ "\ncoord = ")(princ coord)
      (princ "\nE_coord = ")(princ E_coord)
      (princ "\nN_coord = ")(princ N_coord)
      ; OPERATION - Extract IL (strings) and convert them to real
      (setq txt_IL1 (get_block_att VL_ent_name "IL1") IL1 (atof txt_IL1) txt_IL1 (LM:rtos IL1 2 3)
            txt_IL2 (get_block_att VL_ent_name "IL2") IL2 (atof txt_IL2) txt_IL2 (LM:rtos IL2 2 3)
            txt_IL3 (get_block_att VL_ent_name "IL3") IL3 (atof txt_IL3) txt_IL3 (LM:rtos IL3 2 3)
            txt_IL4 (get_block_att VL_ent_name "IL4") IL4 (atof txt_IL4) txt_IL4 (LM:rtos IL4 2 3)
      )

      (princ (strcat "\nID = " ID "\nCL = " CL "\nIL1 = " txt_IL1 "\nIL2 = " txt_IL2 "\nIL3 = " txt_IL3 "\nIL4 = " txt_IL4 ))   ; multilinea
    ); END progn true
    (princ "\nThis object is not a manhole.")         ; False. No se puede aplicar el método
  ); END if
  (princ)
)
(defun DT:autoMSC (ent_name p_ins /
              oldlayer oldosmode oldcmdecho oldattdia oldattreq
              ID CL
              IL0 IL1 IL2 IL3 IL4 txt_IL0 txt_IL1 txt_IL2 txt_IL3 txt_IL4
              p_ins2
              VL_ent_name
              vis_sta
              )
  ; SET - Error handling function
  (defun *error* ( msg )
    (if (not (member msg '("Function cancelled" "quit / exit abort")))
      (princ (strcat "\nError: " msg))
    )
    ; RESTORE PREVIOUS SETTINGS
    (setvar "clayer" oldlayer)
    (setvar "osmode" oldosmode)
    (setvar "cmdecho" oldcmdecho)
    (setvar "attdia" oldattdia)
    (setvar "attreq" oldattreq)
    (princ)
  )

  ; SAVE CURRENT SETTINGS - Current layer, OSMODE and CMDECHO
  (setq
    oldlayer (getvar "clayer")
    oldosmode (getvar "osmode")
    oldcmdecho (getvar "cmdecho")
    oldattdia (getvar "attdia")
    oldattreq (getvar "attreq")
  )

  ; SET INITIAL SETTINGS
  (setvar "osmode" 0)
  (setvar "cmdecho" 0)
  (setvar "attdia" 0)
  (setvar "attreq" 1)

  ; OPERATION - Get Manhole visibility state
  (setq
    VL_ent_name (vlax-ename->vla-object ent_name)
    vis_sta (LM:getvisibilitystate VL_ent_name)
  )

  ; OPERATION - Check if 'getattributes method is applicable
  (if (vlax-method-applicable-p VL_ent_name 'getattributes) ; Condition
    (progn                                                ; True. Se puede aplicar el método
      ; Extract ID and CL
      (setq ID (get_block_att VL_ent_name "ID")
            CL (get_block_att VL_ent_name "CL")
      )
      ; OPERATION - Extract coordinates
      (setq coord (cdr (assoc 10 (entget ent_name))))
      (setq
        E_coord (car coord)  txt_E_coord (rtos E_coord 2 3)
        N_coord (cadr coord) txt_N_coord (rtos N_coord 2 3)
      )
      ; OPERATION - Extract IL (strings) and convert them to real
      (setq txt_IL1 (get_block_att VL_ent_name "IL1") IL1 (atof txt_IL1) txt_IL1 (LM:rtos IL1 2 3)
            txt_IL2 (get_block_att VL_ent_name "IL2") IL2 (atof txt_IL2) txt_IL2 (LM:rtos IL2 2 3)
            txt_IL3 (get_block_att VL_ent_name "IL3") IL3 (atof txt_IL3) txt_IL3 (LM:rtos IL3 2 3)
            txt_IL4 (get_block_att VL_ent_name "IL4") IL4 (atof txt_IL4) txt_IL4 (LM:rtos IL4 2 3)
      )
    ); END progn true
    (princ "\nThis object is not a manhole.")         ; False. No se puede aplicar el método
  ); END if



  ; OPERATION - Activar referencias
  (setvar "osmode" 1)

  ; INPUT - Select block insertion point
  ;(setq p_ins pt)

  ; OPERATION - Unable references
  (setvar "osmode" 0)

  ; OPERATION - Asign the lowest IL (but no zero) to the out IL (IL0)
    ; change zero IL values to huge values
    (if (= IL1 0) (setq IL1 9999.999))
    (if (= IL2 0) (setq IL2 9999.999))
    (if (= IL3 0) (setq IL3 9999.999))
    (if (= IL4 0) (setq IL4 9999.999))
    ; look for the lowest IL and asign it to IL0
    (setq IL0 (min IL1 IL2 IL3 IL4) )
    ; return huge IL values to zero IL values
    (if (= IL1 9999.999) (setq IL1 0.0))
    (if (= IL2 9999.999) (setq IL2 0.0))
    (if (= IL3 9999.999) (setq IL3 0.0))
    (if (= IL4 9999.999) (setq IL4 0.0))
    ; convert IL0 to a string
    (setq txt_IL0 (LM:rtos IL0 2 3))

    ; OPERATION - Create and/or change current layer to insert block
    (if (/= (tblsearch "LAYER" "e-manhole-schedule") nil)
      (command "._layer" "S" "e-manhole-schedule" "C" "7" "" nil)
      (progn
        (princ "\nLooking for e-manhole-schedule layer not found.")
        ; Create "e-manhole-schedule" layer
        (command "._-layer" "N" "e-manhole-schedule" "S" "e-manhole-schedule" "C" "7" "" nil)
        (princ "\n\"e-manhole-schedule\" layer created.")
      ) ; END progn false
    ) ; END if

  (princ (strcat "\nManhole " ID " found at X=" (LM:rtos (car p_ins) 2 3) ", Y=" (LM:rtos (cadr p_ins) 2 3) ))
  ; OPERATION - Insertar el bloque
  (command "._insert" "ManScheduleBody" p_ins "1" "1" "0" ID CL "" "" "" txt_IL1 txt_IL2 txt_IL3 txt_IL4 txt_IL0 "" "" "" "" "" "" "" "" "" "" txt_E_coord txt_N_coord)

  ; OPERATION - Synchronize both visibility states
  (LM:SetVisibilityState (vlax-ename->vla-object (entlast)) vis_sta)

  ; OPERATION - Insert ManDetail block
  (setq
    p_ins2 (polar p_ins   0 121.5)
    p_ins2 (polar p_ins2 (* -0.5 pi) 8.25)
  )
  (command "._insert" "ManDetail" p_ins2 "1" "1" "0")

  ; OPERATION - Sync ManDetail block visibility state
  (LM:SetVisibilityState (vlax-ename->vla-object (entlast)) vis_sta)

  ; RESTORE PREVIOUS SETTINGS
  (setvar "clayer" oldlayer)
  (setvar "osmode" oldosmode)
  (setvar "cmdecho" oldcmdecho)
  (setvar "attdia" oldattdia)
  (setvar "attreq" oldattreq)

  ; End without double messages
  (princ)

  ; v0.2 - 2016.06.27 - Message prompt tidy up
  ;                   - Code tidy up and translation
  ;                   - Input parameters update
  ;                   - Local variables update
  ; v0.1 - 2016.04.09 - Code tidy up and translation
  ;                   - Change and reset ATTDIA and ATTREQ system variables
  ; v0.0 - 2016.02.23 - First issue
  ; Author: David Torralba
  ; Last revision: 2016.04.09
)
(defun c:MSC(
							/
							ans ss
              storm_ID_list storm_data_list sorted_storm_ID_list sorted_storm_data_list
              foul_ID_list  foul_data_list  sorted_foul_ID_list  sorted_foul_data_list
              pS0 pF0 pS pF
							VL_ent_name
              position ent
							)
  ; INPUT - Ask user if he wants to select manholes manually or select all manholes in the drawing
  (initget "All Manually")
  (setq ans (getkword "\nChoose selection mode [All/Manually] <All>: "))

  ; OPERATION - Select manholes according to previous step
  (if (or (not ans) (= ans "All"))
    (setq ss (ssget "x" '((0 . "INSERT"))))
    (setq ss (ssget '((0 . "INSERT"))))
  )

  ;OPERATION - Prepare initial variables and conditions
  (setq
    storm_ID_list (list "")
		foul_ID_list (list "")
		storm_data_list (list "")
		foul_data_list (list "")
		pS0 (getpoint "\nSelect point to build manhole schedule (top left corner): ")
		pF0 (polar pS0 0 180)
		pS (polar pS0 (* pi -0.5) 7.8128656)
		pF (polar pF0 (* pi -0.5) 7.8128656)
	)

  ; OPERATION - Insert Manhole Schedule Header blocks
	(command "._insert" "ManScheduleHeader" pS0 "1" "1" "0")
  (DT:DrawManholeScheduleSuperHeader (entlast) "STORM MANHOLE SCHEDULE")
	(command "._insert" "ManScheduleHeader" pF0 "1" "1" "0")
  (DT:DrawManholeScheduleSuperHeader (entlast) "FOUL MANHOLE SCHEDULE")

	; OPERATION - Run though all existing manhole blocks
	(foreach e (ssnamex ss)
    (if (>= (car e) 0)
      (progn
        (setq VL_ent_name (vlax-ename->vla-object (cadr e)))
        (cond
    			( (= "SW-Manhole" (LM:effectivename VL_ent_name))
    				(setq
    					storm_ID_list (append storm_ID_list (list (get_ID VL_ent_name) ))
    					storm_data_list (append storm_data_list (list (list (get_ID VL_ent_name) (cadr e) )))
    				)
    			);END cond storm
    			(	(= "FW-Manhole" (LM:effectivename VL_ent_name))
    				(setq
    					foul_ID_list (append foul_ID_list (list (get_ID VL_ent_name) ))
    					foul_data_list (append foul_data_list (list (list (get_ID VL_ent_name) (cadr e)) ))
    				)
    			);END cond foul
    		);END cond
      );END progn
    );END if
	);END foreach

	; OPERATION - Short lists by ID
	(setq
		sorted_storm_ID_list (acad_strlsort storm_ID_list)
		sorted_foul_ID_list  (acad_strlsort foul_ID_list)
		sorted_storm_data_list (list "")
		sorted_foul_data_list (list "")
	)

	; OPERATION - Rebuild data lists shorted by lists
	(foreach ID sorted_storm_ID_list
		(cond
			((/= "" ID)
				(setq position (vl-position ID storm_ID_list)) 		; position of the ID at storm_ID_list
				(setq ent (cadr (nth position storm_data_list)))	; data associated to the current ID at storm_data_list
				(DT:autoMSC ent pS)
				(setq pS (polar pS (* pi -0.5) 16.5))							; recalculate next insertion point
			)
		);END cond
	);END foreach
	(foreach ID sorted_foul_ID_list
		(cond
			((/= "" ID)
				(setq position (vl-position ID foul_ID_list)) 		; position of the ID at foul_ID_list
				(setq ent (cadr (nth position foul_data_list)))	; data associated to the current ID at foul_data_list
				(DT:autoMSC ent pF)
				(setq pF (polar pF (* pi -0.5) 16.5))							; recalculate next insertion point
			)
		);END cond
	);END foreach

	(princ)

  ; v0.3 - 2017.06.29 - DT:DrawManholeScheduleSuperHeader implemented as per request
  ; v0.2 - 2016.06.27 - Layer filters removed to select blocks in any layer
  ;                   - Message prompt tidy up
  ;                   - Update local variables
  ;                   - foreach filter condition update
  ; v0.1 - 2016.05.03 - More layers added to filter
  ; v0.0 - 2016.04.10 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.06.29
)
(defun c:MSCS (
                /
                *error*
                oldlayer oldosmode oldcmdecho
                ss
                VL_ent_name
                CL IL0 IL1 IL2 IL3 IL4
                txt_CL txt_IL0 txt_IL1 txt_IL2 txt_IL3 txt_IL4
                P0 P1 P2 P3 P4
                txt_P0 txt_P1 txt_P2 txt_P3 txt_P4
                vis_sta
                top0 top1 top2 top3 top4
                top_level
                biggest_pipe txt_biggest_pipe
                DTS txt_DTS
              )
  ; OPERATION - Check required functions are loaded. If not, exit.
  (if (= (eval LM:effectivename) nil)
    (progn
      (alert "LM:effectivename function is not loaded.\nPlease, load LM:effectivename and run command again.")
      (exit)
    )
  )

  ; SET - Error handling function
  (defun *error* ( msg )
    (if (not (member msg '("\nFunction cancelled" "quit / exit abort")))
      (princ (strcat "\nError: " msg))
    )
    ; Restore previous settings
    (setvar "clayer" oldlayer)
    (setvar "osmode" oldosmode)
    (setvar "cmdecho" oldcmdecho)
    (princ)
  )

  ; SAVE CURRENT SETTINGS - Current layer, OSMODE and CMDECHO
  (setq oldlayer (getvar "clayer")
        oldosmode (getvar "osmode")
        oldcmdecho (getvar "cmdecho")
  )

  ; (Re)load VLISP
  (vl-load-com)

  ; CHANGE INITIAL SETTINGS - "osmode" and "cmdecho"
  (setvar "osmode" 0)
  (setvar "cmdecho" 0)

  ; INPUT - Select schedule block
	(princ "\nLooking for existing Manhole Schedule blocks... ")
  (setq
    ss (ssget '(
								(-4 . "<AND")
									( 0 . "INSERT")
									( 8 . "e-manhole-schedule")
								(-4 . "AND>")
							 )
			 )
  )
  (foreach a (ssnamex ss)
    ; If the entity has ename
    (if (= 'ename (type (cadr a)))
      (progn
        ; OPERATION - Get entity VL name
        (setq VL_ent_name (vlax-ename->vla-object (cadr a)))
        (if (and
              (= "ManScheduleBody" (LM:effectivename VL_ent_name))  ; If is a manhole schedule block
              (vlax-method-applicable-p VL_ent_name 'getattributes) ; If 'getattributes method is applicable
            )
          (progn
            (setq
              ; OPERATION - Extract CL, ILs and pipe sizes, and convert them to a real number
              txt_CL (get_block_att VL_ent_name "CL")               CL (atof txt_CL)
              txt_IL0 (get_block_att VL_ent_name "IL0")             IL0 (atof txt_IL0)
              txt_IL1 (get_block_att VL_ent_name "IL1")             IL1 (atof txt_IL1)
              txt_IL2 (get_block_att VL_ent_name "IL2")             IL2 (atof txt_IL2)
              txt_IL3 (get_block_att VL_ent_name "IL3")             IL3 (atof txt_IL3)
              txt_IL4 (get_block_att VL_ent_name "IL4")             IL4 (atof txt_IL4)
              txt_P0 (substr (get_block_att VL_ent_name "P0") 3 5)  P0 (* 0.001 (atof txt_P0))
              txt_P1 (substr (get_block_att VL_ent_name "P1") 3 5)  P1 (* 0.001 (atof txt_P1))
              txt_P2 (substr (get_block_att VL_ent_name "P2") 3 5)  P2 (* 0.001 (atof txt_P2))
              txt_P3 (substr (get_block_att VL_ent_name "P3") 3 5)  P3 (* 0.001 (atof txt_P3))
              txt_P4 (substr (get_block_att VL_ent_name "P4") 3 5)  P4 (* 0.001 (atof txt_P4))

              ; OPERATION - Get visibility state
              vis_sta (LM:getvisibilitystate VL_ent_name)

              ; OPERATION - Calculate IL + DN according to visibility state
              top0 (+ IL0 P0)
              top1 (+ IL1 P1)
              top2 (+ IL2 P2)
              top3 (+ IL3 P3)
              top4 (+ IL4 P4)
            )

            ; OPERATION - Find highest level
            (cond
              ((or (= vis_sta "0") (= vis_sta "1")) (setq top_level (max top0 top1) ) )   ; 1 IL used
              ((= vis_sta "2") (setq top_level (max top0 top1 top2) ) )                   ; 2 IL used
              ((= vis_sta "3") (setq top_level (max top0 top1 top2 top3) ) )              ; 3 IL used
              ((= vis_sta "4") (setq top_level (max top0 top1 top2 top3 top4) ) )         ; 4 IL used
            )

            ; OPERATION - Find biggest pipe size
            (cond
              ((or (= vis_sta "0") (= vis_sta "1")) (setq biggest_pipe (max P0 P1) ) )    ; 1 IL used
              ((= vis_sta "2") (setq biggest_pipe (max P0 P1 P2) ) )                      ; 2 IL used
              ((= vis_sta "3") (setq biggest_pipe (max P0 P1 P2 P3) ) )                   ; 3 IL used
              ((= vis_sta "4") (setq biggest_pipe (max P0 P1 P2 P3 P4) ) )                ; 4 IL used
            )
            (setq txt_biggest_pipe (rtos biggest_pipe 2 3))

            ; OPERATION - Change Chamber Size (CS) attribute
            (cond                                                                                                                   ; Biggest pipe diameter = Dmax
              ((< biggest_pipe 0.375)                              (LM:vl-setattributevalue VL_ent_name "CS" "DN1200"))             ; Dmax < 375
              ((and (> biggest_pipe 0.375) (< biggest_pipe 0.700)) (LM:vl-setattributevalue VL_ent_name "CS" "DN1500"))             ; 375 < Dmax < 700
              ((and (> biggest_pipe 0.700) (< biggest_pipe 0.900)) (LM:vl-setattributevalue VL_ent_name "CS" "DN1800"))             ; 700 < Dmax < 900
              ((> biggest_pipe 0.9)                                (LM:vl-setattributevalue VL_ent_name "CS" "Consult Undertaker")) ; 900 < Dmax
            )

            ; OPERATION - Calculate DTS ("depth to soffit")
            (setq
              DTS (- CL top_level)
              txt_DTS (LM:rtos DTS 2 3)
            )

            ; OPERATION - Update SOFFIT attribute
            (LM:vl-setattributevalue VL_ent_name "SOFFIT" txt_DTS)

            ; OPERATION - Calculate manhole type and update MT attribute
            (cond
              ((and (<= 3 DTS)(<= DTS 6))
                (LM:vl-setattributevalue VL_ent_name "MT" "A")
                ;(princ "\nDTS = ")(princ DTS)(princ "  3-6m")
              )
              ((and (<= 1.5 DTS)(<= DTS 3))
                (LM:vl-setattributevalue VL_ent_name "MT" "B")
                ;(princ "\nDTS = ")(princ DTS)(princ "  1.5-3m")
              )
              ((and (<= 1 DTS)(<= DTS 1.5))
                (LM:vl-setattributevalue VL_ent_name "MT" "E")
                ;(princ "\nDTS = ")(princ DTS)(princ "  1-1.5m")
              )
              ((< DTS 1)
                (LM:vl-setattributevalue VL_ent_name "MT" "CHECK")
                ;(princ "\nDTS = ")(princ DTS)(princ " < 1m")
              )
              ((> DTS 6)
                (LM:vl-setattributevalue VL_ent_name "MT" "CHECK")
                ;(princ "\nDTS = ")(princ DTS)(princ " > 6m")
              )
            ); END cond
          ); END progn
        ); END if
      );END progn
    );END if
  ); END foreach

  ; RESTORE PREVIOUS SETTINGS
  (setvar "clayer" oldlayer)
  (setvar "osmode" oldosmode)
  (setvar "cmdecho" oldcmdecho)
  (princ)

  ; End without double messages
  (princ)
  ; v0.4 - 2016.04.10 - Code tidy up
  ;                   - While loop changed for foreach
  ; v0.3 - 2016.03.10 - Feature added: chamber size is now calculated based on pipe sizes
  ;                   - Feature added: cuando vas a elegir el manhole, si no seleccionas nada te avisa y te deja volver a intentarlo
  ;                   - Feature added: cuando vas a elegir el manhole, si seleccionas otra cosa que no es un bloque de manhole te avisa y te deja volver a intentarlo
  ; v0.2 - 2016.03.08 - Feature added: coordinates extraction and plot
  ; v0.1 - 2016.03.01 - DTS<1m and DTS>6m added (out of rank conditions)
  ; v0.0 - 2016.02.24 - First issue
  ; Author: David Torralba
  ; Last revision: 2016.04.10
)
(defun c:UpdateManholeSchedule ( / manSchedule manhole manScheduleId manholeId ans )
  ; Update selected manhole's CL and coordinates

  ; INPUT - Ask user to select Manhole Schedule block to update
  (while (not manSchedule)
    (if (not (setq manSchedule (car (entsel "\nSelect Manhole Schedule block to update coordinates: "))))
      (princ "missed. Try again.")
      (princ "object selected.\n")
    ); END if
  );END while

  ; OPERATION - Check selected object is a Manhole Schedule block
  (if (/= "ManScheduleBody" (LM:effectivename (vlax-ename->vla-object manSchedule)))
    (progn
      (alert "Selected object is not a Manhole Schedule Block.")
      (exit)
    );END progn
  );END if

  ; INPUT - Ask user to select Manhole Block to update to
  (while (not manhole)
    (if (not (setq manhole (car (entsel "\nSelect Manhole block to update coordinates to: "))))
      (princ "missed. Try again.")
      (princ "object selected.\n")
    ); END if
  );END while

  ; OPERATION - Check selected object is a Manhole block
  (if (/= "W-Manhole" (substr (LM:effectivename (vlax-ename->vla-object manhole)) 2 9))
    (progn
      (alert "Selected object is not a Manhole Block.")
      (exit)
    );END progn
  );END if

  ; OPERATION - Get block "ID" attributes
  (setq
    manScheduleId (LM:vl-getattributevalue (vlax-ename->vla-object manSchedule) "ID")
    manholeId (LM:vl-getattributevalue (vlax-ename->vla-object manhole) "ID")
  )

  ; OPERATION - Update block attributes
  (if (= manholeId manScheduleId)
    (progn
      (DT:SetManholeScheduleCoordinates manSchedule (cdr (assoc 10 (entget manhole))) )
      (DT:SetManholeScheduleCoverLevel manSchedule (atof (LM:vl-getattributevalue (vlax-ename->vla-object manhole) "CL")) )
    );END progn
    (progn
      (initget "Yes No")
      (setq ans (getkword "\nSelected object don't match. They have different ID.\nAre you sure you want to update the manhole schedule? <Yes/No>: [No]"))
      (if (not ans) (setq ans "No"))
      (if (= ans "Yes")
        (progn
          (DT:SetManholeScheduleCoordinates manSchedule (cdr (assoc 10 (entget manhole))) )
          (DT:SetManholeScheduleCoverLevel manSchedule (atof (LM:vl-getattributevalue (vlax-ename->vla-object manhole) "CL")) )
        );END progn
      );END if
    );END progn
  );END if

  (princ)

  ; v0.1 - 2017.05.12 - DT:SetManholeScheduleCoordinates implemented
  ;                   - DT:SetManholeScheduleCoverLevel implemented
  ;                   - Messages updated
  ; v0.0 - 2016.08.05 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.05.12
)
(defun DT:SetManholeScheduleCoordinates ( manSchedule coordinates / manScheduleObject oldE oldN newE newN Eupdated Nupdated )
  ; Set the coordinate value of Manhole Schedule block
  ; manSchedule [ename] - Manhole Schedule block entity name
  ; coordinates [lst] - List with the point coordinates of the manhole, being
  ;                     list values real numbers
  (if (and manSchedule coordinates)
    (if (and (= 'ename (type manSchedule)) (DT:TypePoint coordinates))
      (progn
        (setq
          manScheduleObject (vlax-ename->vla-object manSchedule)
          oldE (LM:vl-getattributevalue manScheduleObject "E")
          oldN (LM:vl-getattributevalue manScheduleObject "N")
          newE (LM:rtos (nth 0 coordinates) 2 3)
          newN (LM:rtos (nth 1 coordinates) 2 3)
        )
        (if (/= oldE newE)
          (progn
            (LM:vl-setattributevalue manScheduleObject "E" newE)
            (setq Eupdated T)
          );END progn
        );END if
        (if (/= oldN newN)
          (progn
            (LM:vl-setattributevalue manScheduleObject "N" newN)
            (setq Nupdated T)
          );END progn
        );END if
        (cond
          ((and Eupdated Nupdated) (princ "\nE and N coordinates updated.\n") T)
          ((and Eupdated (not Nupdated)) (princ "\nE coordinate updated.\n") T)
          ((and (not Eupdated) Nupdated) (princ "\nN coordinate updated.\n") T)
          (t T)
        );END cond
      );END progn
      (cond
        ((/= 'ename (type manSchedule))   (princ "\nERROR @ DT:SetManholeScheduleCoordinates : manSchedule is not a ename\n") nil )
        ((not (DT:TypePoint coordinates)) (princ "\nERROR @ DT:SetManholeScheduleCoordinates : coordinates is not a point\n")   nil )
      );END cond
    );END if
    (cond
      ((not manSchedule) (princ "\nERROR @ DT:SetManholeScheduleCoordinates : manSchedule=nil\n") nil )
      ((not coordinates) (princ "\nERROR @ DT:SetManholeScheduleCoordinates : coordinates=nil\n") nil )
    );END cond
  );END if

  ; v0.0 - 2017.05.12 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.05.12
)
(defun DT:SetManholeScheduleCoverLevel ( manSchedule coverLevel / manScheduleObject oldCoverLevel newCoverLevel )
  ; Set the cover level value of Manhole Schedule block
  ; manSchedule [ename] - Manhole Schedule block entity name
  ; coverLevel [number] - Number indicating manhole cover level
  (if (and manSchedule coverLevel)
    (if (and (= 'ename (type manSchedule)) (numberp coverLevel))
      (progn
        (setq
          manScheduleObject (vlax-ename->vla-object manSchedule)
          oldCoverLevel (LM:vl-getattributevalue manScheduleObject "CL")
          newCoverLevel (LM:rtos coverLevel 2 3)
        )
        (if (/= oldCoverLevel newCoverLevel)
          (progn
            (LM:vl-setattributevalue manScheduleObject "CL" newCoverLevel)
            (progn (princ "\nCL updated.\n") T)
          );END progn
          T
        );END if
      );END progn
      (cond
        ((/= 'ename (type manSchedule)) (princ "\nERROR @ DT:SetManholeScheduleCoverLevel : manSchedule is not a ename\n") nil )
        ((not (numberp coverLevel))     (princ "\nERROR @ DT:SetManholeScheduleCoverLevel : coverLevel is not a number\n")   nil )
      );END cond
    );END if
    (cond
      ((not manSchedule) (princ "\nERROR @ DT:SetManholeScheduleCoverLevel : manSchedule=nil\n") nil )
      ((not coverLevel)  (princ "\nERROR @ DT:SetManholeScheduleCoverLevel : coverLevel=nil\n") nil )
    );END cond
  );END if

  ; v0.0 - 2017.05.12 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.05.12
)
(defun DT:DrawManholeScheduleSuperHeader ( referenceHeader string / pointList )
  ; Add Manhole Schedule Super Header
  (setq pointList (list (cdr (assoc 10 (entget referenceHeader)))))
  (setq pointList (append pointList (list (polar (nth 0 pointList) (* 0.5 pi) 8))) )
  (setq pointList (append pointList (list (polar (nth 1 pointList) 0 156.4537693901))) )
  (setq pointList (append pointList (list (polar (nth 2 pointList) (* -0.5 pi) 8))) )
  (entmakex
    (append
      (list
        (cons   0 "LWPOLYLINE")         ; Object type
        (cons 100 "AcDbEntity")
        (cons 100 "AcDbPolyline")
        (cons  70 0)                  ; Open(0)/Closed(1)
        (cons  90 (length pointList)) ; Number of vertices
      )
      (mapcar
        '(lambda (pt) (cons 10 pt) )
        pointList
      );END mapcar
    );END append
  )
  (entmakex
    (list
      (cons 0 "TEXT")
      (cons 1 string)
      (cons 10 (polar (nth 0 pointList) (* 0.25 pi) 4))
      (cons 40 2.5)
      (cons 7 "ROMANS")
    );END list
  );END entmakex

  ; v0.0 - 2017.06.29 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.06.29
)
