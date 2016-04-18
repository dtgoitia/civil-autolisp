(defun C:HS ( /
              *error*
              ;oldosmode oldcmdecho oldclayer
              ent_name
            )
  ; SET - Error handling function
  (defun *error* ( msg )
    (if (not (member msg '("Function cancelled" "quit / exit abort")))
      (princ (strcat "\nError: " msg))
    )
    ; RESET to original "osmode" and "cmdecho".
  (setvar "osmode" oldosmode)
  (setvar "cmdecho" oldcmdecho)
  (setvar "clayer" oldclayer)
    (princ)
  )

  ; SAVE "osmode", "cmdecho" and "clayer"
  (setq oldosmode (getvar "osmode"))
  (setq oldcmdecho (getvar "cmdecho"))
  (setq oldclayer (getvar "clayer"))

  ; CHANGE "osmode" and "cmdecho"
  (setvar "osmode" 0)
  (setvar "cmdecho" 0)

  ; INFINITE LOOP
  (while (not kkkkk)
    (while (not ent_name)
      (setq ent_name (car (entsel)))
      (if (not ent_name) (princ "nothing selected."))
    ); END while
    (setq VL_ent_name (vlax-ename->vla-object ent_name)); Convert to VL object
    

    ; OPERATION - Check if it is closed and if not close it
    (if (= :vlax-false (vla-get-closed VL_ent_name)) (vla-put-closed VL_ent_name :vlax-true))

    ; OPERATION - Exxtract object propety list
    (setq entList (entget ent_name))
    
    ; OPERATION - Set zero thickness for closed polyline
    (setq thickness (cdr (assoc 43 entList) ) )
    (if (/= thickness 0)
      (progn
        (setq thickness 0)
        (setq destiny_entList (subst (cons 43 thickness) (assoc 43 entList) entList))
        (entmod destiny_entList)
      ); END True progn
    ); END if

    ; OPERATION - Change color and linetype to "ByLayer"
    (command "SETBYLAYER" ent_name "" "" "")
    
    ; OPERATION - Mode closed polyline to "e-set-out-house" layer
    (setq
      entList (entget ent_name)
      lay (cdr (assoc 8 entList))
    )
    (if (/= lay 0)
      (progn
        ; OPERATION - Create "e-set-out-house" layer if it doesn't exist
        (setq lay "e-set-out-house")
        (command "-layer" "M" lay "")
        (setq destiny_entList (subst (cons 8 lay) (assoc 8 entList) entList))
        (entmod destiny_entList)
      ); ENF True progn
    ); END if

    ; OPERATION - Create "e-work-block" layer if it doesn't exist
    (setq lay "e-work-block")
    (command "-layer" "M" lay "")
    ; OPERATION - Copy closed polyline into "e-work-block" layer
    (command "._CopyToLayer" ent_name "" lay "")

    ; INPUT - Ask building type: house or garage
    (princ "\nSelect building type [House/Garage] <House>: ")
    (setq btype nil)
    (while (not btype)
      (setq btype (grread) )
      (cond
        ( (or
            (= (chr (cadr btype)) "H")
            (= (chr (cadr btype)) "h")
            (= (cadr btype) 13); enter
            (= (cadr btype) 32); space
          ); END or
          (princ "ouse")
          (setq
            btype "House"
            thickness 0.302
            dist (* 0.5 thickness)
          )
        ); END cond H, ENTER, SPACE
        ( (or
            (= (chr (cadr btype)) "G")
            (= (chr (cadr btype)) "g")
          ); END or
          (princ "arage")
          (setq
            btype "Garage"
            thickness 0.215
            dist (* 0.5 thickness)
          )
        ); END cond G
        (t
          (setq btype nil)
          (princ "\nSelect building type [House/Garage] <House>: ")
        ); END else: /=H, /=G, /=ENTER
      );END cond
    );END while

    ; INPUT - Ask to clic inside the building for oncoming offset
    (command "._offset" "E" "Y" dist (entlast) (getpoint "\nSelect a point inside the building: ") "")

    ; OPERATION - Change polyline thickness
    (setq
      entList (entget (entlast))
      destiny_entList (subst (cons 43 thickness) (assoc 43 entList) entList)
      ent_name nil ; Reset entity so start loop again
    )
    (entmod destiny_entList)
  ); END while

  ; RESET to original settings
  (command "._offset" "E" "N" "" "")
  (setvar "osmode" oldosmode)
  (setvar "cmdecho" oldcmdecho)
  (setvar "clayer" oldclayer)

  ; End without double messages
  (princ)

  ; v0.3 - 2016.04.18 - Fix minor bugs in the infinite loop.
  ; v0.2 - 2016.04.17 - Add infinite loop.
  ; v0.1 - 2016.03.22 - Translate into English
  ; v0.0 - 2016.03.14 - First issue
  ; Author: David Torralba
  ; Last revision: 2016.04.18
)
