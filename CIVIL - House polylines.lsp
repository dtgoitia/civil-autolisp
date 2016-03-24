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

  (setq
    ent_name (entlast)                            ; Select last created entity
    VL_ent_name (vlax-ename->vla-object ent_name) ; Convert to VL object
  ); END setq

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

  ; INPUT - Ask if it is "house" or "garage"
  (initget "House Garage")
  (setq btype (getkword "\nSelect building type [House/Garage] <House>: ") )
  (if (not btype) (setq btype "House"))

  ; INPUT - Ask to clic inside the building for oncoming offset
  (setq pt (getpoint "\nSelect a point inside the building: "))

  ; OPERATION - Add inner line
  (cond
    ((= btype "House")
      (setq
        thickness 0.302
        dist (* 0.5 thickness)
      )
    ); END cond house
    ((= btype "Garage")
      (setq
        thickness 0.215
        dist (* 0.5 thickness)
      )
    ); END cond garage
  ); END cond
  (setq ent_name (entlast))
  (command "._offset" "E" "Y" dist ent_name pt "")

  ; OPERATION - Change polyline thickness
  (setq entList (entget (entlast)))
  (setq destiny_entList (subst (cons 43 thickness) (assoc 43 entList) entList))
  (entmod destiny_entList)

  ; RESET to original settings
  (command "._offset" "E" "N" "" "")
  (setvar "osmode" oldosmode)
  (setvar "cmdecho" oldcmdecho)
  (setvar "clayer" oldclayer)

  ; End without double messages
  (princ)

  ; v0.1 - 2016.03.22 - Translate into English
  ; v0.0 - 2016.03.14 - First issue
  ; Author: David Torralba
  ; Last revision: 2016.03.22
)
