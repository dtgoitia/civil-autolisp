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
    ; RESET to original settings
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

  ; OPERATION - Create "e-set-out-house" and "e-work-block" layers if they don't exist
  (if (not (tblsearch "LAYER" "e-work-block")) (command "-layer" "N" "e-work-block" "C" "7" "e-work-block" ""))
  (if (not (tblsearch "LAYER" "e-set-out-house")) (command "-layer" "N" "e-set-out-house" "C" "7" "e-set-out-house" ""))

  ; INFINITE LOOP
  (while (not kkkkk)
    (while (not ent_name)
      (setq ent_name (car (entsel "\nSelect building perimeter polyline (press Esc to exit): ")))
      (if (not ent_name) (princ "nothing selected."))
    ); END while
    (setq VL_ent_name (vlax-ename->vla-object ent_name))

    ; OPERATION - Check if it is closed and if not close it
    (if (= :vlax-false (vla-get-closed VL_ent_name)) (vla-put-closed VL_ent_name :vlax-true))

    ; OPERATION - Extract object propety list
    (setq entList (entget ent_name))

    ; OPERATION - Set zero thickness for closed polyline
    (vlax-put-property VL_ent_name 'ConstantWidth 0)

    ; OPERATION - Change color and linetype to "ByLayer"
    (vlax-put-property VL_ent_name 'Color 256)

    ; OPERATION - Move closed polyline to "e-set-out-house" layer
    (vla-put-layer VL_ent_name "e-set-out-house")

    ; OPERATION - Copy closed polyline into "e-work-block" layer
    (vla-put-layer (vla-copy VL_ent_name) "e-work-block")

    ; INPUT - Ask building type: house or garage
    (initget "House Garage")
    (setq ans (getkword "\nSelect building type [House/Garage] <House>: "))
    (if (not ans) (setq ans "House"))
    (cond
      ((= ans "House")
          (setq
            btype "House"
            thickness 0.302
            dist (* 0.5 thickness)
          )
      )
      ((= ans "Garage")
        (setq
          btype "Garage"
          thickness 0.215
          dist (* 0.5 thickness)
        )
      )
    );END cond

    ; INPUT - Ask to clic inside the building for oncoming offset
    ;(command "._offset" "E" "Y" dist (entlast) (getpoint "\nSelect a point inside the building: ") "")
    (command "._offset" "E" "Y" dist (entlast) (DT:AVE_vertex VL_ent_name) "")

    ; OPERATION - Change polyline thickness
    (vlax-put-property (vlax-ename->vla-object (entlast)) 'ConstantWidth thickness)

    ; OPERATION - Reset entity so start loop again
    (setq ent_name nil)

    ; RESET OFFSET command to original settings
    (command "._offset" "E" "N" "" "")
  ); END while

  ; RESET to original settings
  (setvar "osmode" oldosmode)
  (setvar "cmdecho" oldcmdecho)
  (setvar "clayer" oldclayer)

  ; End without double messages
  (princ)

  ; v0.6 - 2016.07.01 - grread function substituted for initget and getkwrod functions as users requested
  ;                   - Use of DT:AVE_vertex function to find automatically polyline center and offset without asking to click a point inside the building
  ; v0.5 - 2016.06.02 - Add 11 code for grread.
  ;                   - Reset OFFSET command to original settings.
  ; v0.4 - 2016.05.17 - Rewrite many functions to VLA to speed up the routine.
  ;                   - Add left or right clic as possible inputs git when asking for building type.
  ;                   - Ensure "e-work-block" and "e-set-out-house" layers are created on white color (7).
  ; v0.3 - 2016.04.18 - Fix minor bugs in the infinite loop.
  ; v0.2 - 2016.04.17 - Add infinite loop.
  ; v0.1 - 2016.03.22 - Translate into English.
  ; v0.0 - 2016.03.14 - First issue.
  ; Author: David Torralba
  ; Last revision: 2016.07.01
)
