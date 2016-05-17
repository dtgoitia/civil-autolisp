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

  ; OPERATION - Create "e-set-out-house" and "e-work-block" layers if they don't exist
  (if (not (tblsearch "LAYER" "e-work-block")) (command "-layer" "N" "e-work-block" ""))
  (if (not (tblsearch "LAYER" "e-set-out-house")) (command "-layer" "N" "e-set-out-house" ""))

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
    (princ "\nSelect building type [House/Garage] <House>: ")
    (setq btype nil)
    (while (not btype)
      (setq btype (grread))
      (cond
        ( (or
            (and (= (car btype) 2) (= (chr (cadr btype)) "H"))
            (and (= (car btype) 2) (= (chr (cadr btype)) "h"))
            (and (= (car btype) 2) (= (cadr btype) 13)); enter
            (and (= (car btype) 2) (= (cadr btype) 32)); space
            (= (car btype) 3); left clic (point)
            (= (car btype) 25); right clic
          ); END or
          (princ "House")
          (setq
            btype "House"
            thickness 0.302
            dist (* 0.5 thickness)
          )
        ); END cond H, ENTER, SPACE, clic
        ( (or
            (and (= (car btype) 2) (= (chr (cadr btype)) "G"))
            (and (= (car btype) 2) (= (chr (cadr btype)) "g"))
          ); END or
          (princ "Garage")
          (setq
            btype "Garage"
            thickness 0.215
            dist (* 0.5 thickness)
          )
        ); END cond G
        (t
          (setq btype nil)
          (princ "\nSelect building type [House/Garage] <House>: ")
        ); END else: /=H, /=G, /=ENTER, /=SPACE, /=left clic, /=right clic
      );END cond
    );END while

    ; INPUT - Ask to clic inside the building for oncoming offset
    (command "._offset" "E" "Y" dist (entlast) (getpoint "\nSelect a point inside the building: ") "")

    ; OPERATION - Change polyline thickness
    (vlax-put-property (vlax-ename->vla-object (entlast)) 'ConstantWidth thickness)

    ; OPERATION - Reset entity so start loop again
    (setq ent_name nil)
  ); END while

  ; RESET to original settings
  (command "._offset" "E" "N" "" "")
  (setvar "osmode" oldosmode)
  (setvar "cmdecho" oldcmdecho)
  (setvar "clayer" oldclayer)

  ; End without double messages
  (princ)

  ; v0.4 - 2016.05.17 - Rewrite many functions to VLA to speed up the routine.
  ;                   - Add left or right clic as possible inputs git when asking for building type.
  ; v0.3 - 2016.04.18 - Fix minor bugs in the infinite loop.
  ; v0.2 - 2016.04.17 - Add infinite loop.
  ; v0.1 - 2016.03.22 - Translate into English.
  ; v0.0 - 2016.03.14 - First issue.
  ; Author: David Torralba
  ; Last revision: 2016.05.17
)
