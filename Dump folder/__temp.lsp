(defun c:xx ()
  ; Trigger
  (defun *error* ( errorMessage ) (princ (strcat "n-------- ERROR: " errorMessage " --------n")) (vl-bt) (DT:ReportError))
  (DT:AutoLoadFileFromCivilTemp "ErrorTracing.lsp")
  (DT:AutoLoadFileFromCivilTemp "__temp.lsp")
  (princ (strcat "\nTemp file loaded (" (DT:Now) ")\n"))(princ)
  (DT:CreateWorkingDrawingBlock)

  ; v0.0 - 2017.09.20 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.09.20
)
(defun DT:CreateWorkingDrawingBlock ( / p1 p2 ent_name ss blockName)
  ; Create Working Drawings blocks

  ; SAVE SETTINGS
	(save_environment (list "clayer" "osmode" "cmdecho"))

  ; Create Working Drawing Layers if necessary
  (if
    (and
      (DT:CheckIfLayerExists "e-work-hse")
      (DT:CheckIfLayerExists "e-work-services")
    );END and
    nil
    (DT:CreateWorkingDrawingLayers)
  );END if

  (setvar "cmdecho" 1)
  (setvar "clayer" "e-work-hse")

  ; Ask the user block basepoint
  (setvar "osmode" 1)
  (setq p1 (getpoint "\nSpecify the base point of the block: ") )

  (setvar "osmode" 0)
  (setq
    ; Ask the user point to copy the scaled block
    p2 (getpoint "\nSpecify the point to copy and scale the block: ")
    ; Draw a circle of 2m around the selected point to label the block
    ent_name (entmakex
      (list
        (cons 0 "CIRCLE") ; Entity type [ename]
        (cons 10 p2)      ; Circle centre point point [pt]
        (cons 40 2000)    ; Circle radius
      )
    )
    ; Ask user to select the objects to copy, scale and block
    ss (ssget)
    ; Ask user to introduce block name
    blockName (strcat "Plot" (getstring 't "\nPlot... 2017.10.27\nSpecify block name (press Enter to finish): Plot") " 2017.10.27")
  )

  (command "_.scale" ss "" p1 "0.001")
  (command "_.-Block" blockName p1 ss "")
  (command "_.-insert" blockName p2 "" "" "")
  (command "_.-insert" blockName p1 1000 1000 "")
  (command "_.explode" "L")

  ; RESTORE SETTINGS
	(restore_environment)

  (princ)

  ; v0.0 - 2017.02.15 - First issue
  ; Author: David Torralban
  ; Last revision: 2017.02.15
)
