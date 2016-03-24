(defun c:BB ( /
              *error*
              old_osmode old_cmdecho old_clayer
              ss BlockLayer block_name
              pb p_ins
              )
  ; Building Block

  (defun *error* (errmes)
    (setvar "cmdecho" old_osmode)     ; Reset "cmdecho" to previous value
    (setvar "cmdecho" old_cmdecho)    ; Reset "osmode" to previous value
    (setvar "clayer" old_clayer)      ; Reset "clayer" to previous value.
    (princ)
  )

  (setq
    old_clayer (getvar "clayer")
    old_osmode (getvar "osmode")
    old_cmdecho (getvar "cmdecho")
  )

  (setvar "cmdecho" 0)
  (setvar "osmode" 1)

  ;; OPERATION - Change current layer to "e-work-hse", create if doesn't exist yet
  (command "-layer" "m" "e-work-hse" "C" "9" "" "")

  ;; INPUT - Ask the user to specify the block basepoint
  (setq pb (getpoint "\nSelect block base point: "))

  ;; OPERATION - Switch off the snap mode
  (setvar "osmode" 0)

  ;; INPUT - Ask the user to specify the point in where to copy the scaled block
  (setq p_ins (getpoint "\nSelect point to copy and scale the block: "))

  ; Draw a circle of 2m around the selected point to label the block
  (command "_.circle" p_ins "2000")

  ;; INPUT - Ask user to select the objects to copy, scale and block
  (setq ss (ssget))

  ;; INPUT - Ask user to introduce block name
  (setq
    block_name (getstring T "\nIntroduce block name (no commas aloud): ")
    block_name (strcat block_name " " (TODAY))
  )
  (princ (strcat "\nNew block name: " block_name))

  ;; OPERATION - Copy, scale and block the objects.
  (command "_.move" ss "" pb pb)
  (command "_.scale" ss "" pb 0.001)
  (command "_.-Block" block_name pb ss "")
  (command "_.-insert" block_name p_ins "" "" "")
  (command "_.-insert" block_name pb 1000 1000 "")
  (command "._explode" (entlast))

  (setvar "cmdecho" old_osmode)     ; Reset "cmdecho" to previous value
  (setvar "cmdecho" old_cmdecho)    ; Reset "osmode" to previous value
  (setvar "clayer" old_clayer)      ; Reset "clayer" to previous value.

  (princ)
)
