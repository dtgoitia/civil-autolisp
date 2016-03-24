(defun c:ep ( / *error* sset itm num hnd ent width)
  ; SET - Error handling function
  (defun *error* ( msg )
    (if (not (member msg '("Function cancelled" "quit / exit abort")))
      (princ (strcat "\nError: " msg))
    )
    ; RESET to original "osmode" and "cmdecho".
    (setvar "osmode" oldosmode)
    (setvar "cmdecho" oldcmdecho)
    (princ)
  )

  ; SAVE "osmode" and "cmdecho"
  (setq oldosmode (getvar "osmode"))
  (setq oldcmdecho (getvar "cmdecho"))

  ; CHANGE "osmode" and "cmdecho"
  (setvar "osmode" 0)
  (setvar "cmdecho" 0)

  ; INPUT - Select objects, and keep just polylines
  (setq sset (ssget '((-4 . "<OR") (0 . "POLYLINE") (0 . "LWPOLYLINE") (-4 . "OR>"))))

  ; OPERATION - If something (sset) has been selected...
  (if sset
    (progn
      (setq
        itm 0               ; Counter of items to 0
        num (sslength sset) ; Count selected objects
      )

      ; OPERATION - Run through all selected objects
      (while (< itm num)
        (setq
          hnd (ssname sset itm)       ; object name
          ent (entget hnd)            ; object property list
          width (cdr (assoc 43 ent))  ; polyline "Global width" property
        )

        ; OPERATION - Explode the polyline
        (command "._explode" hnd "")

        ; OPERATION - Select the lines after exploding the polyline, convert to polyline and modify the width
        (command "PEDIT" "_M" "_P" "" "Y" "W" width "")

        ; OPERATION - Step into the next selected object
        (setq itm (1+ itm))
      ) ; END WHILE
    ); END progn
  ) ; END - If something (sset) has been selected...

  ; RESET to original "osmode" and "cmdecho".
  (setvar "osmode" oldosmode)
  (setvar "cmdecho" oldcmdecho)

  ; End without double messages
  (princ)

  ; v0.2 - 2016.03.21 - Code optimized and comments translated into English.
  ; v0.1 - 2016.03.02 - Command-line name changed from BRP to EP.
  ;                     Variables added as local variables not to overlap with other routines.
  ; v0.0 - 2016.02.16 - First issue
  ; NOTE: It supports heavy and light polylines.
  ; NOTE: Can be used in polylines with 2 vertexes in the same position.
  ; NOTE: No problem with closed polylines.
  ; Author: David Torralba
  ; Last revision: 2016.03.21
)
