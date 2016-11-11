(defun c:1()
  ; Set up my personal environment
  (setvar "DYNMODE" -1)             ; pointer dynamic input mode to -1
  (setvar "VTDURATION" 0)           ; zoom sin transicion
  (setvar "VTFPS" 1)                ; minimum speed of a smooth zoom transition, in frames per second
  (setvar "MBUTTONPAN" 1)           ; supports panning when you hold and drag the button or wheel
  (setvar "COORDS 2")               ; shows coordinates, and polar ones if possible
  (setvar "PEDITACCEPT" 1)          ; automatically convert lines and arcs to polylines
  (setvar "EXPERT" 2)               ; don’t ask when switching off current layer, etc.
  (setvar "SECURELOAD" 0)           ; don’t ask and just load LISP files
  (setvar "CURSORSIZE" 100)         ; crosshairs extend to viewport limits
  (setvar "GRIDMODE" 0)             ; crosshairs extend to viewport limits
  (setvar "GRIPS" 2)                ; object's grip mode
  (setvar "GRIPSIZE" 5)             ; object's grip size
  (setvar "LUNITS" 2)               ; decimal length units
  (setvar "AUNITS" 0)               ; units for angles: decimal degrees
  (setvar "LUPREC" 3)               ; 3 decimals of precision for units
  (setvar "MEASUREINIT" 1)          ; Metric; uses the hatch pattern file and linetype file designated by the ISOHatch and ISOLinetype registry settings
  (setvar "MEASUREMENT" 1)          ; Metric; uses the hatch pattern file and linetype file designated by the ISOHatch and ISOLinetype registry settings
  (setvar "STARTUP" 0)              ; Starts a drawing without defined settings
  (setvar "APERTURE" 10)            ; Controls the object snap target box size (default 10)
  (setvar "PICKBOX" 5)              ; Crosshair square size to 5 (default is 3)
  (setvar "MIRRTEXT" 0)             ; Keep text readable when mirroring it
  (setvar "PICKFIRST" 1)            ; You can also select objects before you start a command
  (setvar "PICKADD" 2)              ; Selection and command execution managent
  (setvar "SHORTCUTMENU" 2)         ; Mouse right clic settings
  (setvar "ZOOMWHEEL" 0)            ; Moves wheel forward zooms in; moving wheel backwards zooms out.
  (setvar "ZOOMFACTOR" 60)          ; Controls how much the magnification changes when the mouse wheel moves forward or backward.
  (setvar "OSOPTIONS" 7)            ; Controls if to OSNAP lines in hatch patterns, and many others.
  (setvar "LAYLOCKFADECTL" 0)       ; Controls locked layers fade (normal 0-100 fade)
  (setvar "ISAVEBAK" 0)             ; Controls the creation of a backup file (BAK): 0 no, 1 yes.


);END defun
