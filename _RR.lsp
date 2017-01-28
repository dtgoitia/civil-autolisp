(defun c:1()
  (RR
    (car (entsel "\nSelect object to align:"))
    (car (entsel "\nSelect object to align to:"))
  )
; PURPOSE: align text to selected object, keeping readability
)
(defun RR ( ent_name1 ent_name2 )
  ; Align ent_name1 to ent_name2 dynamically
  ; Return aligned and reference entity names
  ; or nil if something wrong
  ; ent_name1 [ename] - Object to align
  ; ent_name2 [ename] - Reference object

  ; SAVE SETTINGS
  ; (save_environment (list "osmode" "angdir" "angbase"))

  ; Get object, cases:
    ; If ent_name2 is line
    ; If ent_name2 is polyline: allow perpendicular to straight segments and curve segments along all polyline, not just the segment where you have clicked
    ; If ent_name2 is circle
    ; If ent_name2 is arc
    ; If ent_name2 is ellipse
    ; If ent_name2 is block
    ; If ent_name2 is 3Dpoly
    ; If ent_name2 is xline
    ; if ent_name2 is ray
  ; object can be nested, so use nentselp


  ; Keep perpendicular on real time, and move at the same time as you move the mouse

  ; Clic, enter, space, Esc to drop the text and stop modification

  ; RESTORE SETTINGS
  ; (restore_environment)

  (princ)

  ; v0.0 - 2017-01-29 - First issue
  ; Author: David Torralba
  ; Last revision: 2017-01-29
);END defun
(defun c:1() (c:name) )
(defun c:MIRROR-name ( / old_error old_sysvars )

  ; SAVE SETTINGS
  ; (save_environment (list "osmode" "angdir" "angbase"))

  --- start here your program ---

  ; RESTORE SETTINGS
  ; (restore_environment)

  (princ)

  ; v0.0 - _DATE_ - First issue
  ; Author: David Torralban
  ; Last revision: _DATE_
);END defun
