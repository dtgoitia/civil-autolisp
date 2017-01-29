(defun c:1()
  (RR
    (car (entsel "\nSelect object to align:"))
    (car (entsel "\nSelect object to align to:"))
  )
; PURPOSE: align text to selected object, keeping readability
; RR (fast shortcut) will be the current RTM
;  - keep RT for other purposes
; Procedure:
;  - clic 1 = select entity (single)
;  - clic 2 = select reference
;    entity aligns to reference and starts following the mouse
;  - clic 3 / Enter / Space / Esc = stop dragging entity and finish command
)
(defun DT:RR ( ent_name1 ent_name2 )
  ; Align ent_name1 to ent_name2 dynamically
  ; Return aligned and reference entity names
  ; or nil if something wrong
  ; ent_name1 [ename] - Object to align
  ; ent_name2 [ename] - Reference object

  ; SAVE SETTINGS
  ; (save_environment (list "osmode" "angdir" "angbase"))

  (if (and ent_name1 ent_name2)
    (if (and (= 'ename (type ent_name1)) (= 'ename (type ent_name2)))
      (princ "\nUnder construction...")
      (cond
        ((/= 'ename (type ent_name1)) (princ "\nERROR @ DT:RR > ent_name1 type is incorreect")(princ))
        ((/= 'ename (type ent_name2)) (princ "\nERROR @ DT:RR > ent_name2 type is incorreect")(princ))
      );END cond
    );END if
    (cond
      ((not ent_name1) (princ "\nERROR @ DT:RR > ent_name1 = nil")(princ))
      ((not ent_name2) (princ "\nERROR @ DT:RR > ent_name2 = nil")(princ))
    );END cond
  );END if
  ; Get object, cases:
    ; If ent_name2 is line
    ; If ent_name2 is polyline: allow perpendicular to straight segments and curve segments along all polyline, not just the segment where you have clicked
    ; If ent_name2 is circle > tangent
    ; If ent_name2 is arc
    ; If ent_name2 is ellipse
    ; If ent_name2 is INSERT, text or Mtext > just copy rotation
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
