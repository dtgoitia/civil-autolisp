; DO NOT REMOVE THIS LINE. It's a checking.
;(alert "nothing saved!")
(defun c:1( / p )
  (setq p (getpoint "\nSelect manhole insertion point: "))
  (setvar "osmode" 0)
  (command "._insert" "SW-Manhole" (DT:flatPoint p) "1" "1")
  (while (> (getvar "CMDACTIVE") 0) (command ""))
  (setvar "osmode" 4)
)
(defun c:4() (DT:RoadOffsetForDrainage) )
(defun DT:RoadOffsetForDrainage( / p ent_name ent_name_block)
  ; Select a polyline within an Xref, and offset it by 0.5m and 1m either side
  (c:N)
  ; Check if groups exists
  (setq
    ent_name (entlast)
    ent_name_block
      (entmakex
        (list
          (cons 0 "BLOCK")
          (cons 10 (list 0 0 0))
          (cons 2 "lalalala")
        )
      )
  )
  (vla-offset (vlax-ename->vla-object ent_name) 0.5)
  (command "_groupedit" "N" "road_offsets" "A" (entlast) "")
  (vla-offset (vlax-ename->vla-object ent_name) 1)
  (command "_groupedit" "N" "road_offsets" "A" (entlast) "")
  (vla-offset (vlax-ename->vla-object ent_name) -0.5)
  (command "_groupedit" "N" "road_offsets" "A" (entlast) "")
  (vla-offset (vlax-ename->vla-object ent_name) -1)
  (command "_groupedit" "N" "road_offsets" "A" (entlast) "")
  (vla-delete (vlax-ename->vla-object ent_name))
  (princ)
)
