; DO NOT REMOVE THIS LINE. It's a checking.
(defun c:1() (C:BYC))
;(defun c:1() (setvar "clayer" "e-afd-PH2") (fbi2 "0000foul") )
(defun c:2() (setvar "clayer" "e-afd-PH2") (fbi2 "Private-Square475-Foul-Manhole") )
(defun c:3() (setvar "clayer" "e-pfd-PH2") (fbi2 "Private-Square300-Foul-Manhole") )
(defun c:4( / p ent_name)
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
)
(defun c:mygroup ( / i l s )
  (if (setq s (ssget))
    (progn
      (repeat (setq i (sslength s) )
        (setq l (cons (vlax-ename->vla-object (ssname s (setq i (1- i)))) l) )
      )
      (vlax-invoke (vla-add (vla-get-groups (vla-get-activedocument (vlax-get-acad-object))) "*")
        'appenditems l
      )
    )
  )
  (princ)
)
