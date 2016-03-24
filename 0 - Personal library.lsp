(defun c:gate ()
  (command "-insert" "Access Gate" pause 0.001 0.001 pause)
)
(defun c:ha ( / ss i ent ent_name VL_ent_name)
  ; automatic HAtching
  ; This function selects the polylines within the selection set,
  ; closes them and creates an individual associative hatch for each polyline
  ;
  ; Load VLA
  (vl-load-com)

  ; INPUT - Select objects and keep just polylines
  (setq ss (ssget '((-4 . "<OR") (0 . "LWPOLYLINE") (0 . "POLYLINE") (-4 . "OR>"))))

  ; OPERATION - Loop to close
  (setq i 0)
  (while (< i (sslength ss) )
    (setq
      ent_name (ssname ss i)
      VL_ent_name (vlax-ename->vla-object ent_name)
    )
    (if (= :vlax-false (vla-get-closed VL_ent_name)) (vla-put-closed VL_ent_name :vlax-true)) ; close if opened
    (command "-hatch" "P" "ANSI31" "0.1" "" "A" "A" "Y" "" "S" ent_name "" "")                ; add hatch
    (setq i (+ i 1))                                                                          ; continue to next polyline
  ); END while

  ; v0.0 - 2016.03.16 - First issue
  ; Author: David Torralba
  ; Last revision: 2016.03.16
)
(defun c:BY ( / ss)
  ; Set by layer
  (setq ss (ssget))
  (command "._SetByLayer" ss "" "y" "y")

  ; v0.0 - 2016.03.18 - First issue
  ; Author: David Torralba
  ; Last revision: 2016.03.18
)
(defun c:au ()
  ; Fast audit and safe
  (command "audit" "Yes")
  (command "qsave")
  (princ)

  ; v0.0 - 2016.03.18 - First issue
  ; Author: David Torralba
  ; Last revision: 2016.03.18
)
(defun c:cl ( / ss i ent ent_name VL_ent_name)
  ; Load VLA
  (vl-load-com)
  ; INPUT - Select entity
  (setq ss (ssget '((-4 . "<OR")
                    (0 . "LWPOLYLINE")
                    (0 . "POLYLINE")
                    (-4 . "OR>")
                   )
           )
  )
  ; OPERATION - Loop to close
  (setq i 0)
  (while (< i (sslength ss) )
    (setq
      ent_name (ssname ss i)
      VL_ent_name (vlax-ename->vla-object ent_name)
    )
    (if (= :vlax-false (vla-get-closed VL_ent_name)) (vla-put-closed VL_ent_name :vlax-true))
    (setq i (+ i 1))
  ); END while

  ; v0.0 - 2016.03.16 - First issue
  ; Author: David Torralba
  ; Last revision: 2016.03.16
)
(defun c:nested_name ( / e obj blkName)
  (while T
    (while (/=(type(setq e (car(last(nentsel "\nSelect any BLOCK to now its name: "))))) 'ENAME))
    (setq obj (vlax-ename->vla-object e))
      (if (= (vlax-get-property obj 'ObjectName) "AcDbBlockReference")
        (setq
          blkName (vlax-get-property obj (if (vlax-property-available-p obj 'effectivename)
                                              'effectivename
                                              'name
                                         ); END if
                  ); END vlax-get-property
        ); END setq
      ); END if
      (princ (strcat "\nBlock Name is: " BlkName))
  )
)
(defun c:info (/ opt obj)
  ;; VLA & DXF Info of selected Primary or Nested object
  ;; Alan J. Thompson
  (initget 0 "Nested Primary")
  (setq opt (cond ((getkword "\nSpecify selection mode [Nested/Primary] <Primary>: "))
                  ("Primary")
            )
  )
  (if (setq obj (car ((if (eq opt "Primary")
                        entsel
                        nentsel
                      )
                       (strcat "\nSelect " opt " object for VLA & DXF info: ")
                     )
                )
      )
    (progn
      (textscr)
      (princ "\nVLA Info:\n\n")
      (vlax-dump-object (vlax-ename->vla-object obj) T)
      (princ "\nDXF Info:\n")
      (mapcar 'print (entget obj '("*")))
    ); END progn
  ); END if
  (princ)
)
(defun c:re ( / ss i ent ent_name VL_ent_name)
  ; Load VLA
  (vl-load-com)
  ; INPUT - Select entity
  (setq ss (ssget '((-4 . "<OR")
                    (0 . "LWPOLYLINE")
                    (0 . "POLYLINE")
                    (-4 . "OR>")
                   )
           )
  )
  ; OPERATION - Loop to close
  (setq i 0)
  (while (< i (sslength ss) )
    (setq ent_name (ssname ss i))
    (command "PEDIT" ent_name "R" "")
    (setq i (+ i 1))
  ); END while

  ; v0.0 - 2016.03.22 - First issue
  ; Author: David Torralba
  ; Last revision: 2016.03.22
)
