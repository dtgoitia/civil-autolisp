;;;  PLDIET.lsp [command name: PLD]
;;;  To put lightweight PolyLines on a DIET (remove excess vertices); usually
;;;    used for contours with too many too-closely-spaced vertices.
;;;  Concept from PVD routine [posted on AutoCAD Customization Discussion
;;;    Group by oompa_l, July 2009] by Brian Hailey, added to by CAB, and
;;;    WEED and WEED2 routines by Skyler Mills at Cadalyst CAD Tips [older
;;;    routines for "heavy" Polylines that won't work on newer lightweight ones];
;;;    simplified in entity data list processing, and enhanced in other ways [error
;;;    handling, default values, join collinear segments beyond max. distance,
;;;    limit to current space/tab, account for change in direction across 0 degrees,
;;;    option to keep or eliminate arc segments] by Kent Cooper, August 2009.
;
(defun C:PLD
  (/ *error* cmde disttemp cidtemp arctemp plinc plsel pldata
  ucschanged front 10to42 vinc verts vert1 vert2 vert3)
;
  (defun *error* (errmsg)
    (if (not (wcmatch errmsg "Function cancelled,quit / exit abort"))
      (princ (strcat "\nError: " errmsg))
    ); end if
    (if ucschanged (command "_.ucs" "_prev"))
      ; ^ i.e. don't go back unless routine reached UCS change but didn't change back
    (command "_.undo" "_end")
    (setvar 'cmdecho cmde)
  ); end defun - *error*
;
  (setq cmde (getvar 'cmdecho))
  (setvar 'cmdecho 0)
  (command "_.undo" "_begin")
  (setq
    disttemp
      (getdist
        (strcat
          "\nMaximum distance between non-collinear vertices to straighten"
          (if *distmax* (strcat " <" (rtos *distmax* 2 2) ">") ""); default only if not first use
          ": "
        ); end strcat
      ); end getdist & disttemp
    *distmax*
      (cond
        (disttemp); user entered number or picked distance
        (T *distmax*); otherwise, user hit Enter - keep value
      ); end cond & *distmax*
    cidtemp
      (getangle
        (strcat
          "\nMaximum change in direction to straighten"
          (strcat ; offer prior choice if not first use; otherwise 15 degrees
            " <"
            (if *cidmax* (angtos *cidmax*) (angtos (/ pi 12)))
            ">"
          ); end strcat
          ": "
        ); end strcat
      ); end getdist & cidtemp
    *cidmax*
      (cond
        (cidtemp); user entered number or picked angle
        (*cidmax*); Enter with prior value set - use that
        (T (/ pi 12)); otherwise [Enter on first use] - 15 degrees
      ); end cond & *cidmax*
    plinc 0 ; incrementer through selection set of Polylines
  ); end setq
  (initget "Retain Straighten")
  (setq
    arctemp
      (getkword
        (strcat
          "\nRetain or Straighten arc segments [R/S] <"
          (if *arcstr* (substr *arcstr* 1 1) "S"); at first use, S default; otherwise, prior choice
          ">: "
        ); end strcat
      ); end getkword
    *arcstr*
      (cond
        (arctemp); if User typed something, use it
        (*arcstr*); if Enter and there's a prior choice, keep that
        (T "Straighten"); otherwise [Enter on first use], Straighten
      ); end cond & *arcstr*
  ); end setq
;
  (prompt "\nSelect LWPolylines to put on a diet, or press Enter to select all: ")
  (cond
    ((setq plsel (ssget '((0 . "LWPOLYLINE"))))); user-selected Polylines
    ((setq plsel (ssget "X" (list '(0 . "LWPOLYLINE") (cons 410 (getvar 'ctab))))))
      ; all Polylines [in current space/tab only]
  ); end cond
;
  (repeat (sslength plsel)
    (setq pldata (entget (ssname plsel plinc)))
    (if (/= (cdr (last pldata)) (trans '(0 0 1) 1 0)); extr. direction not parallel current CS
        ; for correct angle & distance calculations [projected onto current construction
        ; plane], since 10-code entries for LWPolylines are only 2D points:
      (progn
        (command "_.ucs" "_new" "_object" (ssname plsel plinc)) ; set UCS to match object
        (setq ucschanged T) ; marker for *error* to reset UCS if routine doesn't
      ); end progn
    ); end if
    (setq
      front ; list of "front end" [pre-vertices] entries, minus entity names & handle
        (vl-remove-if
          '(lambda (x)
            (member (car x) '(-1 330 5 10 40 41 42 210))
          ); end lambda
          pldata
        ); end removal & front
      10to42 ; list of all code 10, 40, 41, 42 entries only
        (vl-remove-if-not
          '(lambda (x)
            (member (car x) '(10 40 41 42))
          ); end lambda
          pldata
        ); end removal & 10to42
      vinc (/ (length 10to42) 4); incrementer for vertices within each Polyline
      verts nil ; eliminate from previous Polyline [if any]
    ); end setq
    (if (= *arcstr* "Straighten")
      (progn
        (setq bulges ; find any bulge factors
          (vl-remove-if-not
            '(lambda (x)
              (and
                (= (car x) 42)
                (/= (cdr x) 0.0)
              ); end and
            ); end lambda
            10to42
          ); end removal & bulges
        ); end setq
        (foreach x bulges (setq 10to42 (subst '(42 . 0.0) x 10to42)))
          ; straighten all arc segments to line segments
      ); end progn
    ); end if
    (repeat vinc
      (setq
        verts ; sub-group list: separate list of four entries for each vertex
          (cons
            (list
              (nth (- (* vinc 4) 4) 10to42)
              (nth (- (* vinc 4) 3) 10to42)
              (nth (- (* vinc 4) 2) 10to42)
              (nth (1- (* vinc 4)) 10to42)
            ); end list
            verts
          ); end cons & verts
        vinc (1- vinc) ; will be 0 at end
      ); end setq
    ); end repeat
    (while (nth (+ vinc 2) verts); still at least 2 more vertices
      (if
        (or ; only possible if chose to Retain arc segments
          (/= (cdr (assoc 42 (nth vinc verts))) 0.0); next segment is arc
          (/= (cdr (assoc 42 (nth (1+ vinc) verts))) 0.0); following segment is arc
        ); end or
        (setq vinc (1+ vinc)); then - don't straighten from here; move to next
        (progn ; else - analyze from current vertex
          (setq
            vert1 (cdar (nth vinc verts)) ; point-list location of current vertex
            vert2 (cdar (nth (1+ vinc) verts)); of next one
            vert3 (cdar (nth (+ vinc 2) verts)); of one after that
            ang1 (angle vert1 vert2)
            ang2 (angle vert2 vert3)
          ); end setq
          (if
            (or
              (equal ang1 ang2 0.0001); collinear, ignoring distance
              (and
                (<= (distance vert1 vert3) *distmax*)
                  ; straightens if direct distance from current vertex to two vertices later is
                  ; less than or equal to maximum; if preferred to compare distance along
                  ; Polyline through intermediate vertex, replace above line with this:
                  ; (<= (+ (distance vert1 vert2) (distance vert2 vert3)) *distmax*)
                (<=
                  (if (> (abs (- ang1 ang2)) pi); if difference > 180 degrees
                    (+ (min ang1 ang2) (- (* pi 2) (max ang1 ang2)))
                      ; then - compensate for change in direction crossing 0 degrees
                    (abs (- ang1 ang2)); else - size of difference
                  ); end if
                  *cidmax*
                ); end <=
              ); end and
            ); end or
            (setq verts (vl-remove (nth (1+ vinc) verts) verts))
              ; then - remove next vertext, stay at current vertex for next comparison
            (setq vinc (1+ vinc)); else - leave next vertex, move to it as new base
          ); end if - distance & change in direction analysis
        ); end progn - line segments
      ); end if - arc segment check
    ); end while - working through vertices
    (setq
      front (subst (cons 90 (length verts)) (assoc 90 front) front)
        ; update quantity of vertices for front end
      10to42 nil ; clear original set
    ); end setq
    (foreach x verts (setq 10to42 (append 10to42 x)))
      ; un-group four-list vertex sub-lists back to one list of all 10, 40, 41, 42 entries
    (setq pldata (append front 10to42 (list (last pldata))))
      ; put front end, vertex entries and extrusion direction back together
    (entmake pldata)
    (entdel (ssname plsel plinc)); remove original
    (setq plinc (1+ plinc)); go on to next Polyline
    (if ucschanged
      (progn
        (command "_.ucs" "_prev")
        (setq ucschanged nil) ; eliminate UCS reset in *error* since routine did it already
      ); end progn
    ); end if - UCS reset
  ); end repeat - stepping through set of Polylines
  (command "_.undo" "_end")
  (setvar 'cmdecho cmde)
  (princ)
); end defun - PLD
(prompt "\nType PLD to put PolyLines on a Diet.")
