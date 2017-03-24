(defun c:loadCIVILblocks()
  ; OPERATION - Import blocks from master drawing, and explode and remove them
  (command "-insert" "MJA_Block_master_drawing" "0,0" 1 1 0 "erase" "L" "")
  (command "-purge" "B" "MJA_Block_master_drawing" "N")
  (princ)
  ; v0.0 - 2016.04.07 - First issue
  ; Author: David Torralba
  ; Last revision: 2016.04.07
)
;
;---------------------------------------------------------------------------
;
; BLOCK INSERTION MASTER FUNCTION
;
;---------------------------------------------------------------------------
(defun DT:ib (blk lay rot osm
            /
            *error*
            oldosmode oldclayer oldattdia oldattreq oldinsunits oldinsunitsdeftarget oldinsunitsdefsource
            )
  ; Inserts the block according to the following set of parameters
  ; (DT:ib block_name layer_name rotation_value osmode_value)
  ;   layer_name = nil      --> use current layer
  ;   rotation_value = ""   --> rotation = 90 degree
  ;   rotation_value = "P"  --> let user select rotation
  ;   osmode_value = nil    --> use current osmode

  ; ERROR HANDLING FUNCTION
  (defun *error* ( msg )
    (if (not (member msg '("Function cancelled" "quit / exit abort"))) (princ (strcat "\nError: " msg)))
    (setvar "attdia" oldattdia)
    (setvar "attreq" oldattreq)
    (if (/= oldclayer nil)            (setvar "clayer" oldclayer))
    (if (/= oldosmode nil)            (setvar "osmode" oldosmode))
    (if (/= oldinsunits nil)          (setvar "insunits" oldinsunits))
    (if (/= oldinsunitsdeftarget nil) (setvar "insunitsdeftarget" oldinsunitsdeftarget))
    (if (/= oldinsunitsdefsource nil) (setvar "insunitsdefsource" oldinsunitsdefsource))
    (princ)
  )

  ; SAVE OLD SYSTEM VARIABLES
  (setq
    oldattdia (getvar "attdia")
    oldattreq (getvar "attreq")
    oldinsunits (getvar "insunits")
    oldinsunitsdeftarget (getvar "insunitsdeftarget")
    oldinsunitsdefsource (getvar "insunitsdefsource")
    oldclayer (getvar "clayer")
    oldosmode (getvar "osmode")
  )

  ; MODIFY SETTINGS
  (setvar "attdia" 0)
  (setvar "attreq" 0)
  (setvar "insunits" 0)
  (setvar "insunitsdeftarget" 0)
  (setvar "insunitsdefsource" 0)

  ; Chek if block exists
  (if (tblsearch "block" blk)
    ; MAIN ROUTINE
    (progn
      ; Layer control
      (if lay (command "-layer" "M" lay "") )

      ; OSMODE control
      (if osm (setvar "osmode" osm) )

      ; Rotation control
      (cond
        ; Set rotation to 90º
        ( (= rot nil)
          (command "-insert" blk pause 1 1 0)
        )
        ; Allow user to input rotation
        ( (= rot "P")
          (command "-insert" blk pause 1 1 pause)
        )
        ; Fixed rotation, no user input
        ( (and (/= rot nil) (/= rot "P"))
          (princ "\nrot")
          (command "-insert" blk pause 1 1 0)
        )
      );END cond
    );END progn
    (if (findfile (strcat blk ".dwg") )
      (command "-insert" (strcat blk ".dwg") pause 1 1 pause)
      (alert "Sorry, this block is not loaded.\nGo to:\nMenu bar\n   MJA Engineering\n      Load all blocks\nThis will load all the missing blocks.")
    );END if
  );END if

  ; RESTORE PREVIOUS SETTINGS
  (setvar "attdia" oldattdia)
  (setvar "attreq" oldattreq)
  (setvar "insunits" oldinsunits)
  (setvar "insunitsdeftarget" oldinsunitsdeftarget)
  (setvar "insunitsdefsource" oldinsunitsdefsource)
  (setvar "clayer" oldclayer)
  (setvar "osmode" oldosmode)
  (princ)

  ; v0.6 - 2017.03.24 - Bug fixed: look of unloaded blocks separated in their own drawings
  ; v0.5 - 2017.03.13 - Minor bug fixed when rot = nil
  ; v0.4 - 2016.12.02 - Function argument interpretation updated
  ;                   - System variable managment updated
  ; v0.3 - 2016.11.30 - Check if the block exists before you insert it
  ; v0.2 - 2016.07.28 - Update system variable management
  ; v0.1 - 2016.04.15 - ATTDIA and ATTREQ system variable control added
  ; v0.0 - 2016.04.14 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.03.24
)
;
;---------------------------------------------------------------------------
;
; TITLE BLOCK INSERTION MASTER FUNCTION
;
;---------------------------------------------------------------------------
(defun title_block_date ( / d yr mo )
  ; Title block date function
  (setq
    d (rtos (getvar "CDATE") 2 6) ;get the date and time and convert to text
    yr (substr d 3 2)             ;extract the year
    mo (substr d 5 2)             ;extract the month
  )
  (cond
    ((= mo "01") (setq mo "Jan"))
    ((= mo "02") (setq mo "Feb"))
    ((= mo "03") (setq mo "Mar"))
    ((= mo "04") (setq mo "Apr"))
    ((= mo "05") (setq mo "May"))
    ((= mo "06") (setq mo "Jun"))
    ((= mo "07") (setq mo "Jul"))
    ((= mo "08") (setq mo "Aug"))
    ((= mo "09") (setq mo "Sep"))
    ((= mo "10") (setq mo "Oct"))
    ((= mo "11") (setq mo "Nov"))
    ((= mo "12") (setq mo "Dec"))
  ); END cond
  (strcat mo "\'" yr)
  ; v0.0 - 2016.03.29 - First issue
  ; Author: David Torralba
  ; Last revision: 2016.03.29
)
(defun title_block_insert ( blk /  )
  (if (DT:CheckIfBlockExists blk)
    (command "-insert" blk "0,0" 1 1 0 "" "" "" "" "" "" (title_block_date) "" "" "" "" "PRELIMINARY" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "")
    (alert "Sorry, this block is not loaded.\nGo to:\nMenu bar\n   MJA Engineering\n      Load all blocks\nThis will load all the missing blocks.")
  );END if
  ; v0.1 - 2016.12.02 - Check if the block exists before you insert it
  ; v0.0 - 2016.03.29 - First issue
  ; Author: David Torralba
  ; Last revision: 2016.12.02
)
(defun title_block_D_insert ( blk / oldosmode )
  (if (DT:CheckIfBlockExists blk)
    (progn
      (setq oldosmode (getvar "osmode"))
      (setvar "osmode" 0)
      (command "-insert" blk "0,0" 1 1 0 "" "" "" "" "" "" (title_block_date) "" "" "" "" "PRELIMINARY")
      (while (> (getvar "CMDACTIVE") 0) (command ""))
      (vla-put-layer (vlax-ename->vla-object (entlast)) "MJA-Title")
      (vlax-put-property (vlax-ename->vla-object (entlast)) 'Color 256)
      (setvar "osmode" oldosmode)
    );END progn
    (alert "Sorry, this block is not loaded.\nGo to:\nMenu bar\n   MJA Engineering\n      Load all blocks\nThis will load all the missing blocks.")
  );END if
  (princ)
  ; v0.2 - 2016.12.02 - Check if the block exists before you insert it
  ; v0.1 - 2016.08.23 - OSMODE, layer and color management added
  ; v0.0 - 2016.03.29 - First issue
  ; Author: David Torralba
  ; Last revision: 2016.12.02
)
(defun c:MJA_A0_landscape () (title_block_insert "A0-Landscape") (princ))
(defun c:MJA_A1_landscape () (title_block_insert "A1-Landscape") (princ))
(defun c:MJA_A2_landscape () (title_block_insert "A2-Landscape") (princ))
(defun c:MJA_A3_landscape () (title_block_insert "A3-Landscape") (princ))
(defun c:MJA_A0_Portrait () (title_block_insert "A0-Portrait") (princ))
(defun c:MJA_A1_Portrait () (title_block_insert "A1-Portrait") (princ))
(defun c:MJA_A2_Portrait () (title_block_insert "A2-Portrait") (princ))
(defun c:MJA_A3_Portrait () (title_block_insert "A3-Portrait") (princ))
(defun c:MJA_A4_landscape ( / oldosmode )
  (if (DT:CheckIfBlockExists blk)
    (progn
      (setq oldosmode (getvar "osmode"))
      (setvar "osmode" 0)
      (command "-insert" "A4-Landscape" "0,0" 1 1 0 "" "" "" "" "" "" (title_block_date) )
      (while (> (getvar "CMDACTIVE") 0) (command ""))
      (vla-put-layer (vlax-ename->vla-object (entlast)) "MJA-Title")
      (vlax-put-property (vlax-ename->vla-object (entlast)) 'Color 256)
      (setvar "osmode" oldosmode)
    );END progn
    (alert "Sorry, this block is not loaded.\nGo to:\nMenu bar\n   MJA Engineering\n      Load all blocks\nThis will load all the missing blocks.")
  );END if
  (princ)
)
(defun c:MJA_A4_portrait ( / oldosmode )
  (if (DT:CheckIfBlockExists blk)
    (progn
      (setq oldosmode (getvar "osmode"))
      (setvar "osmode" 0)
      (command "-insert" "A4-Portrait" "0,0" 1 1 0 "" "" "" "" "" "" (title_block_date) )
      (while (> (getvar "CMDACTIVE") 0) (command ""))
      (vla-put-layer (vlax-ename->vla-object (entlast)) "MJA-Title")
      (vlax-put-property (vlax-ename->vla-object (entlast)) 'Color 256)
      (setvar "osmode" oldosmode)
    );END progn
    (alert "Sorry, this block is not loaded.\nGo to:\nMenu bar\n   MJA Engineering\n      Load all blocks\nThis will load all the missing blocks.")
  );END if
  (princ)
)
(defun c:MJA_A0_landscape_D () (title_block_D_insert "A0-Landscape_D") (princ))
(defun c:MJA_A1_landscape_D () (title_block_D_insert "A1-Landscape_D") (princ))
(defun c:MJA_A2_landscape_D () (title_block_D_insert "A2-Landscape_D") (princ))
(defun c:MJA_A3_landscape_D () (title_block_D_insert "A3-Landscape_D") (princ))
(defun c:MJA_A0_Portrait_D () (title_block_D_insert "A0-Portrait_D") (princ))
(defun c:MJA_A1_Portrait_D () (title_block_D_insert "A1-Portrait_D") (princ))
(defun c:MJA_A2_Portrait_D () (title_block_D_insert "A2-Portrait_D") (princ))
(defun c:MJA_A3_Portrait_D () (title_block_D_insert "A3-Portrait_D") (princ))
(defun c:MJA_A4_landscape_D ( / oldosmode )
  (if (DT:CheckIfBlockExists blk)
    (progn
      (setq oldosmode (getvar "osmode"))
      (setvar "osmode" 0)
      (command "-insert" "A4-Landscape_D" "0,0" 1 1 0 "" "" "" "" "" "" (title_block_date) )
      (while (> (getvar "CMDACTIVE") 0) (command ""))
      (vla-put-layer (vlax-ename->vla-object (entlast)) "MJA-Title")
      (vlax-put-property (vlax-ename->vla-object (entlast)) 'Color 256)
      (setvar "osmode" oldosmode)
    );END progn
    (alert "Sorry, this block is not loaded.\nGo to:\nMenu bar\n   MJA Engineering\n      Load all blocks\nThis will load all the missing blocks.")
  );END if
  (princ)
)
(defun c:MJA_A4_portrait_D ( / oldosmode )
  (if (DT:CheckIfBlockExists blk)
    (progn
      (setq oldosmode (getvar "osmode"))
      (setvar "osmode" 0)
      (command "-insert" "A4-Portrait_D" "0,0" 1 1 0 "" "" "" "" "" "" (title_block_date) )
      (while (> (getvar "CMDACTIVE") 0) (command ""))
      (vla-put-layer (vlax-ename->vla-object (entlast)) "MJA-Title")
      (vlax-put-property (vlax-ename->vla-object (entlast)) 'Color 256)
      (setvar "osmode" oldosmode)
    );END progn
    (alert "Sorry, this block is not loaded.\nGo to:\nMenu bar\n   MJA Engineering\n      Load all blocks\nThis will load all the missing blocks.")
  );END if
  (princ)
)
(defun c:revision_box_D() (DT:ib "Revision-box" "MJA-Title" nil nil))
; v0.1 - 2016.04.07 - Dynamic functins added
; v0.0 - 2016.03.29 - First issue
; Author: David Torralba
; Last revision: 2016.04.07
;
; North-Arrow insertion function
(defun c:NorthArrow() (DT:ib "North-Arrow" "MJA-Title" nil nil))
;---------------------------------------------------------------------------
;
; DRAINAGE INSERTION AND DRAWING FUNCTIONS
;
;---------------------------------------------------------------------------
; Private sewer - Master routine
(defun DT:drainage_line ( lay osm / *error* old_osmode old_clayer old_cmdecho)
  (defun *error* ( msg )
    (if (not (member msg '("Function cancelled" "quit / exit abort"))) (princ (strcat "\nError: " msg)))
    (setvar "osmode" old_osmode)
    (setvar "clayer" old_clayer)
    (setvar "cmdecho" old_cmdecho)
    (princ)
  )
  (setq
    old_osmode (getvar "osmode")
    old_clayer (getvar "clayer")
    old_cmdecho (getvar "cmdecho")
  )
  (setvar "osmode" osm) ; Was 679 before
  (setvar "cmdecho" 0)
  (command "-layer" "M" lay "")
  (command "line")
  (while (> (getvar "CMDACTIVE") 0) (command pause))

  (setvar "osmode" old_osmode)
  (setvar "clayer" old_clayer)
  (setvar "cmdecho" old_cmdecho)
  (princ)
)
; Private sewer - Foul
(defun c:pfd() (DT:drainage_line "e-pfd" 4))

; Private sewer - Storm
(defun c:psd() (DT:drainage_line "e-psd" 132))

; Private blocks - Foul OLD
(defun c:svp()    (DT:ib "e-pfd-svp"                        "e-pfd"                   nil 5))   ; SVP
(defun c:svp300() (DT:ib "Private-Square300-Foul-Manhole"   "e-pfd"                   nil 5))   ; Private Square 300 Foul Manhole
(defun c:svp475() (DT:ib "Private-Square475-Foul-Manhole"   "e-pfd"                   nil 5))   ; Private Square 475 Foul Manhole
(defun c:svp600() (DT:ib "Adoptable-Round600-Foul-Manhole"  "e-pfd-adoptable-lateral" nil 5))   ; Private Square 600 Foul Manhole

; Private blocks - Storm OLD
(defun c:rwp()    (DT:ib "e-psd-rwp"                        "e-psd"                   nil 5))   ; RWP
(defun c:rwp2()   (DT:ib "Private-Round475-Storm-Manhole"   "e-psd"                   nil 5))   ; Private round storm manhole
(defun c:rwp3()   (DT:ib "Private-Round300-Storm-Manhole"   "e-psd"                   nil 5))   ; Private round storm manhole
(defun c:reye()   (DT:ib "Rodding-Eye"                      "e-psd"                   "P" 129)) ; Rodding eye
(defun c:krwp()   (c:rwp) (c:psd))

; Private blocks - Generic
(defun c:PrivateManhole_315mm_1()  (DT:ib "Manhole-315-1"  nil "P" nil))  ; Private Circular 315 Manhole - 1 inlet
(defun c:PrivateManhole_315mm_2a() (DT:ib "Manhole-315-2a" nil "P" nil))  ; Private Circular 315 Manhole - 2 inlets (A)
(defun c:PrivateManhole_315mm_2b() (DT:ib "Manhole-315-2b" nil "P" nil))  ; Private Circular 315 Manhole - 2 inlets (B)
(defun c:PrivateManhole_315mm_3a() (DT:ib "Manhole-315-3a" nil "P" nil))  ; Private Circular 315 Manhole - 3 inlets (A)
(defun c:PrivateManhole_315mm_3b() (DT:ib "Manhole-315-3b" nil "P" nil))  ; Private Circular 315 Manhole - 3 inlets (B)
(defun c:PrivateManhole_315mm_4a() (DT:ib "Manhole-315-4a" nil "P" nil))  ; Private Circular 315 Manhole - 4 inlets (A)
(defun c:PrivateManhole_315mm_4b() (DT:ib "Manhole-315-4b" nil "P" nil))  ; Private Circular 315 Manhole - 4 inlets (B)
(defun c:PrivateManhole_450mm_1()  (DT:ib "Manhole-450-1"  nil "P" nil))  ; Private Circular 450 Manhole - 1 inlet
(defun c:PrivateManhole_450mm_3a() (DT:ib "Manhole-450-3a" nil "P" nil))  ; Private Circular 450 Manhole - 3 inlets (A)
(defun c:PrivateManhole_450mm_3b() (DT:ib "Manhole-450-3b" nil "P" nil))  ; Private Circular 450 Manhole - 3 inlets (B)
(defun c:PrivateManhole_450mm_5()  (DT:ib "Manhole-450-5"  nil "P" nil))  ; Private Circular 450 Manhole - 1 inlet

; Private blocks - Manhole label
(defun c:manlab()    (DT:ib "manhole-label" nil nil nil))

; v0.2 - 2016.07.29 - "Adoptable Round 600 Foul Manhole" added.
; v0.1 - 2016.07.21 - "manhole-label" added.
; v0.0 - 2016.03.30 - First issue
; Author: David Torralba
; Last revision: 2016.07.29
;
;
;---------------------------------------------------------------------------
;
; MANHOLE SCHEDULE INSERTION FUNCTIONS
;
;---------------------------------------------------------------------------
; Manhole Schedule Header insertion function
(defun c:ManHeader() (DT:ib "ManScheduleHeader" "e-manhole-schedule" nil nil))
;
;
;---------------------------------------------------------------------------
;
; ROAD BLOCKS INSERTION FUNCTIONS
;
;---------------------------------------------------------------------------
(defun c:road_fall_arrow()    (DT:ib "Road-Fall-Arrow"    "e-road" "P" 0))  ; Road Fall Arrow insertion function
(defun c:parking_fall_arrow() (DT:ib "Parking-Fall-Arrow" "e-road" "P" 0))  ; Parking Fall Arrow insertion function
;
;
;---------------------------------------------------------------------------
;
; STREET LIGHT BLOCKS INSERTION FUNCTIONS
;
;---------------------------------------------------------------------------
(defun c:street_light()  (DT:ib "Street-Light"   "e-street-lights" "P" 677)); Street light insertion function
(defun c:street_light1() (DT:ib "Street-Light-1" "e-street-lights" "P" 677)); Street light 1 insertion function
(defun c:street_light2() (DT:ib "Street-Light-2" "e-street-lights" "P" 677)); Street light 2 insertion function
(defun c:street_light3() (DT:ib "Street-Light-3" "e-street-lights" "P" 677)); Street light 3 insertion function
;
;
;---------------------------------------------------------------------------
;
; EXTERNAL WORKS BLOCKS INSERTION FUNCTION
;
;---------------------------------------------------------------------------
(defun c:gate( / p1 p2 dist ang); Gate dynami block insertion function
  ; INPUT - Ask user gate location
  (while (not p1)
    (initget 1 "eXit")
    (if (= "eXit" (setq p1 (getpoint "\nSelect gate first point <eXit>: ")))
      (progn
        (princ "\nRoutine aborted by user.\n")
        (exit)
      );END progn
      (princ "point correctly introduced.")
    );END if
  ); END while
  (while (not p2)
    (initget 1 "eXit")
    (if (= "eXit" (setq p2 (getpoint "\nSelect gate second point <eXit>: ")))
      (progn
        (princ "\nRoutine aborted by user.\n")
        (exit)
      );END progn
      (princ "point correctly introduced.")
    );END if
  ); END while
  (setq
    dist (distance p1 p2)
    ang (angle p1 p2)
  )
  ; OPERATION - Correct angle
  (if (= 1 (getvar "angdir")) (setq ang (- 0 ang)))
  (setq ang (/ (* 180 ang) pi))

  ; OPERATION - Insert block
  (command "-insert" "access-gate" p1 1 1 ang)

  ; OPERATION - Edit block property
  (LM:setdynpropvalue (vlax-ename->vla-object (entlast)) "DistA" dist)
  (princ)
)

(defun c:mpart1() (DT:ib "Part-m-primary"     "e-part-m"  "P" 514)) ; Part-m-primary-0 block insertion function
(defun c:mpart3() (DT:ib "Part-m-secondary"   "e-part-m"  "P" 514)) ; Part-m-secondary-0 block insertion function
(defun c:streetplate() (DT:ib "street-plate"  "e-postal"  nil 0  )) ; Street plate block insertion function
(princ)
