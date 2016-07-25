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
            oldosmode oldclayer oldcmdecho oldattdia oldattreq
            )
  ; (DT:ib "block_name" "layer_name" "rotation_value" "osmode_value")
  ; layer_name = ""       --> use current layer
  ; rotation_value = ""   --> rotation = 90 degree
  ; rotation_value = "P"  --> let user select rotation
  ; osmode_value = ""     --> use current osmode
  (defun *error* ( msg )
    (if (not (member msg '("Function cancelled" "quit / exit abort"))) (princ (strcat "\nError: " msg)))
    (setvar "clayer" oldclayer)
    (setvar "cmdecho" oldcmdecho)
    (setvar "attdia" oldattdia)
    (setvar "attreq" oldattreq)
    (princ)
  )
  (setq
    oldattdia (getvar "attdia")
    oldattreq (getvar "attreq")
  )
  (setvar "attdia" 0)
  (setvar "attreq" 0)
  (cond
    ((= blk nil)
      (princ "\nNo block defined.")
    );END cond - no block
    (t ; if any block specified:
      (if (/= lay "")
        (progn
          (setq oldclayer (getvar "clayer"))
          (command "-layer" "M" lay "")
        ); END progn
      );END if change CLAYER
      (if (/= osm "")
        (progn
          (setq oldosmode (getvar "osmode"))
          (setvar "osmode" osm)
        ); END progn
      );END if change OSMODE
      (cond
        ( (= rot "")
          (command "-insert" blk pause 1 1 0)
        ); END cond no rotation
        ( (= rot "P")
          (command "-insert" blk pause 1 1 pause)
        ); END cond with rotation
        ( (and (/= rot "") (/= rot "P"))
          (princ "\nrot")
          (command "-insert" blk pause 1 1 rot)
        ); END cond no rotation
      );END cond rot
      (if (/= lay "") (setvar "clayer" oldclayer))
      (if (/= osm "") (setvar "osmode" oldosmode))
    );END cond - block OK
  );END cond
  (setvar "attdia" oldattdia)
  (setvar "attreq" oldattreq)
  (princ)
  ; v0.1 - 2016.04.15 - ATTDIA and ATTREQ system variable control added
  ; v0.0 - 2016.04.14 - First issue
  ; Author: David Torralba
  ; Last revision: 2016.04.14
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
    ((= mo "12") (setq mo "Dic"))
  ); END cond
  (strcat mo "\'" yr)
  ; v0.0 - 2016.03.29 - First issue
  ; Author: David Torralba
  ; Last revision: 2016.03.29
)
(defun title_block_insert ( blk /  )
  (command "-insert" blk "0,0" 1 1 0 "" "" "" "" "" "" (title_block_date) "" "" "" "" "PRELIMINARY" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "")
  ; v0.0 - 2016.03.29 - First issue
  ; Author: David Torralba
  ; Last revision: 2016.03.29
)
(defun title_block_D_insert ( blk /  )
  (command "-insert" blk "0,0" 1 1 0 "" "" "" "" "" "" (title_block_date) "" "" "" "" "PRELIMINARY")
  (while (> (getvar "CMDACTIVE") 0) (command ""))
  (princ)
  ; v0.0 - 2016.03.29 - First issue
  ; Author: David Torralba
  ; Last revision: 2016.03.29
)
(defun c:MJA_A0_landscape () (title_block_insert "A0-Landscape") (princ))
(defun c:MJA_A1_landscape () (title_block_insert "A1-Landscape") (princ))
(defun c:MJA_A2_landscape () (title_block_insert "A2-Landscape") (princ))
(defun c:MJA_A3_landscape () (title_block_insert "A3-Landscape") (princ))
(defun c:MJA_A0_Portrait () (title_block_insert "A0-Portrait") (princ))
(defun c:MJA_A1_Portrait () (title_block_insert "A1-Portrait") (princ))
(defun c:MJA_A2_Portrait () (title_block_insert "A2-Portrait") (princ))
(defun c:MJA_A3_Portrait () (title_block_insert "A3-Portrait") (princ))
(defun c:MJA_A4_landscape ()
  (command "-insert" "A4-Landscape" "0,0" 1 1 0 "" "" "" "" "" "" (title_block_date) )
  (while (> (getvar "CMDACTIVE") 0) (command ""))
  (princ)
)
(defun c:MJA_A4_portrait ()
  (command "-insert" "A4-Portrait" "0,0" 1 1 0 "" "" "" "" "" "" (title_block_date) )
  (while (> (getvar "CMDACTIVE") 0) (command ""))
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
(defun c:MJA_A4_landscape_D ()
  (command "-insert" "A4-Landscape_D" "0,0" 1 1 0 "" "" "" "" "" "" (title_block_date) )
  (while (> (getvar "CMDACTIVE") 0) (command ""))
  (princ)
)
(defun c:MJA_A4_portrait_D ()
  (command "-insert" "A4-Portrait_D" "0,0" 1 1 0 "" "" "" "" "" "" (title_block_date) )
  (while (> (getvar "CMDACTIVE") 0) (command ""))
  (princ)
)
(defun c:revision_box_D() (DT:ib "Revision-box" "MJA-Title" "" ""))
; v0.1 - 2016.04.07 - Dynamic functins added
; v0.0 - 2016.03.29 - First issue
; Author: David Torralba
; Last revision: 2016.04.07
;
; North-Arrow insertion function
(defun c:NorthArrow() (DT:IB "North-Arrow" "MJA-Title" "" ""))
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

; Private blocks - Foul
(defun c:svp()    (DT:IB "e-pfd-svp"                      "e-pfd" ""  5)) ; SVP
(defun c:svp300() (DT:IB "Private-Square300-Foul-Manhole" "e-pfd" ""  5)) ; Private Square 300 Foul Manhole
(defun c:svp475() (DT:IB "Private-Square475-Foul-Manhole" "e-pfd" ""  5)) ; Private Square 475 Foul Manhole

; Private blocks - Storm
(defun c:rwp()    (DT:IB "e-psd-rwp"                      "e-psd" ""  5)) ; RWP
(defun c:rwp2()   (DT:IB "Private-Round-Storm-Manhole"    "e-psd" ""  5)) ; Private round storm manhole
(defun c:reye()   (DT:IB "Rodding-Eye"                    "e-psd" "P" 129)) ; Rodding eye
(defun c:krwp()   (c:rwp) (c:psd))

; Private blocks - Manhole label
(defun c:manlab()    (DT:IB "manhole-label" "" "" ""))

; v0.1 - 2016.07.21 - "manhole-label" added.
; v0.0 - 2016.03.30 - First issue
; Author: David Torralba
; Last revision: 2016.03.30
;
;
;---------------------------------------------------------------------------
;
; MANHOLE SCHEDULE INSERTION FUNCTIONS
;
;---------------------------------------------------------------------------
; Manhole Schedule Header insertion function
(defun c:ManHeader() (DT:IB "ManScheduleHeader" "e-manhole-schedule" "" ""))
;
;
;---------------------------------------------------------------------------
;
; ROAD BLOCKS INSERTION FUNCTIONS
;
;---------------------------------------------------------------------------
(defun c:road_fall_arrow()    (DT:IB "Road-Fall-Arrow"    "e-road" "P" 0))  ; Road Fall Arrow insertion function
(defun c:parking_fall_arrow() (DT:IB "Parking-Fall-Arrow" "e-road" "P" 0))  ; Parking Fall Arrow insertion function
;
;
;---------------------------------------------------------------------------
;
; STREET LIGHT BLOCKS INSERTION FUNCTIONS
;
;---------------------------------------------------------------------------
(defun c:street_light()  (DT:IB "Street-Light"   "e-street-lights" "P" 0)); Street light insertion function
(defun c:street_light1() (DT:IB "Street-Light-1" "e-street-lights" "P" 0)); Street light 1 insertion function
(defun c:street_light2() (DT:IB "Street-Light-2" "e-street-lights" "P" 0)); Street light 2 insertion function
(defun c:street_light3() (DT:IB "Street-Light-3" "e-street-lights" "P" 0)); Street light 3 insertion function
(princ)
