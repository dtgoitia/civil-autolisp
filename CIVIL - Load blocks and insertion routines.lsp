(defun c:loadblocks ()
  ; OPERATION - Check if fbi function is loaded. If not, exit.
  (princ "\nChecking if fbi function is loaded... ")
  (if (or
        (= (eval fbi) nil)
      );END or
    (progn
      (princ "not found.\nPlease, load function and run command again.")
      (exit)
    )
    (princ "function loaded.")
  )
  ; OPERATION - Import blocks from master drawing, and explode and remove them TODO



  (princ)

  ; v0.0 - 2016.03.?????? - First issue
  ; Author: David Torralba
  ; Last revision: 2016.03.29
)

;; Drainage block insertion routines
; Drainage block insertion master routine
(defun DT:drainage_block ( block lay osm )
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
  (setvar "osmode" osm)
  (setvar "cmdecho" 0)
  (command "-layer" "M" lay "")
  (fbi2 block)
  (setvar "osmode" old_osmode)
  (setvar "clayer" old_clayer)
  (setvar "cmdecho" old_cmdecho)
  (princ)
)
; Private foul blocks
(defun c:svp()    (DT:drainage_block "e-pfd-svp"                      "e-pfd" 4)) ; SVP
(defun c:svp300() (DT:drainage_block "Private-Square300-Foul-Manhole" "e-pfd" 4)) ; Private Square 300 Foul Manhole
(defun c:svp475() (DT:drainage_block "Private-Square475-Foul-Manhole" "e-pfd" 4)) ; Private Square 475 Foul Manhole

; Private storm blocks
(defun c:rwp() (DT:drainage_block "e-psd-rwp"                    "e-psd" 4)) ; RWP
(defun c:rwp2() (DT:drainage_block "Private-Round-Storm-Manhole" "e-psd" 4)) ; Private round storm manhole

; Storm - Private Storm Drainage line
(defun c:psd( / *error* old_osmode old_clayer)
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
  (setvar "osmode" 132) ; Was 679 before
  (setvar "cmdecho" 0)
  (command "-layer" "M" "e-psd" "")
  (command "line")
  (while (> (getvar "CMDACTIVE") 0) (command pause))

  (setvar "osmode" old_osmode)
  (setvar "clayer" old_clayer)
  (setvar "cmdecho" old_cmdecho)
  (princ)
)
; Foul - Private Foul Drainage line
(defun c:pfd( / *error* old_osmode old_clayer)
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
  (setvar "osmode" 132) ; Was 679 before
  (setvar "cmdecho" 0)
  (command "-layer" "M" "e-pfd" "")
  (command "line")
  (while (> (getvar "CMDACTIVE") 0) (command pause))

  (setvar "osmode" old_osmode)
  (setvar "clayer" old_clayer)
  (setvar "cmdecho" old_cmdecho)
  (princ)
)
; v0.0 - 2016.03.30 - First issue
; Author: David Torralba
; Last revision: 2016.03.30
;
;
;
;; Title Blocks insertion routines
(defun title_block_date ( / d yr mo )
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
  (command "-insert" blk "-0,0" 1 1 0 "" "" "" "" "" "" (title_block_date) "" "" "" "" "PRELIMINARY" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "")
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
; v0.0 - 2016.03.29 - First issue
; Author: David Torralba
; Last revision: 2016.03.29

(princ)
