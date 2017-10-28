(defun DT:InsertBlockFast ( blockName pt / )
  ; Insert "blockName" block at "pt" point
  ; If successfull return insert VL object name
  ; If not return nil
  (if (and blockName pt)
    (if (and (= 'str (type blockName)) (= 'list (type pt)))
      (if (DT:GetBlockVLAObject blockName)
        (vlax-invoke-method
          (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object)))
          'InsertBlock
          (vlax-3d-point pt)
          blockName
          1.0
          1.0
          1.0
          0.0
        );END vlax-invoke-method
        (progn (princ "\nERROR @ DT:ib > blockName block does not exist\n") nil )
      );END if
      (cond
        ((/= 'str  (type blockName)) (princ "\nERROR @ DT:ib > blockName is not a string\n") nil )
        ((/= 'list (type pt       )) (princ "\nERROR @ DT:ib > pt is not a list\n") nil )
      );END cond
    );END if
    (cond
      ((not blockName) (princ "\nERROR @ DT:ib > blockName = nil\n") nil )
      ((not pt       ) (princ "\nERROR @ DT:ib > pt = nil\n") nil )
    );END cond
  );END if

  ; v0.0 - 2017.04.12 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.04.12
)
(defun c:wblockm ()
  ; Export al the blocks into individual drawings
  (setq cmdecho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  ;
  (if (not dos_getdir)
    (setq path (getstring "\nDS> Target Folder: " T))
    (setq path (dos_getdir "Target Folder" (getvar "DWGPREFIX")))
  )
  (if (/= path nil)
    (progn
      (if (= (substr path (strlen path) 1) "\\")
        (setq path (substr path 1 (1- (strlen path))))
      )
      (princ "\nDS> Building List of Blocks ... ")
      (setq lst nil)
      (setq itm (tblnext "BLOCK" T))
      (while (/= itm nil)
        (setq nam (cdr (assoc 2 itm)))
        (setq pass T)
        (if (/= (cdr (assoc 1 itm)) nil)
          (setq pass nil)
          (progn
            (setq ctr 1)
            (repeat (strlen nam)
              (setq chk (substr nam ctr 1))
              (if (or (= chk "*")(= chk "|"))
                (setq pass nil)
              )
              (setq ctr (1+ ctr))
            )
          )
        )
        (if (= pass T)
          (setq lst (cons nam lst))
        )
        (setq itm (tblnext "BLOCK"))
      )
      (setq lst (acad_strlsort lst))
      (princ "Done.")
      ;
      (foreach blk lst
        (setq fn (strcat path (chr 92) blk))
        (if (findfile (strcat fn ".dwg"))
          (command "_.WBLOCK" fn "_Y" blk)
          (command "_.WBLOCK" fn blk)
        )
      )
    )
  )
  ;
  (setvar "CMDECHO" cmdecho)
  (princ)
)
(defun c:xx ()
  (wblockm "\\\\mjafs01\\Data\\Standard Details Library\\MJA Standards\\MJA Engineering Menu\\development" "MJA_Block_master_drawing")

  ; v0.0 - _DATE_ - First issue
  ; Author: David Torralba
  ; Last revision: _DATE_
)
(defun DT:InsertTitleBLock ( blockName )
  ; Insert "blockName" block in the layer "MJA-Title" and add current date
  (if blockName
    (if (= 'str (type blockName))
      (progn
        ; SAVE OLD SYSTEM VARIABLES
        (save_environment (list "attdia" "attreq" "insunits" "insunitsdeftarget" "insunitsdefsource" "osmode"))

        ; MODIFY SETTINGS
        (setvar "osmode" 0)
        (setvar "attdia" 0)
        (setvar "attreq" 0)
        (setvar "insunits" 0)
        (setvar "insunitsdeftarget" 0)
        (setvar "insunitsdefsource" 0)

        ; Insert block at origin (0,0)
        (command "-insert" blockName "0,0" 1 1 0)

        ; Set block correct layer
        (vla-put-layer (vlax-ename->vla-object (entlast)) "MJA-Title")

        ; Set block color ByLayer
        (vlax-put-property (vlax-ename->vla-object (entlast)) 'Color 256)

        ; Set title block date
        (LM:vl-setattributevalue (vlax-ename->vla-object (entlast)) "***'**" (title_block_date) )

        ; RESTORE PREVIOUS SETTINGS
        (restore_environment)
        (princ)
      );END progn
      (progn (princ "ERROR @ DT:CheckIfBlockExists > blockName is not a string\n") nil )
    );END if
    (progn (princ "ERROR @ DT:CheckIfBlockExists > blockName = nil\n") nil )
  );END if

  ; v0.0 - 2017.02.12 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.02.12
)
