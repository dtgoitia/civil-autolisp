(defun open_dbx ( dwg / dbx )
  ; To go into a file without opening it :
  (if (< (atoi (substr (getvar "ACADVER") 1 2)) 16)
    (setq dbx (vlax-create-object "ObjectDBX.AxDbDocument"))
    (setq dbx (vlax-create-object (strcat "ObjectDBX.AxDbDocument." (substr (getvar "ACADVER") 1 2)) ))
  )
  (vla-open dbx dwg)
  dbx
)
(defun get_dbx_blk ( filePath blockName / Dbx )
  ; Import "blockName" block from "filePath" path
  (setq Dbx (open_dbx filePath))
  (vla-CopyObjects
    Dbx
    (vlax-safearray-fill
      (vlax-make-safearray vlax-vbObject '(0 . 0))
      (list (vla-item (vla-get-blocks dbx) blockName)) ; Change the block name here
    )
    (vla-get-blocks
      (vla-get-activedocument (vlax-get-acad-object))
    )
  )
  ; Clean-up :
  (vlax-release-object dbx)
)
(defun c:xx ()
  (get_dbx_blk "\\\\mjafs01\\Data\\Standard Details Library\\MJA Standards\\MJA Engineering Menu\\development\\MJA_Block_master_drawing.dwg" "A4-Portrait_D")

  ; v0.0 - _DATE_ - First issue
  ; Author: David Torralba
  ; Last revision: _DATE_
)
