;; COPYBLOCK.LSP  Copyright ©1999  Tony Tanzillo
;;
;; AutoCAD 2000  Visual LISP / ObjectDBX Example
;;
;;   http://www.caddzone.com
;;   tony.tanzillo@caddzone.com
;;
;; This example demonstrates how to use ObjectDBX in
;; Visual LISP to directly add blocks defined in any
;; drawing file, to the current drawing.
;;
;; (CopyBlock <FileName> <BlockName>)
;;
;;This function copies the definition of the block whose name is <BlockName> (a string), from the .DWG
;;file whose name is <FileName> (a string), into the current drawing's block table.
;;
;; CopyBlock returns the new copy of the block
;; object in the current drawing's block table.
;;
;; Note that this example performs none of the
;; error checking that is required, and does not
;; attempt to determine what actually happened
;; within the deep clone operation.

(vl-load-com)

(setq *acad* (vlax-get-acad-object))

(defun CopyBlock (DwgName BlkName / blocks dbxDoc)

   (setq blocks
      (vla-get-blocks
         (vla-get-ActiveDocument *acad*)
      )
   )

   (setq dbxDoc
      (vla-GetInterfaceObject
         *acad*
         "ObjectDBX.AxDbDocument"
      )
   )

   (vla-open dbxDoc DwgName)

   (vla-CopyObjects
      dbxDoc
      (vlax-safearray-fill
         (vlax-make-safearray
            vlax-vbObject
           '(0 . 0)
         )
         (list
            (vla-item
               (vla-get-blocks dbxDoc)
               BlkName
            )
         )
      )
      blocks
   )

   (vlax-release-object dbxDoc)

   (vla-item blocks BlkName)

)

(princ "\n(CopyBlock <DrawingFileName> <BlockName>)")

(princ)

;;;;;;;;;;;;;;;;;  CopyBlock.lsp ;;;;;;;;;;;;;;;;;;;;
