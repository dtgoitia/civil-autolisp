(princ
  "\nLoading _TEMPPloting.lsp...\n"
  ; Page Setup Management with AutoLISP (very clear!)
  ; http://jtbworld.com/autocad-pagesetup-lsp
)
(defun c:1 ( / ent_name VL_ent_name layoutNumber newProjectNumber newLayoutNumber )
  (if (setq ent_name (car (entsel "\nSelect title block to update: ")))
    (progn
      ; Remove revision
      (LM:vl-setattributevalue (vlax-ename->vla-object ent_name) "*" "" )

      ; Update project number
      (setq
        layoutNumber (substr (LM:vl-getattributevalue (vlax-ename->vla-object ent_name) "****:**") 6)
        newProjectNumber "5613"
        newLayoutNumber (strcat newProjectNumber ":" layoutNumber)
      )
      (LM:vl-setattributevalue (vlax-ename->vla-object ent_name) "****:**" newLayoutNumber )

      ; Update Date
      (LM:vl-setattributevalue (vlax-ename->vla-object ent_name) "***'**" "Apr 17" )
    );END progn
  );END if
  (setvar "osmode" 1)
  (setvar "clayer" "MJA-Title")
  (command "-insert" "AbbeyLogoTEMP" pause 1 1 0)
  (command "_.explode" (entlast) "")
)
(defun c:2 ( / ;|ent_name|; blockName paperSize paperOrientation )
  ;(if (setq ent_name (car (entsel "\nSelect title block to print: ")))
  (if ent_name
    (progn
      (setq
        ; Get block name
        blockName (LM:effectivename (vlax-ename->vla-object ent_name))
        ; Get paper size
        paperSize
          (cond
            ((= "A4" (substr blockName 1 2)) "A4 (210 x 297 mm)" )
            ((= "A3" (substr blockName 1 2)) "A3 (297 x 420 mm)" )
            ((= "A2" (substr blockName 1 2)) "A2 (420 x 594 mm)" )
            ((= "A1" (substr blockName 1 2)) "A1 (594 x 841 mm)" )
            ((= "A0" (substr blockName 1 2)) "A0 (841 x 1189 mm)" )
          );END cond
        ; Get paper orientation
        paperOrientation
          (cond
            ((= "Landscape" (substr blockName 4)) "L")
            ((= "Portrait"  (substr blockName 4)) "P")
          );END cond
        ; Ger window corners coordinates
        lowerLeftCorner (cdr (assoc 10 (entget ent_name)))
      )
      (setq
        upperRightCorner
          (cond
            ((= "A1" (substr blockName 1 2)) (list (+ (nth 0 lowerLeftCorner) 840) (+ (nth 1 lowerLeftCorner) 594) 0.0))
            (t "nothing")
          );END cond
      );END setq
    );END progn
  );END if
  (princ "\nblockName = ")(princ blockName)
  (princ "\npaperSize = ")(princ paperSize)
  (princ "\npaperOrientation = ")(princ paperOrientation)
  (princ "\nlowerLeftCorner = ")(princ lowerLeftCorner)
  (princ "\nupperRightCorner = ")(princ upperRightCorner)
  (princ "\n")

  (command
    "-plot"
    "Yes"                             ; Detailed plot configuration
    ""                                ; Default layout name
    "\\\\MJAFS01\\RICOH MP CW2200 PS" ; Printer name
    paperSize                         ; Paper size
    "Milimetres"
    paperOrientation                  ; Drawing orientation [Portrait/Landscape]
    "No"                              ; Plot upside down?
    "Window"                          ; Enter plot area:
    lowerLeftCorner                   ; Enter lower left corner of the window
    upperRightCorner                  ; Enter upper right corner of the window
  )
)
(defun c:xx ( / VLCurrentLayout currentTabName newTabName )
  ; Rename current tab    name
  ; Rename current layout name
  (setq
    ; Get current layout object
    VLCurrentLayout (vla-get-activelayout (vla-get-ActiveDocument (vlax-get-acad-object)))
    currentTabName (vla-get-Name VLCurrentLayout)
    newTabName (strcat "5613-" (substr currentTabName 6))
  );END setq
  (vla-Put-Name VLCurrentLayout newTabName)

  ; v0.0 - 2017.04.10 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.04.10
)
(defun c:xx ( )
  ; Rename all tabs    starting with "5092-" to "5613-"
  ; Rename all layouts starting with "5092-" to "5613-"
  (vlax-for x (vla-get-Layouts (vla-get-ActiveDocument (vlax-get-acad-object) ))
    (if (= (substr (vla-get-Name x) 1 5) "5092-")
      (vla-Put-Name x (strcat "5613-" (substr (vla-get-Name x) 6)))
    );END if
  );END vlax-for

  ; v0.0 - 2017.04.10 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.04.10
)
(defun c:xx ( / layoutName layoutStylesheet layoutSetup layoutSetupList )
  ; Print all layout setups

  ; Source: http://www.cadtutor.net/forum/showthread.php?92397-Setting-a-CTB-by-a-command-or-lisp-in-AutoCAD
  (vlax-for x (vla-get-Layouts (vla-get-ActiveDocument (vlax-get-acad-object) ))
    (setq
      layoutName        (vla-get-Name x)                          ; Get tab name
      layoutStylesheet  (vla-get-stylesheet x)                    ; Get CTB
      layoutSetup (list layoutName layoutStylesheet)              ; Join single layout info
      layoutSetupList (append layoutSetupList (list layoutSetup)) ; Join all layouts info
    )
  );END vlax-for

  (princ
    (strcat
      "\n"
      (DT:ListToTable layoutSetupList)
    )
  )
  (princ)

  ; v0.0 - 2017.04.11 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.04.11
)
