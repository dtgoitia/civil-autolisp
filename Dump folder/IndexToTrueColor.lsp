(defun DT:ChangeIndexToRGB ( ent_name / object objectColor objectColorRed objectColorGreen objectColorBlue )
  ; Convert index color to RGB color keeping the same color
  ;|
  (setq truecol (vla-get-truecolor lay))
  (if (= (vla-get-ColorMethod truecol) acColorMethodByACI)
    (progn
      (setq aci (vla-get-ColorIndex truecol))
      (vla-put-ColorMethod truecol acColorMethodByRGB)
      (vla-put-ColorIndex truecol aci)
      (setq R (vla-get-red truecol))
      (setq G (vla-get-green truecol))
      (setq B (vla-get-blue truecol))
      (vla-setRGB truecol R G B)
      (vla-put-truecolor lay truecol)
    )
  )
  ; Source: http://www.cadforum.cz/cadforum_en/how-to-recolor-layers-from-aci-to-truecolor-tip10071
  |;
  (if (DT:Arg 'DT:ChangeIndexToRGB '((ent_name 'ename)))
    (progn
      (setq
        object            (vlax-ename->vla-object ent_name)

        objectColor       (vlax-get-property object      'TrueColor)
        objectColorIndex  (vlax-get-property objectColor 'ColorIndex)
        objectColorRed    (vlax-get-property objectColor 'Red   )
        objectColorGreen  (vlax-get-property objectColor 'Green )
        objectColorBlue   (vlax-get-property objectColor 'Blue  )
      );END setq
      (princ "\nOK\n")
      ;(vlax-put-property objectColor 'ColorMethod acColorMethodByRGB)
      (vla-put-ColorMethod objectColor acColorMethodByRGB)
      (vla-put-ColorIndex objectColor objectColorIndex)
      (vla-setRGB objectColor objectColorRed objectColorGreen objectColorBlue)
      ;|(vlax-put-property objectColor 'Red objectColorRed)
      (vlax-put-property objectColor 'Green objectColorGreen)
      (vlax-put-property objectColor 'Blue objectColorBlue)
      (princ "\nOK")
      (vlax-dump-object objectColor)
      (princ "\nOK")
      (vlax-put-property object 'TrueColor objectColor)
      (princ "\nOK")|;
      (princ)
    );END progn
  );END if

  ; v0.0 - 2017.05.31 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.05.31
)
(defun c:1 () (DT:ChangeIndexToRGB (car (entsel "\nChoose one entity! "))))
(defun c:TestTrueClr()
   (vl-load-com)
   (setq oApp (vlax-get-Acad-Object)
           oDoc (vla-get-ActiveDocument oApp)
           oMSp (vla-get-ModelSpace oDoc)
   )
   ;; Add a Line
   (setq oLine (vla-AddLine oMSp  (vlax-3d-point '(1.0 1.0 0.0))  (vlax-3d-point '(10.0 10.0 0.0))))
    (vla-update oLine)

   ;; Get the current color values
   (setq oColor (vlax-get-property oLine 'TrueColor)
           clrR (vlax-get-property oColor 'Red)
           clrG (vlax-get-property oColor 'Green)
           clrB (vlax-get-property oColor 'Blue)
   )
   (alert (strcat "Old Color - Red: " (itoa clrR) " Green: " (itoa clrG) " Blue: " (itoa clrB)))

   ;; Set TrueColor to blue (R=0, G=101, B=204)
   (vlax-invoke-method oColor 'SetRGB 0 101 204)
   (vlax-put-property oLine 'TrueColor oColor)
   (vla-update oLine)

   ;; Inspect the new color values
   (setq oColor (vlax-get-property oLine 'TrueColor)
           clrR (vlax-get-property oColor 'Red)
           clrG (vlax-get-property oColor 'Green)
           clrB (vlax-get-property oColor 'Blue)
   )
   (alert (strcat "New Color - Red: " (itoa clrR) " Green: " (itoa clrG) " Blue: " (itoa clrB)))

   ;; Reset color to acBylayer
   (vlax-put-property oColor 'ColorIndex acByLayer)
   (vlax-put-property oLine 'TrueColor oColor)
   (vla-update oLine)

   ;; Inspect the color values
   (setq oColor (vlax-get-property oLine 'TrueColor)
           clrR (vlax-get-property oColor 'Red)
           clrG (vlax-get-property oColor 'Green)
           clrB (vlax-get-property oColor 'Blue)
   )
   (alert (strcat "New Color - Red: " (itoa clrR) " Green: " (itoa clrG) " Blue: " (itoa clrB)))
)
