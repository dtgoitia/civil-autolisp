; (defun c:xx ()
;   ; Trigger
;   (defun *error* ( errorMessage ) (princ (strcat "n-------- ERROR: " errorMessage " --------n")) (vl-bt) (DT:ReportError))
;   (DT:AutoLoadFileFromCivilTemp "ErrorTracing.lsp")
;   (DT:AutoLoadFileFromCivilTemp "LayerUtilities.lsp")
;   (c:entryFunction)
;
;   ; v0.0 - 2017.10.06 - First issue
;   ; Author: David Torralba
;   ; Last revision: 2017.10.06
; )
(defun c:entryFunction ( / nename layerName )
  (if (setq nename (car (nentsel "\nSelect entity to grey out:")))
    (if (setq layerName (cdr (assoc 8 (entget nename))))
      (DT:SetLayerColorByViewport layerName 250)
      (DT:Error 'c:entryFunction "layerName=nil")
    );END if
    (DT:Error 'c:entryFunction "nename=nil")
  );END if

  ; v0.0 - 2017.10.06 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.10.06
)
(defun DT:SetLayerColorByViewport ( layerName color )
  (if (DT:Arg 'DT:SetLayerColorByViewport '((layerName 'str)(color 'int)))
    (progn
      (princ (strcat "\nChanging \"" layerName "\" color to " (itoa color) "...\n"))
      (command "vplayer" "C" color layerName "C" "")
    );END progn
  );END if

  ; v0.0 - 2017.10.06 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.10.06
)
