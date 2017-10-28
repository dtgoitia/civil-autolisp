(defun c:Clean_Street_lighting_Drawing ()
  (initget "Yes No")
  (if (= "Yes" (getkword "\n--- WARNING ---\nThis command will remove layers, and join lines and change their colors.\nAre you sure you want to continue? [Yes/No] <No>:"))
    (progn
      (setq luxLayerList (DT:GetLuxLayers))
      (foreach layerName luxLayerList
        (DT:JoinLuxLevels layerName)    ; Join lux level lines
        (DT:LuxLevelsToTrue layerName)  ; Change entity color form index to true
      );END foreach
      (DT:CleanLuxLayers)               ; Clean unnecessary layers
    );END progn
    (princ "\naborting...")
  );END if
  (princ)
)
(defun DT:GetLuxLayers ( / layerName return )
  ; Return layers lux level layers
  (vlax-for layerObject (vla-get-layers (vla-get-activedocument (vlax-get-acad-object)))
    (setq layerName (vla-get-name layerObject))
    (if
      (and
        (vl-string-search "Reality Grid " layerName)
        (vl-string-search " Contour " layerName)
        (vl-string-search " lux" layerName)
      );END and
      (setq return (cons layerName return))
      ;(progn (princ "\n")(princ layerName)(princ " layer name doesn't match the filter")(princ ))
    );END if
  )
  return
  ; v0.0 - 2017.07.25 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.07.25
)
(defun DT:JoinLuxLevels ( layerName / ss )
  ; Join lux levels
  (if (DT:Arg 'DT:JoinLuxLevels '((layerName 'str)))
    (if (setq ss (ssget "x" (list (cons -4 "<AND") (cons 8 layerName) (cons 0 "LINE") (cons -4 "AND>"))))
      (DT:JoinLines ss)
    );END if
  );END if

  ; v0.0 - 2017.07.25 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.07.25
)
(defun DT:JoinLines ( ss )
  ; Join all the lines, arcs, polylines and lwpolylines passed
  ; ss [pickset] - Selection set with all the entities to join

  (if (DT:Arg 'DT:JoinLines '((ss 'pickset)))
    (progn
      (save_environment (list "cmdecho" "peditaccept"))
      (setvar "cmdecho" 0)
      (setvar "peditaccept" 1)

      (if (/= ss nil)
        (progn
          (if (= (sslength ss) 1)
            (progn
              (sssetfirst nil ss)
              (command "_.pedit" "_J" (ssget "X" '((0 . "LINE,ARC,POLYLINE,LWPOLYLINE"))) "" "")
            );END progn
            (command "_.pedit" "_M" ss "" "_J" "0.0" "")
          );END if
        );END progn
      );END if

      ; RESTORE SETTINGS
      (restore_environment)
    );END progn
  );END if

  ; v0.0 - 2017.07.25 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.07.25
)
(defun DT:ColorToTrue ( ename )
  ; Set entity colour as True color

  (if (not LM:ACI->RGB) (DT:AutoLoadFileFromCivilTemp "LmColourConversion.lsp"))
  (if (not VxSetTrueCol2) (DT:AutoLoadFileFromCivilTemp "ColourConversion.lsp"))
  (VxSetTrueCol2 (vlax-ename->vla-object ename) (LM:ACI->RGB (cdr (assoc 62 (entget ename)))))

  ; v0.0 - 2017.07.25 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.07.25
)
(defun DT:LuxLevelsToTrue ( layerName / ss )
  ; Change color to TrueColor for all objects within layerName layer

  (if (DT:Arg 'DT:LuxLevelsToTrue '((layerName 'str)))
    (if (setq ss (ssget "x" (list (cons 8 layerName))))
      (foreach x (ssnamex ss)
        (DT:ColorToTrue (cadr x))
      );END foreach
    );END if
  );END if

  ; v0.0 - 2017.07.25 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.07.25
)
(defun DT:CleanLuxLayers ()
  ; Clean innecessary layers

  (if (not DT:CleanLayerAtOnce) (DT:AutoLoadFileFromCivilTemp "CleanLayer.lsp") )
  (mapcar
    '(lambda (layerName)
      (DT:CleanLayerAtOnce layerName)
      (if (tblsearch "layer" layerName) (command "-purge" "LA" layerName "N") )
    );END lambda
    '(
      "Reality Work Area Title"
      "Reality Work Area Points"
      "Reality Work Area Coordinates"
      "Reality Work Area"
      "Reality Luminaire Data"
      "Reality Key"
      "Reality Grid 1 Title"
      "Reality Grid 1 Results Summary"
      "Reality Grid 1 Coordinates"
      "Reality Grid 1 Area"
      "Reality Frame"
    )
  );END mapcar

  ; v0.0 - 2017.07.25 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.07.25
)
