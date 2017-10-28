; (defun c:xx ()
;   ; Trigger
;   (DT:AutoLoadFileFromCivilTemp "LayerManager.lsp")
;   (c:AddMyLayerFilters)
;
;   ; v0.0 - 2017.08.23 - First issue
;   ; Author: David Torralba
;   ; Last revision: 2017.08.23
; )
(defun c:AddMyLayerFilters ()
  ; Add my custom layer filters to Layer Manager Pannel

  ; Settingn out
  (DT:AddLayerFilter "NAME==\"e-set*out*\"" "Setting Out")

  ; Tracking
  (DT:AddLayerFilter "NAME==\"*atr*\"" "Tracking")

  ; v0.0 - 2017.08.23 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.23
)
(defun DT:AddLayerFilter ( filterExpression filterName )
  ; Add passed layer filter
  (if (DT:Arg 'DT:AddLayerFilter '((filterExpression 'str)(filterName 'str)))
    (command
      "_.-layer"  ; start "-layer" command
      "Filter"    ; access "filter" option
      "New"       ; create new filter
      "Property"  ; select type of filter (group/property)
      "All"       ; select parent filter
      filterExpression
      filterName
      "X"         ; exit
      ""          ; exit "-layer" command
    );END command
  );END if

  ; v0.0 - 2017.08.23 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.23
)
(defun c:LayerFilters ( / propertyList )
  (setq propertyList
    (list
      (cons "name" "e-xref")
      (cons "rules"
        (list
          "NAME==\"*e-xref*\""
          "PLOTTABLE==\"true\""
        );END list
      )
    );END list
  )

  (DT:CreatePropertyFilter propertyList)

  ; v0.0 - 2017.10.17 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.10.17
)
(defun DT:CreatePropertyFilter ( propertyList / name )
  ; USED==\"TRUE\"
  ; VPOVERRIDES==\"FALSE\"
  ; NAME==\"A*\"
  ; OFF==\"False\"
  ; FROZEN==\"True\"
  ; LOCKED==\"True\"
  ; COLOR==\"10\"
  ; LINETYPE==\"DASHED\"
  ; LINEWEIGHT==\"0.18\"
  ; PLOTTABLE==\"True\"
  ; NEWVPFROZEN==\"True\""

  (if (DT:Arg 'DT:CreatePropertyFilter '((propertyList 'list)))
    (if (= 'str (type (setq name (cdr (assoc "name" propertyList)))))
      (if (= 'list (type (setq ruleList (cdr (assoc "rules" propertyList)))))
        (if (> 1 (length ruleList))
          (if (setq concatRules (DT:JoinStringList ruleList " AND "))
            (progn
              (princ 1)
              (DT:PrintVar 'concatRules)
              (command "_.-layer" "filter" "New" "Property" "All" concatRules name "x" "")
            );END progn
          );END if
          (progn
            (princ 2)
            (DT:PrintVar 'concatRules)
            (command "_.-layer" "filter" "New" "Property" "All" (nth 0 ruleList) name "x" "")
          );END progn
        );END if
        (DT:Error 'DT:CreatePropertyFilter "propertyList \"rules\" member should be a list")
      );END if
      (DT:Error 'DT:CreatePropertyFilter "propertyList \"name\" member should be a string")
    );END if
  );END if

  ; v0.0 - 2017.10.17 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.10.17
)
; (command "._-layer" "filter" "new" "group" "" "YOUR_FILTER*" "YOUR_FILTER_NAME" "" "")
(defun DT:JoinStringList ( lst delimiter / i return )
  (if (DT:Arg 'DT:JoinStringList '((lst 'list)(delimiter 'str)))
    (progn
      (setq i 0)
      (setq return "")
      (foreach item lst
        (if (> i 0)
          (setq return (strcat return delimiter item))
          (setq return (strcat return item))
        );END if
        (setq i (1+ i))
      );END foreach
      return
    );END progn
  );END if

  ; v0.0 - 2017.10.17 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.10.17
)
