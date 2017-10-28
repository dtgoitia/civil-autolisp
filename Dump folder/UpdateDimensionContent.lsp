(defun c:xx ()
  ; Trigger
  (defun *error* ( errorMessage ) (princ (strcat "n-------- ERROR: " errorMessage " --------n")) (vl-bt) (DT:ReportError))
  (DT:AutoLoadFileFromCivilTemp "ErrorTracing.lsp")
  (DT:AutoLoadFileFromCivilTemp "UpdateDimensionContent.lsp")
  (c:SetDimensionContent)

  ; v0.0 - 2017.08.24 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.24
)
(defun c:SetDimensionContent ( / ss ename content newContent )
  ; Update selected dimension content
  (setq ss (ssget '(( 0 . "DIMENSION" )) ))
  (foreach a (ssnamex ss)
    (if (= 'ename (type (cadr a)))
      (progn
        (setq ename (cadr a))
        (setq content (DT:GetText ename))
        (if (vl-string-search "Gran B & S" content)
          (setq newContent (vl-string-subst "Gran S" "Gran B & S" content))
        );END if
        (if (vl-string-search "Conc B & S" content)
          (setq newContent (vl-string-subst "S & ConSlab" "Conc B & S" content))
        );END if

        (if newContent (DT:SetText ename newContent) )
        (setq newContent nil)
      );END progn
    );END if
  );END foreach

  ; v0.0 - 2017.08.24 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.24
)
