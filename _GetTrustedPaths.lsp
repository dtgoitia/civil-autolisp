(defun c:zz( / paths )
  (DT:GetTrustedPaths)
  ;(DT:StringToList "a" ".")
  ;(DT:StringToList "...a.a...a..." ".")
  ;(DT:StringToList ".hi.I am.splitted.and..very properly.splitted.....a...a." ".")
)
(defun DT:GetTrustedPaths( / var )
  (if (setq var (getvar 'trustedpaths))
    (DT:StringToList var ";")
    (progn (princ "\nERROR @ DT:GetTrustedPaths > var = nil")(princ))
  );END if

  ; v0.0 - 2017.01.27 - DT:StringToList implemented
  ; v0.0 - 2017.01.23 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.01.27
)
