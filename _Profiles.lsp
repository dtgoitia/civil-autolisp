(defun DT:GetActiveProfileName ( / profiles )
  ; Return active profile name as string
  (setq profiles (vla-get-profiles (vla-get-preferences (vlax-get-acad-object))))
  (if (vlax-property-available-p profiles 'ActiveProfile)
    (vlax-get-property profiles 'ActiveProfile)
  );END if

  ; v0.0 - 2017.05.05 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.05.05
)
