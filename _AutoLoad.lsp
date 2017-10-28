(defun DT:AutoLoadFileFromCivil ( filename )
  ; Reload from civil-autolisp repository and trigger test function

  (DT:LoadWithoutSecureload
    (strcat "C:\\Users\\" (getvar "LOGINNAME") "\\projects\\civil-autolisp\\" filename)
    "\nERROR @ DT:LoadWithoutSecureload > some problem happened!"
  )
  ; v0.0 - 2017.07.25 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.07.25
)
(defun DT:AutoLoadFileFromCivilTemp ( filename )
  ; Reload from civil-autolisp-TEMP repository and trigger test function

  (DT:LoadWithoutSecureload
    (strcat "C:\\Users\\" (getvar "LOGINNAME") "\\projects\\civil-autolisp-TEMP\\" filename)
    "\nERROR @ DT:LoadWithoutSecureload > some problem happened!"
  )
  ; v0.0 - 2017.07.25 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.07.25
)
