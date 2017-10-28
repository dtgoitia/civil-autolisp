;|
Stuff to be deployed
 - [ ] Trusted paths  --> http://blogs.autodesk.com/autocad/bootstrap-autocad-deployments-customizations-4/
 - [ ] Support paths
 - [ ] CUI files
 - [ ] profiles       --> http://blogs.autodesk.com/autocad/bootstrap-autocad-deployments-customizations-5/

|;
(defun c:xx () (DT:DeployCustomEnvironment) )
(defun DT:DeployCustomEnvironment ()
  ; Create a new profile
  ; Set new profile as current

    (setq
      ; Get shell VL object
      shellApp (vla-getInterfaceObject (vlax-get-acad-object) "Shell.Application")
    );END setq
    nil
  ; v0.0 - _DATE_ - First issue
  ; Author: David Torralba
  ; Last revision: _DATE_
)
(defun DT:DeployTestingEnvironment ()
  ; Create a new profile
  ; Set new profile as current
  nil
  ; v0.0 - _DATE_ - First issue
  ; Author: David Torralba
  ; Last revision: _DATE_
)
(defun DT:CreateProfile ( newProfileName )
  ;http://jtbworld.com/autocad-profiles-lsp
  nil
)
