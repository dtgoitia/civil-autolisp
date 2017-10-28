(defun dump ( VlaObject )
  (vlax-dump-object VlaObject)
  ; v0.0 - 2017.05.08 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.05.08
)
(defun DT:MaximiseAutocadWindow ()
  ; Maximise AutoCAD window
  (vlax-put-property (vlax-get-acad-object) 'WindowState 3)

  ; v0.0 - 2017.05.05 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.05.05
)
(defun DT:MaximiseAutocadWindowAtRightScreen ()
  ; Maximise AutoCAD window at the right screen
  (vlax-put-property (vlax-get-acad-object) 'WindowLeft 1920)
  (if (= 1920 (vlax-get-property (vlax-get-acad-object) 'WindowLeft))
    (DT:MaximiseAutocadWindow)
  );END if

  ; v0.0 - 2017.05.05 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.05.05
)
(defun c:xx ()
  ; Get AutoCAD Application object
  (setq acad (vlax-get-acad-object)) ; (vlax-dump-object acadApp)
  ; Get AutoCAD Application window properties
  (setq
    width (vlax-get-property acadApp 'Width)
    height (vlax-get-property acadApp 'Height)
    windowLeft (vlax-get-property acadApp 'WindowLeft)
  )

  ; Ensure window is in the right-hand side screen
  (vlax-put-property acadApp 'Width 1920)
  (vlax-put-property acadApp 'WindowLeft 1920)
  ; Screen resolution 2 x 1920 x 1080

  (DT:MaximiseAutocadWindow)



  ; v0.0 - 2017.05.05 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.05.05
)
