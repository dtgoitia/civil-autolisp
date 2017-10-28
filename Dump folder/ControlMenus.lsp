;|
AutoLISP Functions By Name and Feature Reference (AutoLISP)
https://knowledge.autodesk.com/search-result/caas/CloudHelp/cloudhelp/2015/ENU/AutoCAD-AutoLISP/files/GUID-4CEE5072-8817-4920-8A2D-7060F5E16547-htm.html

How to control menus with AutoLISP:
https://knowledge.autodesk.com/search-result/caas/CloudHelp/cloudhelp/2015/ENU/AutoCAD-AutoLISP/files/GUID-92DA21E0-1E33-45AD-B2F8-1AB788392E4B-htm.html

This explains brilliantly POP menus:
https://forums.autodesk.com/t5/visual-lisp-autolisp-and-general/show-hide-menu-s-in-workspace/td-p/3662730

Display Control Functions Reference (AutoLISP)
https://knowledge.autodesk.com/search-result/caas/CloudHelp/cloudhelp/2015/ENU/AutoCAD-AutoLISP/files/GUID-9C2C56A3-0BEF-45AB-A7FE-5825339DCB41-htm.html

How to read configuration files:
https://knowledge.autodesk.com/search-result/caas/CloudHelp/cloudhelp/2015/ENU/AutoCAD-AutoLISP/files/GUID-11BEB9BE-2E76-4180-AB1F-1D05CEFCA9C8-htm.html
|;
(defun c:1() (DT:UnloadMenu "MJA_ENG") )
(defun c:2() (command "_.menuload" "\\\\mjafs01\\data\\standard details library\\mja standards\\mja engineering menu\\MJA Eng.cuix") )

(defun c:xx ()
  ; Get AutoCAD Application object
  (setq acadApp (vlax-get-acad-object))

  ; Get menubar object
  (setq menubarObject (vla-get-menubar acadApp))

  ; Get pop menus
  (vlax-for popmenu menubarObject
    (setq popmenuList (append popmenuList (list popmenu)))
  )

  popmenuList

  ; v0.0 - 2017.05.05 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.05.05
)
(nth 1 popmenuList)
(vlax-dump-object (nth 1 popmenuList) )
