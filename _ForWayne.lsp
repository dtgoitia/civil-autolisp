(defun c:pex( )
  ; Add 3d polyline choosing the level of each point
  (princ "\n >>> CAUTION!!\n >>> This program is in alpha version. It hasn't been tested yet.")
  (command "_.3dpoly")
  (while T
    (command ".xy" (getpoint "\nSelect XY position:") pause )
  );END while
  (princ)
  ; v0.0 - 2017.02.09 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.02.09
)
