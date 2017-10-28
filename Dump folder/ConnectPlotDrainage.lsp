(defun c:ccc() (c:ConnectPlots))
(defun c:ConnectPlots()
  (defun c:1 ( / sp p )
    ; Connect Plot Storm Water connection
    (princ "\nCONNECT PLOTS STORM:\n")
    (save_environment (list "osmode" "clayer"))
    (setvar "osmode" 4)
    (setvar "clayer" "e-psd-PH2")
    (c:RWP)
    (restore_environment)
    ; v0.0 - 2017.06.09 - First issue
    ; Author: David Torralba
    ; Last revision: 2017.06.09
  )
  (defun c:2 ( / sp p doc )
    ; Connect Plot Foul Water connection
    (princ "\nCONNECT PLOTS FOUL:\n")
    (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
    (vla-startUndoMark doc)
    (save_environment (list "osmode" "clayer"))
    (setvar "osmode" 4)
    (setvar "clayer" "e-pfd-PH2")
    (DT:ib "Private-Square300-Foul-Manhole" nil nil 0)
    (setq sp (getvar "lastpoint"))
    (while (setq p (getpoint "\nSelect SVP: "))
      (command "_.pline" "_non" sp "_cen" p "")
      (entmakex
        (list
          (cons 0 "INSERT")
          (cons 2 "e-pfd-svp")
          (cons 10 p)
        );END list
      );END entmakex
      (setq p nil)
    );END while
    (vla-endUndoMark doc)
    (restore_environment)
  )
  (defun c:11 ( / doc potEntity potLayer potPoint pipeEntity pipeLayer targetPoint connectionEntity )
    ; Connect pipes with a LWPolyline
    ; Select pot
    (if (setq potEntity (car (entsel "\nSelect pot: ")))
      (setq
        potLayer (cdr (assoc 8  (entget potEntity)))
        potPoint (cdr (assoc 10 (entget potEntity)))
      );END setq
    );END if
    ; Select pipe
    (if (setq pipeEntity (car (entsel "\nSelect pipe: ")))
      (setq
        pipeLayer (cdr (assoc 8 (entget pipeEntity)))
        pipeVertexes (DT:GetPolylineVertexCoordinates pipeEntity)
        pipeP1 (nth 0 pipeVertexes)
        pipeP2 (nth 1 pipeVertexes)
      );END setq
    );END if
    ; Give connection point
    (if (= potLayer pipeLayer)
      (progn
        (princ "\nPress ENTER to finish...")
        (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
        (vla-startUndoMark doc)
        (while (= 5 (car (setq gr (grread 't 13 0))))
          (if connectionEntity
            (if (entget connectionEntity)
              (vla-delete (vlax-ename->vla-object connectionEntity))
            );END if
          );END if
          (setq
            targetPoint (cadr gr)
            connetionPoint (inters potPoint targetPoint pipeP1 pipeP2 nil)
            connectionEntity (entmakex
              (list
                (cons 0 "LINE")
                (cons 8 potLayer)
                (cons 10 potPoint)
                (cons 11 connetionPoint)
              );END list
            );END entmakex
          );END
        );END while
        (if connectionEntity
          (if (setq connectionEntget (entget connectionEntity))
            (progn
              (entmakex
                (append
                  (list
                    (cons   0 "LWPOLYLINE")         ; Object type
                    (cons 8 potLayer)
                    (cons 100 "AcDbEntity")
                    (cons 100 "AcDbPolyline")
                    (cons  70 0)                  ; Open(0)/Closed(1)
                    (cons  90 2) ; Number of vertices
                    (cons 10 (cdr (assoc 10 connectionEntget)))
                    (cons 10 (cdr (assoc 11 connectionEntget)))
                  )
                );END append
              )
              (vla-delete (vlax-ename->vla-object connectionEntity))
            );END progn
          );END if
        );END if
        (vla-endUndoMark doc)
      );END progn
      (progn
        (princ "\nSelect entities are not in the same layer")
      );END progn
    );END if

    (princ)

    ; v0.0 - 2017.06.09 - First issue
    ; Author: David Torralba
    ; Last revision: 2017.06.09
  )
)
