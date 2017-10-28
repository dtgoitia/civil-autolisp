(defun c:xx ()
  ; Trigger
  (defun *error* ( errorMessage ) (princ (strcat "n-------- ERROR: " errorMessage " --------n")) (vl-bt) (DT:ReportError))
  (DT:AutoLoadFileFromCivilTemp "ErrorTracing.lsp")
  (DT:AutoLoadFileFromCivilTemp "GardenLevels.lsp")
  (c:CopyLevelToPlotLevel)

  ; v0.0 - 2017.08.29 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.29
)
(defun c:FFLstepped ( / fflEname plotLevelSs )
  ; Command version of DT:FFLstepped
  (if (setq fflEname (car (nentsel "\nSelect FFL: ")))
    (progn
      (DT:PrintVar 'fflEname)
      (princ "\nSelect garden levels to be -150mm: ")
      (if (setq plotLevelSs (ssget))
        (DT:FFLstepped fflEname plotLevelSs)
        (DT:Error 'c:FFLstepped "plotLevelSs=nil")
      );END if
    );END progn
    (DT:Error 'c:FFLstepped "fflEname=nil")
  );END if
  (c:engset)

  ; v0.0 - 2017.08.29 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.29
)
(defun DT:FFLstepped ( fflEname plotLevelSs / fflText fflLevel plotLevelText )
  ; Set plot level text value to FFL-150mm
  (if (DT:Arg 'DT:FFLstepped '((fflEname 'ename)(plotLevelSs 'pickset)))
    (if (setq fflText (DT:GetText fflEname))
      (if (setq fflLevel (substr fflText 5) )
        (if (setq plotLevelText (strcat "%%U" (LM:rtos (- (atof fflLevel) 0.150 ) 2 2)))
          (foreach a (ssnamex plotLevelSs)
            (if (= 'ename (type (cadr a)))
              (DT:SetText (cadr a) plotLevelText)
            );END if
          );END foreach
          (DT:Error 'DT:FFLstepped "plotLevelText=nil")
        );END if
        (DT:Error 'DT:FFLstepped "fflLevel=nil")
      );END if
      (DT:Error 'DT:FFLstepped "fflText=nil")
    );END if
  );END if

  ; v0.0 - 2017.08.29 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.29
)
(defun c:ShortCut ( / p1 oldosmode )
  (save_environment (list "osmode"))
  (setvar "osmode" 512)
  (setq p1 (getpoint))
  (DT:DrawPlotLevel 0 p1 0)
  (DT:R2 (entlast) p1 (getpoint "\nSelect point 2: ") )
  (command "_.move" (entlast) "" "_non" (cadr (grread 't)) "_non" pause)

  (restore_environment)
  ; v0.0 - 2017.08.29 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.29
)
(defun c:OffsetPlotLevel ( / ss dist ans angFlag )
  ; Command version of DT:OffsetPlotLevel
  (setq dist 3.0)
  (setq ss (ssget '(( 8 . "e-plot-levels")) ))
  (setq ans (getint "\nType anything to copy upwards, ENTER to rotate downwards:"))
  (DT:PrintVar 'ans)
  (setq angFlag (if ans 1 0))
  (DT:PrintVar 'angFlag)
  (foreach a (ssnamex ss)
    (if (= 'ename (type (cadr a)))
      (DT:OffsetPlotLevel (cadr a) dist angFlag)
    );END if
  );END foreach

  ; v0.0 - 2017.08.29 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.29
)
(defun DT:OffsetPlotLevel ( ename dist angFlag / object textRotation textInsertionPoint newRotation newInsertionPoint )
  ; DT:OffsetPlotLevel function description
  (if (DT:Arg 'DT:OffsetPlotLevel '((ename 'ename)(dist 'real)(angFlag 'int)))
    (progn
      (setq object (vlax-ename->vla-object ename))
      (setq textRotation (vlax-get-property object 'Rotation))
      (setq textInsertionPoint (cdr (assoc 10 (entget ename))) )
      (setq newRotation
        (if (= 0 angFlag)
          (+ textRotation (+ pi (* 0.5 pi)) )
          (+ textRotation (- pi (* 0.5 pi)) )
        );END if
      )
      (setq newInsertionPoint (polar textInsertionPoint newRotation dist))
      (command "_.copy" ename "" textInsertionPoint newInsertionPoint "")
      (DT:SetText (entlast) (strcat "%%U" (LM:rtos (- (atof (substr (DT:GetText ename) 4) ) 0.150) 2 2)))
    );END progn
  );END if

  ; v0.0 - 2017.08.29 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.29
)
(defun c:CopyLevelToPlotLevel ( / targetEname sourceEname )
  (setq targetEname (car (nentsel "\nSelect target text: ")))
  (setq sourceEname (car (nentsel "\nSelect source text:")))
  (DT:SetText targetEname (strcat "%%U" (substr (DT:GetText sourceEname) 1 6)))

  ; v0.0 - 2017.08.29 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.29
)
