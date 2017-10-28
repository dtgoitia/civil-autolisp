(defun c:xx ()
  ; Trigger
  (defun *error* ( errorMessage ) (princ (strcat "n-------- ERROR: " errorMessage " --------n")) (vl-bt) (DT:ReportError))
  (DT:AutoLoadFileFromCivilTemp "ErrorTracing.lsp")
  (DT:AutoLoadFileFromCivilTemp "KtfLevelBlocks.lsp")
  (c:UpdateKtfZToAtt)

  ; v0.0 - 2017.08.30 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.30
)
(defun c:UpdateKtfZToAtt ( / ss)
  ; Command version of DT:UpdateKtfZToAtt
  (if (setq ss (ssget '(( 0 . "INSERT")) ))
    (foreach a (ssnamex ss)
      (if (= 'ename (type (cadr a)))
        (DT:UpdateKtfZToAtt (cadr a))
      );END if
    );END foreach
  );END if

  ; v0.0 - 2017.08.30 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.30
)
(defun DT:UpdateKtfZToAtt ( ename / object attributeValue currentXyz newXyz newEntList )
  ; Update KTF block Z coordinate to match LEVEL attribute
  (if (DT:Arg 'DT:UpdateKtfZToAtt '((ename 'ename)))
    (progn
      (setq object (vlax-ename->vla-object ename))                  ; Get object
      (setq newZ (atof (LM:vl-getattributevalue object "LEVEL")))   ; Get attribute value
      (setq currentZ (caddr (cdr (assoc 10 (entget ename)))))
      (if (/= newZ currentZ)
        (vla-put-color object 5)
      );END if
      (setvar "osmode" 0)
      (command "_.move" ename "" "0,0,0" (strcat "0,0," (LM:rtos (- newZ currentZ) 2 5)) )
    );END progn
  );END if


  ; v0.0 - 2017.08.30 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.30
)
