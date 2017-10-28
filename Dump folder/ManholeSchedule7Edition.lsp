(defun c:xx ()
  ; Trigger
  (DT:AutoLoadFileFromCivilTemp "ManholeSchedule7Edition.lsp")
  (c:test)

  ; v0.0 - _DATE_ - First issue
  ; Author: David Torralba
  ; Last revision: _DATE_
)
(defun c:MSCS7 ( / ss manholeData manholeScheduleAttributes )
  ; MSCS for Sewers for Adoption 7th Edition (SfA7)

  (princ "\nSewers for Adoption 7th Edition\nSelect manhole schedule block:")
  (if (setq ss (ssget))
    (foreach a (ssnamex ss)
      (if (= 'ename (type (cadr a)))
        (if (setq manholeData (DT:ManholeScheduleExtractData (cadr a)))
          (if (setq manholeScheduleAttributes (DT:MSCS7 manholeData))
            (LM:vl-setattributevalues (vlax-ename->vla-object (cadr a)) manholeScheduleAttributes)
          );END if
        );END if
      );END if
    );END foreach
  );END if

  ; v0.0 - 2017.06.30 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.06.30
)
(defun DT:ManholeScheduleExtractVisibleData ( manholeScheduleBlockEntity )
  nil

  ; v0.0 - 2017.08.01 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.01
)
(defun DT:MSCS7 ( manholeData / zeroData CL smallestSoffit biggestPipeSize manholeTypeValue return )
  ; MSCS for Sewers for Adoption 7th Edition (SfA7)
  ; Returns a list with the attributes for the block "ManScheduleBody"
  ; calculated based on the SfA7 criteria
  ; (("SOFFIT". soffitValue) ("CS" . chamberSizeValue) ("MT" . manholeTypeValue))

  (if (DT:Arg 'DT:MSCS7 '((manholeData 'list)))
    (progn
      (setq test manholeData)
      (DT:PrintVar 'test)
      ; Check there is no zero data (pipe size or IL):
      (mapcar
        '(lambda (sewerData)  ; (sewerData) = (cons pipe (list IL DN))
          (cond
            ; Check and record zero pipe size
            ((= 0 (nth 1 (cdr sewerData)))
              (setq zeroData (append (list (strcat "Pipe size is zero at: manhole " (cdr (assoc "ID" manholeData)) ) )) )
            );END subcond
            ; Check and record zero IL
            ((= 0 (nth 1 (cdr sewerData)))
              (setq zeroData (append (list (strcat "Pipe size is zero at: manhole " (cdr (assoc "ID" manholeData)) ) )) )
            );END subcond
          );END cond
        );END lambda
        (cdr (assoc "IO" manholeData))
      );END mapcar

      (DT:PrintVar 'zeroData)

      (setq
        CL (cdr (assoc "CL" manholeData)) ; Get cover level
        smallestSoffit ; Get the smallest depth to soffit
          (apply
            'min
            (mapcar                 ; Depth soffit for all pipes
              '(lambda (sewerData)  ; (sewerData) = (cons pipe (list IL DN))
                (- CL                                   ; Cover level
                  (+         (nth 0 (cdr sewerData))    ; Invert level
                    (* 0.001 (nth 1 (cdr sewerData)) )  ; Pipe diameter
                  );END +
                );END -
              );END lambda
              (cdr (assoc "IO" manholeData))
            );END mapcar
          );END apply
      );END setq
      (if (/= 0 smallestSoffit)
        (setq return (list (cons "SOFFIT" (LM:rtos smallestSoffit 2 3))))
      );END if

      ; Get pipe type based on the smalles depth to soffit
      (cond
        ((< 6 smallestSoffit)
          (setq manholeTypeValue "CHECK")
        );END subcond
        ((and (< 3 smallestSoffit)(<= smallestSoffit 6))
          (setq manholeTypeValue "1A")
        );END subcond
        ((and (<= 0.9 smallestSoffit)(<= smallestSoffit 3))
          (setq manholeTypeValue "2")
        );END subcond
        (t
          (setq manholeTypeValue "CHECK")
        );END subcond
      );END cond
      (if manholeTypeValue
        (setq return (append return (list (cons "MT" manholeTypeValue))))
      );END if

      ; Get biggest pipe
      (setq
        biggestPipeSize
        (apply
          'max
          ; pipe size list
          (mapcar
            '(lambda (sewerData) ; (sewerData) = (cons pipe (list IL DN))
                (nth 1 (cdr sewerData)) ; Pipe diameter
            );END lambda
            (cdr (assoc "IO" manholeData))
          );END mapcar
        );END apply
      );END setq

      ; For manhole type 1 and 2 only:
      (cond
        ((<= biggestPipeSize 375)
          (setq chamberSizeValue "DN1200")
        );END subcond
      ;|
        Case 375-450mm pipe size ommited because is not so common

        ((and (< 375 biggestPipeSize)(<= biggestPipeSize 450))
          (setq chamberSizeValue "DN1350")
        );END subcond
        ( (and (< 450 biggestPipeSize)(<= biggestPipeSize 700))
          (setq chamberSizeValue "DN1500")
        );END subcond
      |;
        ( (and (< 375 biggestPipeSize)(<= biggestPipeSize 700))
          (setq chamberSizeValue "DN1500")
        );END subcond
        ( (and (< 700 biggestPipeSize)(<= biggestPipeSize 900))
          (setq chamberSizeValue "DN1800")
        );END subcond
        ( (< biggestPipeSize 900)
          (setq chamberSizeValue (+ biggestPipeSize 900))
        );END subcond
        (t
          (setq chamberSizeValue "CHECK")
        );END subcond
      );END cond

      (if chamberSizeValue
        (setq return (append return (list (cons "CS" chamberSizeValue))))
      );END if

      ; Return
      return

    );END progn
  );END if

  ; v0.0 - 2017.06.30 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.06.30
)
(defun DT:ManholeScheduleExtractData ( manholeScheduleBlockEntity / rawData indexList pipeName invertLevelName pipeDataList return )
  ; Return manholeData in the proper structure:
  ; (list
  ;   (cons "ID"  manholeReference)
  ;   (cons "CL"  coverLevel)
  ;   (cons "IO"
  ;     (list
  ;       (cons "P0" (list invertLevel pipeDiameter)) ; pipe 0
  ;       (cons "P1" (list invertLevel pipeDiameter)) ; pipe 1
  ;       (cons "P2" (list invertLevel pipeDiameter)) ; pipe 2
  ;       (cons "P3" (list invertLevel pipeDiameter)) ; pipe 3
  ;       (cons "P4" (list invertLevel pipeDiameter)) ; pipe 4
  ;     );END list
  ;   );END cons
  ; );END list
  (if (DT:Arg 'DT:ManholeScheduleExtractData '((manholeScheduleBlockEntity 'ename)))
    (progn
      ; Extract raw data
      (if (setq rawData (LM:vl-getattributes (vlax-ename->vla-object manholeScheduleBlockEntity)))
        (progn

        );END progn
        (DT:Error 'DT:ManholeScheduleExtractData "no attributes")
      );END if

      (setq indexList (list "0" "1" "2" "3" "4"))

      ; Get pipe data if it's bigger than 0
      (mapcar
        '(lambda (i)
          (setq pipeName (strcat "P" i))
          (setq invertLevelName (strcat "IL" i))
          (if (< 0.0 (atof (cdr (assoc invertLevelName rawData))))
            (setq
              pipeDataList (append pipeDataList
                (list
                  (cons
                    pipeName
                    (list
                      (atof (cdr (assoc invertLevelName rawData)))
                      (atoi (substr (cdr (assoc pipeName rawData)) 3))
                    );END list
                  );END cons
                );END list
              );END append
            );END setq
          );END if
        );END lambda
        indexList
      );END mapcar

      ; Join all the data and return it
      (setq
        return
        (append
          (list
            (cons "ID" (cdr (assoc "ID" rawData))        )
            (cons "CL" (atof (cdr (assoc "CL" rawData))) )
          );END list
          (if pipeDataList
            (list (cons "IO" pipeDataList))
          );END if
        );END append
      );END setq
    );END progn
  );END if

  ; Return
  return


  ; v0.0 - 2017.06.30 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.06.30
)
