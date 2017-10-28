
(defun c:1()
  (DT:SupersedCurrentFile)
)
(defun DT:SupersedCurrentFile( / fileName fileFolder filePath date supersededFileName supersededFileFolder supersededFilePath )
  (defun DT:SupersedeFile ( filePath supersededFilePath )
    (if (vl-file-copy filePath supersededFilePath)
      (alert
        (strcat
          "FILE CREATED at:"
          "\n" supersededFilePath
        )
      )
    );END if
  )
  (setq
    ; Curernt file
    fileName (getvar "dwgname")
    fileFolder (getvar "dwgprefix")
    filePath (strcat fileFolder fileName )

    ; Superseded file
    date (PrintSupersedDate)
    supersededFileName (strcat (substr fileName 1 (- (strlen fileName) 4)) " " date ".dwg")
    supersededFileFolder (strcat fileFolder "Superseded\\")
    supersededFilePath (strcat supersededFileFolder supersededFileName )
  )

  (princ "\nfilePath = ")(princ filePath)
  (if (findfile filePath)       ; if current file exists go on
    (progn ; filePath exists
      ; Create "Superseded" folder (if exists fill not overwrite it)
      (if (vl-mkdir supersededFileFolder)
        (princ "\n\"Superseded\" folder created.")
        (princ "\n\"Superseded\" folder already exists.")
      );END if

      (princ "\nsupersededFilePath = ")(princ supersededFilePath)
      (while (findfile supersededFilePath); if target file exists update name, if not go on
        (princ "\n  Upps! avobe file already exists...renaming:")
        ;rename supersededFilePath till doesn't exist and
        (setq
          datePosition (vl-string-search date supersededFilePath)
          counterPosition (+ datePosition (strlen date) 1)
          counter (substr supersededFilePath counterPosition 1)
        )
        (cond
          ( (= counter ".")
            (vl-file-rename supersededFilePath (vl-string-subst "-1.dwg" ".dwg" supersededFilePath))
            (setq supersededFilePath (vl-string-subst "-2.dwg" ".dwg" supersededFilePath))
          )
          ( (= counter "-")
            (setq
              counterPosition (+ datePosition (strlen date) 2)
              counter (substr supersededFilePath counterPosition 1)
              supersededFilePath (vl-string-subst (strcat "-" (itoa (+ (atoi counter) 1)) ".dwg") (strcat "-" counter ".dwg") supersededFilePath)
            )
          )
        );END cond
        (princ "\nsupersededFilePath = ")(princ supersededFilePath)
      );END while
    );END if
    (alert "(vl-file-systime filePath) = false")
  );END if
  filePath
)
(defun DT:AddFileNameCounter

)
