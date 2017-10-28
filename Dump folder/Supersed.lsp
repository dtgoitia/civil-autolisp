(defun c:1 () (c:ss) )
(defun c:ss ( / currentFile currentFileName currentFilePath )
  ; Supersed current file

  (setq
    currentFile (getvar "dwgname")
    currentFileName (vl-filename-base currentFile)
    currentFileDirectory (getvar "dwgprefix")
    currentFilePath (strcat currentFileDirectory currentFile)
  )

  ; Ensure superseded folder exists
  (if (setq supersededFolderSubdirectory (DT:GetSupersededFolderName currentFileDirectory))
    ; exists. Carry on
    (princ "\nSuperseded folder found...")
    ; doesn't exist. Create it
    (DT:CreateSupersededFolder currentFileDirectory)
  );END if

  ; Get new file name
  (setq newFile (DT:GetSupersededFileName currentFile ) )

  ; Check if the file exists, if not create superseded copy
  (setq newFilePath (strcat currentFileDirectory "" supersededFolderSubdirectory "\\" newFile) )

  (princ "\nnewFilePath = ")(princ newFilePath)

  (princ )
  ; Look for existing superseded copies with the same name
  ; Rename
  ; Look for existing superseded copies with the same name
  ; Rename again...
  ; Look for existing superseded copies with the same name

  ; v0.0 - 2017.05.10 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.05.10
)
(defun DT:GetSupersededFileName ( fileName )
  ; Return a string with supersided file name and extension
  ; IN  = "EngArch.dwg"
  ; OUT = "EngArch 10.05.17.dwg"
  (if fileName
    (if (= 'str (type fileName))
      (strcat (vl-filename-base fileName) " "(PrintSupersedDate) ".dwg")
      (progn (princ "\nERROR @ function : fileName is not a string\n") nil )
    );END if
    (progn (princ "\nERROR @ function : fileName=nil\n") nil )
  );END if

  ; v0.0 - 2017.05.10 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.05.10
)
(defun DT:CreateSupersededFolder ( fileDirectory )
  ; Create superseded folder and return the path
  ; if unsucessfull return nil
  (if fileDirectory
    (if (= 'str (type fileDirectory))
      (progn
        (setq
          newDirectory (strcat fileDirectory "\\Superseded")
          return (vl-mkdir newDirectory)
        )
      );END progn
      (progn (princ "\nERROR @ DT:CreateSupersededFolder : fileDirectory is not a string\n") nil )
    );END if
    (progn (princ "\nERROR @ DT:CreateSupersededFolder : fileDirectory=nil\n") nil )
  );END if

  ; Return new direectory path
  (if return
    newDirectory
    nil
  );END if

  ; v0.0 - 2017.05.10 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.05.10
)
(defun DT:GetSupersededFolderName ( currentFileDirectory / subdirectoryList return )
  ; Return superseded folder subdirectory name if found within "currentFileDirectory"
  ; Possible superseded folder subdirectory names:
  ;  - Superseded
  ;  - Supersed
  ;  - SS
  (if currentFileDirectory
    (if (= 'str (type currentFileDirectory))
      (if (setq subdirectoryList (vl-directory-files currentFileDirectory "*" -1))
        (foreach subdirectory subdirectoryList
          (if (= subdirectory "Superseded")
            (setq return "Superseded")
            (if (= subdirectory "Supersed")
              (setq return "Supersed")
              (if (= subdirectory "SS")
                (setq return "SS")
                (setq return nil)
              );END if
            );END if
          );END if
        );END foreach
      );END if
      (progn (princ "\nERROR @ DT:GetSupersededFolderName : currentFileDirectory is not a string\n") nil )
    );END if
    (progn (princ "\nERROR @ DT:GetSupersededFolderName : currentFileDirectory=nil\n") nil )
  );END if

  return

  ; v0.0 - 2017.05.10 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.05.10
)
