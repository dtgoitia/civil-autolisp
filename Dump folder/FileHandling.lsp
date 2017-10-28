; (defun c:xx ()
;   ; Trigger
;
;   (DT:AutoLoadFileFromCivilTemp "FileHandling.lsp")
;   (c:CreateNewFile)
;
;   ; v0.0 - 2017.08.23 - First issue
;   ; Author: David Torralba
;   ; Last revision: 2017.08.23
; )
(defun c:CreateNewFile ( / newFilePath newFileHandler )
  ; Create a new file
  ; (setq newFilePath "C:/Users/davidt/%username%/test.txt")
  (setq newFilePath (strcat "C:/Users/" (getenv "username") "/Desktop/test.txt"))
  (princ (strcat "\nFile path: " newFilePath))
  (DT:WriteFile "conte\nnt" newFilePath)
  (princ)

  ; v0.0 - 2017.08.23 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.23
)
(defun DT:WriteFile ( content path / fileHandler )
  ; Write content to path
  ; New line = "\n"
  (if (setq fileHandler (open path "w"))
    (progn
      (write-line content fileHandler)
      (close fileHandler)
      (princ "\nFile successfuly written")
    );END progn
  );END if

  ; v0.0 - 2017.10.25 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.10.25
)
(defun c:Test1 ( / testPath mode )
  ; Set a test path
  (setq testPath "C:\\Users\\davidt\\Desktop\\test.txt")
  ; Open an empty fie
  ; You create an empty file if there is no file in the provided path (testPath), as long as you don't use "Read" mode.
  ; Mode:
  ;   r - Read
  ;   w - Write
  ;   a - Append
  (setq mode "r")
  (open testPath mode)
)
; (defun c:xx ( / filePath )
;   ; Descarga mi libreria personal
;   (setq filePath "C:\\_.lsp")
;   (if (= T (download "https://raw.githubusercontent.com/dtgoitia/civil-autolisp/master/_PersonalLibrary.lsp" "C:/"))
;     (princ "\nEverything looks alright.")
;     (princ "\nNo he podido descargar el archivo.")
;   );END if
;   (princ)
; )
;|
    getfiled (AutoLISP)
    findtrustedfile (AutoLISP)
    vl-directory-files (AutoLISP)
    vl-filename-directory (AutoLISP)

|;
