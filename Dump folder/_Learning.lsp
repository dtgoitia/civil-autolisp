(defun c:xx ()
  (setq filePath "C:/00/_PersonalLibrary.lsp")
  (if (download "https://raw.githubusercontent.com/dtgoitia/civil-autolisp/master/_PersonalLibrary.lsp" "C:/00/")
    (if (vl-file-delete filePath)
      (princ "\nFile downloaded and deleted succesfully.")
      (DT:Error 'vl-file-delete "impossible to delete the file")
    );END if
    (DT:Error 'download "something went wrong")
  );END if
  (princ)

  ; v0.0 - 2017.07.03 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.07.03
)
(defun download (url destinyDirectory / cp ok tmp utilityObject)
  ; Get utility object: object to store multiple usefull methods.
  (setq utilityObject (vla-get-Utility (vla-get-ActiveDocument (vlax-get-acad-object) ) ) )
  ; If "url" is an URL
  (if (= (vla-isurl utilityObject url) :vlax-true)
    ; If any error, return it
    (if (vl-catch-all-error-p (vl-catch-all-apply 'vla-GetRemoteFile (list utilityObject url 'tmp :vlax-true)))
      (princ "\nDownload error.")
      (progn
        (setq cp (strcat destinyDirectory (vl-filename-base url) (vl-filename-extension url)))
        (if (findfile cp) ; If a previous file with the same name exists, delete it.
          (vl-file-delete cp)
        );END if
        (if (vl-catch-all-error-p (vl-catch-all-apply 'vl-file-copy (list tmp cp)))
          (progn
            (princ "\nUnable to move the file \""
              (strcat (vl-filename-base cp)(vl-filename-extension cp))
              "\" since the directory \n\""
              tmp
              )
              (vl-file-delete tmp)
          );END progn
          (progn
            (vl-file-delete tmp)
            (if (vl-file-size cp)
              (if (zerop (vl-file-size cp))
                (progn
                  (vl-file-delete cp)
                  (princ "\nUnable to download the file.")
                );END progn
                (setq ok T)
              );END if
              (progn
                (princ "\nUnable to delete the file.")
              );END progn
            );END if
          );END progn
        );END if
      );END progn
    );END if
    (princ "\nThe url is not valid.")
  );END if
  ok
)
XX
