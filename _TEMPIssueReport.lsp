(defun c:xx ()
  ; Function trigger for test
  (c:IssueReport)
)
(defun c:IssueReport ( / tabNameList i )
  ; Show report with tabs with "NOT ISSUED YET"
  ; Get all tab names
  ; Run foreach over each tab
    ; Scan paperspace looking for "TEXT", within layer "MJA-Title", with (cons 0 "NOT ISSUED YET") and (cons 62 1)
    ; if found add tab name to a list that will be returned later.
    ; if not found jump to next tab
  ; if any list returned
  (vlax-for x
	  (vla-get-Layouts (vla-get-ActiveDocument (vlax-get-acad-object) ))
    (setq tabNameList (append tabNameList (list (vla-get-Name x))))
  );END vlax-for

  (setq i 0)
  (if tabNameList
    (foreach a tabNameList
      (setq i (+ i 1))
      (princ "\n")(princ i)(princ " ")(princ a)
    );END foreach
  );END if
  (princ )
)
