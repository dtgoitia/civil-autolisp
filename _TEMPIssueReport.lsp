(defun c:xx ()
  ; Function trigger for test
  (c:IssueReport)
)
(defun c:IssueReport ()
  ; Show report with tabs with "NOT ISSUED YET"
  ; Get all tab names
  ; Run foreach over each tab
    ; Scan paperspace looking for "TEXT", within layer "MJA-Title", with (cons 0 "NOT ISSUED YET") and (cons 62 1)
    ; if found add tab name to a list that will be returned later.
    ; if not found jump to next tab
  ; if any list returned
)
