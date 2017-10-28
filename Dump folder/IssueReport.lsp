(defun c:xx ()
  ; Trigger
  (defun *error* ( errorMessage ) (princ (strcat "n-------- ERROR: " errorMessage " --------n")) (vl-bt) (DT:ReportError))
  (DT:AutoLoadFileFromCivilTemp "ErrorTracing.lsp")
  (DT:AutoLoadFileFromCivilTemp "IssueReport.lsp")
  (c:IssueReport)

  ; v0.0 - 2017.09.22 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.09.22
)
(DT:AutoLoadFileFromCivilTemp "SubList.lsp")
(defun c:IssueReport ( / ans tabNameList i tabInfo tabReport tabReportHeader filteredEntries filteredTabReport )
  ; Show report with tabs with "NOT ISSUED YET"

  ; This command will:
  ; 1. Get all tab names
  ; 2. Run over each tab and:
  ; 3.    Scan paperspace looking for "TEXT", within layer "MJA-Title", with (cons 0 "NOT ISSUED YET") and (cons 62 1)
  ;       if found add tab name to a list that will be returned later. If nothing found, jump to next tab
  ; 4. If any tab found
  ; if any list returned

  ; Ask user options
  (initget "All Pending")
  (if (not (setq ans (getkword "\nSelect report options [All/Pending] <Pending>: "))) (setq ans "Pending") )

  ; Get all tab names
  (princ "\nRetrieving tab list...")
  (setq tabNameList (layoutlist))

  (if tabNameList
    (progn
      ; Add header at the begining of the report
      (setq tabReport (list (list "-----" "-----" "-----")(list "LAYOUT NAME" "REVISION" "STATUS")(list "-----" "-----" "-----")) )
      ; Run over each tab getting the revision information
      (princ "\nLooking for tab information...")
      (foreach a tabNameList
        (setq tabReport (append tabReport (list (DT:GetTabInformation a))) )
      );END foreach

      ; Filter report based on option selected by the user
      (cond
        ((= "All" ans) (setq filteredTabReport tabReport) )
        ((= "Pending" ans)
          ; Get report headers
          (setq tabReportHeader (DT:SubList tabReport 0 2))
          (foreach reportEntry tabReport
            (if (= "NOT ISSUED YET" (nth 2 reportEntry))
              ; Store only the entries with "NOT ISSUED YET" status
              (setq filteredEntries (append filteredEntries (list reportEntry)))
            );END if
          );END foreach
          ; Join header and fitered entries
          (setq filteredTabReport (append tabReportHeader filteredEntries) )
        );END subcond
      );END cond

      ; Convert list type report into a string type report formated as a table
      (if tabReport
        (progn
          (princ "\nConverting extracted information...")
          (setq tabReportText (DT:ListToTable filteredTabReport))
          ; Add key at the end of the reports
          (setq tabReportText (strcat tabReportText "\n.\nKEY: \"????\"=more than 1 title block found in the tab\n"))
          ; Print report
          (princ tabReportText)
        );END progn
      );END if
    );END progn
  );END if

  (princ)

  ; v0.2 - 2017.09.22 - Option added: fliter only "NOT ISSUED YET" layouts
  ; v0.1 - 2017.06.15 - Key added
  ; v0.0 - 2017.06.12 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.09.22
)
(defun DT:GetLayoutLatestRevision ( tabName / stringA stringBC stringB stringC  )
  ; Return layout latest revision with the following format: A - B (C)
  ; being:
  ;   - A = layout revision
  ;   - B = latest revision box revision letter
  ;   - C = latest revision box revision date
  (if (not (setq stringA (DT:GetTabTitleBlockRevision tabName)))
    (setq stringA " ")
  );END if
  (if (= "??" stringA)
    ; If there is more than one title block within the tab, return "????"
    "????"
    ; If there is only 1 title block within the tab:
    (progn
      (if (setq stringBC (DT:GetTabRevisionBoxLastRevision tabName))
        (setq
          stringB (nth 0 stringBC)
          stringC (nth 1 stringBC)
        );END setq
        (setq
          stringB ""
          stringC ""
        );END setq
      );END if

      (strcat stringA "|" stringB " " stringC)
    );END progn
  );END if

  ; v0.1 - 2017.06.15 - Update return if more than 1 tittleblock found: (DT:GetTabTitleBlockRevision tabName)="??"
  ; v0.0 - 2017.06.12 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.06.15
)
(defun DT:GetTabRevisionBoxLastRevision ( tabName / ss object revisionLetter revisionDate return )
  ; Return a pair-list with "REVISION-LETTER" and "DATE" attribute values
  ; from the "Revision-box" block with the latest date found within "tabName" tab.
  ; If nothing found return nil.
  (if (DT:Arg 'DT:GetTabRevisionBoxLastRevision  '((tabName 'str)))
    (if (setq ss (ssget "x" (list (cons 67  1)(cons 410 tabName)(cons 8   "MJA-Title")(cons 0   "INSERT")) ))
      (progn
        (foreach a (ssnamex ss)
          (if (= 'ename (type (cadr a)))
            (progn
              (setq object (vlax-ename->vla-object (cadr a)))
              (if (setq revisionLetter (LM:vl-getattributevalue object "REVISION-LETTER"))
                (if (setq revisionDate (LM:vl-getattributevalue object "DATE"))
                  (if
                    (or
                      ; Current revision box is the first one to be checked
                      (not return)
                      ; Current revision box "DATE" attribute value is "00"
                      (= "00" revisionDate)
                      (and
                        ; Current revision box is NOT the first one to be checked
                        return
                        ; Previous revision box "DATE" attribute value was different to "00"
                        (/= "00" (nth 1 return))
                        ; Previous revision box date is previous to curent one
                        (<
                          (DT:DateStringToInt (nth 1 return))
                          (DT:DateStringToInt revisionDate)
                        )
                      );END and
                    );END or
                    (setq return (list revisionLetter revisionDate))
                  );END if
                );END if
              );END if
            );END progn
          );END if
        );END foreach
        return
      );END progn
      nil
    );END if
  );END if

  ; v0.1 - 2017.06.15 - Comments updated
  ; v0.0 - 2017.06.14 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.06.15
)
(defun DT:DateStringToInt ( stringDate )
  ; Return date as integer yyyymmdd
  ; Supported string formats:
  ; - dd.mm.yy
  ; - dd.mm.yyyy
  ; - yyyy.mm.dd
  ; if date format not understood will return nil
  (if (DT:Arg 'DT:DateStringToInt '((stringDate 'str)))
    (cond
      ; CASE 1: dd.mm.yy
      ( (= 8 (strlen stringDate))
        (atoi
          (strcat
            "20"
            (substr stringDate 7 2)
            (substr stringDate 4 2)
            (substr stringDate 1 2)
          );END strcat
        );END atoi
      );END subcond

      ; CASE 2: dd.mm.yyyy
      ( (and
          (= 10 (strlen stringDate))
          (= "." (substr stringDate 3 1)) ; 3rd character is a "."
        );END and
        (atoi
          (strcat
            (substr stringDate 7 4)
            (substr stringDate 4 2)
            (substr stringDate 1 2)
          );END strcat
        );END atoi
      );END subcond

      ; CASE 3: yyyy.mm.dd
      ( (and
          (= 10 (strlen stringDate))
          (= "." (substr stringDate 5 1)) ; 5th character is a "."
          (>= 12 (atoi (substr stringDate 6 2)))
        );END and
        (atoi
          (strcat
            (substr stringDate 1 4)
            (substr stringDate 6 2)
            (substr stringDate 9 2)
          );END strcat
        );END atoi
      );END subcond

      ; DEFAULT CASE: date format not understood
      (t
        nil
      );END subcond
    );END cond
  );END if

  ; v0.1 - 2017.06.15 - Comments updated
  ; v0.0 - 2017.06.14 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.06.15
)
(defun DT:GetTabTitleBlockRevision ( tabName / ss object revision return )
  ; Return "*" attribute value from the title block found within "tabName" tab.
  ; Return "??" if more than one title block found.
  ; Return nil if title block found.
  ; Return nil if "tabName" tab not found.
  (if (DT:Arg 'DT:GetTabTitleBlockRevision  '((tabName 'str)))
    (if (setq ss (ssget "x" (list (cons 67  1)(cons 410 tabName)(cons 8   "MJA-Title")(cons 0   "INSERT")) ))
      (progn
        (foreach a (ssnamex ss)
          (if (= 'ename (type (cadr a)))
            (progn
              (setq object (vlax-ename->vla-object (cadr a)))
              (if (setq revision (LM:vl-getattributevalue object "*"))
                (if return
                  ; This is NOT the first title block found
                  (setq return "??")
                  ; This is the first title block found
                  (setq return revision)
                );END if
              );END if
            );END progn
          );END if
        );END foreach
        return
      );END progn
      nil
    );END if
  );END if

  ; v0.1 - 2017.06.15 - Comments updated
  ; v0.0 - 2017.06.14 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.06.15
)
