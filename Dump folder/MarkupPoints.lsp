(defun c:xx ()
  ; Trigger
  (defun *error* ( errorMessage ) (princ (strcat "n-------- ERROR: " errorMessage " --------n")) (vl-bt) (DT:ReportError))
  (DT:AutoLoadFileFromCivilTemp "ErrorTracing.lsp")
  (DT:AutoLoadFileFromCivilTemp "MarkupPoints.lsp")
  (DT:AutoLoadFileFromCivilTemp "SplitString.lsp")
  (princ (strcat "nTemp file loaded (" (DT:Now) ")"))(princ)
  (c:MarkupPoints)

  ; v0.0 - 2017.08.02 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.02
)
(defun DT:GetBreaklineCrossingPointCoordinate ( filePath / f txtLine chunkedString pointCoordinates return )
  ; Read filePath and return a list extract coordinates
  ; Dependencies:
  ;   DT:SplitString > (DT:AutoLoadFileFromCivilTemp "SplitString.lsp")
  (if (setq f (open filePath "r"))
    (progn
      (while (setq txtLine (read-line f))
        (setq chunkedString (DT:SplitString txtLine " "))
        (foreach chunk chunkedString
          (if (and
              (/= "" chunk) ; if the chunk is not ""
              (> 3 (length pointCoordinates)) ; and the the stored pointCoordinates has less than 3 elements (element=coordinate)
            );END or
            (setq pointCoordinates (append pointCoordinates (list (atof chunk))))
          );END if
        );END foreach
        (setq return (append return (list pointCoordinates)))
        (setq pointCoordinates nil)
      );END while
      (close f)
      return
    );END progn
    (DT:Error 'DT:GetBreaklineCrossingPointCoordinate "file was not opened")
  );END if

  ; v0.0 - 2017.08.16 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.16
)
(defun c:MarkupPoints ( / pointList )
  ; Command to mark up points for KTF ground models breaklines (error)
  (setq
    pointList (list
      '(874.176 1038.567 0.0)
      '(1047.225 1019.762 0.0)
    );END list
  );END setq
  (setq pointList (DT:GetBreaklineCrossingPointCoordinate "C:/Users/davidt/Desktop/kk.txt"))
  (foreach pt pointList
    (entmakex
      (list
        (cons 0 "CIRCLE")
        (cons 10 pt)
        (cons 40 10.0)
      );END list
    );END entmakex
  );END foreach

  ; v0.0 - 2017.08.02 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.02
)
