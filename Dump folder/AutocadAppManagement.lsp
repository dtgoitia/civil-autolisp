(defun ListAppIDs ( / item lst)
  ; List registered app IDs
  (while (setq item (tblnext "APPID" (not item)))
    (setq lst (cons (cdr (assoc 2 item)) lst))
  )
  (acad_strlsort lst)
)
("KEYSYSTEMS_KGM_CONT_22970PEBRA" "KEYSYSTEMS_ROAD_CTLN_22970PEBRA" "KEYSYSTEMS_SECT_LSEC_22970PEBRA" "KEYSYSTEMS_SURV_STRS_22970PEBRA" "PalladioXData" "PE_URL" "PLOTTRANSPARENCY" "RAK")
(defun c:test( / apps )
  (if (setq apps (ListAppIDs) )
    (foreach appID apps
      (princ (strcat "\nApp \"" appID "\": "))
      (princ (entget (car (entsel)) '( "KEYSYSTEMS_KGM_CONT_22970PEBRA" ) ))
      (princ "\n.\n.")
    );END foreach
  );END if
)
(defun c:test( / apps )
  (if (setq apps (ListAppIDs) )
    (foreach appID apps
      (princ (strcat "\nApp \"" appID "\": "))
      (princ (entget (car (entsel)) '( "KEYSYSTEMS_KGM_CONT_22970PEBRA" ) ))
      (princ "\n.\n.")
    );END foreach
  );END if
)
