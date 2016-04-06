(princ "\n.")
(princ "\n.")
(princ "\n.")
(princ "\n.")
(princ "\nLoading David's developing library:\n")
(setq library
  (list
  ; Libraries
  '("0 - Function library.lsp" "--")
  '("0 - Personal library.lsp" "--")

  ;; General commands
  '("0 - Dynamic Interpolation.lsp" "DIP")
  '("0 - Explode polyline.lsp" "EP")
  '("0 - Title Block Update.lsp" "TBU")
  '("RT.lsp" "RT")

  ;; CIVILS
  '("CIVIL - Load blocks and insertion routines.lsp" "--")
  '("CIVIL - Interpolate level.lsp" "INT")
  '("CIVIL - Interpolate level 3D.lsp" "INT3D")


  ;; STRUCTURALS
  '("STRUCTURAL - Create building blocks.lsp" "BB")
  )
)
(setq
  listado_de_rutinas_no_cargadas nil
  contador_de_rutinas_no_cargadas 0
)
(foreach item library
    (setq
      url (car item)
      url_length (strlen url)
      i 0 ; counter
      position 40
      shortcut (cadr item)
    )
    (princ "\n")
    (princ (strcat "\nLoading \"" url "\" file..."))
    (while (< i (- position url_length))
      (princ ".")
      (setq i (+ i 1))
    )
    (if (= (load url "NO_LOADED") "NO_LOADED")
      (progn ; True
        (princ ".................................ERROR.")
        (setq listado_de_rutinas_no_cargadas (append listado_de_rutinas_no_cargadas (list url)))
        (setq contador_de_rutinas_no_cargadas (+ contador_de_rutinas_no_cargadas 1))
      ); END progn True
      (princ (strcat "correct.  c:" shortcut))  ; False
    ); END if
)
(princ "\n-")
(if (= contador_de_rutinas_no_cargadas 0)
  (princ "\nAll routines were correctly loaded.")
  (progn
    (princ (strcat "\nNumber of not loaded routines: "  (itoa contador_de_rutinas_no_cargadas) ))
    (setq contador 1)
    (foreach rutina listado_de_rutinas_no_cargadas
      (princ (strcat "\n  " (itoa contador) " - " rutina))
      (setq contador (+ 1 contador))
    )
  )
); END if

(princ)
