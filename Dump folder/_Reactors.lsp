(vl-load-com)
; when prompt, when princ, when print
(prompt " \nLoad only...DO NOT RUN!\n")
(vlr-command-reactor "Backup After Plot" '((:vlr-commandEnded . endPlot)))

(defun endPlot  (calling-reactor endfommandInfo
                /
                theCommandEnd drgName newName
                )

)



(vlr-dwg-reactor "Save Complete"  '((:vlr-savecomplete . saveDrawingInfo)) )
(defun saveDrawingInfo (callingReactor commandInfo
                        /
                        dwgName fileSize
                        reactType reactData reactCall reactEvent reactCallback
                        )

  (setq
    reactInfo     callingReactor                        ; Get the reactor Object
    reactType     (vl-symbol-name (vlr-type reactInfo)) ; Get the reactor Type
    reactData     (vlr-data reactInfo)                  ; Get the Application Data
    reactCall     (car (vlr-reactions reactInfo))       ; Get the Callback list
    reactEvent    (vl-symbol-name (car reactCall))      ; Extract the Event Reactor
    reactCallback (vl-symbol-name (cdr reactCall))      ; Extract the Callback function
    dwgName       (cadr commandInfo)                    ; Get the Drawing Name
    fileSize      (vl-file-size dwgName)                ; Extract the file size
  );END setq

  (alert
    (strcat "The file size of " dwgName " is " (itoa fileSize) " bytes.")
  );END alert

  (alert
    (strcat
      "A \"" reactType "\" named \"" reactData "\" was triggered by a \""
      reactEvent "\" event call."
      "\nCallback Data was passed to the \"" reactCallback "\" call back function."
    );END strcat
  );END alert
  (princ)
);END defun
