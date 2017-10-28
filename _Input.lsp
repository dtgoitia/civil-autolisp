; (defun c:xx ()
;   ; Trigger
;   (DT:AutoLoadFileFromCivil "_Input.lsp")
;   (c:PreFilledGetString)
;
;   ; v0.0 - 2017.08.11 - First issue
;   ; Author: David Torralba
;   ; Last revision: 2017.08.11
; )
; (defun c:PreFilledGetString ()
;   ; Command version of DT:PreFilledGetString
;   ; (setq return (DT:PreFilledGetString "Type text: " "HW9"))
;   ; (setq return (DT:PreFilledGetString nil nil))
;   (setq return (DT:PreFilledGetString nil 1))
;   (DT:PrintVar 'return)
;
;   ; v0.0 - 2017.08.11 - First issue
;   ; Author: David Torralba
;   ; Last revision: 2017.08.11
; )
(defun DT:PreFilledGetString ( msg defaultText / in return ch )
  ; Ask user to input a string and returns the input
  ;  - Allows spaces
  ;  - Allows a message (msg)
  ;  - Allows a pretyped answer (defaultText) which can be edited by the user
  ;  - Clicking or pressing ENTER will finish the function and return the value
  ;
  ; msg [str]         - Optional string to shown before the input. Pass "" or
  ;                     nil to avoid it.
  ; defaultText [str] - Optional string shown as default input which can be
  ;                     edited by the user. Pass "" or nil to avoid it.

  (if (and
    (or (not msg)         (= (type msg) 'str)         )
    (or (not defaultText) (= (type defaultText) 'str) )
  );END and
    (progn
      (princ (strcat (if msg msg "") "\n" (if defaultText defaultText "")))
      (setq
        in (grread nil (+ 4 8) 1)
        return (if defaultText defaultText "")
      );END setq
      (while (and
          (= 2 (car in))    ; key is pressed
          (/= 13 (cadr in)) ; pressed key is not ENTER
        );END and
        (if (/= 8 (cadr in))
          (progn ; if pressed key is not DELETE
            (setq
              ch (chr (cadr in))        ; convert input key (chr) in to a character (ch)
              return (strcat return ch) ; join it with the previous string and store it
            )
            (princ ch)
          );END progn
          (progn ; if pressed key is DELETE
            (setq
              ch (chr (cadr in))                              ; convert input key (chr) in to a character (ch)
              return (substr return 1 (- (strlen return) 1 )) ; remove last character from the string
            )
            (princ ch)
          );END progn
        );END if
        (setq in (grread nil (+ 4 8) 1)) ; wait for new input
      );END while
      return
    );END progn
    (DT:Error 'DT:PreFilledGetString "msg or defaultText arguments can only be nil or a string")
  );END if

  ; v0.0 - 2017.08.11 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.11
)
