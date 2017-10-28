; (defun c:xx ()
;   ; Trigger
;   (DT:AutoLoadFileFromCivilTemp "TextHandling.lsp")
;   (c:MTextWidthHeight)
;
;   ; v0.0 - 2017.08.11 - First issue
;   ; Author: David Torralba
;   ; Last revision: 2017.08.11
; )
(defun c:MTextWidthHeight ()
  ; Command version of DT:MTextWidthHeight
  (DT:MTextWidthHeight (car (entsel)))

  ; v0.0 - 2017.08.11 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.11
)
(defun DT:MTextWidthHeight ( ename )
  ; Set MTEXT width and height to zero
  (if (DT:Arg 'DT:MTextWidthHeight '((ename 'ename)))
    (progn
      (setq object (vlax-ename->vla-object ename))
      (vlax-put-property object 'Width 0.0)
      (vlax-put-property object 'Height 0.0)
    );END progn
  );END if

  ; v0.0 - 2017.08.11 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.11
)
(defun c:UpdateTextJustification()
  ; Command version of DT:UpdateTextJustification
  (DT:UpdateTextJustification (car (entsel)))

  ; v0.0 - 2017.03.23 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.03.23
)
(defun DT:UpdateTextJustification ( ent_name / dirtyWay p1 p2 old_error old_sysvars )
  ; Update text justification without shifting it.
  ; This routine processes a single text. Create a c:UpdateTextJustification with
  ; a foreach to run over more than one text.
  ;
  ; Check ent_name input variable is correct
  ; Get text box relative coordinates
  ; Get text insertion point (assoc 10 ...) and rotation (assoc 50 ...)
  ; Calculate text box absolute coordinates (rotatin matrix? polar?)
  ; Calculate text middle point
  ; Create new text entity (entmakex) with desired justification in the point calculated in the previous step
  ; Delete old text

  ; SAVE SETTINGS
  ; (save_environment (list "osmode" "angdir" "angbase"))

  (if (= testmode 0)
    (progn
      (setq
        p0 (cdr (assoc 10 (entget ent_name)))
        rot (cdr (assoc 50 (entget ent_name)))
        rot (if (= 0 (getvar "angdir")) (setq rot rot) (setq rot (* -1 rot)))
        p1 (textbox (entget ent_name))
        p2 (cadr p1)
        p1 (car p1)
        p1 (list (+ (nth 0 p0) (nth 0 p1)) (+ (nth 1 p0) (nth 1 p1)) (+ (nth 2 p0) (nth 2 p1)) )
        p2 (list (+ (nth 0 p0) (nth 0 p2)) (+ (nth 1 p0) (nth 1 p2)) (+ (nth 2 p0) (nth 2 p2)) )
      )
      (princ "\nrot = ")(princ rot)
      (command
        "_.rectangle" "_non" p1 "_non" p2
        ^C
        ^C
        "_.rotate" (entlast) "" "_non" p1 (DT:RadToDeg rot)
      )
    );END progn
    (progn
      (princ "\nTEST MODE PROCEDURE:\n")
      (setq
        p0 (cdr (assoc 10 (entget ent_name)))
        rot (cdr (assoc 50 (entget ent_name)))
        p1 (textbox (entget ent_name))
        p2 (cadr p1)
        p1 (car p1)
        ang12 (angle p1 p2)
        pm12 (list (* 0.5 (+ (nth 0 p1) (nth 0 p2) )) (* 0.5 (+ (nth 1 p1) (nth 1 p2) )) (* 0.5 (+ (nth 2 p1) (nth 2 p2) )) )
        dist12 (distance (list 0 0 0) pm12)
        p3 (polar p0 (+ rot ang12) (* 0.5 dist12))
      )
      (entmakex (list (cons 0 "CIRCLE") (cons 10 p0) (cons 40 0.1)))
      ;(entmakex (list (cons 0 "CIRCLE") (cons 10 p1) (cons 40 0.1)))
      ;(entmakex (list (cons 0 "CIRCLE") (cons 10 p2) (cons 40 0.1)))
      (entmakex (list (cons 0 "CIRCLE") (cons 10 p3) (cons 40 0.1)))
      ;(entmakex (list (cons 0 "CIRCLE") (cons 10 pm12) (cons 40 0.1)))
      ;|
      (entmakex
        (append
          (list
            (cons 0 "TEXT")
            (assoc 1 (entget ent_name))
            (assoc 7 (entget ent_name))
            (assoc 8 (entget ent_name))
            (if (assoc 62 (entget ent_name))
              (assoc 62 (entget ent_name))
              (cons 62 256)
            );END if
            ;(cons 10 (polar p3 (* 1.5 pi) 2.2))
            (cons 10 p3)
            (cons 11 p3) ; needed for text justification
            (cons 40 (cdr (assoc 40 (entget ent_name))))
            (cons 50 (cdr (assoc 50 (entget ent_name))))
            (cons 71 0)
            (cons 72 1)
            (cons 73 2)
          );END list
        );END append
          ; 71=0, 72=1, 73=1 Bottom centre
          ; 71=0, 72=1, 73=2 Middle centre
      );END entmakex
      |;
    );END progn
  );END if
  ; RESTORE SETTINGS
  ; (restore_environment)

  ;(princ)

  ; v0.0 - 2017.03.23 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.03.23
)
