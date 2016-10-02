(defun c:cb ( /
              ss
              pile_ID_list        pile_list
              pile_ID_list_sorted pile_list_sorted
              att_p att_w
            )
  ; ERROR HANDLING FUNCTION ----------------------------------------------------
  (defun *error* ( msg )
    (if (not (member msg '("Function cancelled" "quit / exit abort")))
      (princ (strcat "\nError: " msg))
    )
    ; RESTORE PREVIOUS SETTINGS
    (setvar "osmode" oldosmode)
    (setvar "attdia" oldattdia)
    (setvar "attreq" oldattreq)
    (princ)
  )

  ; AUXILIARY FUNCTIONS --------------------------------------------------------
  (defun LM:effectivename ( obj )
  ;; Effective Block Name  -  Lee Mac
  ;; obj - [vla] VLA Block Reference object
      (vlax-get-property obj
          (if (vlax-property-available-p obj 'effectivename)
              'effectivename
              'name
          )
      )
  )
  (defun LM:vl-getattributevalue ( blk tag )
  ;; Get Attribute Value  -  Lee Mac
  ;; Returns the value held by the specified tag within the supplied block, if present.
  ;; blk - [vla] VLA Block Reference Object
  ;; tag - [str] Attribute TagString
  ;; Returns: [str] Attribute value, else nil if tag is not found.
      (setq tag (strcase tag))
      (vl-some '(lambda ( att ) (if (= tag (strcase (vla-get-tagstring att))) (vla-get-textstring att)))
          (vlax-invoke blk 'getattributes)
      )
  )
  (defun LM:rtos ( real units prec / dimzin result )
  ;; rtos wrapper  -  Lee Mac
  ;; A wrapper for the rtos function to negate the effect of DIMZIN
      (setq dimzin (getvar 'dimzin))
      (setvar 'dimzin 0)
      (setq result (vl-catch-all-apply 'rtos (list real units prec)))
      (setvar 'dimzin dimzin)
      (if (not (vl-catch-all-error-p result))
          result
      )
  )

  ; SAVE SYSTEM VARIABLES
  (setq
    oldosmode (getvar "osmode")
    oldattdia (getvar "attdia")
    oldattreq (getvar "attreq")
  )

  ; SET SYSTEM VARIABLES
  (setvar "osmode" 0)
  (setvar "attdia" 0)
  (setvar "attreq" 1)

  ; INPUT - Select blocks
  (setq
    ss (ssget '(( 0 . "INSERT" )))
    pile_list nil
    pile_ID_list nil
    pile_list_sorted nil
  )

  ; OPERATION - Run through each selected block
  (foreach a (ssnamex ss)
    (if (= 'ename (type (cadr a)))
        (progn
          ; OPERATION - If "pile_DT" block's effective name, get it's attributes
          (if (= "pile_DT" (LM:effectivename (vlax-ename->vla-object (cadr a)) ))
            (progn
              (setq
                pile_list
                (append pile_list
                  (list (list
                    (strcat "P" (LM:vl-getattributevalue (vlax-ename->vla-object (cadr a)) "P"))  ; pile number
                    (LM:vl-getattributevalue (vlax-ename->vla-object (cadr a)) "W")               ; weight
                    (LM:rtos (car (cdr (assoc 10 (entget (cadr a))))) 2 3)                        ; East coordinate
                    (LM:rtos (cadr (cdr (assoc 10 (entget (cadr a))))) 2 3)                       ; North coordinate
                    ; if any more property is added, copy and paste the line inmediately above and change the tag ("P", "W", etc.)
                  ));END list
                );END append
              )
              (setq
                pile_ID_list
                (append pile_ID_list (list (LM:vl-getattributevalue (vlax-ename->vla-object (cadr a)) "P")))
              );END setq
            );END progn
          );END if2
        );END progn
    );END if1
  );END foreach

  (if (= nil pile_ID_list)
    (alert "Ups! No pile block selected :) Try again!")
    (progn
      ; OPERATION - Sort pile list by pile number
      (setq pile_ID_list_sorted (acad_strlsort pile_ID_list))

      ; OPERATION - Rebuild the pile list from sorted ID list
      (foreach ID pile_ID_list_sorted
        (setq
          position (vl-position ID pile_ID_list) 		; position of the ID at pile_ID_list
          pile_list_sorted (append pile_list_sorted (list (nth position pile_list)))	; data associated to the current ID at pile_list
        )
      );END foreach

      ; INPUT - Ask user to select top left corner of the table
      (setq
        title (strcat (getstring T "\nEnter Titel of Co-ordinate Box.") " Co-ordinates")
        p_ins (getpoint "\nPick Top Left Corner of Box: ")
      )

      ; OPERATION - Build table
      ; Insert header
      (command "-insert" "PileTable_Header" p_ins "" "" "" title)
      ; Recalculate next insertion point
      (setq p_ins (polar p_ins (* pi -0.5) 8))
      ; Insert subheader
      (command "-insert" "PileTable_SubHeader" p_ins "" "" "")
      ; Recalculate next insertion point
      (setq p_ins (polar p_ins (* pi -0.5) 8))

      (foreach a pile_list_sorted
        ; Insert rows
        (command "-insert" "PileTable_Row" p_ins "" "" "" (nth 0 a) (nth 2 a) (nth 3 a))
        ; Recalculate next insertion point
        (setq p_ins (polar p_ins (* pi -0.5) 8))
      );END foreach
      ; Draw table bottom line
      (command "line" p_ins (polar p_ins 0 100) "")
    );END progn false
  );END if

  ; RESTORE PREVIOUS SETTINGS
  (setvar "osmode" oldosmode)
  (setvar "attdia" oldattdia)
  (setvar "attreq" oldattreq)

  (princ)

  ; v0.0 - 2015.07.29 - First issue
  ; Author: David Torralba
  ; Last revision: 2016.07.29
)
