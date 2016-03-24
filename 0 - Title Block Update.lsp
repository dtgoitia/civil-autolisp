(defun get_title_block_att ( txt_msg )
  ; (Re)load VLISP
  (vl-load-com)

	(setq
    ent (entsel txt_msg)                          ; INPUT - Select object
    ent_name (car ent)                            ; OPERATION - Get the entity and entity name
    VL_ent_name (vlax-ename->vla-object ent_name) ; OPERATION - Convert to VL object
  )

  ; OPERATION - Check if method 'getattributes is aplicable, if yes extract
  (if (vlax-method-applicable-p VL_ent_name 'getattributes)
    (progn
      ; Extract attributes (as strings)
      (setq
        field01 (get_block_att VL_ent_name "CLIENT-MID")
        field02 (get_block_att VL_ent_name "CLIENT-TOP")
        field03 (get_block_att VL_ent_name "CLIENT-BOT")
        field04 (get_block_att VL_ent_name "PROJECT-TOP")
        field05 (get_block_att VL_ent_name "PROJECT-BOT")
        field06 (get_block_att VL_ent_name "*:***")
        field07 (get_block_att VL_ent_name "***'**")
        field08 (get_block_att VL_ent_name "****")
        field09 (get_block_att VL_ent_name "****:**")
        field10 (get_block_att VL_ent_name "*")
        field11 (get_block_att VL_ent_name "**")
        field12 (get_block_att VL_ent_name "PRELIMINARY")
        field13 (get_block_att VL_ent_name "TITLE-TOP")
        field14 (get_block_att VL_ent_name "TITLE-MID")
        field15 (get_block_att VL_ent_name "TITLE-BOT")
        field16 (get_block_att VL_ent_name "SEC_A")
        field17 (get_block_att VL_ent_name "SEC_B")
        field18 (get_block_att VL_ent_name "SEC_C")
        field19 (get_block_att VL_ent_name "SEC_D")
        field20 (get_block_att VL_ent_name "SEC_E")
      )
      (princ
        (strcat
          "\n CLIENT-MID: " field01
          "\n CLIENT-TOP: " field02
          "\n CLIENT-BOT: " field03
          "\nPROJECT-TOP: " field04
          "\nPROJECT-BOT: " field05
          "\n      *:***: " field06
          "\n     ***'**: " field07
          "\n       ****: " field08
          "\n    ****:**: " field09
          "\n          *: " field10
          "\n         **: " field11
          "\nPRELIMINARY: " field12
          "\n  TITLE-TOP: " field13
          "\n  TITLE-MID: " field14
          "\n  TITLE-BOT: " field15
          ;"\n      SEC_A: " field16
          ;"\n      SEC_B: " field17
          ;"\n      SEC_C: " field18
          ;"\n      SEC_D: " field19
          ;"\n      SEC_E: " field20
        ); END strcat
      ); END princ
    ); END progn
    (princ "\nThis object is not a manhole.")         ; False. Method cannot be applied
  ); END if
  (princ)
)
(defun c:TBU (/ oldlayer oldosmode oldcmdecho
                p_ins p_ins2
                ID CL IL0 IL1 IL2 IL3 IL4
                txt_IL0 txt_IL1 txt_IL2 txt_IL3 txt_IL4
                ent ent_name VL_ent_name
              )
  ;TitleBoxUpdate

  ; SET - Error handling function
  (defun *error* ( msg )
    (if (not (member msg '("Function cancelled" "quit / exit abort")))
      (princ (strcat "\nError: " msg))
    )
    ; Restore previous settings
    (setvar "clayer" oldlayer)
    (setvar "osmode" oldosmode)
    (setvar "cmdecho" oldcmdecho)
    (princ)
  )

  ; SAVE CURRENT SETTINGS - Current layer, OSMODE and CMDECHO
  (setq oldlayer (getvar "clayer")
        oldosmode (getvar "osmode")
        oldcmdecho (getvar "cmdecho")
  )

  ; CHANGE INITIAL SETTINGS - "osmode" and "cmdecho"
  (setvar "osmode" 0)
  (setvar "cmdecho" 0)

  ; INPUT - Choose action
  (initget 1 "Copy Paste CLean")
  (setq answer (getkword "\nChoose [Copy/Paste/CLean]: "))

  ; OPERATION - Actions copy, paste and clean
  (cond
  ((= answer "Copy")
    ; INPUT - Seleccionar el manhole
    (get_title_block_att "\nSelect tittle block:")  ; Extraigo los atributos a las correspondientes variables (ver funcion mas arriba)
  ); END cond "Copy"
  ((= answer "Paste")
    ; Confirmar que hay algo que pegar y si no dar un mensaje de error
    (if (or
          (/= field01 nil)
          (/= field02 nil)
          (/= field03 nil)
          (/= field04 nil)
          (/= field05 nil)
          (/= field06 nil)
          (/= field07 nil)
          (/= field08 nil)
          (/= field09 nil)
          (/= field10 nil)
          (/= field11 nil)
          (/= field12 nil)
          (/= field13 nil)
          (/= field14 nil)
          (/= field15 nil)
          (/= field16 nil)
          (/= field17 nil)
          (/= field18 nil)
          (/= field19 nil)
          (/= field20 nil)
        )
      (progn
        (setq
          ent (entsel "\nSelect tittle block:")         ; INPUT - Select object
          ent_name (car ent)                            ; OPERATION - get the entity and entity name
          VL_ent_name (vlax-ename->vla-object ent_name) ; OPERATION - Convert to vl object
        )

        ; OPERATION - Update tittle block
        (LM:vl-setattributevalue VL_ent_name "CLIENT-MID" field01)
        (LM:vl-setattributevalue VL_ent_name "CLIENT-TOP" field02)
        (LM:vl-setattributevalue VL_ent_name "CLIENT-BOT" field03)
        (LM:vl-setattributevalue VL_ent_name "PROJECT-TOP" field04)
        (LM:vl-setattributevalue VL_ent_name "PROJECT-BOT" field05)
        (LM:vl-setattributevalue VL_ent_name "*:***" field06)
        (LM:vl-setattributevalue VL_ent_name "***'**" field07)
        (LM:vl-setattributevalue VL_ent_name "****" field08)
        (LM:vl-setattributevalue VL_ent_name "****:**" field09)
        (LM:vl-setattributevalue VL_ent_name "*" field10)
        (LM:vl-setattributevalue VL_ent_name "**" field11)
        (LM:vl-setattributevalue VL_ent_name "PRELIMINARY" field12)
        (LM:vl-setattributevalue VL_ent_name "TITLE-TOP" field13)
        (LM:vl-setattributevalue VL_ent_name "TITLE-MID" field14)
        (LM:vl-setattributevalue VL_ent_name "TITLE-BOT" field15)
        (LM:vl-setattributevalue VL_ent_name "SEC_A" field16)
        (LM:vl-setattributevalue VL_ent_name "SEC_B" field17)
        (LM:vl-setattributevalue VL_ent_name "SEC_C" field18)
        (LM:vl-setattributevalue VL_ent_name "SEC_D" field19)
        (LM:vl-setattributevalue VL_ent_name "SEC_E" field20)
      ); END progn
      (alert "Nothing copied before.") ; If false
    ) ; END if
  ); END cond "Paste"
  ((= answer "CLean")
    (progn
      ; OPERATION - Clean memory
      (setq field01 nil
            field02 nil
            field03 nil
            field04 nil
            field05 nil
            field06 nil
            field07 nil
            field08 nil
            field09 nil
            field10 nil
            field11 nil
            field12 nil
            field13 nil
            field14 nil
            field15 nil
            field16 nil
            field17 nil
            field18 nil
            field19 nil
            field20 nil
      )
      (princ "\nTitle box data removed from memory.")
    ); END progn
  ); END cond "Clean"
); END Cond

  ; RESTORE PREVIOUS SETTINGS
  (setvar "clayer" oldlayer)
  (setvar "osmode" oldosmode)
  (setvar "cmdecho" oldcmdecho)

  ; End without double messages
  (princ)

  ; v0.3 - 2016.03.23 - SEC_A to SEC_E omited in prompt, to avoid errors with old TitleBlock
  ; v0.2 - 2016.03.21 - Code optimized and translated into English.
  ; v0.1 - 2016.02.25 - New tittle bloc atributes added to routine.
  ; v0.0 - 2016.02.24
  ; Author: David Torralba
  ; Last revision: 2016.03.23
)
