(defun LM:getdynpropvalue ( blk prp )
  ;; Get Dynamic Block Property Value  -  Lee Mac
  ;; Returns the value of a Dynamic Block property (if present)
  ;; blk - [vla] VLA Dynamic Block Reference object
  ;; prp - [str] Dynamic Block property name (case-insensitive)

    (setq prp (strcase prp))
    (vl-some '(lambda ( x ) (if (= prp (strcase (vla-get-propertyname x))) (vlax-get x 'value)))
        (vlax-invoke blk 'getdynamicblockproperties)
    )
)
(defun LM:setdynpropvalue ( blk prp val )
  ;; Set Dynamic Block Property Value  -  Lee Mac
  ;; Modifies the value of a Dynamic Block property (if present)
  ;; blk - [vla] VLA Dynamic Block Reference object
  ;; prp - [str] Dynamic Block property name (case-insensitive)
  ;; val - [any] New value for property
  ;; Returns: [any] New value if successful, else nil

    (setq prp (strcase prp))
    (vl-some
       '(lambda ( x )
            (if (= prp (strcase (vla-get-propertyname x)))
                (progn
                    (vla-put-value x (vlax-make-variant val (vlax-variant-type (vla-get-value x))))
                    (cond (val) (t))
                )
            )
        )
        (vlax-invoke blk 'getdynamicblockproperties)
    )
)
(defun LM:getdynprops ( blk )
  ;; Get Dynamic Block Properties  -  Lee Mac
  ;; Returns an association list of Dynamic Block properties & values.
  ;; blk - [vla] VLA Dynamic Block Reference object
  ;; Returns: [lst] Association list of ((<prop> . <value>) ... )

    (mapcar '(lambda ( x ) (cons (vla-get-propertyname x) (vlax-get x 'value)))
        (vlax-invoke blk 'getdynamicblockproperties)
    )
)
(defun LM:setdynprops ( blk lst / itm )
  ;; Set Dynamic Block Properties  -  Lee Mac
  ;; Modifies values of Dynamic Block properties using a supplied association list.
  ;; blk - [vla] VLA Dynamic Block Reference object
  ;; lst - [lst] Association list of ((<Property> . <Value>) ... )
  ;; Returns: nil

    (setq lst (mapcar '(lambda ( x ) (cons (strcase (car x)) (cdr x))) lst))
    (foreach x (vlax-invoke blk 'getdynamicblockproperties)
        (if (setq itm (assoc (strcase (vla-get-propertyname x)) lst))
            (vla-put-value x (vlax-make-variant (cdr itm) (vlax-variant-type (vla-get-value x))))
        )
    )
)
(defun LM:getdynpropallowedvalues ( blk prp )
  ;; Get Dynamic Block Property Allowed Values  -  Lee Mac
  ;; Returns the allowed values for a specific Dynamic Block property.
  ;; blk - [vla] VLA Dynamic Block Reference object
  ;; prp - [str] Dynamic Block property name (case-insensitive)
  ;; Returns: [lst] List of allowed values for property, else nil if no restrictions

    (setq prp (strcase prp))
    (vl-some '(lambda ( x ) (if (= prp (strcase (vla-get-propertyname x))) (vlax-get x 'allowedvalues)))
        (vlax-invoke blk 'getdynamicblockproperties)
    )
)
(defun LM:toggleflipstate ( blk )
  ;; Toggle Dynamic Block Flip State  -  Lee Mac
  ;; Toggles the Flip parameter if present in a supplied Dynamic Block.
  ;; blk - [vla] VLA Dynamic Block Reference object
  ;; Return: [int] New Flip Parameter value

    (vl-some
       '(lambda ( prp / rtn )
            (if (equal '(0 1) (vlax-get prp 'allowedvalues))
                (progn
                    (vla-put-value prp (vlax-make-variant (setq rtn (- 1 (vlax-get prp 'value))) vlax-vbinteger))
                    rtn
                )
            )
        )
        (vlax-invoke blk 'getdynamicblockproperties)
    )
)
(defun LM:getvisibilityparametername ( blk / vis )
  ;; Get Visibility Parameter Name  -  Lee Mac
  ;; Returns the name of the Visibility Parameter of a Dynamic Block (if present)
  ;; blk - [vla] VLA Dynamic Block Reference object
  ;; Returns: [str] Name of Visibility Parameter, else nil

    (if
        (and
            (vlax-property-available-p blk 'effectivename)
            (setq blk
                (vla-item
                    (vla-get-blocks (vla-get-document blk))
                    (vla-get-effectivename blk)
                )
            )
            (= :vlax-true (vla-get-isdynamicblock blk))
            (= :vlax-true (vla-get-hasextensiondictionary blk))
            (setq vis
                (vl-some
                   '(lambda ( pair )
                        (if
                            (and
                                (= 360 (car pair))
                                (= "BLOCKVISIBILITYPARAMETER" (cdr (assoc 0 (entget (cdr pair)))))
                            )
                            (cdr pair)
                        )
                    )
                    (dictsearch
                        (vlax-vla-object->ename (vla-getextensiondictionary blk))
                        "ACAD_ENHANCEDBLOCK"
                    )
                )
            )
        )
        (cdr (assoc 301 (entget vis)))
    )
)
(defun LM:getvisibilitystate ( blk )
  ;; Get Dynamic Block Visibility State  -  Lee Mac
  ;; Returns the value of the Visibility Parameter of a Dynamic Block (if present)
  ;; blk - [vla] VLA Dynamic Block Reference object
  ;; Returns: [str] Value of Visibility Parameter, else nil

    (LM:getdynpropvalue blk (LM:getvisibilityparametername blk))
)
(defun LM:SetVisibilityState ( blk val / vis )
  ;; Set Dynamic Block Visibility State  -  Lee Mac
  ;; Sets the Visibility Parameter of a Dynamic Block (if present) to a specific value (if allowed)
  ;; blk - [vla] VLA Dynamic Block Reference object
  ;; val - [str] Visibility State Parameter value
  ;; Returns: [str] New value of Visibility Parameter, else nil

    (if
        (and
            (setq vis (LM:getvisibilityparametername blk))
            (member (strcase val) (mapcar 'strcase (LM:getdynpropallowedvalues blk vis)))
        )
        (LM:setdynpropvalue blk vis val)
    )
)
