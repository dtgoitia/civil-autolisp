(defun c:1( /
            ss
            pile_list pile_ID_list
            att_p att_w
          )
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
  (setq
    ss (ssget '(( 0 . "INSERT" )))
    pile_list nil
    pile_ID_list nil
  )
  (foreach a (ssnamex ss)
    (if (= 'ename (type (cadr a)))
        (progn
;          (princ "\nBlock found = ")(princ (cadr a))
          (if (= "pile_DT" (LM:effectivename (vlax-ename->vla-object (cadr a)) ))
            (progn
              (setq
                pile_list
                (append pile_list
                  (list (list
                    (LM:vl-getattributevalue (vlax-ename->vla-object (cadr a)) "P")
                    (LM:vl-getattributevalue (vlax-ename->vla-object (cadr a)) "W")
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
  ; OPERATION - Sort pile list by pile number
  (setq pile_ID_list (acad_strlsort pile_ID_list))
  (foreach a pile_ID_list
    (princ "\n")
    (princ a)
  );END foreach
  (princ)
)
