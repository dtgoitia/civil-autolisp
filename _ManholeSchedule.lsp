(defun c:FindManhole ( / id ent_name )
  ; Find manhole by ID
  ; If duplicated will show coordinates for duplicated manholes
  (if (setq id (getstring "\nIntroduce manhole reference: "))
    (if (setq ent_name (DT:GetManholeByID id))
      (cond
        ((= 'ename (type ent_name))
          (command "_.zoom" "O" ent_name "")
          (princ)
        );END subcond
        ((= 'list (type ent_name))
          (princ (strcat "\n" (itoa (length ent_name)) " manholes found under \"" id "\" reference." ))(princ)
          (foreach a ent_name
            (princ
              (strcat
                "\n\t"
                (LM:rtos (nth 0 (cdr (assoc 10 (entget a)))) 2 3)
                ","
                (LM:rtos (nth 1 (cdr (assoc 10 (entget a)))) 2 3)
              );END strcat
            );END princ
          );END foreach
          (princ)
        );END subcond
      );END cond
      (progn
        (princ "\nManhole not found.")
        (princ)
      );END progn
    );END if
  );END if

  ; v0.0 - 2017.03.14 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.03.14
)
(defun DT:GetManholeByID ( id / ss blockEffectiveName output )
  ; Return manhole ent_name
  (if id
    (if (= 'str (type id))
      (if (setq ss (ssget "x" '(( 0 . "INSERT")) ))
        (progn
          (foreach a (ssnamex ss)
            (if (= 'ename (type (cadr a)))
              (progn
                (setq blockEffectiveName (LM:effectivename (vlax-ename->vla-object (cadr a))))
                (if (= "W-Manhole" (substr blockEffectiveName 2 9))
                  (if (= id (LM:vl-getattributevalue (vlax-ename->vla-object (cadr a)) "ID" ))
                    (if (not output)
                      (setq output (cadr a))
                      (setq output (list output (cadr a)))
                    );END if
                  );END if
                );END if
                (setq blockEffectiveName nil)
              );END progn
            );END if
          );END foreach
          (if output
            output
            nil
          );END if
        );END progn
      );END if
      (progn (princ "\nERROR @ DT:GetManholeByID > id is not a string")(princ))
    );END if
    (progn (princ "\nERROR @ DT:GetManholeByID > id=nil")(princ))
  );END if

  ; v0.0 - 2017.03.14 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.03.14
)
