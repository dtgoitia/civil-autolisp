(defun c:11 () (DT:SetVar "*DT:associativeHatch*" 1) )
(defun DT:SetVar ( customVariable customVariableValue )
  (if customVariable
    (if (= 'str (type customVariable))
      (progn
        ;(princ "\ncustomVariable = ")customVariable
        (princ "\n(eval customVariable) = ")(eval customVariable)
        ;(princ "\n(read (eval customVariable)) = ")(read (eval customVariable))
        ;(princ "\n(read customVariable) = ")(read customVariable)
      );END progn
      (progn (princ "\nERROR @ DT:SetVar : customVariable is not a string\n") nil )
    );END if
    (progn (princ "\nERROR @ DT:SetVar : customVariable=nil\n") nil )
  );END if
  (princ )

  ; v0.0 - 2017.04.18 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.04.18
)
(defun setValue ( var val / tmp1 tmp2 )
  (if (not val)
    (progn
      (or
        (setq tmp1 (eval (read var)))
        (setq tmp1 "3£12")
      );END or
      (set (read var)
          (if (/= (setq tmp2
                        (getstring (strcat "Enter new value for [" var "] <" tmp1 ">")))
                  "")
           tmp2
           tmp1)
      );END set
    );END progn
    (set (read var) val)
  );END if
  (princ)
)
