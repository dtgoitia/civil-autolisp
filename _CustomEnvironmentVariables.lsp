(defun c:ShowGlobalVariables ()
  ; Shortcut
  (c:ShowEnvironmentVariables)

  ; v0.0 - 2017.06.08 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.06.08
)
(defun c:ShowEnvironmentVariables ( / l )
  ; Show cusotm environment variable list and their values
  (if customEnvironmentVariables
    (progn
      (setq l (list (list "ENVVAR" "VALUE")))
      (foreach var customEnvironmentVariables
        (setq l (append l (list (list (vl-princ-to-string (nth 0 var)) (vl-princ-to-string (cdr var)) )) ))
      );END foreach
      (princ (DT:ListToTable l))
    );END progn
    (princ "\nNo custom environment variables found.")
  );END if

  (princ)

  ; v0.0 - 2017.06.08 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.06.08
)
(defun c:CleanCustomVariables ()
  ; Clean all custom environment variables
  (setq customEnvironmentVariables nil)
  (princ "\nAll custom environment variables deleted.")
  (princ)

  ; v0.0 - 2017.06.08 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.06.08
)
(defun DT:GetEnv ( variableName / var )
  ; Return custom environment variable value,
  ; return nil if not found
  (if (DT:Arg 'DT:GetEnv '((variableName 'str)))
    (if (setq var (assoc variableName customEnvironmentVariables))
      (cdr var)
      nil
    );END if
  );END if

  ; v0.0 - 2017.06.08 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.06.08
)
(defun DT:SetEnv ( variableName variableValue / tmp )
  ; Set custom environment variable
  (if (DT:Arg 'DT:SetEnv '((variableName 'str)))
    (if (assoc variableName customEnvironmentVariables)
      ; Check if the variable "variableName" already exists, and overwrite the "variableValue".
      (progn
        (foreach var customEnvironmentVariables
          (if (= variableName (nth 0 var))
            ; If variableValue = nil, don't keep the variable in the list
            (if variableValue
              (setq tmp (append tmp (list (cons variableName variableValue))))
            );END if
            (setq tmp (append tmp (list var)))
          );END if
        );END foreach
        (setq customEnvironmentVariables tmp)
      );END progn
      ; If doesn't exist just add it to the list:
      (setq customEnvironmentVariables (append customEnvironmentVariables (list (cons variableName variableValue))))
    );END if
  );END if

  ; v0.0 - 2017.06.08 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.06.08
)
