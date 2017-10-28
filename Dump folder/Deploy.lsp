(defun DT:GetCharFromPosition ( str n )
  ; Return the character in the position n, 1-based numbering
  ; or return nil if n is out of range
  (if (and str n)
    (progn
      (if (= 'str (type n)) (setq n (atoi n)) ) ; convert n to integer
      (if (and (<= n (strlen str)) (> n 0) )     ; check n is not bigger than str length or les than zero
        (substr str n 1)
        nil
      );END if
    );END progn
    (progn (princ "\nERROR @ DT:GetCharPosition > str or n = nil")(princ))
  );END if

  ; v0.0 - 2017.01.27 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.01.27
)
(defun DT:GetCharPositions ( str ch / i ls )
  ; Return a list with the position of the character, 1-based numbering
  ; or return nil if nothing found
  (if (and ch str)
    (progn
      (setq i 0)
      (while (<= i (strlen str))
        (setq i (+ i 1))
        (if (= ch (DT:GetCharFromPosition str i) )
          (setq ls (append ls (list i) ))
        );END if
      );END while
      ls
    );END progn
    (progn (princ "\nERROR @ DT:GetCharPosition > ch or str = nil")(princ))
  );END if

  ; v0.0 - 2017.01.27 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.01.27
)
(defun DT:StringToList ( str delimiter / ls previousPosition )
  ; Return a list with the stirng splitted
  ; str [str]       - String to split
  ; delimiter [str] - Single character string
  (if (and str delimiter)
    (if (and (= 'str (type str)) (= 'str (type delimiter)))
      (if (= 1 (strlen delimiter))
        (progn
          (if (setq positionList (DT:GetCharPositions str delimiter) )
            (if (/= (strlen str) (length positionList))
              (progn
                (foreach currentPosition positionList
                  (setq
                    previousPosition (if previousPosition previousPosition 1)
                    ls (if (= previousPosition currentPosition)
                      ls ; don't do anything
                      (append ls (list (substr str previousPosition (- currentPosition previousPosition)) ))
                    );END if
                    previousPosition (+ currentPosition 1)
                  )
                );END foreach
                (setq
                  ls (if (= previousPosition (+ (strlen str) 1))
                      ls ; don't do anything
                      (append ls (list (substr str previousPosition (- (+ (strlen str) 1) previousPosition)) ))
                    );END if
                  );END setq
                ls
              );END progn
              nil
            );END if
            (list str)
          );END if
        );END progn
        (progn (princ "\nERROR @ DT:StringToList > delimiter can only has 1 character")(princ))
      );END if
      (cond
        ((/= 'str (type str)      ) (princ "\nERROR @ DT:StringToList > str is not a string")(princ) )
        ((/= 'str (type delimiter)) (princ "\nERROR @ DT:StringToList > delimiter is not a string")(princ) )
      );END cond
    );END if
    (progn (princ "\nERROR @ DT:StringToList > str or delimiter = nil")(princ))
  );END if

  ; v0.0 - 2017.01.27 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.01.27
)
(defun DT:GetTrustedPaths ( / trustedPaths )
  ; Return a list with each trusted path as a list element
  (if (setq trustedPaths (getvar "trustedpaths"))
    (DT:StringToList trustedPaths ";")
    (progn (princ "\nERROR @ DT:GetTrustedPaths > trustedPaths = nil\n") nil )
  );END if

  ; v0.2 - 2017.04.12 - Code tidy up
  ; v0.1 - 2017.01.27 - DT:StringToList implemented
  ; v0.0 - 2017.01.23 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.04.12
)
(defun DT:AddTrustedPath ( newTrustedPath )
  (if newTrustedPath
    (if (= 'str (type newTrustedPath))
      (setvar "trustedpaths"
        (strcat
          (getvar "trustedpaths")
          ";"
          newTrustedPath
        );END strcat
      );END setvar
      (progn (princ "\nERROR @ DT:AddTrustedPath > newTrustedPath is not a string\n") nil )
    );END if
    (progn (princ "\nERROR @ DT:AddTrustedPath > newTrustedPath = nil\n") nil )
  );END if

  ; v0.0 - 2017.04.12 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.04.12
)
(defun DT:RemoveTrustedPath ( trustedPathToRemove / updatedTrustedPaths currentTrustedPaths return )
  ; Return T if "trustedPathToRemove" was found and removed from trusted paths,
  ; otherwise will return nil
  (if trustedPathToRemove
    (if (= 'str (type trustedPathToRemove))
      (progn
        ; Get current trusted paths
        (if (setq currentTrustedPaths (DT:GetTrustedPaths))
          (foreach path currentTrustedPaths
            (if (/= path trustedPathToRemove)
              (if updatedTrustedPaths
                (setq updatedTrustedPaths (strcat updatedTrustedPaths ";" path))
                (setq updatedTrustedPaths path)
              );END if
            );END if
          );END foreach
        );END if

        ; Set return
        (if (= (getvar "trustedpaths") updatedTrustedPaths )
          (setq return nil)
          (setq return T)
        );END if

        ; Update trusted paths
        (setvar "trustedpaths" updatedTrustedPaths)

        ; Return value
        return

      );END progn
      (progn (princ "\nERROR @ DT:RemoveTrustedPath > newTrustedPath is not a string\n") nil )
    );END if
    (progn (princ "\nERROR @ DT:RemoveTrustedPath > trustedPathToRemove = nil\n") nil )
  );END if

  ; v0.0 - 2017.04.12 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.04.12
)
(defun DT:GetSupportPaths ( / supportPaths )
  ; Return a list with each trusted path as a list element
  (if (setq supportPaths (getenv "ACAD"))
    (DT:StringToList supportPaths ";")
    (progn (princ "\nERROR @ DT:GetSupportPaths > supportPaths = nil\n") nil )
  );END if

  ; v0.0 - 2017.05.04 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.05.04
)
(defun DT:AddSupportPath ( newSupportPath )
  (if newSupportPath
    (if (= 'str (type newSupportPath))
      (setenv "ACAD"
        (strcat
          (getenv "ACAD")
          ";"
          newSupportPath
        );END strcat
      );END setvar
      (progn (princ "\nERROR @ DT:AddSupportPath > newSupportPath is not a string\n") nil )
    );END if
    (progn (princ "\nERROR @ DT:AddSupportPath > newSupportPath = nil\n") nil )
  );END if

  ; v0.0 - 2017.04.12 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.04.12
)
(defun DT:RemoveSupportPath ( supportPathToRemove / updatedSupportPaths currentSupportPaths return )
  ; Return T if "supportPathToRemove" was found and removed from trusted paths,
  ; otherwise will return nil
  (if supportPathToRemove
    (if (= 'str (type supportPathToRemove))
      (progn
        ; Get current trusted paths
        (if (setq currentSupportPaths (DT:GetSupportPaths))
          (foreach path currentSupportPaths
            (if (/= path supportPathToRemove)
              (if updatedSupportPaths
                (setq updatedSupportPaths (strcat updatedSupportPaths ";" path))
                (setq updatedSupportPaths path)
              );END if
            );END if
          );END foreach
        );END if

        ; Set return
        (if (= (getenv "acad") updatedSupportPaths )
          (setq return nil)
          (setq return T)
        );END if

        ; Update trusted paths
        (setenv "acad" updatedSupportPaths)

        ; Return value
        return

      );END progn
      (progn (princ "\nERROR @ DT:RemoveSupportPath > newSupportPath is not a string\n") nil )
    );END if
    (progn (princ "\nERROR @ DT:RemoveSupportPath > supportPathToRemove = nil\n") nil )
  );END if

  ; v0.0 - 2017.05.04 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.05.04
)
(defun DT:SetEnterpriseMenu ( enterpriseMenuPath )
  ; DT:SetEnterpriseMenu
  ; enterpriseMenuPath [str] - Full path of the enterprise CUI file
  (if enterpriseMenuPath
    (if (= 'str (type enterpriseMenuPath))
      (if (findfile enterpriseMenuPath)
        (progn
          (setq enterpriseMenuPath (findfile enterpriseMenuPath))
          (setenv "EnterpriseMenuFile" enterpriseMenuPath)
        );END progn
        (progn (princ "\nERROR @ DT:SetEnterpriseMenu : no file found at provided enterpriseMenuPath\n") nil )
      );END if
      (progn (princ "\nERROR @ DT:SetEnterpriseMenu : enterpriseMenuPath is not a string\n") nil )
    );END if
    (progn (princ "\nERROR @ DT:SetEnterpriseMenu : enterpriseMenuPath=nil\n") nil )
  );END if
)
(defun DT:UnloadMenu ( menu )
  (if menu
    (if (= 'str (type menu))
      (progn
        (setvar "filedia" 0)
        (command "menuunload" menu)
        (setvar "filedia" 1)
      );END progn
      (progn (princ "\nERROR @ DT:UnloadMenu > menu is not a string\n") nil )
    );END if
    (progn (princ "\nERROR @ DT:UnloadMenu > menu = nil\n") nil )
  );END if

  ; v0.0 - 2017.05.04 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.05.04
)
(defun DT:UnloadMjaOldMenu ()
  (princ "\nUnloading old MJA Engineering Menu...")
  (DT:UnloadMenu "MJA_ENG")
  (princ " done")
)
(defun DT:SetSupportPaths ()
  (princ "\nRemoving old Support File Search Paths...")
  (DT:RemoveSupportPath "\\\\mjafs01\\data\\standard details library\\mja standards\\mja engineering menu\\development")
  (DT:RemoveSupportPath "\\\\mjafs01\\data\\standard details library\\mja standards\\mja engineering menu")
  (princ " done")
  (princ "\nAdding new Support File Search Paths...")
  (DT:AddSupportPath "\\\\mjafs01\\data\\standard details library\\mja standards\\mja engineering menu\\MjaEnterpriseCui")
  (DT:AddSupportPath "\\\\mjafs01\\data\\standard details library\\mja standards\\mja engineering menu\\MjaEnterpriseCui\\img")
  (DT:AddSupportPath "\\\\mjafs01\\data\\standard details library\\mja standards\\mja engineering menu\\MjaEnterpriseCui\\lsp")
  (DT:AddSupportPath "\\\\mjafs01\\data\\standard details library\\mja standards\\mja engineering menu\\MjaEnterpriseCui\\lsp\\old")
  (DT:AddSupportPath "\\\\mjafs01\\data\\standard details library\\mja standards\\mja engineering menu\\MjaEnterpriseCui\\dwg")
  (princ " done")
  (princ "\nRemoving old Trusted Paths...")
  (DT:RemoveTrustedPath "\\\\mjafs01\\data\\standard details library\\mja standards\\mja engineering menu\\development")
  (DT:RemoveTrustedPath "\\\\mjafs01\\data\\standard details library\\mja standards\\mja engineering menu")
  (princ " done")
  (princ "\nAdding new Trusted Paths...")
  (DT:AddTrustedPath "\\\\mjafs01\\data\\standard details library\\mja standards\\mja engineering menu\\MjaEnterpriseCui")
  (DT:AddTrustedPath "\\\\mjafs01\\data\\standard details library\\mja standards\\mja engineering menu\\MjaEnterpriseCui\\img")
  (DT:AddTrustedPath "\\\\mjafs01\\data\\standard details library\\mja standards\\mja engineering menu\\MjaEnterpriseCui\\lsp")
  (DT:AddTrustedPath "\\\\mjafs01\\data\\standard details library\\mja standards\\mja engineering menu\\MjaEnterpriseCui\\lsp\\old")
  (DT:AddTrustedPath "\\\\mjafs01\\data\\standard details library\\mja standards\\mja engineering menu\\MjaEnterpriseCui\\dwg")
  (princ " done")
)
(defun DT:SetMjaEnterpriseMenu ()
  (princ "\nAdding new MJA Engineering Menu...")
  (DT:SetEnterpriseMenu "\\\\mjafs01\\Data\\Standard Details Library\\MJA Standards\\MJA Engineering Menu\\MjaEnterpriseCui\\MjaEnterpriseCui.cuix")
  (princ " done")
)
(defun c:deploy ()
  (DT:UnloadMjaOldMenu)
  (DT:SetSupportPaths)
  (DT:SetMjaEnterpriseMenu)
  (alert "Please, restart AutoCAD to ensure everything runs propperly.\n\nShould you have any problem, blame David")
  (load "0 - Reload_LISP_library.lsp")
)
(defun c:000()
  (setenv "EnterpriseMenuFile" ".")
  (command "_.menuload" "\\\\mjafs01\\data\\standard details library\\mja standards\\mja engineering menu\\MJA Eng.cuix")
  (load "0 - Reload_LISP_library.lsp")
)
;(menucmd "P200=+MJA_ENG.POP1") ;load MJA Enterprise POP menu
(menucmd "P201=+MjaEnterpriseCui.POP1") ;load MJA Enterprise POP menu
(menucmd "P201=*")
(menucmd "MJAENTERPRISECUI.POP1=*")
(menucmd "MJAENTERPRISECUI.POP1=-")
(menucmd "MJAENTERPRISECUI.POP200=-")
(menucmd "P201=-")
(menucmd "P16=-")


; Crea a mano perfil con todo lo de MJA antiguo y normal.
; Crea rutina para duplicar perfil actual y cargar el nuevo perfil
; Crea rutina para deploy
; Crea rutina para testear
