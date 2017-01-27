(defun c:zz( / paths )
  (DT:GetTrustedPaths)
  ;(DT:StringToList "a" ".")
  ;(DT:StringToList "...a.a...a..." ".")
  ;(DT:StringToList ".hi.I am.splitted.and..very properly.splitted.....a...a." ".")
)
(defun DT:GetTrustedPaths( / var )
  (if (setq var (getvar 'trustedpaths))
    (DT:StringToList var ";")
    (progn (princ "\nERROR @ DT:GetTrustedPaths > var = nil")(princ))
  );END if

  ; v0.0 - 2017.01.27 - DT:StringToList implemented
  ; v0.0 - 2017.01.23 - First issue
  ; Author: David Torralban
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
  ; Author: David Torralban
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
  ; Author: David Torralban
  ; Last revision: 2017.01.27
)
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
  ; Author: David Torralban
  ; Last revision: 2017.01.27
)
