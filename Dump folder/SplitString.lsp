(defun DT:SplitString ( string pattern / position chunkList )
  ; Split provided string based on the pattern and return it as a list of string chunks
  ; Example:  (DT:SplitString "\\PThis is chunk one\\Pand this is chunk 2.\\P(this is the 3rd chunk)\\P" "\\P")
  ; Return:   ("" "This is chunk one" "and this is chunk 2." "(this is the 3rd chunk)" "")

  (if (DT:Arg 'DT:SplitString '((string 'str)(pattern 'str)))
    (progn
      ; Continue while there is some string not converted to a chunk
      (while string
        (if (setq position (vl-string-search pattern string))
          ; If patter found
          (progn
            ; Add chunk to list
            (setq chunkList (append chunkList (list (substr string 1 position))))
            ; Prepare next loop values
            (setq string (substr string (+ position (strlen pattern) 1)) )
          );END progn
          ; If pattern not found
          (setq
            ; Take last bit of string and add it as a chunk
            chunkList (append chunkList (list string))
            ; Exit the loop
            string nil
          );END setq
        );END if
      );END while

      ; Return chunk list
      chunkList

    );END progn
  );END if

  ; v0.0 - 2017.06.29 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.06.29
)
