(defun DT:SortAlphanumeric ( l / onlyStrings chunkedList )
  ; Sort considering numbers and letters
  (if (DT:Arg 'DT:SortAlphanumeric '((l 'list)))
    (progn
      (setq onlyStrings T)
      (foreach element l (if (/= 'str (type element)) (setq onlyStrings nil)))
      (if onlyStrings
        (progn
          ; ALPHANUMERIC SORT ALGORITHM:
          ; Split strings in only number and only letter chunks
          (setq chunkedList (DT:SplitAlphanumericList l))
          ; Sort split strings
          (setq sortedchunkedList (DT:ShortChunkedLists chunkedList))
          ; Join chunks in sortedchunkedList
          (setq sortedList (DT:ConcatenateChunkedList sortedchunkedList))
        );END progn
        (DT:Error 'DT:SortAlphanumeric "one or more elements in l are not strings")
      );END if
    );END progn
  );END if
  sortedList

  ; v0.0 - 2017.06.02 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.06.02
)
(defun DT:SplitAlphanumericList ( l / return )
  ; Split passed list of strings in chunks of only letters and only numbers, and return it
  (if (DT:Arg 'DT:SplitAlphanumericList '((l 'list)))
    (progn
      (foreach element l
        (setq return (append return (list (DT:SplitAlphanumeric element))))
      );END foreach
      return
    );END progn
  );END if

  ; v0.0 - 2017.06.02 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.06.02
)
(defun DT:SplitAlphanumeric ( string / stringLength charPosition char asciiChar charType lastCharType return )
  ; Split string in chunks of only letters and only numbers, and return them in a list
  (if (DT:Arg 'DT:SplitAlphanumeric '((string 'str)))
    (progn
      (setq stringLength (strlen string))
      (setq charPosition 0)
      (repeat stringLength
        (setq charPosition (+ charPosition 1))
        (setq char (substr string charPosition 1))
        (setq asciiChar (ascii char))

        ; if same type, concatenate at the end of last element of the list
        ; if different type, add new element to list (add new chunck)
        (if (and (<= 48 asciiChar) (<= asciiChar 57))
          (progn
            ; char is a number
            (if lastCharType
              ; Later rounds
              (if (= 1 lastCharType)
                ; previous char was same type, concatenate
                (setq return (DT:SplitAlphanumericConcatenate return char))
                ; previous char was different type, add new chunk
                (setq return (append return (list char)))
              );END if
              ; First round
              (setq return (list char) )
            );END if
            (setq charType 1)
          );END progn
          (progn
            ; char is not a number
            (if lastCharType
              ; Later rounds
              (if (= 0 lastCharType)
                ; previous char was same type, concatenate
                (setq return (DT:SplitAlphanumericConcatenate return char))
                ; previous char was different type, add new chunk
                (setq return (append return (list char)))
              );END if
              ; First round
              (setq return (list char) )
            );END if
            (setq charType 0)
          );END progn
        );END if
        (setq lastCharType charType)

      );END repeat
      return
    );END progn
  );END if

  ; v0.0 - 2017.06.02 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.06.02
)
(defun DT:SplitAlphanumericConcatenate ( l char / nChunks i return )
  ; Concatenate at the end
  (setq nChunks (length l))
  (setq i 0)
  (foreach chunk l
    (setq i (+ i 1))
    (if (= i nChunks)
      (setq return (append return (list (strcat chunk char))))
      (setq return (append return (list chunk)))
    );END if
  );END foreach
  return

  ; v0.0 - 2017.06.02 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.06.02
)
(defun DT:SplitAlphanumericAddChunk ( l char )
  ; Add a new chunk
  (append l (list char))
  ; v0.0 - 2017.06.02 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.06.02
)
(defun DT:ConcatenateChunkedList ( chunkedList / return )
  ; Concatenate chunked list
  (if (DT:Arg 'DT:ConcatenateChunkedList '((chunkedList 'list)))
    (mapcar
      '(lambda (chunk)
        (apply 'strcat chunk)
      );END lambda
      chunkedList
    );END mapcar
  );END if

  ; v0.0 - 2017.06.02 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.06.02
)
(defun DT:CompareChunks ( a b )
  ; Compare chunks and return:
  ;  0 : if a = b
  ;  1 : if a < b
  ;  2 : if a > b
  (if (and a b)
    (progn
      (if (and (>= (ascii a) 48) (<= (ascii a) 57) ) (setq a (atoi a)) nil )
      (if (and (>= (ascii b) 48) (<= (ascii b) 57) ) (setq b (atoi b)) nil )
      (cond
        ; Both are strings
        ((and (= 'str (type a)) (= 'str (type b)))
          (if (< a b) 1 (if (> a b) 2 0) )
        );END subcond

        ; Both are numbers
        ((and (numberp a) (numberp b))
          (if (< a b) 1 (if (> a b) 2 0) )
        );END subcond

        ; One number and one letter
        ((or (and (= 'str (type a)) (numberp b) ) (and (numberp a) (= 'str (type b)) ) )
          (if (numberp a) 1 2 )
        );END subcond

        ; Return nil if a or b was not a string or a number
        (t nil)
      );END cond
    );END progn
    nil
  );END if

  ; v0.0 - 2017.07.11 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.07.11
)
(defun DT:CompareChunksByIndex ( a b n )
  ; Compare a and b chunks' n-th elements:
  (if (and a b n) ; Ensure all a b and n are passed
    (if
      (and
        (<= n (length a))
        (<= n (length b))
      );END and
      (DT:CompareChunks (nth n a) (nth n b))
      nil
    );END if
    nil
  );END if

  ; v0.0 - 2017.07.11 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.07.11
)
(defun DT:ShortChunkedLists ( chunkedLists / i ii chunkA chunkB n comparisonResult return )
  ; Sort chunked lists according to ASCII
  (if (DT:Arg 'DT:ShortChunkedLists '((chunkedList 'list)))
    (progn
      (setq i (- (length chunkedList) 1))
      (setq return chunkedList)
      ; some of the iteration indexes are wrong, is a failure for 1 loop in "while" or in "repeat"
      (while (> i 0)
        (setq ii 0)
        (repeat i
          (setq chunkA (nth ii return))
          (setq chunkB (nth (+ ii 1) return))
          (setq n 0)
          (setq comparisonResult (tempDTCompare chunkA chunkB n))
          (cond
            ((not comparisonResult) ; A = B   leave them as they are
              nil
            );END subcond
            ((= 1 comparisonResult) ; A < B   leave them as they are
              nil
            );END subcond
            ((= 2 comparisonResult) ; A > B   swap them
              (setq return (LM:SubstNth chunkA (+ ii 1) return))
              (setq return (LM:SubstNth chunkB ii return))
            );END subcond
            (t
              (DT:Error 'DT:ShortChunkedLists "comparisonResult has an unexpected value.")
              (exit)
            );END subcond
          );END cond
          (setq ii (+ ii 1))
        );END repeat
        (setq i (- i 1))
      );END while
    );END progn
  );END if
  return

  ; v0.0 - 2017.06.02 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.06.02
)
(defun tempDTCompare (a b n / result)
  (if (= 0 (setq result (DT:CompareChunksByIndex a b n)))
    (tempDTCompare a b (+ n 1))
    result
  );END if
)
