(defun DT:SubList ( l ii fi / lLength i returnLength return )
  ; Return a slice of l
  ; l [list] - List where to take the slice from
  ; ii [int] - Initial index, zero-based
  ; fi [int] - Final index, zero-based

  (if (DT:Arg 'DT:SubList '((l 'list)(ii 'int)(fi 'int)))
    (if (< ii fi)
      (progn
        (setq lLength (- (length l) 1))
        (if (and (<= ii lLength) (<= fi lLength))
          (if (= ii fi)
            ; return nil
            nil
            (progn
              (setq returnLength (+ (- fi ii) 1)) ; get number of elements to return
              (setq i 0)
              (repeat returnLength
                (setq return (append return (list (nth i l))))
                (setq i (1+ i))
              );END repeat
              return
            );END progn
          );END if
          (DT:Error 'DT:SubList
            (strcat "ii (" (itoa ii) ") and/or fi (" (itoa fi) ") is/are higher than the actual length of the list (" (itoa lLength) ")")
          );END DT:Error
        );END if
      );END progn
      (DT:Error 'DT:SubList
        (strcat "final index (" (itoa fi) ") cannot be bigger than initial index (" (itoa ii) ")" )
      );END DT:Error
    );END if
  );END if


  ; v0.0 - 2017.09.22 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.09.22
)
