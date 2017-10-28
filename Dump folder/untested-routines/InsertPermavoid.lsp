(defun c:insertPermavoid ( / p0 p1 ang readableAng )
  (command "_.pline" (setq p0 (getpoint)) pause "")
  (if (and p0 (setq p1 (getvar "lastpoint")))
    (progn
      (setq ang (angle p0 p1))
      (setq readableAng (DT:ReadableTextAngle ang))
      (entmakex
        (list
          (cons 0 "INSERT")
          (cons 2 "Private-Surface-Permavoid")
          (cons 10 (if (= ang readableAng) (polar p1 ang 0.708) p1))
          (cons 50 (if (= ang readableAng) ang readableAng))
        );END list
      );END entmakex
    );END progn
  );END if

  ; v0.0 - 2017.10.18 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.10.18
)
(defun c:insertPermavoidWithIc ( / p0 p1 p2 ang readableAng )
  (command "_.pline" (setq p0 (getpoint)) pause "")
  (if (and p0 (setq p1 (getvar "lastpoint")))
    (if (and
      (entmakex (list (cons 0 "INSERT") (cons 2 "e-psd-rwp") (cons 10 p0)))
      (entmakex (list (cons 0 "INSERT") (cons 2 "Private-Round475-Storm-Manhole") (cons 10 p1)))
    );END and
      (progn
        (command "_.pline" p1 pause "")
        (if (and p1 (setq p2 (getvar "lastpoint")))
          (progn
            (setq ang (angle p1 p2))
            (setq readableAng (DT:ReadableTextAngle ang))
            (entmakex
              (list
                (cons 0 "INSERT")
                (cons 2 "Private-Surface-Permavoid")
                (cons 10 (if (= ang readableAng) (polar p2 ang 0.708) p2))
                (cons 50 (if (= ang readableAng) ang readableAng))
              );END list
            );END entmakex
          );END progn
        );END if
      );END progn
    );END if
  );END if

  ; v0.0 - 2017.10.18 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.10.18
)
(defun c:1 () (c:insertPermavoid) )
(defun c:2 () (c:insertPermavoidWithIc) )
