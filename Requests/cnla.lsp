(defun c:cnla( / ent_name lay)
  ; Print nested object real layer and copy its name to clipboard
  (if (setq ent_name (car (nentsel "\nSelect object to know layer: ")))
    (progn
      (setq lay (DT:nla ent_name))
      (princ
        (strcat
          "\nDXF Layer = " lay
          " (colour " (itoa (cdr (assoc 62 (tblsearch "layer" lay)))) ")"
        )
      )
     (CopyToClipboard lay)
    ); END progn
  ); END if
  (princ)

  ; v0.0 - 2017.??.?? - First issue
  ; Author: David Torralba
  ; Last revision: 2017.??.??
);END defun
(defun DT:nla (ent_name / lay)
  ; Returns nested object's layer name
  (if (/= ent_name nil)
    (progn
      (mapcar
        '(lambda (x)
            (if (= (car x) 8) (setq lay (cdr x)) )
          ); END lambda
        (entget ent_name '("*"))
      );END mapcar)
      (setq lay lay)
    );END progn
  );END if

  ; v0.0 - 2017.??.?? - First issue
  ; Author: David Torralba
  ; Last revision: 2017.??.??
);END defun
