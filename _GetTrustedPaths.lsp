(defun c:1()
  ;|
  C:\Users\davidt\appdata\roaming\keyterra-firma\ktf\7.17\support\
  c:\program files\ktf\7.17\2017\
  \\mjafs01\data\standard details library\mja standards\mja engineering menu\development
  \\mjafs01\data\standard details library\mja standards\mja engineering menu
  |;
  (GetTrustedPaths)
)
(defun GetTrustedPaths( / var paths )
  (setq
    var (getvar "trustedpaths")
    paths (list
      "\\\\mjafs01\\data\\standard details library\\mja standards\\mja engineering menu"
      "\\\\mjafs01\\data\\standard details library\\mja standards\\mja engineering menu\\development"
      )
  );END setq
  (princ "\n.")
  (princ "lst = ")(princ lst)
  (princ "\n.")
  (setq ii 0)
  (foreach a lst
    (setq ii (+ ii 1))
    (princ "\n")(princ ii)(princ " - ")(princ a)
  );END foreach
  (princ)
)
