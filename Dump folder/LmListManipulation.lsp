(defun LM:SubstNth ( a n l / i )
  ;;---------------------=={ Subst Nth }==----------------------;;
  ;;                                                            ;;
  ;;  Substitutes an item at the nth position in a list.        ;;
  ;;------------------------------------------------------------;;
  ;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;
  ;;------------------------------------------------------------;;
  ;;  Arguments:                                                ;;
  ;;  a - item to substitute                                    ;;
  ;;  n - position in list to make the substitution             ;;
  ;;  l - list in which to make the substitution                ;;
  ;;------------------------------------------------------------;;
  ;;  Returns:  Resultant list following the substitution       ;;
  ;;------------------------------------------------------------;;
  (setq i -1)
  (mapcar '(lambda ( x ) (if (= (setq i (1+ i)) n) a x)) l)
)
