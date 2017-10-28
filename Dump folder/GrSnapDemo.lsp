;; Demo Program 1  -  Lee Mac
;;
;; Basic Implementation (no keyboard input)
;; Prompts the user to pick a point and creates a POINT entity at the selected point.
;;
;; Requires GrSnap.lsp

(defun c:test1 ( / *error* grr osf osm )

    (defun *error* ( msg )
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (redraw) (princ)
    )
    
    (setq osf (LM:grsnap:snapfunction)
          osm (getvar 'osmode)
    )
    (princ "\nPick point: ")
    (while (= 5 (car (setq grr (grread t 15 0))))
        (redraw)
        (osf (cadr grr) osm)
    )
    (if (listp (cadr grr))
        (entmake (list '(0 . "POINT") (cons 10 (trans (osf (cadr grr) osm) 1 0))))
    )
    (redraw) (princ)
)

;; Demo Program 2  -  Lee Mac
;;
;; Full Implementation (osnap toggle / keyboard point input / osnap modifiers)
;; Prompts the user to specify a point and creates a POINT entity at the point specified.
;;
;; Requires GrSnap.lsp

(defun c:test2 ( / *error* gr1 gr2 msg osf osm pt1 str tmp )

    (defun *error* ( msg )
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (redraw) (princ)
    )
    
    (setq osf (LM:grsnap:snapfunction)
          osm (getvar 'osmode)
          msg "\nSpecify point: "
          str ""
    )
    (princ msg)
    (while
        (progn
            (setq gr1 (grread t 15 0)
                  gr2 (cadr gr1)
                  gr1 (car  gr1)
            )
            (cond
                (   (or (= 5 gr1) (= 3 gr1))
                    (redraw)
                    (osf gr2 osm)
                    (= 5 gr1)
                )
                (   (= 2 gr1)
                    (cond
                        (   (= 6 gr2)
                            (if (zerop (logand 16384 (setq osm (setvar 'osmode (boole 6 16384 (getvar 'osmode))))))
                                (princ "\n<Osnap on>")
                                (princ "\n<Osnap off>")
                            )
                            (princ msg)
                        )
                        (   (= 8 gr2)
                            (if (< 0 (strlen str))
                                (progn
                                    (princ "\010\040\010")
                                    (setq str (substr str 1 (1- (strlen str))))
                                )
                            )
                            t
                        )
                        (   (< 32 gr2 127)
                            (setq str (strcat str (princ (chr gr2))))
                        )
                        (   (member gr2 '(13 32))
                            (cond
                                (   (= "" str) nil)
                                (   (setq gr2 (LM:grsnap:parsepoint pt1 str))
                                    (setq osm 16384)
                                    nil
                                )
                                (   (setq tmp (LM:grsnap:snapmode str))
                                    (setq osm tmp
                                          str ""
                                    )
                                )
                                (   (setq str "")
                                    (princ (strcat "\n2D / 3D Point Required." msg))
                                )
                            )
                        )
                    )
                )
            )
        )
    )
    (if (listp gr2)
        (entmake (list '(0 . "POINT") (cons 10 (trans (osf gr2 osm) 1 0))))
    )
    (redraw) (princ)
)

;; Demo Program 3  -  Lee Mac
;;
;; Full Implementation with Vector Graphics
;; Prompts the user to specify the center of a circle and creates a circle with radius 1/20th
;; of the screen height, centered at the selected point.
;;
;; Requires GrSnap.lsp

(defun c:test3 ( / *error* ang fac gr1 gr2 inc lst mat msg ocs osf osm pt1 rad str tmp )

    (defun *error* ( msg )
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (redraw) (princ)
    )

    (setq fac 20.0
          ang 0.0
          inc (/ pi 25.0)
    )
    (repeat 50
        (setq lst (cons (list (cos ang) (sin ang)) lst)
              ang (+ ang inc)
        )
    )
    (setq lst (cons 1 (apply 'append (mapcar 'list (cons (last lst) lst) lst)))
          mat (trp (mapcar '(lambda ( x ) (trans x 1 2 t)) '((1.0 0.0) (0.0 1.0) (0.0 0.0))))
          ocs (trans '(0.0 0.0 1.0) 1 0 t)
          osf (LM:grsnap:snapfunction)
          osm (getvar 'osmode)
          msg "\nSpecify point for circle: "
          str ""
    )
    (princ msg)
    (while
        (progn
            (setq gr1 (grread t 15 0)
                  gr2 (cadr gr1)
                  gr1 (car  gr1)
            )
            (cond
                (   (or (= 5 gr1) (= 3 gr1))
                    (redraw)
                    (osf gr2 osm)
                    (setq rad (/ (getvar 'viewsize) fac))
                    (grvecs lst
                        (append
                            (mapcar 'append
                                (mxm mat
                                    (list
                                        (list rad 0.0 0.0)
                                        (list 0.0 rad 0.0)
                                       '(0.0 0.0 1.0)
                                    )
                                )
                                (mapcar 'list (trans gr2 1 2))
                            )
                           '((0.0 0.0 0.0 1.0))
                        )
                    )
                    (= 5 gr1)
                )
                (   (= 2 gr1)
                    (cond
                        (   (= 6 gr2)
                            (if (zerop (logand 16384 (setq osm (setvar 'osmode (boole 6 16384 (getvar 'osmode))))))
                                (princ "\n<Osnap on>")
                                (princ "\n<Osnap off>")
                            )
                            (princ msg)
                        )
                        (   (= 8 gr2)
                            (if (< 0 (strlen str))
                                (progn
                                    (princ "\010\040\010")
                                    (setq str (substr str 1 (1- (strlen str))))
                                )
                            )
                            t
                        )
                        (   (< 32 gr2 127)
                            (setq str (strcat str (princ (chr gr2))))
                        )
                        (   (member gr2 '(13 32))
                            (cond
                                (   (= "" str) nil)
                                (   (setq gr2 (LM:grsnap:parsepoint pt1 str))
                                    (setq osm 16384)
                                    nil
                                )
                                (   (setq tmp (LM:grsnap:snapmode str))
                                    (setq osm tmp
                                          str ""
                                    )
                                )
                                (   (setq str "")
                                    (princ (strcat "\n2D / 3D Point Required." msg))
                                )
                            )
                        )
                    )
                )
            )
        )
    )
    (if (listp gr2)
        (entmake
            (list
               '(0 . "CIRCLE")
                (cons 010 (trans (osf gr2 osm) 1 ocs))
                (cons 040 (/ (getvar 'viewsize) fac))
                (cons 210 ocs)
            )
        )
    )
    (redraw) (princ)
)

;; Matrix Transpose  -  Doug Wilson
;; Args: m - nxn matrix
 
(defun trp ( m )
    (apply 'mapcar (cons 'list m))
)
 
;; Matrix x Matrix  -  Vladimir Nesterovsky
;; Args: m,n - nxn matrices
 
(defun mxm ( m n )
    ((lambda ( a ) (mapcar '(lambda ( r ) (mxv a r)) m)) (trp n))
)
 
;; Matrix x Vector  -  Vladimir Nesterovsky
;; Args: m - nxn matrix, v - vector in R^n
 
(defun mxv ( m v )
    (mapcar '(lambda ( r ) (apply '+ (mapcar '* r v))) m)
)

(vl-load-com)
(princ)