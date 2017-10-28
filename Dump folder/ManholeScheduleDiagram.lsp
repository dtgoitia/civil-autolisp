(defun c:UMSD ( / manholeBlock insertionPoint pointList blockVisibilityState i angleReference angleList diagramObject )
  ; Update Manhole Schedule Diagram
  ; Set the manhole schedule diagram with the same number of brances and
  ; orientation as the manhole block and its input and output sewers
  (setq
    manholeBlock (car (entsel "\nSelect manhole: "))
    insertionPoint (cdr (assoc 10 (entget manholeBlock)))
    pointList (DT:ManholeScheduleDiagramGetPoints)

    ; Get manhole schedule diagram visibility state
    blockVisibilityState (itoa (- (length pointList) 1))

    ; angles of reference to compensate the starting angle of each branch within the manhole schedule diagram
    angleReference (list 0.0 1.76873 2.87959 4.04397 4.90793 5.60072)

    ; calculate angles for each branch taking insertionPoint as reference to find the angles, and
    ; correcting the angles with the angleReference list
    i -1
    angleList (mapcar
      '(lambda (pt)
        (setq i (+ i 1))
        (cons (strcat "Angle" (itoa i)) (- (angle insertionPoint pt) (nth i angleReference)))
      );END lambda
      pointList
    );END mapcar
  );END setq

  ; Select manhole schedule diagram visibility state
  (setq diagramObject (vlax-ename->vla-object (car (entsel "\nSelect manhole schedule diagram: "))))

  ; Set manhole schedule diagram visibility state
  (LM:SetVisibilityState diagramObject blockVisibilityState)

  ; Set branch positions where needed:
  (LM:SetDynProps diagramObject angleList)

  (princ)

  ; v0.0 - 2017.06.29 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.06.29
)
(defun DT:ManholeScheduleDiagramGetPoints ( / pt return )
  ; Return a point list with the selected points
  (while (setq pt (getpoint (strcat "\n" (itoa (length return)) " points selected. Press ENTER to escape or select point: ")))
    (setq return (append return (list pt)))
  );END while
  return

  ; v0.0 - 2017.06.29 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.06.29
)
(defun c:UMSP ( / pipeSize )
  ; Update Manhole Schedule Pipe sizes
  ; Insert Manhole Schedule pipe diametre in multiple places at once
  (if (setq pipeSize (getint "\nType pipeSize: DN"))
    (if (setq pipeSize (strcat "DN" (itoa pipeSize)))
      (while (setq ent_name (car (nentsel (strcat "\nSelect text to insert \"" pipeSize "\" text"))))
        (DT:SetText ent_name pipeSize)
      );END while
    );END if
  );END if

  ; v0.0 - 2017.06.29 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.06.29
)
