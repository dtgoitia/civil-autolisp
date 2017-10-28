;|
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

This file contains the following routines:

--- (c:MulitpleObjectArea)
    To insert a text with the total area of the selected objects

--- (c:GetGullyArea) and (DT:GetGullyArea)
    Click > Get boundary > Get area > insert text with area
    Best way to use it:
      1. Split screen in to
      2. Isolate the gully area layer on right viewport.
      3. Draw a closed polyline around total catchment area to be split.
      4. Draw contours of catchment area.
      5. Draw lines to separate gully catchment areas on left viewport and
         use right viewport to run (c:GetGullyArea) command

--- (DT:GetArea)
    Return the area of the object if available

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
|;
; (defun c:xx ()
;   ; Trigger
;   (DT:AutoLoadFileFromCivilTemp "GullyCatchmentArea.lsp")
;   (c:UpdateAreaLabel)
;
;   ; v0.0 - 2017.08.18 - First issue
;   ; Author: David Torralba
;   ; Last revision: 2017.08.18
; )
(defun c:MulitpleObjectArea ( / ss totalArea pt )
  ; Insert text whith total area of selected objects
  (if (setq ss (ssget))
    (if (setq totalArea (DT:TotalArea ss))
      (if (setq ent_name (DT:DrawText (setq pt (cadr (grread 't))) (getvar "clayer") (strcat (LM:rtos totalArea 2 2) "m2") 1 0 ))
        (command "_.move" ent_name "" "_non" pt "_non" pause)
      );END if
    );END if
  );END if

  ; Return total area
  (if totalArea totalArea)

  ; v0.0 - 2017.04.18 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.04.18
)
(defun c:GetGullyArea ()
  ; Command version of DT:GetGullyArea
  (DT:GetGullyArea (getpoint))

  ; v0.0 - 2017.08.18 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.18
)
(defun DT:GetGullyArea ( point / enameLast area ename pt )
  ; Return the area value
  (if (DT:Arg 'DT:GetGullyArea '((point 'list)))
    (progn
      ; Get last entity name
      (setq enameLast (entlast))
      (command "-boundary" point "")
      (if (/= (entlast) enameLast)
        (if (setq area (DT:GetArea (entlast)))
          (if (setq ename (DT:DrawText (setq pt (cadr (grread 't))) (getvar "clayer") (strcat (LM:rtos area 2 0) "m2") 1 0 ))
            (command "_.move" ename "" "_non" pt "_non" pause)
          );END if
        );END if
      );END if
    );END progn
  );END if


  ; v0.0 - 2017.08.18 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.18
)
(defun DT:GetArea ( ename )
  ; Return the area of the object, if available
  (if (DT:Arg 'DT:GetArea '((ename 'ename)))
    (progn
      (setq object (vlax-ename->vla-object ename))
      (if (vlax-property-available-p object 'Area)
        (vlax-get-property object 'Area)
        (DT:Error 'DT:GetArea "AREA property is not available in this object")
      );END if
    );END progn
  );END if


  ; v0.0 - 2017.08.18 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.18
)
(defun c:UpdateAreaLabel ( / enamePoly areaPoly stringArea enameText )
  ; Update area label selecting polyline and label
  (if (setq enamePoly (car (entsel "\nSelect object to get area: ")))
    (if (setq areaPoly (DT:GetArea enamePoly))
      (if (setq stringArea (strcat (LM:rtos areaPoly 2 0) "m2"))
        (if (setq enameText (car (entsel (strcat "\nSelect text to update to " stringArea ": "))))
          (DT:SetText enameText stringArea)
        );END if
      );END if
    );END if
  );END if

  ; v0.0 - 2017.10.12 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.10.12
)
