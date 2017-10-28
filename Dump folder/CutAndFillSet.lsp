;|
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

This file contains the following routines:

--- (c:Create_House_Strings) and (DT:CutFillHouseStrings)
    To model house perimeter and the 150mm step all in one go

--- (c:Create_Garage_Strings) and (DT:CutFillGarageStrings)
    To model garage perimeter in one go

--- (c:CutFillReformatFflText) and (DT:CutFillReformatFflText)
    To add a zero to FFL texts with only 2 decimals

--- (c:CutFillRetainingWall) and (DT:CutFillRetainingWall)
    To model a retaining wall

--- (c:CutFillSpotLevel)
    Dot a point by XY and type/click Z

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
|;
(defun c:xx ()
  ; Trigger
  (DT:AutoLoadFileFromCivilTemp "CutAndFillSet.lsp")
  (c:CutFillSpotLevel)

  ; v0.0 - 2017.07.25 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.07.25
)
(DT:AutoLoadFileFromCivilTemp "3dOffset.lsp")
(DT:AutoLoadFileFromCivilTemp "LevelDetection.lsp")
(DT:AutoLoadFileFromCivilTemp "PolylineUtils.lsp")
(defun c:Create_House_Strings ( / ss ffl doc )
  ; Create house strings and -150mm offset with all LWPOLYLINES selected

  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (vla-startUndoMark doc)

  (if (setq ss (ssget '(( 0 . "LWPOLYLINE" ))))
    (foreach a (ssnamex ss)
      (if (= 'ename (type (cadr a)))
        (DT:CutFillHouseStrings (cadr a))
      );END if
    );END foreach
  );END if
  (vla-endUndoMark doc)

  ; v0.0 - 2017.07.27 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.07.27
)
(defun c:Create_Garage_Strings ( / ss ffl doc )
  ; Create garage strings with all LWPOLYLINES selected

  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (vla-startUndoMark doc)

  (if (setq ss (ssget '(( 0 . "LWPOLYLINE" ))))
    (foreach a (ssnamex ss)
      (if (= 'ename (type (cadr a)))
        (DT:CutFillGarageStrings (cadr a))
      );END if
    );END foreach
  );END if
  (vla-endUndoMark doc)

  ; v0.0 - 2017.07.27 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.07.27
)
(defun DT:CutFillHouseStrings ( housePerimeter / ffl housePerimeter )
  ; Build 3D strings for house
  ; housePerimeter [ename] - Entity name of polyline representing house perimeter

  (if (DT:Arg 'DT:CutFillHouseStrings '((housePerimeter 'ename)))
    ; Get FFL of the building
    (if (setq ffl (DT:GetFFlFromPlot housePerimeter))
      (progn
        ; Set the perimeter at real elevation
        (vlax-put-property (vlax-ename->vla-object housePerimeter) 'Elevation ffl)
        ; Add vertices to the perimeter of the building every 0.5m
        (DT:AddVertexInterval housePerimeter 0.5)
        ; Convert perimetre to 3D polyline
        (lwpTo3d (vlax-ename->vla-object housePerimeter))
        (vla-delete (vlax-ename->vla-object housePerimeter))
        (setq housePerimeter (entlast))
        ; 3D offset house perimeter 10mm away and 150mm down
        (off3DP housePerimeter 0.010 -0.150 '(-10000 -10000 0))
      );END progn
      (DT:Error 'DT:CutFillHouseStrings "no FFL text found")
    );END if
  );END if

  ; v0.1 - 2017.08.14 - Error message printed if no FFl found
  ; v0.0 - 2017.07.27 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.14
)
(defun DT:CutFillGarageStrings ( garagePerimeter / ffl garagePerimeter )
  ; Build 3D strings for garages
  ; garagePerimeter [ename] - Entity name of polyline representing garage perimeter

  (if (DT:Arg 'DT:CutFillGarageStrings '((garagePerimeter 'ename)))
    ; Get FFL of the building
    (if (setq ffl (DT:GetFFlFromPlot garagePerimeter))
      (progn
        ; Set the perimeter at real elevation
        (vlax-put-property (vlax-ename->vla-object garagePerimeter) 'Elevation ffl)
        ; Add vertices to the perimeter of the building every 0.5m
        (DT:AddVertexInterval garagePerimeter 0.5)
        ; Convert perimetre to 3D polyline
        (lwpTo3d (vlax-ename->vla-object garagePerimeter))
        (vla-delete (vlax-ename->vla-object garagePerimeter))
        (setq garagePerimeter (entlast))
      );END progn
      (DT:Error 'DT:CutFillHouseStrings "no FFL text found")
    );END if
  );END if

  ; v0.1 - 2017.08.14 - Error message printed if no FFl found
  ; v0.0 - 2017.07.27 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.14
)
(defun DT:GetFFlFromPlot ( buildingPerimeter / coordinates ss content numberContent return )
  ; Return FFL value as real
  ; This function looks for a FFL text label inside the house perimeter provided.
  ; buildingPerimeter [ename] - Entity name of polyline representing house perimeter

  (if (DT:Arg 'DT:GetFFlFromPlot '((buildingPerimeter 'ename)))
    (progn
      ; Get perimeter coordinates
      (if (setq coordinates (DT:GetPolylineVertexCoordinates buildingPerimeter))
        ; Select texts within this coordinates
        (if (setq ss (ssget "wp" coordinates))
          (foreach a (ssnamex ss)
            (if (= 'ename (type (cadr a)))
              (if (setq content (DT:GetText (cadr a)))
                (if (setq numberContent (cadr (DT:ParseLevelString content)))
                  (setq return numberContent)
                );END if
              );END if
            );END if
          );END foreach
        );END if
      );END if
      return
    );END progn
  );END if

  ; v0.0 - 2017.07.27 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.07.27
)
(defun c:CutFillReformatFflText ( / doc ss ename fflText )
  ; Command version of DT:CutFillReformatFflText

  (if (setq ename (car (entsel "\nSelect FFL to be reformated: ")))
    (DT:CutFillReformatFflText ename)
  );END if

  ; v0.0 - 2017.08.14 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.14
)
(defun DT:CutFillReformatFflText ( ename / fflText newTextContent )
  ; Set FFl level to 3 decimals
  (if (DT:Arg 'DT:CutFillReformatFflText '((ename 'ename)))
    (if (setq fflText (DT:GetText ename))
      (progn
        (setq newTextContent (strcat fflText "0"))
        (DT:SetText ename newTextContent)
      );END progn
      (DT:Error 'DT:CutFillReformatFflText "not text found in ename entity")
    );END if
  );END if

  ; v0.0 - 2017.08.14 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.14
)
(defun c:CutFillRetainingWall ()
  ; Command version of DT:CutFillRetainingWall
  (setq ename (car (entsel "\nSelect 2D polyline representing retaining wall: ")))
  (setq headLevel (getreal "\nType retaining wall head level: "))
  (setq footLevel (getreal "\nType retaining wall foot level: "))
  (setq topPoint (getpoint "\nSelect point on retaining wall top side: "))
  (DT:CutFillRetainingWall ename headLevel footLevel topPoint)

  ; v0.0 - 2017.08.14 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.14
)
(defun DT:CutFillRetainingWall ( ename headLevel footLevel topPoint )
  ; Build a retaining wall given a plan polyline and wall head and bottom levels.
  ; ename [ename] - Entity name of LWPolyline representing retaning wall
  ; headLevel [real] - Retaining wall head level
  ; footLevel [real] - Retaining wall foot level
  ; topPoint [point] - Point on the top side of the retaining wall
  (if (DT:Arg 'DT:CutFillRetainingWall '((ename 'ename)(headLevel 'real)(footLevel 'real)(topPoint 'list)))
    T
  );END if

  ; v0.0 - 2017.08.14 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.08.14
)
(defun c:CutFillSpotLevel ( / pt z )
  ; Dot a point by XY and type/click Z
  (if (setq pt (getpoint "\nSelect point: "))
    (if (setq z (DT:clic_or_type_level))
      (entmakex
        (list
          (cons 0 "POINT")
          (cons 10 (list (nth 0 pt) (nth 1 pt) z))
        );END list
      );END entmakex
    );END if
  );END if

  ; v0.0 - 2017.10.19 - First issue
  ; Author: David Torralba
  ; Last revision: 2017.10.19
)
