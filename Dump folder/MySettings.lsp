;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                           SYSTEM VARIABLES
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setvar "PRESELECTIONEFFECT" 0)
; Specifies the visual effect used for preselection of objects.
;  - 0: Dashed and thickened lines (legacy behavior)
;  - 1: Glowing line highlighting effect
(setvar "NAVVCUBEDISPLAY" 0)
; Controls the display of the ViewCube tool in the current visual style and the
; current viewport.
;  - 0: ViewCuube is not displayed in 2D and 3D visual styles.
;  - 1: ViewCube is displayed in 3D, but not in 2D visual styles.
;  - 2: ViewCube is displayed in 2D, but not in 3D visual styles.
;  - 3: ViewCube is displayed in both 2D and 3D visual styles.
(setvar "NAVBARDISPLAY" 0)
; Controls the display of the ViewCube tool in the current visual style and the
; current viewport.
;  - 0: ViewCuube is not displayed in 2D and 3D visual styles.
;  - 1: ViewCube is displayed in 3D, but not in 2D visual styles.
;  - 2: ViewCube is displayed in 2D, but not in 3D visual styles.
;  - 3: ViewCube is displayed in both 2D and 3D visual styles.
(setvar "NAVBARDISPLAY" 0)
; Controls the display of the navigation bar in the current viewport.
;  - 0: Naviagation bar is not displayed
;  - 1: Naviagation bar is displayed
(setvar "VTENABLE" 0)
; Controls when smooth view transitions are used.
; Detailed info at https://knowledge.autodesk.com/support/autocad-lt/learn-explore/caas/CloudHelp/cloudhelp/2018/ENU/AutoCAD-LT/files/GUID-D0AF2638-DE82-4CB2-A7D8-C771EB5CDD6D-htm.html
(setvar "SELECTIONPREVIEW" 0)
; Controls the display of selection previewing.
;  - 0: Off
;  - 1: On when no commands are active
;  - 2: On when a command prompts for object selection
(setvar "EDGEMODE" 1)
; Controls how the TRIM and EXTEND commands determine cutting and boundary edges.
;  - 0: Uses the selected edge without any extensions.
;  - 1: Extends or trims the selected object to an imaginary extension of the cutting or boundary edge.
(setvar "MIRRTEXT" 0)
; Controls how MIRROR reflects text.
;  - 0: Retains text direction.
;  - 1: Mirrors the text .
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                        COMAND SPECIFIC SETTINGS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(command "_LAYISO" "S" "O" "V" "")
; Specifies the LAYISO command behaviour in model space and viewports

; Options:
;   Display > Colours:
;     Model space background: black
;     Paper space background: black

; TODO:
; - Remove the .bak file.
; - Remove the welcome screen with the "Recent documents", etc.
