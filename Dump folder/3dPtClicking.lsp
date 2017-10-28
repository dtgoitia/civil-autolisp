(defun c:3DPT ( / *error* VL_ent_name arr narr larr nz z oldosmode)
	; EDIT 3D polyline vertex levels, INPUT: typing or clicking

  ; PENDIENTE! - Refrescar tras cada edición la entidad y mostrar el nuevo listado
  ; de puntos. Si no, al reseleccionar un vértice no te muestra la cota cmabiada
  ; hasta que terminas el comando y reseleccionas.
  (vl-load-com)
  (defun *error* ( msg )
;    (if (not (member msg '("Function cancelled" "quit / exit abort")))
;      (princ (strcat "\nError: " msg))
;    )
    ; RESET system variables
    (setvar "osmode" oldosmode)
    (princ)
  )
  ; SAVE system variables
  (setq oldosmode (getvar "osmode"))

  ; CHANGE system variables
  (setvar "osmode" 1)
  ; El Param daba cero porque se vuelve loco con el OSMODE = 0, pon OSMODE = 1 y funciona perfecto. Pon la cabecera de copiar y salvar el OSMODE y blablabla

  ; INPUT - Select polyline
  (if (setq ent (entsel "\nSelect 3D polyline: "))
    (progn
      (setq VL_ent_name (vlax-ename->vla-object (car ent)))
      (if (= "AcDb3dPolyline" (vla-get-ObjectName VL_ent_name))
        (progn
          (while (not exit_variable)
            (setq
              ; OPERATION - Save coordinates array and convert it to a list
              arr (vlax-variant-value (vla-get-Coordinates VL_ent_name))
              larr (vlax-safearray->list arr)
            )
            ; INPUT - Select point to return parameter
            (if (setq p (getpoint "\nSelect vertex to edit <Esc to exit>: "))
              (progn
                (setq
                  ; OPERATION - Get data of point
                  Param (atoi (LM:rtos (vlax-curve-getParamAtPoint VL_ent_name (vlax-curve-getClosestPointTo VL_ent_name p)) 2 0))
                  z (nth (+ 2 (* 3 Param)) larr)
                )
                (princ (strcat "\nZ value <" (LM:rtos z 2 3) ">: "))
                (setq
                  ; INPUT - Ask for new Z value por the selected point
                  nz (DT:clic_or_type_level)
                )
                (cond
                  ((not nz)
                    (princ "...nothing selected. Z coordinate not changed.")
                  );END cond not nz
                  ((= nz z)
                    (princ (strcat (LM:rtos nz 2 3) "m. Same level selected. Z coordinate not changed."))
                  );END cond nz = z (new level = old level)
                  (t
                    (vlax-safearray-put-element arr (+ 2 (* 3 Param)) nz)
                    (vlax-put-property VL_ent_name 'Coordinates arr)
                    (princ (strcat "\nVertex level updated to " (LM:rtos nz 2 3) "m."))
                  );END cond new nz
                );END cond
              );END progn p point selected
              (progn
                (princ "\nNo vertex selected.")
                (*error*)
                (exit)
              )
            );END if
          );END while
        ); END progn
        (alert "Sorry, that is not a 3D polyline.")
      ); END if
    );END progn
    (princ "\nNothing selected.")
  );END if
  (princ)
)
