(defun c:SETPROP (/ ent obj trueColor colorString transObj transStr width)
  (prompt "\nSelect an object to get its properties: ")
  (if (setq ent (car (entsel)))
    (progn
      (setq obj (vlax-ename->vla-object ent))

      ;; Layer
      (setvar "CLAYER" (vlax-get-property obj 'Layer))

      ;; Linetype
      (setvar "CELTYPE" (vlax-get-property obj 'Linetype))

      ;; Linetype scale
      (setvar "CELTSCALE" (vlax-get-property obj 'LinetypeScale))

      ;; Lineweight
      (setvar "CELWEIGHT" (vlax-get-property obj 'Lineweight))

      ;; Color via TrueColor
      (setq trueColor (vlax-get-property obj 'TrueColor))
      (setq colorString (vlax-get-property trueColor 'ColorName))
      (if colorString
        (setvar "CECOLOR" colorString)
      )

      ;; Transparency
      (if (vlax-property-available-p obj 'Transparency)
        (progn
          (setq transObj (vlax-get-property obj 'Transparency))
          (setq transStr (vlax-get-property transObj 'TransparencyValue)) ; Integer from 0-90
          (setvar "CTRANSPARENCY" (itoa transStr)) ; Set as string
        )
      )

      ;; Polyline Width
      (if (and (vlax-property-available-p obj 'ConstantWidth)
               (/= (vlax-get-property obj 'ConstantWidth) 0.0))
        (setvar "PLINEWID" (vlax-get-property obj 'ConstantWidth))
      )

      (princ "\nDrawing default properties updated.")
    )
    (prompt "\nNo object selected.")
  )
  (princ)
)
