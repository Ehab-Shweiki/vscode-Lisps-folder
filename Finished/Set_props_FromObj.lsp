(defun c:SETPROP-EXT (/ ent obj tc colorStr transObj transStr width txtStyle dimStyle mldrStyle hatchName hatchScale)
  (prompt "\nSelect an object to match its properties: ")
;   
  (progn (setq obj (vlax-ename->vla-object (car (entsel))))
  (setq obj2 (vlax-ename->vla-object (car (entsel))))
  (setq obj3 (vlax-ename->vla-object (car (entsel)))))
;   
  (if (setq ent (car (entsel)))
    (progn
      (setq obj (vlax-ename->vla-object ent))

      ;; Layer
      (setvar "CLAYER" (vlax-get-property obj 'Layer))

      ;; Linetype
      (setvar "CELTYPE" (vlax-get-property obj 'Linetype))

      ;; Linetype Scale
      (setvar "CELTSCALE" (vlax-get-property obj 'LinetypeScale))

      ;; Lineweight
      (setvar "CELWEIGHT" (vlax-get-property obj 'Lineweight))

      ;; Color via TrueColor
      ;; Get ACI color number
		(setq aci (vla-get-Color obj2))
      ;; Convert ACI to string expected by CECOLOR
		(defun aci-to-colorname (c)
			(cond
				((= c 256) "BYLAYER")
				((= c 0)   "BYBLOCK")
				((and (>= c 1) (<= c 255)) (itoa c)) ; Convert number to string
				(T "BYLAYER") ; fallback
			)
		)
		;; Set default color
		(setvar "CECOLOR" (aci-to-colorname aci))
    ;   (setq tc (vlax-get-property obj 'Color))
    ;   (setq colorStr (vlax-get-property tc 'ColorName))
    ;   (if colorStr (setvar "CECOLOR" colorStr))

      ;; Transparency
      (if (vlax-property-available-p obj 'Transparency)
        (progn
          (setq transObj (vlax-get-property obj 'Transparency))
          (setq transStr (itoa (vlax-get-property transObj 'TransparencyValue))) ; integer (0–90)
          (setvar "CTRANSPARENCY" transStr)
        )
      )

      ;; Polyline Width
      (if (and (vlax-property-available-p obj 'ConstantWidth)
               (/= (vlax-get-property obj 'ConstantWidth) 0.0))
        (setvar "PLINEWID" (vlax-get-property obj 'ConstantWidth))
      )

      ;; Text Style
      (if (or (eq "AcDbText" (vla-get-objectname obj))
              (eq "AcDbMText" (vla-get-objectname obj)))
        (setvar "TEXTSTYLE" (vlax-get-property obj 'StyleName))
      )

      ;; Dimension Style
      (if (eq "AcDbDimension" (vla-get-objectname obj))
        (setvar "DIMSTYLE" (vlax-get-property obj 'StyleName))
      )

      ;; Multileader Style
      (if (eq "AcDbMLeader" (vla-get-objectname obj))
        (setvar "MLEADERSTYLE" (vlax-get-property obj 'StyleName))
      )

      ;; Hatch Pattern & Scale (display only)
      (if (eq "AcDbHatch" (vla-get-objectname obj))
        (progn
          (setq hatchName (vlax-get-property obj 'PatternName))
          (setq hatchScale (vlax-get-property obj 'PatternScale))
          (princ (strcat "\nHatch pattern: " hatchName ", scale: " (rtos hatchScale 2 2)))
          (princ "\nNote: Hatch properties are not global. Use templates or copy-match.")
        )
      )

      (princ "\nCurrent drawing defaults updated from selected object.")
    )
    (prompt "\nNo object selected.")
  )
  (princ)
)
