(defun c:DSL (/ *error* blkName blkScale topOff botOff vertLayer dimStyle offsetFromStart
                              userInput slope dx dy pt1 pt2 midPt ent
                              yBase vertPtTop vertPtBot dimPt1 dimPt2 dimY)

  ;; --- Helpers to store settings persistently ---
  (defun load-setting (key default)
    (if (getenv key)
      (read (getenv key))
      default
    )
  )
  (defun save-setting (key value)
    (setenv key (vl-princ-to-string value))
  )

  ;; --- Load saved settings or defaults ---
  (setq blkName        (load-setting "DSLT_BlockName" "MyDefaultBlock"))
  (setq blkScale       (load-setting "DSLT_BlockScale" 1.0))
  (setq vertLayer      (load-setting "DSLT_VertLayer" "VertLine"))
  (setq dimStyle       (load-setting "DSLT_DimStyle" "ITALIC"))
  (setq dimStyle       (load-setting "DSLT_DimStyle" "ITALIC"))
  (setq topOff         (load-setting "DSLT_TopOffset" 150))
  (setq botOff         (load-setting "DSLT_BotOffset" 450))
  (setq offsetFromStart (load-setting "DSLT_OffsetBase" T)) ; T = from start

  ;; --- Create layer if missing ---
  (defun ensure-layer (name color ltype)
    (if (not (tblsearch "layer" name))
      (command "._LAYER" "M" name "C" color "" "L" ltype "" "")
    )
  )

  ;; Add Italic Text Style Definition
  (defun ensure-textstyle (name font oblique)
		(if (not (tblsearch "style" name))
			(progn
				(command "._STYLE" name font 0 1 0 oblique "N" "N")
				(prompt (strcat "\nCreated text style: " name))
			)
		)
	)
  
  ;; --- Error handler ---
  (defun *error* (msg)
    (princ (strcat "\nError: " msg))
    (princ)
  )

  ;; --- Options Menu ---
  (while
    (progn
      ;; Set keyword options
      (initget "VertLine Block Offsets DimStyle ToggleOffsetBase")
      (setq userInput 
		(getkword
		  "\nPress Enter to draw, or choose [VertLine/Block/Offsets/DimStyle/OffsetBase/ToggleOffsetBase]: "
	      )
        )
      
      ;; Handle each keyword
      (cond
        ; Copy vertical line layer
        ((= userInput "VertLine")
         (prompt "\nPick a vertical line to copy its layer...")
         (setq ent (car (entsel)))
         (if ent
           (progn
             (setq vertLayer (cdr (assoc 8 (entget ent))))
             (save-setting "DSLT_VertLayer" vertLayer)
             (prompt (strcat "\n✅ Saved vertical line layer: " vertLayer))
           )
         )
         T
        )

        ; Set default block
        ((= userInput "Block")
         (prompt "\nPick a block to set as default...")
         (setq ent (car (entsel)))
         (if (and ent (= "INSERT" (cdr (assoc 0 (entget ent)))))
           (progn
             (setq blkName (cdr (assoc 2 (entget ent))))
             (save-setting "DSLT_BlockName" blkName)
             (prompt (strcat "\n✅ Saved block: " blkName))
           )
         )
         T
        )

        ; Set offsets
        ((= userInput "Offsets")
         (setq topOff (getreal (strcat "\nEnter TOP offset <" (rtos topOff 2 2) ">: ")))
         (setq botOff (getreal (strcat "\nEnter BOTTOM offset <" (rtos botOff 2 2) ">: ")))
         (save-setting "DSLT_TopOffset" topOff)
         (save-setting "DSLT_BotOffset" botOff)
         (prompt "\n✅ Offsets saved.")
         T
        )

        ; Set dimension style
        ((= userInput "DimStyle")
         (prompt "\nPick a dimension to set its style...")
         (setq ent (car (entsel)))
         (if (and ent (= "DIMENSION" (cdr (assoc 0 (entget ent)))))
           (progn
             (setq dimStyle (cdr (assoc 3 (entget ent))))
             (save-setting "DSLT_DimStyle" dimStyle)
             (prompt (strcat "\n✅ Saved DimStyle: " dimStyle))
           )
         )
         T
        )
        

        ; ??
        ((= userInput "OffsetBase")
					(setq offsetFromStart (not offsetFromStart))
					(save-setting "DSLT_OffsetBase" offsetFromStart)
					(prompt (strcat "\nOffset base set to: " (if offsetFromStart "Start Point" "Sloped Line End")))
					T
					)
        
        ; Toggle offset base
        ((= userInput "ToggleOffsetBase")
         (setq offsetFromStart (not offsetFromStart))
         (save-setting "DSLT_OffsetBase" offsetFromStart)
         (prompt (strcat "\n✅ Offset base set to: " (if offsetFromStart "Start Point" "Sloped Line End")))
         T
        )

        (T nil) ; exit loop if Enter
      )
    )
  )

  ;; --- Drawing phase ---
  (setq pt1 (getpoint "\nSelect start point: "))
  (setq dx (getreal "\nEnter horizontal distance (dx): "))
  (setq slope (getreal "\nEnter slope (dy/dx): "))
  (setq dy (* dx slope))
  (setq pt2 (list (+ (car pt1) dx) (+ (cadr pt1) dy) 0))
  (setq midPt (list (/ (+ (car pt1) (car pt2)) 2) (/ (+ (cadr pt1) (cadr pt2)) 2) 0))

  ;; Ensure layers
  (ensure-layer "SlopeLine" "1" "Continuous")
  (ensure-layer vertLayer "3" "Hidden")
  (ensure-layer "TextAnno" "2" "Continuous")

  ;; Draw sloped line
  (setvar "CLAYER" "SlopeLine")
  (command "._PLINE" pt1 pt2 "")

  ;; Set vertical line Y base
  (setq yBase (if offsetFromStart (cadr pt1) (cadr pt2)))
  (setq vertPtTop (list (car pt2) (+ yBase topOff) 0))
  (setq vertPtBot (list (car pt2) (- yBase botOff) 0))

  ;; Draw vertical line
  (setvar "CLAYER" vertLayer)
  (command "._LINE" vertPtBot vertPtTop "")

  ;; Insert block
  (if (tblsearch "block" blkName)
    (command "._INSERT" blkName pt2 blkScale blkScale 0)
    (prompt (strcat "\nBlock \"" blkName "\" not found. Skipping insert."))
  )

  ;; Text at midpoint
  (setvar "CLAYER" "TextAnno")
  (command "._TEXT" midPt 2.5 0 (strcat "Slope = " (rtos slope 2 4)))

  ;; Horizontal dimension line
  (setq dimY (+ yBase topOff))
  (setq dimPt1 (list (car pt1) dimY 0))
  (setq dimPt2 (list (car pt2) dimY 0))
  (command "._DIMSTYLE" "R" dimStyle)
  (command "._DIMLINEAR" "_non" dimPt1 "_non" dimPt2 "_non" (list (/ (+ (car pt1) (car pt2)) 2) (+ dimY 0.5) 0))

  ;;; Finishing   
  ;; === Restore OSNAP ===
  (setvar "OSMODE" oldOsmode)

  ;; === Force REGEN to update fields and attributes ===
  (command "_.REGEN")
  
  (princ "\n✅ Done. Settings saved. Run DSL again anytime.")
  (princ)
)

;; Prompt for the command in the AutoCAD command line
(princ "\nType 'DSL' to run the command.")
(princ)