(defun c:FilterByUsedColors ( / ss i ent color allColors distinctColors selectedColor matchSS)

  ;; Step 1: Use pre-selection or ask to select
  (if (not (setq ss (ssget "_I")))
    (setq ss (ssget "\nSelect objects to filter by color: "))
  )

  (if (not ss)
    (progn (prompt "\nNo objects selected.") (exit))
  )

  ;; Step 2: Collect color numbers
  (setq allColors '())
  (setq i 0)
  (while (< i (sslength ss))
    (setq ent (entget (ssname ss i)))
    (setq color (cdr (assoc 62 ent)))
    (if (and color (not (member color allColors)))
      (setq allColors (cons color allColors)))
    (setq i (1+ i))
  )

  ;; Step 3: Show sorted list
  (setq distinctColors (vl-sort allColors '<))
  (prompt "\nColors found in selection:")
  (foreach c distinctColors
    (prompt (strcat "\n  → " (itoa c)))
  )

  ;; Step 4: Ask user to select from the list
  (initget 128)
  (setq selectedColor (getint "\nEnter one of the above color numbers to filter by: "))

  (if (not selectedColor)
    (prompt "\nNo color selected. Cancelled.")
    (progn
      ;; Step 5: Build new selection of matching color
      (setq matchSS (ssadd))
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (entget (ssname ss i)))
        (setq color (cdr (assoc 62 ent)))
        (if (= color selectedColor)
          (ssadd (ssname ss i) matchSS))
        (setq i (1+ i))
      )

      ;; Highlight matching
      (sssetfirst nil matchSS)
      (prompt (strcat "\nFiltered objects with color " (itoa selectedColor)))
      (command "_.REGEN")
    )
  )
  (princ)
)

(princ "\nType FilterByUsedColors to run color filter on preselected objects.")
(princ)