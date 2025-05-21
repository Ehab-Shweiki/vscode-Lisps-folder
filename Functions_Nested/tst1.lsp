; -------------

(defun GetSSWithFallback (filterFnSym / ss result)
  ;; filterFnSym: symbol of a function that takes a selection set (ss)
  ;; Returns result from applying filter function to preselection or prompted selection

  ;; Try with pre-selection
  (setq ss (ssget "_I"))
  (if ss
    (setq result (eval (list filterFnSym ss)))
  )

  ;; If no valid result, prompt user
  (if (not result)
    (progn
      (prompt "\nNo valid pre-selected objects. Please select:")
      (setq ss (ssget))
      (if ss
        (setq result (eval (list filterFnSym ss)))
      )
    )
  )

  result ; return the final filtered result
)

; ---------------------
; example

(defun FilterLinesOnly (ss / i e ents)
  (setq i 0 ents '())
  (while (< i (sslength ss))
    (setq e (ssname ss i))
    (if (eq (cdr (assoc 0 (entget e))) "LINE")
      (setq ents (cons e ents))
    )
    (setq i (1+ i))
  )
  ents ; return filtered entity names
)

(defun c:SmartSelectLines ( / result)
  (setq result (GetSSWithFallback 'FilterLinesOnly))

  (if result
    (progn
      (prompt (strcat "\nFound " (itoa (length result)) " LINE(s)."))
      (foreach e result (redraw e 3)) ; highlight
    )
    (prompt "\nNo LINEs found.")
  )

  (princ)
)


