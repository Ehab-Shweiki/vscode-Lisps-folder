
(defun GetSelectionSet ( / ss)
  ;; Try to get pre-selected objects (PICKFIRST)
  (setq ss (ssget "_I"))

  ;; If no pre-selection, prompt user to select
  (if (not ss)
    (prompt "\nSelect objects:")
    (setq ss (ssget)) ; standard prompt selection
  )

  ss ; return the selection set (or nil)
)

(defun PromptSelectionSet ( / ss)
  ;; Prompt user to select objects
  (prompt "\nSelect objects:")
  (setq ss (ssget)) ; interactive selection only
  ss ; return the selection set (or nil)
)

; -------------
; simple filter:

(defun GetFilteredPreOrPromptSS (filterList / ss)
  ;; filterList: DXF filter list, e.g. '((0 . "LINE")) or '((0 . "TEXT,MTEXT"))
  ;; Returns: filtered selection set (ss) from PICKFIRST or prompt

  ;; First try to get matching objects from PICKFIRST
  (setq ss (ssget "_I" filterList))

  ;; If no match, prompt user with same filter
  (if (not ss)
    (progn
      (prompt "\nNo matching pre-selected objects. Please select:")
      (setq ss (ssget filterList))
    )
  )

  ss ; return the final filtered selection set (may be nil)
)

; ----------------------
; generel function filter:

(defun GetFilteredSSWithFallback (filterFnSym / ss result)
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

