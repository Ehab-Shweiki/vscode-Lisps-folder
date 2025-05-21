(defun c:HighlightTextTypes ( )
  (RunWithPreselectFallback
    "TEXT" ;; this string won't matter; overridden below
    (function (lambda (e) (redraw e 3)))
  )
)

(defun c:HighlightTexts ()
  ;; Uses updated prompt for multiple types
  (setq filtered (PromptSelectionByType '("TEXT" "MTEXT")))
  (if filtered
    (ActOnEntities filtered (function (lambda (e) (redraw e 3))))
    (prompt "\nNo matching text objects found.")
  )
  (princ)
)