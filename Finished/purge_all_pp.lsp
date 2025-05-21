(defun c:pp () ; Shortcut command: PP
  (command "._-purge" "all" "*" "n")
  (princ "\nAll purgeable items removed.")
  (princ)
)

;; Prompt for the command in the AutoCAD command line
(princ "\nType 'pp' to run the command 'purge_all'.")
(princ)