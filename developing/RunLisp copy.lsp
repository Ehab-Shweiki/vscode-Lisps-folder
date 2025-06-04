(setq folderpath ("C:\\Users\\Amira\\Desktop\\Ehab\\Mine\\vscode Lisps folder\\developing\\dummy.lsp"))

(defun is-callable (sym)
  (or
    (and (wcmatch (vl-symbol-name sym) "c:*") ; it's a command
         (command-sym-exists-p sym))          ; defined as a command
    (and (boundp sym)
         (= (type (eval sym)) 'EXR))          ; it's a regular function
  )
)

(defun command-sym-exists-p (sym)
  (member (strcase (vl-symbol-name sym)) (atoms-family 1))
)

(defun c:RunLispOnce ( / filepath allCommands i cmd cmdStr pickedIndex )

  (vl-load-com)

  ;; Ask user for file
  (setq filepath (getfiled "Select a LISP file to load" "C:\\MyLispScripts\\dummy.lsp" "lsp" 4))
  (if (and filepath (findfile filepath))
    (progn
      (load filepath)
      (setenv "LAST_LISP_FILE" filepath)

      ;; Get all defined command names (starting with "C:")
      (setq allCommands
        (vl-remove-if-not
          (function
            (lambda (name)
              (wcmatch name "C:*")
            )
          )
          (atoms-family 1)
        )
      )

      ;; Handle command selection
      (cond
        ((= (length allCommands) 0)
         (setenv "LAST_LISP_FN" "")
         (princ "\n❌ No commands found in the loaded LISP.")
        )
        ((= (length allCommands) 1)
         (setq cmdStr (car allCommands))
         (setenv "LAST_LISP_FN" cmdStr)
         (princ (strcat "\n✅ Found single command: " cmdStr))
        )
        (T
         (princ "\n📋 Multiple commands found:")
         (setq i 0)
         (foreach cmd allCommands
           (princ (strcat "\n[" (itoa i) "] " cmd))
           (setq i (1+ i))
         )
         (initget 128)
         (setq pickedIndex (getint "\nChoose command index to run later (or press Enter to skip): "))
         (if (and pickedIndex (nth pickedIndex allCommands))
           (progn
             (setq cmdStr (nth pickedIndex allCommands))
             (setenv "LAST_LISP_FN" cmdStr)
             (princ (strcat "\n✅ Saved command: " cmdStr))
           )
           (setenv "LAST_LISP_FN" "")
         )
        )
      )
    )
    (princ "\n❌ File not selected or not found.")
  )
  (princ)
)


(defun c:RunLastLisp ( / filepath cmdName cmdCall )

  (vl-load-com)

  (setq filepath (getenv "LAST_LISP_FILE"))
  (setq cmdName (getenv "LAST_LISP_FN"))

  (cond
    ((null filepath)
     (princ "\n❌ No file stored. Run RunLispOnce first."))

    ((not (findfile filepath))
     (princ (strcat "\n❌ Stored file not found: " filepath)))

    (T
     ;; Load the LISP
     (load filepath)
     (princ (strcat "\n✅ Loaded: " filepath))

     ;; Try to run the stored command
     (if (and cmdName (/= cmdName "") (member (strcase cmdName) (atoms-family 1)))
       (progn
         (setq cmdCall (substr cmdName 3)) ; remove "c:"
         (princ (strcat "\n▶ Running command: " cmdCall))
         (command (strcat "_" cmdCall)) ; prefix with underscore
       )
       (princ "\n❌ No valid command saved.")
     )
    )
  )

  (princ)
)