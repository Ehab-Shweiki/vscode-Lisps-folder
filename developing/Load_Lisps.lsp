(defun c:Load_Lisps ( / folder1 folder2 files path)

  ;; Define your fixed folder paths here
  (setq folder1 "D:/Mine/vscode Lisps folder/LISPS imported")
  (setq folder2 "D:/Mine/vscode Lisps folder/Finished")
  (setq folder3 "D:/Mine/vscode Lisps folder/Functions")

  ;; Helper to load lisps from folder
  (defun load-lisps-from-folder (folder)
    (if (and folder (vl-file-directory-p folder))
      (progn
        (setq files (vl-directory-files folder nil 1))
		
        ;; Ensure trailing slash
		(if (/= (substr folder (strlen folder)) "/")
		(setq folder (strcat folder "/"))
		)
        (foreach f files
          ;; Skip if it’s a shortcut (.lnk)
          (if (not (wcmatch (strcase f) "*.LNK"))
            (progn
              ;; Optional: filter only .lsp, .fas, .vlx
              (if (wcmatch (strcase f) "*.LSP;*.FAS;*.VLX")
                (progn
                  (setq path (strcat folder f))
                  (princ (strcat "\nLoading: " path))
                  (load path)
                )
              )
            )
          )
          
          (princ (strcat "\nLoading: " folder f))
          (setq path (strcat folder f))
          (load path)
        )
      )
      (princ (strcat "\nFolder not found: " folder))
    )
  )

  ;; Load from both folders
  (load-lisps-from-folder folder1)
  (load-lisps-from-folder folder2)
  (load-lisps-from-folder folder3)

  (princ "\nDone loading .lsp files from all folders.")
  (princ)
)

(princ "\nType Load_Lisps to load LISP files from the two folders.")
(princ)

(c:Load_Lisps)