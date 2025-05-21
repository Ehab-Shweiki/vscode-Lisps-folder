(setq folderpath "D:\\Mine\\vscode Lisps folder\\developing\\dummy.lsp")
;--------------------------------;

(defun c:RL ( / file ) ; RL: Run Lisp 
  (setq file (getfiled "Select LISP file to load" folderpath "lsp" 4))
  (if (and file (findfile file))
    (progn
      (load file)
      (setenv "LAST_LISP_FILE" file)
      (princ (strcat "\nLoaded and saved for next time: " file))
    )
    (princ "\nNo file selected or file not found.")
  )
  (princ)
)


(defun c:RLL ( / file )  ; RLL: Run Last Lisp
  (setq file (getenv "LAST_LISP_FILE"))
  (if (and file (findfile file))
    (progn
      (princ (strcat "\nSuccessfully Reloaded: " file))
      (load file)
    )
    (princ "\nNo last LISP file found or file missing.")
  )
  (princ)
)
