(defun c:LoadFontFolder ( / folder dlgresult oldpaths newpaths )

  ;; Open folder browser dialog
  (vl-load-com)
  (setq dlgresult
    (vl-file-dialog
      "Select Font Folder"
      nil
      nil
      128 ; folders only
    )
  )

  (if dlgresult
    (progn
      (setq folder (car dlgresult))
      ;; Get current ACAD support path
      (setq oldpaths (getenv "ACAD"))
      ;; Avoid duplicate entry
      (if (not (vl-string-search folder oldpaths))
        (progn
          ;; Add folder to the beginning of search path (temporary)
          (setq newpaths (strcat folder ";" oldpaths))
          (setenv "ACAD" newpaths)
          (princ (strcat "\nFont folder added: " folder))
        )
        (princ "\nFolder is already in the search path.")
      )
    )
    (princ "\nNo folder selected.")
  )

  (princ)
)