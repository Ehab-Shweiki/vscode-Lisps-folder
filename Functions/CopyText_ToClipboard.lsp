(defun copy-to-clipboard (txt / f file)
  (setq file (vl-filename-mktemp "clip.txt"))
  (setq f (open file "w"))
  (write-line txt f)
  (close f)
  (startapp "cmd.exe" (strcat "/c type \"" file "\" | clip"))
)

(defun c:CT ( / ent obj txt)
  (vl-load-com)
  (setq ent (car (entsel "\nSelect a TEXT or MTEXT object: ")))
  (if ent
    (progn
      (setq obj (vlax-ename->vla-object ent))
      (cond
        ((= (vla-get-objectname obj) "AcDbText")
         (setq txt (vla-get-textstring obj)))
        ((= (vla-get-objectname obj) "AcDbMText")
         (setq txt (vla-get-textstring obj)))
        (T (setq txt nil))
      )
      (if txt
        (progn
          (copy-to-clipboard txt)
          (princ (strcat "\nCopied to clipboard: \"" txt "\""))
        )
        (princ "\nSelected object is not TEXT or MTEXT.")
      )
    )
    (princ "\nNothing selected.")
  )
  (princ)
)

(princ "\nType CT to copy text content to clipboard.")
(princ)
