(defun c:RenameBlocks ( / mode newBaseName prefix suffix ss i ent ename oldname newname renamedList blkdef entList nestedFound)

  ;; Choose rename mode
  (initget "PrefixSuffix FullName")
  (setq mode (getkword "\nChoose rename mode [PrefixSuffix/FullName] <PrefixSuffix>: "))
  (if (null mode) (setq mode "PrefixSuffix"))

  ;; Get naming input based on mode
  (cond
    ((= mode "PrefixSuffix")
     (setq prefix (getstring T "\nEnter prefix (or press Enter to skip): "))
     (setq suffix (getstring T "\nEnter suffix (or press Enter to skip): "))

     (if (> (strlen prefix) 0)
       (setq prefix (strcat prefix " "))
     )
     (if (> (strlen suffix) 0)
       (setq suffix (strcat " " suffix))
     )
    )

    ((= mode "FullName")
     (setq newBaseName (getstring T "\nEnter full new name: "))
     (if (= newBaseName "") (progn (prompt "\nInvalid name.") (exit)))
    )
  )

  ;; Select block references only
  (setq ss (ssget '((0 . "INSERT"))))
  (if ss
    (progn
      (setq i 0
            renamedList '())

      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq ename (entget ent))
        (setq oldname (cdr (assoc 2 ename)))

        ;; Determine new name based on mode
        (setq newname
          (cond
            ((= mode "PrefixSuffix") (strcat prefix oldname suffix))
            ((= mode "FullName") newBaseName)
          )
        )

        ;; Rename only if needed
        (if (and (tblsearch "BLOCK" oldname)
                 (not (equal oldname newname)))
          (progn
            (command "_.-rename" "_block" oldname newname)

            ;; Check for nested blocks in renamed block
            (setq blkdef (tblobjname "BLOCK" newname))
            (setq entList (if blkdef (entnext blkdef)))
            (setq nestedFound nil)

            (while (and entList (not nestedFound))
              (if (eq (cdr (assoc 0 (entget entList))) "INSERT")
                (setq nestedFound T)
              )
              (setq entList (entnext entList))
            )

            ;; Save rename result
            (setq renamedList
              (cons (list oldname newname nestedFound) renamedList))
          )
        )
        (setq i (1+ i))
      )

      ;; Summary
      (prompt "\n--- Rename Summary ---")
      (foreach entry (reverse renamedList)
        (progn
          (prompt (strcat "\n" (car entry) " => " (cadr entry)))
          (if (caddr entry)
            (prompt "    → Note: This block contains nested blocks.")
          )
        )
      )
    )
    (prompt "\nNo blocks selected.")
  )
  (princ)
)

(defun c:Ren () (c:RenameBlocks))

;; Prompt for the command in the AutoCAD command line
(princ "\nType 'Ren' to run the command 'RenameBlocksPS'.")
(princ)