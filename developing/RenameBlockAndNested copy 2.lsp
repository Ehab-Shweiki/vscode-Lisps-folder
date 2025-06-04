(defun c:RenameBlockAndNested (/ ss idx ent newPrefix newName blkDef blkName)
  ;; --- Helper to generate unique name ---
  (defun GenerateNewName (oldName prefix)
    (if (/= prefix "")
      (strcat prefix "_" oldName)
      oldName ; return the original name unchanged
    )
  )

  ;; --- Helper to rename a block definition ---
  (defun RenameBlock (blkDef prefix / newName)
    (setq newName (GenerateNewName (vla-get-Name blkDef) prefix))
    (vla-put-Name blkDef newName)
    newName
  )

  ;; --- Helper to override color for all entities inside a block ---
  (defun SetBlockColor (blkDef colorIndex / ent)
    (vlax-for ent blkDef
      (if (vlax-property-available-p ent 'Color)
        (vla-put-Color ent colorIndex)
      )
    )
  )

  ;; --- Helper to process nested blocks recursively ---
  (defun ProcessBlock (blkDef prefix colorIndex / ent nestedBlks)
    (setq nestedBlks '())
    (vlax-for ent blkDef
      (cond
        ((= (vla-get-ObjectName ent) "AcDbBlockReference")
          (setq nestedBlks (cons (vla-Item (vla-get-Blocks (vla-get-Document ent)) (vla-get-Name ent)) nestedBlks))
        )
      )
    )
    ;; Process nested blocks first
    (foreach nestedBlk nestedBlks
      (ProcessBlock nestedBlk prefix colorIndex)
    )
    ;; Rename the block
    (RenameBlock blkDef prefix)
    ;; Change color if needed
    (if colorIndex
      (SetBlockColor blkDef colorIndex)
    )
  )

  ;; --- Main Program ---

  (prompt "\nSelect blocks to rename and recolor:")
  (setq ss (ssget '((0 . "INSERT")))) ; Only select block references
  (if (not ss)
    (progn (prompt "\nNo blocks selected.") (exit))
  )

  (initget 1)
  (setq newPrefix (getstring "\nEnter prefix to add to block names: "))

  (if (not newPrefix)
    (progn (prompt "\nPrefix is required.") (exit))
  )

  (initget "NoColor ByBlock Yellow Magenta Custom")
  (setq colorChoice (getkword "\nOverride color? [NoColor/ByBlock/Yellow/Magenta/Custom] <NoColor>: "))

  (cond
    ((or (null colorChoice) (= colorChoice "NoColor")) (setq colorIndex nil))
    ((= colorChoice "ByBlock") (setq colorIndex 0))
    ((= colorChoice "Yellow") (setq colorIndex 2))
    ((= colorChoice "Magenta") (setq colorIndex 6))
    ((= colorChoice "Custom")
      (setq colorIndex (getint "\nEnter AutoCAD color index (1-255): "))
    )
  )

  (setq idx 0)
  (while (< idx (sslength ss))
    (setq ent (vlax-ename->vla-object (ssname ss idx)))
    (setq blkName (vla-get-Name ent))
    (setq blkDef (vla-Item (vla-get-Blocks (vla-get-Document ent)) blkName))

    (ProcessBlock blkDef newPrefix colorIndex)
    (setq idx (1+ idx))
  )

  (princ "\nBlocks and nested blocks renamed and updated successfully.")
  (princ)
)

;; Prompt for the command in the AutoCAD command line
(princ "\nType 'RenameBlockAndNested' to run the command.")
(princ)