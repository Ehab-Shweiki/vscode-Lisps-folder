(defun c:RenameBlockAndNested (/ ss idx ent newPrefix newName blkDef blkName
                                 colorChoice colorIndex styleChoice applyStyle)

  ;; --- Helper: Generate a new block name ---
  (defun GenerateNewName (oldName prefix)
    (if (/= prefix "")
      (strcat prefix "_" oldName)
      oldName ; return original if no prefix
    )
  )

  ;; --- Helper: Rename a block definition ---
  (defun RenameBlock (blkDef prefix / oldName newName)
    (setq oldName (vla-get-Name blkDef))
    (setq newName (GenerateNewName oldName prefix))
    (if (/= oldName newName)
      (vla-put-Name blkDef newName)
    )
    newName
  )

  ;; --- Helper: Set display properties for all entities in block ---
  (defun SetBlockDisplayProperties (blkDef colorIndex applyStyle / ent app cmTrans)
    (vlax-for ent blkDef
      ;; Set color (or ByBlock if not specified)
      (if (vlax-property-available-p ent 'Color)
        (vla-put-Color ent (if colorIndex colorIndex 0))
      )

      ;; Set other styles only if chosen
      (if applyStyle
        (progn
          (if (vlax-property-available-p ent 'Linetype)
            (vla-put-Linetype ent "BYBLOCK")
          )
          (if (vlax-property-available-p ent 'LinetypeScale)
            (vla-put-LinetypeScale ent 1.0)
          )
          (if (vlax-property-available-p ent 'Lineweight)
            (vla-put-Lineweight ent acLnWtByBlock)
          )
          (if (vlax-property-available-p ent 'Transparency)
            (progn
              (setq app (vla-get-Application ent))
              (setq cmTrans (vla-GetInterfaceObject app "AutoCAD.AcCmTransparency.1"))
              (vla-put-Transparency ent cmTrans)
            )
          )
        )
      )
    )
  )

  ;; --- Helper: Process nested blocks recursively ---
  (defun ProcessBlock (blkDef prefix colorIndex applyStyle / ent nestedBlks)
    (setq nestedBlks '())
    (vlax-for ent blkDef
      (if (= (vla-get-ObjectName ent) "AcDbBlockReference")
        (setq nestedBlks
               (cons (vla-Item (vla-get-Blocks (vla-get-Document ent)) (vla-get-Name ent))
                     nestedBlks))
      )
    )
    ;; First process nested blocks
    (foreach nestedBlk nestedBlks
      (ProcessBlock nestedBlk prefix colorIndex applyStyle)
    )
    ;; Then process this block
    (RenameBlock blkDef prefix)
    (SetBlockDisplayProperties blkDef colorIndex applyStyle)
  )

  ;; --- Main Program ---
  (prompt "\nSelect blocks to rename and apply properties:")
  (setq ss (ssget '((0 . "INSERT")))) ; Only block references

  (if (not ss)
    (progn (prompt "\nNo blocks selected.") (exit))
  )

  ;; Get name prefix
  (initget 128)
  (setq newPrefix (getstring "\nEnter prefix to add to block names (leave blank for no rename): "))

  ;; Color override options
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

  ;; Ask about overriding other styles
  (initget "Yes No")
  (setq styleChoice (getkword "\nAlso override styles (linetype, lineweight, etc.)? [Yes/No] <No>: "))
  (setq applyStyle (= styleChoice "Yes"))

  ;; Loop over all selected blocks
  (setq idx 0)
  (while (< idx (sslength ss))
    (setq ent (vlax-ename->vla-object (ssname ss idx)))
    (setq blkName (vla-get-Name ent))
    (setq blkDef (vla-Item (vla-get-Blocks (vla-get-Document ent)) blkName))

    (ProcessBlock blkDef newPrefix colorIndex applyStyle)

    (setq idx (1+ idx))
  )

  (princ "\nBlocks and nested blocks renamed and updated successfully.")
  (princ)
)

;; Command registration
(princ "\nType 'RenameBlockAndNested' to run the command.")
(princ)