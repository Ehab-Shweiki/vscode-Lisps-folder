(defun c:Del_Dims_InSelection ( / ss idx ent blkDef blkName)

  (vl-load-com)

  ;; --- Helper: Delete dimensions inside a block definition ---
  (defun DeleteDimsInBlock (blkDef / ent nestedBlks nestedBlk)
    (setq nestedBlks '())
    ;; First collect nested blocks
    (vlax-for ent blkDef
      (cond
        ;; If it's a block reference, store for recursion
        ((= (vla-get-ObjectName ent) "AcDbBlockReference")
         (setq nestedBlk (vla-Item (vla-get-Blocks (vla-get-Document ent)) (vla-get-Name ent)))
         (setq nestedBlks (cons nestedBlk nestedBlks))
        )
      )
    )
    ;; Recurse into nested blocks first
    (foreach blk nestedBlks
      (DeleteDimsInBlock blk)
    )
    ;; Now delete DIMENSION entities in current block
    (vlax-for ent blkDef
      (if (= (vla-get-ObjectName ent) "AcDbDimension")
        (vla-delete ent)
      )
    )
  )

  ;; --- Main Routine ---
  (prompt "\nSelect blocks to clean dimensions from:")
  (setq ss (ssget '((0 . "INSERT")))) ; Only block references

  (if (not ss)
    (progn (prompt "\nNo blocks selected.") (exit))
  )

  (setq idx 0)
  (while (< idx (sslength ss))
    (setq ent (vlax-ename->vla-object (ssname ss idx)))
    (setq blkName (vla-get-Name ent))
    (setq blkDef (vla-Item (vla-get-Blocks (vla-get-Document ent)) blkName))

    (DeleteDimsInBlock blkDef)

    (setq idx (1+ idx))
  )

  (prompt "\nAll dimension entities inside selected blocks and nested blocks have been deleted.")
  (princ)
)

(princ "\nType Del_Dims_InSelection to run.")
(princ)