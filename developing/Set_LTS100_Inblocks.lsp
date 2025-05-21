(defun c:SetLTS100InBlocks ( / ss i blk blkname)
  (vl-load-com)

  ;; Recursive function to update entities in block definition
  (defun UpdateLTSInBlock (blkname / blkdef ent)
    (setq blkdef (tblobjname "BLOCK" blkname))
    (if blkdef
      (progn
        (setq ent (entnext blkdef))
        (while ent
          (setq edata (entget ent))
          ;; Check for nested INSERT
          (if (= (cdr (assoc 0 edata)) "INSERT")
            (UpdateLTSInBlock (cdr (assoc 2 edata))) ; recurse for nested blocks
          )
          ;; For any entity with a non-"CONTINUOUS" linetype
          (if (and (assoc 6 edata) (/= (strcase (cdr (assoc 6 edata))) "CONTINUOUS"))
            (progn
              ;; Set linetype scale to 100
              (setq edata (subst (cons 48 100.0) (assoc 48 edata) edata))
              (entmod edata)
            )
          )
          (setq ent (entnext ent))
        )
      )
    )
  )

  ;; Select blocks
  (prompt "\nSelect block references to update linetype scale inside:")
  (setq ss (ssget '((0 . "INSERT"))))

  (if ss
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq blk (ssname ss i))
        (setq blkname (cdr (assoc 2 (entget blk))))
        (UpdateLTSInBlock blkname)
        (setq i (1+ i))
      )
      (princ "\nDone updating linetype scales in blocks.")
    )
    (prompt "\nNo blocks selected.")
  )

  (princ)
)