(defun c:OverkillFromBlock (/ blkName blkDef blkEnts msEnts blkData entData matches)
  (vl-load-com)

  ;;--- Helper to get basic entity signature
  (defun get-entity-signature (ent)
    (if (and ent (entget ent))
      (vl-remove-if
        (function
          (lambda (x)
            (or (member (car x) '(5 330 370 62)) ; ignore handles, parent, lineweight, color
                (eq (car x) 100) ; skip subclass markers
            )
          )
        )
        (entget ent)
      )
    )
  )

  ;;--- Get block name
  (setq blkName (getstring T "\nEnter block name to match entities from: "))
  (setq blkDef (tblobjname "BLOCK" blkName))
  (if (not blkDef)
    (progn (princ "\nBlock not found.") (exit))
  )

  ;;--- Get entities in block definition
  (setq blkEnts '())
  (setq entData nil)
  (setq blkDef (entnext blkDef))
  (while (and blkDef (/= (cdr (assoc 0 (entget blkDef))) "ENDBLK"))
    (setq blkEnts (cons (get-entity-signature blkDef) blkEnts))
    (setq blkDef (entnext blkDef))
  )

  ;;--- Scan through model space
  (setq msEnts (ssget "_X" '((410 . "Model"))))
  (if msEnts
    (progn
      (setq i 0)
      (while (< i (sslength msEnts))
        (setq ent (ssname msEnts i))
        (setq entData (get-entity-signature ent))
        ;; Check if entity is not part of a block (i.e., top-level)
        (if (and entData
                 (not (assoc 330 (entget ent))) ; skip block subentities
                 (member entData blkEnts)
            )
          (progn
            (entdel ent)
            (princ (strcat "\nDeleted entity matching block item: " (rtos i 2 0)))
          )
        )
        (setq i (1+ i))
      )
    )
    (princ "\nNo model space entities found.")
  )

  (princ "\nDone.")
  (princ)
)