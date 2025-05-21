(defun c:RENUM_Inc ( / sel inc i ent ename txt newnum newtxt pos)
  (setq inc (getint "\nEnter increment value (default 1): "))
  (if (not inc) (setq inc 1))

  (setq sel (ssget '((0 . "TEXT,MTEXT")))) ; Select text and mtext only
  (if sel
    (progn
      (setq i 0)
      (repeat (sslength sel)
        (setq ent (ssname sel i))
        (setq ename (entget ent))
        (setq txt (cdr (assoc 1 ename)))
        (if (setq pos (vl-string-search (itoa (atoi txt)) txt)) ; find numeric part
          (progn
            (setq newnum (+ (atoi txt) inc))
            (setq newtxt (vl-string-subst (itoa newnum) (itoa (atoi txt)) txt))
            (entmod (subst (cons 1 newtxt) (assoc 1 ename) ename))
          )
        )
        (setq i (1+ i))
      )
      (princ "\nRenumbering complete.")
    )
    (princ "\nNo text selected.")
  )
  (princ)
)

;; Prompt for the command in the AutoCAD command line
(princ "\nType 'RENUM_Inc' to run the command.")
(princ)