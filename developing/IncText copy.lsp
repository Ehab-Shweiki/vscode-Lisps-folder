(defun c:IncText2 (/ ent ename edata txtval basept num layer style height pt step
                   shapeEnt shapeChoice ssref dvec props rot col wid obl halign valign alignpt)

  ;; Helper to get number from string
  (defun extract-number (str / val)
    (setq val (read str))
    (if (numberp val) val nil)
  )

  ;; Step 1: Select text entity
  (prompt "\nSelect a TEXT entity with a number:")
  (setq ent (entsel))
  (if (null ent) (progn (prompt "\nNothing selected.") (exit)))

  (setq ename (car ent))
  (setq edata (entget ename))
  (setq txtval (cdr (assoc 1 edata)))
  (setq num (extract-number txtval))
  (if (not num) (progn (prompt "\nSelected text does not contain a valid number.") (exit)))

  ;; Step 2: Extract all relevant properties
  (setq layer    (cdr (assoc 8 edata)))
  (setq style    (cdr (assoc 7 edata)))
  (setq height   (cdr (assoc 40 edata)))
  (setq rot      (cdr (assoc 50 edata)))
  (setq col      (assoc 62 edata)) ;; optional
  (setq wid      (assoc 41 edata)) ;; width factor
  (setq obl      (assoc 51 edata)) ;; obliquing angle
  (setq halign   (cdr (assoc 72 edata)))
  (setq valign   (cdr (assoc 73 edata)))
  (setq alignpt  (cdr (assoc 11 edata)))

  ;; Step 3: Ask for base point and shape (optional)
  (setq basept (getpoint "\nSpecify base point of selected text: "))
  (if (null basept) (exit))

  ;; Optional: Shape
  (initget "Yes No")
  (setq shapeChoice (getkword "\nDo you want to wrap number with shape? [Yes/No] <No>: "))
  (if (eq shapeChoice "Yes")
    (progn
      (prompt "\nSelect the shape object around the number:")
      (setq ssref (entsel "\nSelect shape: "))
    )
  )

  ;; Step 4: Ask for insertion point
  (setq pt (getpoint "\nSpecify insertion point for first new number: "))
  (if (null pt) (exit))

  ;; Step 5: Loop insertions
  (setq step 1)
  (setq num (+ num step))

  (while pt
    (setq props
      (list
        (cons 0 "TEXT")
        (cons 8 layer)
        (cons 7 style)
        (cons 10 pt)
        (cons 40 height)
        (cons 1 (rtos num 2 0))
        (cons 50 rot)
        (cons 72 halign)
        (cons 73 valign)
        (cons 11 pt)
      )
    )

    ;; Optional properties
    (if col (setq props (append props (list col))))
    (if wid (setq props (append props (list wid))))
    (if obl (setq props (append props (list obl))))

    ;; Create the new text
    (entmakex props)

    ;; Optional: copy the shape
    (if ssref
      (command "_.COPY" (car ssref) "" basept pt)
    )

    (setq num (+ num step))
    (setq pt (getpoint "\nNext point (ESC to stop): "))
  )

  (princ "\nCommand 'IncText' finished.")(princ)
)

(princ "\nType 'IncText' to run the command.")
(princ)