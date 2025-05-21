(defun get-wrap-shape-setting ()
  (if (getenv "IncTextShape")
    (= (getenv "IncTextShape") "1")
    nil ; default = no
  )
)

(defun set-wrap-shape-setting (val)
  (setenv "IncTextShape" (if val "1" "0"))
)

(defun c:IncTextShapeToggle ( / current new)
  (setq current (get-wrap-shape-setting))
  (setq new (not current))
  (set-wrap-shape-setting new)
  (prompt (strcat "\nShape wrapping is now " (if new "ENABLED" "DISABLED")))
  (princ)
)

(defun c:IncText (/ ent ename edata txtval basept num layer style height pt step
                   shapeEnt ssref props rot col wid obl halign valign alignpt shapeOn)

  ;; --- Get saved setting ---
  (setq shapeOn (not T))

  ;; Step 1: Select text entity
  (prompt "\nSelect a TEXT entity with a number:")
  (setq ent (entsel))
  (if (null ent) (progn (prompt "\nNothing selected.") (exit)))

  (setq ename (car ent))
  (setq edata (entget ename))
  (setq txtval (cdr (assoc 1 edata)))
  (setq num (read txtval))
  (if (not (numberp num)) (progn (prompt "\nSelected text does not contain a valid number.") (exit)))

  ;; Step 2: Extract properties
  (setq layer    (cdr (assoc 8 edata)))
  (setq style    (cdr (assoc 7 edata)))
  (setq height   (cdr (assoc 40 edata)))
  (setq rot      (cdr (assoc 50 edata)))
  (setq col      (assoc 62 edata))
  (setq wid      (assoc 41 edata))
  (setq obl      (assoc 51 edata))
  (setq halign   (cdr (assoc 72 edata)))
  (setq valign   (cdr (assoc 73 edata)))
  (setq alignpt  (cdr (assoc 11 edata)))

  ;; Step 3: Ask for base point
  (setq basept (getpoint "\nSpecify base point of selected text: "))
  (if (null basept) (exit))

  ;; Step 4: If shape is enabled, allow shape selection
  (if shapeOn
    (progn
      (prompt "\nShape wrapping is ON.")
      (setq ssref (entsel "\nSelect shape to copy: "))
    )
  )

  ;; Step 5: Ask for insertion point
  (setq pt (getpoint "\nSpecify insertion point for first new number: "))
  (if (null pt) (exit))

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

    (if col (setq props (append props (list col))))
    (if wid (setq props (append props (list wid))))
    (if obl (setq props (append props (list obl))))

    (entmakex props)

    (if (and shapeOn ssref)
      (command "_.COPY" (car ssref) "" basept pt)
    )

    (setq num (+ num step))
    (setq pt (getpoint "\nNext point (ESC to stop): "))
  )

  (princ "\nCommand 'IncText' finished.")(princ)
)

(princ "\nType 'IncText' to run the command.")
(princ "\nUse 'IncTextShapeToggle' to enable or disable shape wrapping.")
(princ)