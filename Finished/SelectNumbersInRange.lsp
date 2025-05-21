(defun SelectNumbersInRangeByUser (ss minNum maxNum / i ent obj txt n val filtered)
  (setq filtered '())

  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq obj (vlax-ename->vla-object ent))
        (setq txt (vla-get-TextString obj))
        (if (and txt (numberp (read txt)))
          (progn
            (setq n (atoi txt))
            (if (and (>= n minNum) (<= n maxNum))
              (setq filtered (cons ent filtered))
            )
          )
        )
        (setq i (1+ i))
      )
    )
  )
  filtered
)

(defun c:SelectNumbersInRange ( / ss ents minNum maxNum tmp newSS )

  ;; Step 1: Ask for min and max
  (initget 7)
  (setq minNum (getint "\nEnter minimum number: "))
  (setq maxNum (getint "\nEnter maximum number: "))
  (if (> minNum maxNum)
    (progn (setq tmp minNum) (setq minNum maxNum) (setq maxNum tmp))
  )

  ;; Step 2: Get selection set (PICKFIRST or prompt)
  (setq ss (ssget "_I" '((0 . "TEXT,MTEXT"))))
  (if (not ss)
    (progn
      (prompt "\nSelect TEXT or MTEXT containing numbers:")
      (setq ss (ssget '((0 . "TEXT,MTEXT"))))
    )
  )

  ;; Step 3: Filter and reselect
  (if ss
    (progn
      (setq ents (SelectNumbersInRangeByUser ss minNum maxNum))

      (if ents
        (progn
          (prompt (strcat "\nFound " (itoa (length ents)) " numbers between "
                          (itoa minNum) " and " (itoa maxNum) "."))

          ;; Create new selection set from filtered list
          (setq newSS (ssadd))
          (foreach e ents
            (setq newSS (ssadd e newSS))
          )

          ;; Visually select the result in AutoCAD (PICKFIRST)
          (sssetfirst nil newSS)
        )
        (prompt "\nNo text found in specified number range.")
      )
    )
    (prompt "\nNo text selected.")
  )

  (princ "\nType 'SelectNumbersInRange' to run the command.")
  (princ)
)

(princ "\nType 'SelectNumbersInRange' to run the command.")
(princ)