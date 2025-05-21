(defun c:FindNumberJumps ( / ss i ent obj txt val curr next numList sorted jumps
                             obj1 obj2 val1 val2 pt1 pt2 mid h windowHeight extWidth
                             aspectRatio winWidth minPt maxPt )

  ;; Helper: zoom to two text objects (centered, scaled)
  (defun ZoomToPair (obj1 obj2 / pt1 pt2 mid h windowHeight extWidth aspectRatio winWidth minPt maxPt)
    (setq pt1 (vlax-get obj1 'InsertionPoint))
    (setq pt2 (vlax-get obj2 'InsertionPoint))

    ;; Midpoint between the two
    (setq mid (mapcar '(lambda (a b) (/ (+ a b) 2.0)) pt1 pt2))

    ;; Estimate height
    (setq h (+
              (if (vlax-property-available-p obj1 'Height) (vlax-get obj1 'Height) (vlax-get obj1 'TextHeight))
              (if (vlax-property-available-p obj2 'Height) (vlax-get obj2 'Height) (vlax-get obj2 'TextHeight))
            ))
    (setq h (/ h 2.0))
    (if (<= h 0.0) (setq h 1.0)) ; fallback

    ;; Aspect ratio logic
    (setq windowHeight (* 30.0 h))
    (setq extWidth (car (getvar "SCREENSIZE")))
    (setq aspectRatio (/ (float extWidth) (cadr (getvar "SCREENSIZE"))))
    (setq winWidth (* windowHeight aspectRatio))

    ;; Zoom window box
    (setq minPt (vlax-3d-point (list (- (car mid) (/ winWidth 2.0)) (- (cadr mid) (/ windowHeight 2.0)) 0.0)))
    (setq maxPt (vlax-3d-point (list (+ (car mid) (/ winWidth 2.0)) (+ (cadr mid) (/ windowHeight 2.0)) 0.0)))

    (vla-ZoomWindow (vlax-get-acad-object) minPt maxPt)
  )

  ;; Step 1: Get selection (pre or prompt)
  (setq ss (ssget "_I" '((0 . "TEXT,MTEXT"))))
  (if (not ss)
    (progn
      (prompt "\nSelect TEXT or MTEXT containing numbers:")
      (setq ss (ssget '((0 . "TEXT,MTEXT"))))
    )
  )

  ;; Step 2: Store entities and clear selection
  (setq numList '())
  (if ss
    (progn
      (command "_.SELECT" "_None") ; clear grips for highlight visibility
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq obj (vlax-ename->vla-object ent))
        (setq txt (vla-get-TextString obj))
        (if (and txt (numberp (read txt)))
          (setq numList (cons (cons (atoi txt) obj) numList))
        )
        (setq i (1+ i))
      )
    )
    (progn (prompt "\nNo valid numeric text selected.") (exit))
  )

  ;; Step 3: Sort numbers
  (setq sorted (vl-sort numList '(lambda (a b) (< (car a) (car b)))))

  ;; Step 4: Detect jumps
  (setq jumps '())
  (setq i 0)
  (while (< (1+ i) (length sorted))
    (setq curr (nth i sorted))
    (setq next (nth (1+ i) sorted))
    (if (> (- (car next) (car curr)) 1)
      (setq jumps (cons (list curr next) jumps))
    )
    (setq i (1+ i))
  )

  ;; Step 5: Report + zoom to each jump
  (if jumps
    (progn
      ;; Print full report
      (princ "\nNumber jumps found:\n")
      (foreach pair (reverse jumps)
        (setq val1 (car (car pair)))
        (setq val2 (car (cadr pair)))
        (princ (strcat "\n  Jump: " (itoa val1) " -> " (itoa val2)))
      )

      ;; Zoom to each jump with user prompt
      (foreach pair (reverse jumps)
        (setq val1 (car (car pair)))
        (setq val2 (car (cadr pair)))
        (setq obj1 (cdr (car pair)))
        (setq obj2 (cdr (cadr pair)))

        (vla-Highlight obj1 :vlax-true)
        (vla-Highlight obj2 :vlax-true)
        (ZoomToPair obj1 obj2)

        (initget 1)
        (getstring (strcat "\nJump " (itoa val1) " -> " (itoa val2) " — press Enter to continue..."))

        (vla-Highlight obj1 :vlax-false)
        (vla-Highlight obj2 :vlax-false)
      )
    )
    (prompt "\nNo jumps found.")
  )

  (princ)
)

(princ "\nType 'FindNumberJumps' to run the command.")
(princ)