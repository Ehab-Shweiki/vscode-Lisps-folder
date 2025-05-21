(defun c:FindDuplicatedText ( / doc ss i ent obj txt countAlist dupList dupEntities totalCount )
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq countAlist '())      ; Assoc list (string . count)
  (setq dupList '())         ; List of (string . count) with duplicates
  (setq dupEntities '())     ; List of (string . (list of VLA objects))
  (setq totalCount 0)

  ;; -------------------------------------
  ;; Helpers
  ;; -------------------------------------
  (defun getCount (key) (cdr (assoc key countAlist)))
  (defun setCount (key val)
    (if (assoc key countAlist)
      (setq countAlist (subst (cons key val) (assoc key countAlist) countAlist))
      (setq countAlist (cons (cons key val) countAlist))
    )
  )

  (defun addEntityToGroup (key obj)
    (if (assoc key dupEntities)
      (setq dupEntities
            (subst (cons key (cons obj (cdr (assoc key dupEntities))))
                   (assoc key dupEntities)
                   dupEntities))
      (setq dupEntities (cons (cons key (list obj)) dupEntities)))
  )

  (defun addText (str obj)
    (if (/= str "")
      (progn
        (setq cnt (getCount str))
        (if cnt
          (setCount str (+ cnt 1))
          (setCount str 1)
        )
        (addEntityToGroup str obj)
      )
    )
  )

  ;; -------------------------------------
  ;; Select text/mtext from pre/prompt
  ;; -------------------------------------
  (defun GetTextSelection ( / ss)
    (setq ss (ssget "_I" '((0 . "TEXT,MTEXT"))))
    (if (not ss)
      (progn
        (prompt "\nSelect TEXT or MTEXT:")
        (setq ss (ssget '((0 . "TEXT,MTEXT"))))
      )
    )
    ss
  )

  ;; -------------------------------------
  ;; Process selection
  ;; -------------------------------------
  
  ;; Get preselected or prompted TEXT/MTEXT
  (setq ss (ssget "_I" '((0 . "TEXT,MTEXT"))))
  (if (not ss)
    (progn
      (prompt "\nSelect TEXT or MTEXT:")
      (setq ss (ssget '((0 . "TEXT,MTEXT"))))
    )
  )
  
  ;; Store entities in list and clear visual selection
  (setq storedEnts '())
  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq storedEnts (cons (ssname ss i) storedEnts))
        (setq i (1+ i))
      )
  
      ;; Clear grips/highlight so our vla-Highlight will be visible
      (command "_.SELECT" "_None")
    )
    (prompt "\nNo valid text selected.")
  )
  
  ;; Process stored entities
  (foreach ent storedEnts
    (setq obj (vlax-ename->vla-object ent))
    (setq txt (vla-get-TextString obj))
    (addText txt obj)
  )
  
  ;; -------------------------------------
  ;; Build dupList and count total
  ;; -------------------------------------
  (foreach pair countAlist
    (if (> (cdr pair) 1)
      (progn
        (setq dupList (cons pair dupList))
        (setq totalCount (+ totalCount (cdr pair)))
      )
    )
  )

  ;; -------------------------------------
  ;; Print Results
  ;; -------------------------------------
  (if dupList
    (progn
      (princ "\nDuplicate Text Entries Found:\n")
      (foreach item (vl-sort dupList (function (lambda (a b) (< (car a) (car b)))))
        (princ (strcat "\n  \"" (car item) "\" - " (itoa (cdr item)) " times"))
      )
      (princ (strcat "\n\nTotal number of duplicated entries: " (itoa totalCount)))

      ;; -------------------------------------
      ;; Zoom to each duplicate instance
      ;; -------------------------------------
      (prompt "\n\nPress Enter to zoom to each duplicate (Esc to cancel)...")
      (foreach d dupList
		(setq val (car d))
		(setq objList (cdr (assoc val dupEntities)))

		;; Highlight all instances of this text
		(foreach obj objList
			(vla-Highlight obj :vlax-true)
		)

		(prompt (strcat "\n\n--- \"" val "\" appears " (itoa (length objList)) " times ---"))

		;; Zoom through each one
		(foreach obj objList
			(ZoomToText obj)
			(initget 1)
			(getstring (strcat "\n[" val "] - Press Enter to go to next... "))
		)

		;; Unhighlight all after viewing
		(foreach obj objList
			(vla-Highlight obj :vlax-false)
		)
	  )
    )
    (princ "\nNo duplicate text found.")
  )

  (princ)
)


(defun ZoomToText (obj / pt h windowHeight minPt maxPt extWidth aspectRatio winWidth)

  ;; Get insertion point
  (setq pt (vlax-get obj 'InsertionPoint))

  ;; Get text height (TEXT vs MTEXT)
  (setq h (if (vlax-property-available-p obj 'Height)
            (vlax-get obj 'Height)
            (vlax-get obj 'TextHeight))) ; for MTEXT

  ;; Use default if text has 0 height (fallback)
  (if (<= h 0.0) (setq h 1.0))

  ;; Window height = 30 × text height
  (setq windowHeight (* 50.0 h))

  ;; Get aspect ratio of screen (width / height)
  (setq extWidth (car (getvar "SCREENSIZE")))
  (setq aspectRatio (/ (float extWidth) (cadr (getvar "SCREENSIZE"))))

  ;; Calculate window width based on aspect ratio
  (setq winWidth (* windowHeight aspectRatio))

  ;; Build min and max points
  (setq minPt (vlax-3d-point (list (- (car pt) (/ winWidth 2.0))
                                   (- (cadr pt) (/ windowHeight 2.0))
                                   0.0)))
  (setq maxPt (vlax-3d-point (list (+ (car pt) (/ winWidth 2.0))
                                   (+ (cadr pt) (/ windowHeight 2.0))
                                   0.0)))

  ;; Zoom to calculated window
  (vla-ZoomWindow (vlax-get-acad-object) minPt maxPt)
)

(princ "\nType 'FindDuplicatedText' to run the command.")
(princ)