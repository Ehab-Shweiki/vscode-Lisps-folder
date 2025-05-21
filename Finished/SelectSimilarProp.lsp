; (defun c:SelSimilarProps ( / ent ed color lw ltype ltscale ss matchList resultEnt e i ed2 match)
;   (prompt "\nSelect a reference object: ")
;   (setq ent (car (entsel)))

;   (if ent
;     (progn
;       (setq ed (entget ent))
;       (setq color (cdr (assoc 62 ed)))
;       (setq lw (cdr (assoc 370 ed)))
;       (setq ltype (cdr (assoc 6 ed)))
;       (setq ltscale (cdr (assoc 48 ed)))

;       ;; Get all entities in drawing
;       (setq ss (ssget "_X")) ; all visible entities

;       (setq matchList '())
;       (setq i 0)

;       (while (< i (sslength ss))
;         (setq resultEnt (ssname ss i))
;         (setq ed2 (entget resultEnt))

;         (setq match
;           (and
;             (= (cdr (assoc 62 ed2)) color)
;             (= (cdr (assoc 370 ed2)) lw)
;             (= (cdr (assoc 6 ed2)) ltype)
;             (= (cdr (assoc 48 ed2)) ltscale)
;           )
;         )

;         (if match
;           (setq matchList (cons resultEnt matchList))
;         )

;         (setq i (1+ i))
;       )

;       ;; Create selection set of matched entities
;       (if matchList
;         (progn
;           (sssetfirst nil (list->ss matchList))
;           (prompt (strcat "\nSelected " (itoa (length matchList)) " matching objects."))
;         )
;         (prompt "\nNo matching objects found.")
;       )
;     )
;     (prompt "\nNo object selected.")
;   )
;   (princ)
; )

; ;; Helper: Convert a list of entity names to a selection set
; (defun list->ss (lst / ss)
;   (if lst
;     (progn
;       (setq ss (ssadd))
;       (foreach e lst
;         (setq ss (ssadd e ss))
;       )
;       ss
;     )
;   )
; )




(defun c:sss ( / base edBase ss filtered ent ed color lw ltype lts objType i)

  (prompt "\nSelect base object (already ran SELECTSIMILAR): ")
  (setq base (car (entsel)))
  
  (if base
    (progn
      (setq edBase (entget base))
      (setq color (cdr (assoc 62 edBase)))
      (setq lw (cdr (assoc 370 edBase)))
      (setq ltype (cdr (assoc 6 edBase)))
      (setq lts (cdr (assoc 48 edBase)))
      (setq objType (cdr (assoc 0 edBase)))

      ;; Use current selection (from SELECTSIMILAR)
      (setq ss (ssget "_P"))

      (if ss
        (progn
          (setq filtered (ssadd))
          (setq i 0)

          (while (< i (sslength ss))
            (setq ent (ssname ss i))
            (setq ed (entget ent))

            (if (and
                  (= (cdr (assoc 0 ed)) objType)
                  (= (cdr (assoc 62 ed)) color)
                  (= (cdr (assoc 370 ed)) lw)
                  (= (cdr (assoc 6 ed)) ltype)
                  (= (cdr (assoc 48 ed)) lts)
                )
              (ssadd ent filtered)
            )

            (setq i (1+ i))
          )

          (sssetfirst nil filtered)
          (prompt (strcat "\nFiltered to " (itoa (sslength filtered)) " matching object(s)."))
        )
        (prompt "\nNo objects selected. Please run SELECTSIMILAR before running SSS.")
      )
    )
    (prompt "\nNo base object selected.")
  )

  (princ)
)

; (defun c:sss (c:SelSimilarProps))

; (princ "\nType sss to run SelSimilarProps.")
; (princ)