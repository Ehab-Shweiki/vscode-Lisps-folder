(defun c:RENUM_Val (/ p1 p2 ss lst order ent obj cen dx dy baseVal newVal i)

  ;; --- Global holder for last value ---
  (if (not *RENUM_LastValue*)
    (setq *RENUM_LastValue* 0)
  )

  ;; --- Helper: Get center or insertion point ---
  (defun ent-center (obj)
    (cond
      ((= (vla-get-objectname obj) "AcDbMText")
       (if (and (vlax-property-available-p obj 'boundingbox))
         (let* ((ext (vlax-get obj 'boundingbox)))
           (if (and (car ext) (cadr ext))
             (let ((min (vlax-safearray->list (car ext)))
                   (max (vlax-safearray->list (cadr ext))))
               (mapcar '(lambda (a b) (/ (+ a b) 2.0)) min max)
             )
           )
         )
       )
      )
      ((= (vla-get-objectname obj) "AcDbText")
       (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint obj))))
    )
  )

  ;; --- Sorting function ---
  (defun sort1D-func (a b)
    (cond
      ((= order "X+") (< (car a) (car b))) ;; Left → Right
      ((= order "X-") (> (car a) (car b))) ;; Right → Left
      ((= order "Y+") (< (cadr a) (cadr b))) ;; Bottom → Top
      ((= order "Y-") (> (cadr a) (cadr b))) ;; Top → Bottom
    )
  )

  ;; --- Ask for base number ---
  (prompt "\nSelect a TEXT/MTEXT to get starting number, or press Enter to type or use previous.")
  (setq ent (car (entsel "\nSelect starting text (or Enter): ")))

  (cond
    (ent
      (setq obj (vlax-ename->vla-object ent))
      (setq baseVal (atoi (vla-get-TextString obj)))
      (prompt (strcat "\nStarting number taken from selected text: " (itoa baseVal)))
    )
    (T
      (initget 128)
      (setq baseVal (getint (strcat "\nEnter starting number <" (itoa *RENUM_LastValue*) ">: ")))
      (if (null baseVal)
        (setq baseVal *RENUM_LastValue*)
      )
    )
  )

  (setq *RENUM_LastValue* baseVal)

  ;; --- Loop for multiple renumbering rounds ---
  (while T
    (prompt "\nPick selection window for next group (Enter to finish)...")
    (setq p1 (getpoint "\nPick first corner of selection window (or Enter to finish): "))
    (if (null p1)
      (progn
        (prompt "\nRenumbering finished.")
        (exit) ; exit loop
      )
    )
    (setq p2 (getcorner p1 "\nPick opposite corner: "))

    ;; Determine direction
    (setq dx (abs (- (car p2) (car p1))))
    (setq dy (abs (- (cadr p2) (cadr p1))))
    (setq order
      (if (> dx dy)
        (if (> (car p2) (car p1)) "X+" "X-")
        (if (> (cadr p2) (cadr p1)) "Y+" "Y-")
      )
    )

    (prompt (strcat "\nSorting direction: "
      (cond
        ((= order "X+") "Left → Right")
        ((= order "X-") "Right → Left")
        ((= order "Y+") "Bottom → Top")
        ((= order "Y-") "Top → Bottom")
      )
    ))

    ;; --- Select texts to renumber ---
    (setq ss (ssget "C" p1 p2 '((0 . "TEXT,MTEXT"))))
    (if (not ss)
      (prompt "\nNo valid TEXT or MTEXT selected.")
      (progn
        ;; Build sortable list
        (setq lst '())
        (repeat (setq i (sslength ss))
          (setq ent (ssname ss (setq i (1- i))))
          (setq obj (vlax-ename->vla-object ent))
          (setq cen (ent-center obj))
          (if cen
            (setq lst (cons (cons cen obj) lst))
          )
        )

        ;; Sort
        (setq lst (vl-sort lst '(lambda (a b) (sort1D-func (car a) (car b)))))

        ;; Apply renumbering
        (setq newVal (1+ *RENUM_LastValue*))
        (foreach item lst
          (setq obj (cdr item))
          (vla-put-TextString obj (itoa newVal))
          (setq newVal (1+ newVal))
        )

        ;; Update stored value
        (setq *RENUM_LastValue* (1- newVal))

        (prompt (strcat "\nRenumbered " (itoa (length lst)) " items. Last value now: " (itoa *RENUM_LastValue*)))
      )
    )
  )

  (princ)
)

(princ "\nType 'RENUM_Val' to run the looped renumbering command.")
(princ)