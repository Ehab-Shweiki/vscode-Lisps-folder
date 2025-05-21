  ;; --- Sorting function ---
  (defun sort1D-func (a b)
    (cond
      ((= order "X+") (< (car a) (car b))) ;; Left to Right
      ((= order "X-") (> (car a) (car b))) ;; Right to Left
      ((= order "Y+") (< (cadr a) (cadr b))) ;; Bottom to Top
      ((= order "Y-") (> (cadr a) (cadr b))) ;; Top to Bottom
    )
  )
 (defun sort2d-func (a b)
    (cond
      ((= order "YX") ;; Y ascending, X ascending
        (if (/= (cadr a) (cadr b)) (< (cadr a) (cadr b)) (< (car a) (car b)))
      )
      ((= order "Y-X") ;; Y descending, X ascending
        (if (/= (cadr a) (cadr b)) (> (cadr a) (cadr b)) (< (car a) (car b)))
      )
      ((= order "XY") ;; X ascending, Y ascending
        (if (/= (car a) (car b)) (< (car a) (car b)) (< (cadr a) (cadr b)))
      )
      ((= order "X-Y") ;; X descending, Y ascending
        (if (/= (car a) (car b)) (> (car a) (car b)) (< (cadr a) (cadr b)))
      )
    )
  )
(defun c:SelOrderText ( / p1 p2 ss lst order ent obj cen dx dy)

  ;; --- Get center or insertion point safely ---
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
       (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint obj)))
      )
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

  ;; --- Ask for 2 corners of rectangle (simulate normal window selection) ---
  (setq p1 (getpoint "\nPick first corner of selection window: "))
  (setq p2 (getcorner p1 "\nPick opposite corner: "))

  ;; --- Make selection using crossing window ---
  (setq ss (ssget "C" p1 p2 '((0 . "TEXT,MTEXT"))))
  (if (not ss)
    (progn (prompt "\nNo valid TEXT or MTEXT selected.") (exit))
  )
  (prompt (strcat "\nSelected " (itoa (sslength ss)) " objects."))

  ;; --- Determine direction from box shape ---
  (setq dx (abs (- (car p2) (car p1)))) ;; width
  (setq dy (abs (- (cadr p2) (cadr p1)))) ;; height

  (if (> dx dy)
    (if (> (car p2) (car p1))
      (setq order "X+")
      (setq order "X-")
    )
    (if (> (cadr p2) (cadr p1))
      (setq order "Y+")
      (setq order "Y-")
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

  ;; --- Build sorted list ---
  (setq lst '())
  (repeat (setq i (sslength ss))
    (setq ent (ssname ss (setq i (1- i))))
    (setq obj (vlax-ename->vla-object ent))
    (setq cen (ent-center obj))
    (if cen
      (setq lst (cons (cons cen obj) lst))
      (prompt (strcat "\nWarning: Cannot locate " (vla-get-objectname obj)))
    )
  )

  ;; --- Sort and print values ---
  (if (not lst)
    (prompt "\nNo valid entities found.")
    (progn
      (setq lst (vl-sort lst
        '(lambda (a b) (sort1D-func (car a) (car b)))
      ))
      (prompt "\nOrdered text values:")
      (foreach item lst
        (prompt (strcat "\n" (vla-get-TextString (cdr item))))
      )
    )
  )

  (princ)
)

(princ "\nType 'SelOrderTextAuto' to run the command.")
(princ)