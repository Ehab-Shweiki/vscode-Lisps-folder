(defun c:rr (/ sel ent blkEnt basePt ents longest maxDist refPt newPt)
  ;; Step 1: Select a block
  (setq sel (entsel "\nSelect block: "))
  (if (not sel)
    (progn (princ "\nNothing selected.") (exit))
  )

  (setq ent (car sel))
  (setq blkEnt (vlax-ename->vla-object ent))

  ;; Step 2: Ask for base point
  (setq basePt (getpoint "\nSpecify base point for rotation: "))

  ;; Step 3: Get all nested entities in the block
  (setq ents (vlax-invoke blkEnt 'Explode)) ; Temporarily explode to get geometry

  ;; Step 4: Find the longest LINE or LWPOLYLINE
  (setq longest nil)
  (setq maxDist 0.0)
  (foreach obj ents
    (cond
      ;; LINE case
      ((= (vla-get-objectname obj) "AcDbLine")
       (let* (
              (p1 (vlax-get obj 'StartPoint))
              (p2 (vlax-get obj 'EndPoint))
              (d (distance p1 p2))
             )
         (if (> d maxDist)
           (progn
             (setq maxDist d)
             (setq longest obj)
           )
         )
       )
      )
      ;; LWPOLYLINE case
      ((= (vla-get-objectname obj) "AcDbPolyline")
       (let* (
              (n (fix (vla-get obj 'NumberOfVertices)))
              (coords (vlax-get obj 'Coordinates))
              (i 0)
             )
         (while (< i (1- n))
           (let* (
                  (pt1 (list (nth (* i 2) coords) (nth (+ 1 (* i 2)) coords)))
                  (pt2 (list (nth (* (+ i 1) 2) coords) (nth (+ 1 (* (+ i 1) 2)) coords)))
                  (d (distance pt1 pt2))
                 )
             (if (> d maxDist)
               (progn
                 (setq maxDist d)
                 (setq longest (list pt1 pt2))
               )
             )
             (setq i (1+ i))
           )
         )
       )
      )
    )
  )

  ;; Step 5: Get reference point (farther from base)
  (cond
    ((vlax-objectp longest)
     ;; LINE case
     (setq refPt
       (if (> (distance basePt (vlax-get longest 'EndPoint))
              (distance basePt (vlax-get longest 'StartPoint)))
         (vlax-get longest 'EndPoint)
         (vlax-get longest 'StartPoint)
       )
     )
    )
    ((and (listp longest) (= (length longest) 2))
     ;; LWPOLYLINE case
     (setq refPt
       (if (> (distance basePt (cadr longest))
              (distance basePt (car longest)))
         (cadr longest)
         (car longest)
       )
     )
    )
    (T
     (princ "\nNo suitable geometry found.")
     (exit)
    )
  )

  ;; Step 6: Ask for destination reference point
  (setq newPt (getpoint "\nSpecify destination of reference point: "))

  ;; Step 7: Use ROTATE in reference mode
  (command "_.ROTATE" ent "" basePt "_Reference" refPt newPt)

  ;; Done
  (princ "\nDone. Type RR to run again.")
  (princ)
)

(princ "\nType RR to rotate block using reference.")(princ)