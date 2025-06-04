(defun c:ExportGroupedBoundaries (/ ss idx ent ed layerName basementMap exportFolder blkIgnore
                                     basename filename refEnt currEnt refPt currPt angleRef angleCurr
                                     alignAngle ssInBoundary)

  (setq exportFolder (getstring "\nEnter export folder path: "))
  (setq blkIgnore (getstring "\nEnter name of reference block to ignore: "))
  (setq ss (ssget '((0 . "LWPOLYLINE"))))
  (if (not ss)
    (progn (prompt "\nNo polylines selected.") (exit))
  )

  ;; Collect boundaries grouped by basement names
  (setq basementMap (make-hash-table))
  (setq idx 0)
  (while (< idx (sslength ss))
    (setq ent (ssname ss idx))
    (setq ed (entget ent))
    (setq layerName (cdr (assoc 8 ed)))

    ;; Split layer name by "_"
    (if (and layerName (wcmatch layerName "*_*"))
      (progn
        (setq parts (parse-by-underscore layerName))
        (if (= (length parts) 2)
          (let* (
                  (bName (car parts))
                  (fName (cadr parts))
                  (existing (gethash bName basementMap))
                )
            ;; Store as: (list (referenceEnt . others))
            (if existing
              (puthash bName (cons (cons ent fName) existing) basementMap)
              (puthash bName (list (cons ent fName)) basementMap)
            )
          )
        )
      )
    )
    (setq idx (1+ idx))
  )

  ;; Loop through each basement group
  (mapcar
    (lambda (bKey)
      (setq boundaries (reverse (gethash bKey basementMap)))
      (setq refEnt (car (car boundaries))) ;; First boundary = reference
      (setq refPt (centroid-of-boundary refEnt))
      (setq angleRef (boundary-angle refEnt))

      ;; Process all other boundaries
      (foreach pair (cdr boundaries)
        (setq currEnt (car pair))
        (setq fname (cadr pair))
        (setq currPt (centroid-of-boundary currEnt))
        (setq angleCurr (boundary-angle currEnt))
        (setq alignAngle (- angleRef angleCurr))

        ;; Select all entities inside current boundary
        (setq ssInBoundary (ssget "_WP" (boundary-coords currEnt)))
        (setq ssInBoundary (remove-blocks-by-name ssInBoundary blkIgnore))

        ;; Check for blocks extending
        (if (check-blocks-crossing currEnt ssInBoundary)
          (prompt (strcat "\nWARNING: Blocks extend outside boundary for file: " bKey "_" fname))
        )

        ;; Transform and export
        (command "_.COPYBASE" currPt ssInBoundary "")
        (command "_.PASTECLIP" refPt)
        (command "_.ROTATE" "_L" "" refPt (* alignAngle (/ 180.0 pi)))

        ;; Export filename
        (setq filename (strcat exportFolder "\\" bKey "_" fname ".dwg"))
        (command "_.WBLOCK" filename "" "_L")
        (command "_.ZOOM" "_E")
      )
    )
    (hash-keys basementMap)
  )

  (prompt "\nAll grouped boundaries exported.")
  (princ)
)

;; --- Parse layer name by "_"
(defun parse-by-underscore (str)
  (vl-string->list (vl-string-subst " " "_" str))
  (mapcar 'vl-string-trim (parse-by-delim str "_"))
)

(defun parse-by-delim (str delim / pos parts)
  (setq parts '())
  (while (setq pos (vl-string-search delim str))
    (setq parts (cons (substr str 1 pos) parts))
    (setq str (substr str (+ pos 2)))
  )
  (reverse (cons str parts))
)

(defun hash-keys (ht / keys)
  (vlax-for k ht (setq keys (cons k keys)))
  keys
)

(defun make-hash-table () (vlax-create-object "Scripting.Dictionary"))

(defun gethash (k h) (if (= (vlax-invoke h "Exists" k) :vlax-true) (vlax-invoke h "Item" k) nil))
(defun puthash (k v h) (vlax-invoke h "Add" k v))

;; --- Geometry functions reused ---
(defun boundary-coords (ent / elist)
  (mapcar 'cdr (vl-remove-if-not '(lambda (x) (= (car x) 10)) (entget ent)))
)

(defun centroid-of-boundary (ent / pts sumx sumy)
  (setq pts (boundary-coords ent)
        sumx 0 sumy 0)
  (foreach pt pts
    (setq sumx (+ sumx (car pt)))
    (setq sumy (+ sumy (cadr pt)))
  )
  (list (/ sumx (length pts)) (/ sumy (length pts)))
)

(defun boundary-angle (ent / pts)
  (setq pts (boundary-coords ent))
  (angle (nth 0 pts) (nth 1 pts))
)

(defun remove-blocks-by-name (ss name / i ent newSS ed)
  (setq i 0 newSS (ssadd))
  (while (< i (sslength ss))
    (setq ent (ssname ss i))
    (setq ed (entget ent))
    (if (or (/= (cdr (assoc 0 ed)) "INSERT")
            (not (= (strcase (cdr (assoc 2 ed))) (strcase name))))
      (setq newSS (ssadd ent newSS))
    )
    (setq i (1+ i))
  )
  newSS
)

(defun check-blocks-crossing (boundary ss / i ent inside allInside)
  (setq i 0 allInside T)
  (while (< i (sslength ss))
    (setq ent (ssname ss i))
    (if (= (cdr (assoc 0 (entget ent))) "INSERT")
      (progn
        (if (block-outside-boundary? ent boundary)
          (setq allInside nil)
        )
      )
    )
    (setq i (1+ i))
  )
  (not allInside)
)

(defun block-outside-boundary? (ent boundary / vlaEnt minPt maxPt corners insideCount pt)
  (setq vlaEnt (vlax-ename->vla-object ent))
  (vla-getboundingbox vlaEnt 'minPt 'maxPt)
  (setq minPt (vlax-safearray->list minPt))
  (setq maxPt (vlax-safearray->list maxPt))
  (setq corners
    (list
      (list (car minPt) (cadr minPt))
      (list (car maxPt) (cadr minPt))
      (list (car minPt) (cadr maxPt))
      (list (car maxPt) (cadr maxPt))
    )
  )
  (setq insideCount 0)
  (foreach pt corners
    (if (not (vl-catch-all-error-p
               (vl-catch-all-apply 'vlax-curve-getParamAtPoint
                                   (list (vlax-ename->vla-object boundary)
                                         (list (car pt) (cadr pt) 0.0)))))
      (setq insideCount (1+ insideCount))
    )
  )
  (< insideCount 4)
)

(princ "\nType 'ExportGroupedBoundaries' to run the command.")
(princ)