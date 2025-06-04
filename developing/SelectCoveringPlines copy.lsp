(defun center-point (minPt maxPt)
  (mapcar '(lambda (a b) (/ (+ a b) 2.0)) minPt maxPt)
)

(defun point-nearby-check (pt tol)
  (list pt
        (mapcar '+ pt (list tol 0.0))     ; Right
        (mapcar '- pt (list tol 0.0))     ; Left
        (mapcar '+ pt (list 0.0 tol))     ; Top
        (mapcar '- pt (list 0.0 tol))     ; Bottom
  )
)

(defun point-in-poly? (plineObj ptList)
  ;; Return T if any point in list is inside polyline
  (and (vlax-curve-isClosed plineObj)
       (some (lambda (pt)
               (not (vl-catch-all-error-p
                     (vl-catch-all-apply 'vlax-curve-getParamAtPoint (list plineObj pt))
               )))
             ptList)
  )
)

(defun c:SelectCoveringPlines (/ ssHatch i hatchEnt minH maxH centerPt ptList
                                 allPlines j plineEnt resultList deselOpt tol)

  (setq tol 0.5) ;; tolerance in drawing units (adjust as needed)

  (prompt "\nSelect hatch objects to find covering polylines:")
  (setq ssHatch (ssget '((0 . "HATCH"))))
  (if (not ssHatch)
    (progn (prompt "\nNo hatches selected.") (exit))
  )

  ;; Deselect option
  (initget "Yes No")
  (setq deselOpt (getkword "\nDeselect hatches after selection? [Yes/No] <No>: "))
  (if (null deselOpt) (setq deselOpt "No"))

  ;; Get all LWPOLYLINEs
  (setq allPlines (ssget "X" '((0 . "LWPOLYLINE"))))
  (if (not allPlines)
    (progn (prompt "\nNo polylines found.") (exit))
  )

  ;; Result
  (setq resultList (ssadd))

  ;; For each hatch
  (setq i 0)
  (while (< i (sslength ssHatch))
    (setq hatchEnt (vlax-ename->vla-object (ssname ssHatch i)))
    (vla-GetBoundingBox hatchEnt 'minH 'maxH)
    (setq centerPt (center-point (vlax-safearray->list minH) (vlax-safearray->list maxH)))
    (setq ptList (point-nearby-check centerPt tol))

    ;; For each polyline
    (setq j 0)
    (while (< j (sslength allPlines))
      (setq plineEnt (vlax-ename->vla-object (ssname allPlines j)))
      (if (point-in-poly? plineEnt ptList)
        (ssadd (vlax-vla-object->ename plineEnt) resultList)
      )
      (setq j (1+ j))
    )
    (setq i (1+ i))
  )

  ;; Deselect hatches if requested
  (if (eq deselOpt "Yes")
    (command "_.SELECT" "_Remove" ssHatch "")
  )

  ;; Select found plines
  (if (> (sslength resultList) 0)
    (progn
      (sssetfirst nil resultList)
      (prompt (strcat "\nFound " (itoa (sslength resultList)) " covering polylines."))
    )
    (prompt "\nNo matching polylines found.")
  )
  (princ)
)