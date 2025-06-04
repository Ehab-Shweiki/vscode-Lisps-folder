(defun c:SelectCoveringPlines (/ ssHatch i hatchEnt hatchArea hatchBox
                                 allPlines j plineEnt plineArea plineBox
                                 tolerance resultList deselOpt)

  ;; --- Tolerance for area difference ---
  (setq tolerance 0.5) ;; Adjust as needed

  ;; --- Select HATCH objects ---
  (prompt "\nSelect hatch objects to find covering polylines:")
  (setq ssHatch (ssget '((0 . "HATCH"))))
  (if (not ssHatch)
    (progn (prompt "\nNo hatches selected.") (exit))
  )

  ;; --- Ask if user wants to deselect hatches ---
  (initget "Yes No")
  (setq deselOpt (getkword "\nDeselect hatches after selection? [Yes/No] <No>: "))
  (if (null deselOpt) (setq deselOpt "No")) ;; Default to No

  ;; Get all polylines in the drawing
  (setq allPlines (ssget "X" '((0 . "LWPOLYLINE"))))
  (if (not allPlines)
    (progn (prompt "\nNo polylines found in drawing.") (exit))
  )

  ;; Initialize result list
  (setq resultList (ssadd))

  ;; Loop through hatches
  (setq i 0)
  (while (< i (sslength ssHatch))
    (setq hatchEnt (vlax-ename->vla-object (ssname ssHatch i)))
    (setq hatchArea (vla-get-Area hatchEnt))
    (setq hatchBox (vla-getboundingbox hatchEnt 'minH 'maxH))

    ;; Loop through polylines
    (setq j 0)
    (while (< j (sslength allPlines))
      (setq plineEnt (vlax-ename->vla-object (ssname allPlines j)))
      (setq plineArea (vla-get-Area plineEnt))
      (setq plineBox (vla-getboundingbox plineEnt 'pminH 'pmaxH))

      ;; Compare area within tolerance
      (if (< (abs (- hatchArea plineArea)) tolerance)
        (ssadd (vlax-vla-object->ename plineEnt) resultList)
      )
      (setq j (1+ j))
    )
    (setq i (1+ i))
  )

  ;; Optionally deselect hatches
  (if (eq deselOpt "Yes")
    (command "_.SELECT" "_Remove" ssHatch "")
  )

  ;; Highlight result
  (if (> (sslength resultList) 0)
    (progn
      (sssetfirst nil resultList)
      (prompt (strcat "\nFound " (itoa (sslength resultList)) " covering polylines."))
    )
    (prompt "\nNo matching polylines found.")
  )
  (princ)
)

