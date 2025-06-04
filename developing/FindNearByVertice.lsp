(defun c:FindNearbyVerts (/ baseSel baseCopy baseEnt basePts allObjs ent nearbyObjs tol countAll proceed keepBase)
  (defun get-pline-verts (ent / lst entData typ)
    (setq lst '())
    (setq entData (entget ent))
    (setq typ (cdr (assoc 0 entData)))
    (cond
      ((= typ "LINE")
       (setq lst (list (cdr (assoc 10 entData)) (cdr (assoc 11 entData)))))
      ((= typ "LWPOLYLINE")
       (foreach dxf entData
         (if (= (car dxf) 10)
           (setq lst (cons (cdr dxf) lst)))))
    )
    (reverse lst)
  )

  ;; Select multiple base entities
  (prompt "\nSelect LINEs or POLYLINEs as base:")
  (setq baseSel (ssget '((0 . "LINE,LWPOLYLINE"))))
  (if (null baseSel)
    (progn (prompt "\nNo valid base objects selected.") (exit))
  )

  ;; Copy baseSel for later decision
  (setq baseCopy (ssadd))
  (repeat (sslength baseSel)
    (setq baseEnt (ssname baseSel 0))
    (setq baseSel (ssdel baseEnt baseSel))
    (ssadd baseEnt baseCopy)
  )

  ;; Tolerance distance
  (setq tol 0.1)

  ;; Collect all vertices from base objects
  (setq basePts '())
  (repeat (sslength baseCopy)
    (setq baseEnt (ssname baseCopy 0))
    (setq baseCopy (ssdel baseEnt baseCopy))
    (setq basePts (append basePts (get-pline-verts baseEnt)))
  )

  ;; Get all LINE and LWPOLYLINE objects
  (setq allObjs (ssget "_X" '((0 . "LINE,LWPOLYLINE"))))
  (if (null allObjs)
    (progn (prompt "\nNo lines or polylines found in drawing.") (exit))
  )

  ;; Warn if too many objects
  (setq countAll (sslength allObjs))
  (if (> countAll 1000)
    (progn
      (initget "Yes No")
      (setq proceed (getkword (strcat "\nWARNING: " (itoa countAll) " objects found. Proceed? [Yes/No] <No>: ")))
      (if (/= proceed "Yes")
        (progn (prompt "\nCommand canceled.") (exit))
      )
    )
  )

  ;; Loop to find nearby objects
  (setq nearbyObjs (ssadd))
  (repeat (sslength allObjs)
    (setq ent (ssname allObjs 0))
    (setq allObjs (ssdel ent allObjs))
    (if (not (ssmemb ent baseCopy)) ; avoid comparing with base
      (progn
        (setq pts (get-pline-verts ent))
        (if (vl-some
              '(lambda (pt1)
                 (vl-some
                   '(lambda (pt2)
                      (< (distance pt1 pt2) tol)
                    ) basePts
                 )
               ) pts
            )
          (ssadd ent nearbyObjs)
        )
      )
    )
  )

  ;; Ask if user wants to keep base selection
  (initget "Yes No")
  (setq keepBase (getkword "\nKeep base objects selected? [Yes/No] <No>: "))
  (if (not keepBase) (setq keepBase "No"))

  ;; Final selection
  (if (> (sslength nearbyObjs) 0)
    (progn
      (prompt (strcat "\nFound " (itoa (sslength nearbyObjs)) " nearby object(s)."))
      (if (= keepBase "Yes")
        (progn
          (repeat (sslength baseCopy)
            (setq ent (ssname baseCopy 0))
            (setq baseCopy (ssdel ent baseCopy))
            (ssadd ent nearbyObjs)
          )
        )
      )
      (sssetfirst nil nearbyObjs)
    )
    (prompt "\nNo nearby objects found.")
  )

  (princ)
)