(defun c:FilterSelection_Type ( / key key2 colorKey colorCode filterDimType
                                filter ss i ent entType entList newSS colorMap dimtype addIt dimTypeMap)

  ;; Color shortcuts map
  (setq colorMap
    '(("L" . 256); Layer
      ("r" . 1)  ; Red
      ("y" . 2)  ; Yellow
      ("g" . 3)  ; Green
      ("c" . 4)  ; Cyan
      ("b" . 5)  ; Blue
      ("m" . 6)  ; Magenta
      ("w" . 7)) ; White
  )

  ;; Ask for main filter
  (initget "L B D T C")
  (setq key (getkword "\nFilter by [L:Lines / B:Blocks / D:Dimensions / T:Text / C:Color only] <L>: "))
  (if (not key) (setq key "L"))

  ;; Sub-filter prompt
  (setq key2
    (cond
      ((= key "L")
        (progn
          (initget "Line Polyline Circle Arc")
          (getkword "\nRefine [Line / Polyline / Circle / Arc] <All>: ")))
      ((= key "D")
        (progn
          (initget "Aligned Linear Radial Diameter Ordinate Angular")
          (getkword "\nRefine [Aligned / Linear / Radial / Diameter / Ordinate / Angular] <All>: ")))
      ((= key "T")
        (progn
          (initget "TEXT MTEXT")
          (getkword "\nRefine text type [TEXT / MTEXT] <All>: ")))
      (T nil)
    )
  )

  ;; Build type filter
  (cond
    ((= key "L")
     (setq filter (list
       (cons 0
         (cond
           ((= key2 "Line") "LINE")
           ((= key2 "Polyline") "LWPOLYLINE,POLYLINE")
           ((= key2 "Circle") "CIRCLE")
           ((= key2 "Arc") "ARC")
           (T "LINE,LWPOLYLINE,POLYLINE,CIRCLE,ARC")
         )))))
    ((= key "B") (setq filter (list '(0 . "INSERT"))))
    ((= key "T")
     (setq filter (list
       (cons 0
         (cond
           ((= key2 "TEXT") "TEXT")
           ((= key2 "MTEXT") "MTEXT")
           (T "TEXT,MTEXT")
         )))))
    ((= key "D")
     (progn
       (setq filter (list '(0 . "DIMENSION")))
       (setq dimTypeMap
         '(("Aligned" . 0) ("Linear" . 1) ("Angular" . 2)
           ("Diameter" . 3) ("Radial" . 4) ("Ordinate" . 5)))
       (if (and key2 (/= key2 ""))
         (setq filterDimType (cdr (assoc key2 dimTypeMap)))
         (setq filterDimType nil)
       )
     ))
  )

  ;; Ask for color input
  (initget 128)
  (setq colorKey (getstring "\nEnter color [l/0/r/y/g/c/b/m/w or ACI number] <All>: "))

  ;; Convert color key to ACI
  (if (and colorKey (/= colorKey ""))
    (progn
      (setq colorKey (strcase colorKey T)) ; convert to lowercase
      (if (assoc colorKey colorMap)
        (setq colorCode (cdr (assoc colorKey colorMap)))
        (if (numberp (read colorKey))
          (setq colorCode (read colorKey))
          (prompt "\nInvalid color input - ignored.")
        )
      )
    )
  )

  ;; Add color filter if valid
  (if colorCode
    (setq filter (cons (cons 62 colorCode) filter)))

  ;; Get entities
  (setq ss (ssget filter))

  ;; If found, process
  (if ss
    (progn
      (prompt (strcat "\nFound " (itoa (sslength ss)) " object(s):"))
      (setq i 0 entList '())
      (while (< i (sslength ss))
        (setq ent (ssname ss i)
              entType (cdr (assoc 0 (entget ent)))
              addIt T)

        ;; Post-filter for dimension type
        (if (and (= key "D") filterDimType)
          (progn
            (setq dimtype (cdr (assoc 70 (entget ent))))
            (if (/= dimtype filterDimType)
              (setq addIt nil))))

        (if addIt
          (progn
            (setq entList (cons ent entList))))
        (setq i (1+ i))
      )

      ;; Highlight selection
      (sssetfirst nil (ssadd))
      (setq newSS (ssadd))
      (foreach e entList (ssadd e newSS))
      (sssetfirst nil newSS)
      (command "_.REGEN")
    )
    (prompt "\nNo matching objects found.")
  )

  (princ)
)

(defun c:FS () (c:FilterSelection_Type))

(princ "\nType FS to run the command FilterSelection_Type.")
(princ)