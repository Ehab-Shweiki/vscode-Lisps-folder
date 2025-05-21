(defun dimtype-match (val code) 
  ;; Checks if bit-flag (e.g., 163 for diameter) is set
  (= (logand val code) code)
)

(defun FilterSelection_Type (key key2 color_passed / colorMap filterDimType filter ss i ent entType entList newSS dimtype addIt dimTypeMap colorKey color_passed)


  ;; Color shortcuts map
  (setq colorMap
    '(("L" . 256); ByLayer
      ("0" . 0)  ; ByBlock
      ("r" . 1)  ; Red
      ("y" . 2)  ; Yellow
      ("g" . 3)  ; Green
      ("c" . 4)  ; Cyan
      ("b" . 5)  ; Blue
      ("m" . 6)  ; Magenta
      ("w" . 7)) ; White
  )

  ;; If key pre-set (for shortcuts), use it; otherwise ask
  (if (not key) 
    (progn
      (initget "L B D T C")
      (setq key (getkword "\nFilter by [L:Lines / B:Blocks / D:Dimensions / T:Text / C:Color only] <L>: "))
      (if (not key) (setq key "L"))
    )
  )

  ;; Sub-filter
  (if (not key2)
    (setq key2
      (cond
        ((= key "L")
          (progn
            (initget "Line Polyline Circle Arc")
            (getkword "\nRefine [Line / Polyline / Circle / Arc] <All>: ")))
        ((= key "D")
          (progn
            (initget "Rotated Aligned Angular Arc Ordinate Diameter Radial Jogged")
            (getkword "\nRefine [Rotated / Aligned / Angular / Arc / Ordinate / Diameter / Radial / Jogged] <All>: ")))
        ((= key "T")
          (progn
            (initget "TEXT MTEXT")
            (getkword "\nRefine text type [TEXT / MTEXT] <All>: ")))
        (T nil)
      )
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
    ((= key "B")
     (setq filter (list '(0 . "INSERT"))))
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
         '(("Rotated" . 32)
           ("Aligned" . 33)
           ("Angular" . 37)
           ("Arc" . 37)
           ("Ordinate" . 102)
           ("Diameter" . 163)
           ("Radial" . 164)
           ("Jogged" . 169)))
       (if (and key2 (/= key2 ""))
         (setq filterDimType (cdr (assoc key2 dimTypeMap)))
         (setq filterDimType nil)
       )
     ))
  )

  ;; Ask for color only if desired
  (if color_passed
    ;; color filtering
    (progn
      (initget 128)
      (setq colorKey (getstring "\nEnter color [l/0/r/y/g/c/b/m/w or ACI number] <All>: "))

      ;; Convert color key to ACI
      (if (and colorKey (/= colorKey ""))
        (progn
          (setq colorKey (strcase colorKey T))
        )
        (if (assoc colorKey colorMap)
          (setq colorCode (cdr (assoc colorKey colorMap)))
          (if (and colorKey (numberp (read colorKey)))
          (setq colorCode (read colorKey))
          )
        )
      )
    )
    ;; No color filtering
    (setq colorCode nil)
  )
  


  

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
  (if (and color_passed (not colorKey))
    (setq filter (cons (cons 62 colorCode) filter)))

  ;; Get selection
  (setq ss (ssget filter))

  ;; Process results
  (if ss
    (progn
      (prompt (strcat "\nFound " (itoa (sslength ss)) " object(s):"))
      (setq i 0 entList '())
      (while (< i (sslength ss))
        (setq ent (ssname ss i)
              entType (cdr (assoc 0 (entget ent)))
              addIt T)

        ;; Filter dimension by bitflag
        (if (and (= key "D") filterDimType)
          (progn
            (setq dimtype (cdr (assoc 70 (entget ent))))
            (if (not (dimtype-match dimtype filterDimType))
              (setq addIt nil))))

        (if addIt
          (setq entList (cons ent entList)))
        (setq i (1+ i))
      )

      ;; Highlight
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

;; Shortcut variants with predefined filters
(defun c:FS () (FilterSelection_Type nil nil T))
(defun c:FSB () (FilterSelection_Type "B" "" nil))
(defun c:FSL () (FilterSelection_Type "L" "" nil))
(defun c:FST () (FilterSelection_Type "T" "" nil))
(defun c:FSD () (FilterSelection_Type "D" "" nil))

; (defun c:FS () (c:FilterSelection_Type))

(princ "\nType FS to run the command FilterSelection_Type.")
(princ "\nType FSB,FSL,FST,FSD to run the specified command FilterSelection_Type.")
(princ)

