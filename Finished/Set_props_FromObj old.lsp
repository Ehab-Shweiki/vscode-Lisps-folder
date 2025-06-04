(defun c:Set_Props_FromObj ( / ent entData lay col ltype)
  (prompt "\nSelect an object to get its properties: ")
  (setq ent (car (entsel)))
  (if ent
    (progn
      (setq entData (entget ent))
      ;; Get Layer
      (setq lay (cdr (assoc 8 entData)))
      (setvar "clayer" lay)
      
      ;; Get Color (DXF 62)
      (setq col (cdr (assoc 62 entData)))
      (if col
        (setvar "cecolor" (itoa col))
        (setvar "cecolor" "BYLAYER")
      )
      
      ;; Get Linetype (DXF 6)
      (setq ltype (cdr (assoc 6 entData)))
      (if ltype
        (setvar "celtype" ltype)
        (setvar "celtype" "BYLAYER")
      )
      
      (prompt (strcat "\nProperties set: Layer=" lay ", Color=" (itoa col) ", Linetype=" ltype))
    )
    (prompt "\nNo object selected.")
  )
  (princ)
)