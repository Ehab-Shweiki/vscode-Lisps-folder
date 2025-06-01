(defun c:enb ( / sel blockpath ent target answer indent )

  ;; Ask user to select a nested block
  (while
    (progn
      (setvar 'errno 0)
      (setq sel (nentselp "\nSelect nested block: "))
      (cond
        ((= 7 (getvar 'errno)) (princ "\nMissed, try again."))
        ((null sel) nil) ; User canceled
        ((null (cadr (setq sel (last sel))))
         (princ "\nObject is not a nested block."))
        ;; Check if selected entity is on a locked layer
        ((= 4 (logand 4 (cdr (assoc 70 (tblsearch "layer"
                                        (cdr (assoc 8 (entget (car sel)))))))))
         (princ "\nThe selected block is on a locked layer."))
        ;; Disallow blocks from xrefs
        ((vl-some 'enb:isxref (cdr sel))
         (princ "\nThis program is not compatible with blocks nested within xrefs."))
        ;; Disallow dynamic block containers
        ((vl-some 'enb:isdynamic (cdr sel))
         (princ "\nThis program is not compatible with blocks nested within dynamic blocks."))
      )
    )
  )

  ;; Continue if a valid selection
  (if sel
    (progn
      (setq blockpath nil)

      ;; Step 1: Add all container blocks from outer to inner
      (foreach ent (cdr sel)
        (if (setq ent (entget ent))
          (setq blockpath (append blockpath (list (cdr (assoc 2 ent)))))
        )
      )

      ;; Step 2: Add the actual selected block (car sel), if it's an INSERT
      (if (and (setq ent (entget (car sel)))
               (equal (cdr (assoc 0 ent)) "INSERT")
               (assoc 2 ent))
        (setq blockpath (append blockpath (list (cdr (assoc 2 ent)))))
      )

      ;; Step 3: Print the path in stepped format
      (princ "\nNested block path:\n")
      (setq indent "")
      (foreach bname blockpath
        (princ (strcat indent "-> " bname "\n"))
        (setq indent (strcat indent "   "))
      )

      ;; Step 4: Prompt to open the selected block in BEDIT
	  (if (and blockpath (setq target (last blockpath)))
	    (progn
	    (setq answer (strcase (getstring (strcat "\nOpen  block   '" target "' in BEDIT? [Y/N] <Y>: "))))
	    (if (or (= answer "") (= answer "Y"))
	      (command "_.BEDIT" target)
	    )
	  )
	  )
      
      
    )
  )

  (princ)
)

;; --- Helper Functions ---

(defun enb:isxref ( ent )
  ;; Returns T if the block is from an XREF
  (= 4 (logand 4
        (cdr (assoc 70 (tblsearch "block" (cdr (assoc 2 (entget ent))))))))
)

(defun enb:isdynamic ( ent / obj )
  ;; Returns T if the block is dynamic
  (and (vlax-property-available-p
         (setq obj (vlax-ename->vla-object ent)) 'isdynamicblock)
       (= :vlax-true (vla-get-isdynamicblock obj)))
)

;; Load COM interface
(vl-load-com)
(princ "\n:: ENB loaded. Type ENB to select and edit a nested block ::")
(princ)