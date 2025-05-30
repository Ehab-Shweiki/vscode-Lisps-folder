;;-----------------------=={ Dimension Overlap }==----------------------;;
;;                                                                      ;;
;;  This program will automatically detect overlapping dimensions in    ;;
;;  all layouts and all blocks in a drawing, and will move such         ;;
;;  dimensions to a separate layer specified in the code.               ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright � 2015  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.0    -    2015-12-12                                      ;;
;;                                                                      ;;
;;  - First release.                                                    ;;
;;----------------------------------------------------------------------;;
;;  Version 1.1    -    2015-12-31                                      ;;
;;                                                                      ;;
;;  - dimoverlap:processblock function rewritten to detect dimensions   ;;
;;    which overlap on both sides.                                      ;;
;;----------------------------------------------------------------------;;
;;  Version 1.2    -    2017-04-25                                      ;;
;;                                                                      ;;
;;  - Added parameter to allow the user to alter the tolerance for      ;;
;;    dimension comparison.                                             ;;
;;  - Added list of layer properties to be applied to layer assigned    ;;
;;    to overlapping dimensions.                                        ;;
;;----------------------------------------------------------------------;;
;;  Version 1.3    -    2024-10-21                                      ;;
;;                                                                      ;;
;;  - Modified program to prompt user for dimension selection, or       ;;
;;    process all dimensions (including nested) on Enter.               ;;
;;----------------------------------------------------------------------;;

(defun c:dimoverlap ( / *error* sel )

    (defun *error* ( msg )
        (setvar 'nomutt 0)
        (LM:endundo (LM:acdoc))
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )

    (LM:startundo (LM:acdoc))
    (princ "\nSelect dimensions to check <All>: ")
    (setvar 'nomutt 1)
    (setq sel
        (vl-catch-all-apply 'ssget
           '(
                (
                    (0 . "DIMENSION")
                    (-4 . "<OR")
                        (70 . 000)
                        (70 . 001)
                        (70 . 032)
                        (70 . 033)
                        (70 . 064)
                        (70 . 065)
                        (70 . 096)
                        (70 . 097)
                        (70 . 128)
                        (70 . 129)
                        (70 . 160)
                        (70 . 161)
                        (70 . 192)
                        (70 . 193)
                        (70 . 224)
                        (70 . 225)
                    (-4 . "OR>")
                )
            )
        )
    )
    (setvar 'nomutt 0)

    (if (vl-catch-all-error-p sel)
        (princ "\n*Cancel*")
        (dimoverlap:main sel)
    )
    (LM:endundo (LM:acdoc))
    (princ)
)    

(defun dimoverlap:main ( sel / cn1 cn2 ent lay tol )
    (setq
        
;;----------------------------------------------------------------------;;
;;                          Program Parameters                          ;;
;;----------------------------------------------------------------------;;

        ;; Tolerance within which dimensions are deemed to overlap
        tol 0.01

        ;; Layer properties for overlapping dimensions
        lay
       '(
            (002 . "DIMOVERLAP") ;; Layer Name
            (062 .  1)           ;; Layer Colour (1-255)
            (006 . "Continuous") ;; Layer Linetype (Must be loaded in drawing)
            (370 . -3)           ;; Layer Lineweight (Default = -3, else lineweight*100 e.g. 2.11 = 211)
        )

;;----------------------------------------------------------------------;;

        cn1 0
        cn2 0
    )
    (if (= 'pickset (type sel))
        (setq cn1 (dimoverlap:processselection sel (cdr (assoc 2 lay)) tol))
        (vlax-for blk (vla-get-blocks (LM:acdoc))
            (if (= :vlax-false (vla-get-isxref blk))
                (if (= :vlax-true (vla-get-islayout blk))
                    (setq cn1 (+ cn1 (dimoverlap:processblock blk (cdr (assoc 2 lay)) tol)))
                    (setq cn2 (+ cn2 (dimoverlap:processblock blk (cdr (assoc 2 lay)) tol)))
                )
            )
        )
    )
    (if (< 0 cn2)
        (vla-regen (LM:acdoc) acallviewports)
    )
    (if (< 0 (+ cn1 cn2))
        (progn
            (if (setq ent (tblobjname "layer" (cdr (assoc 2 lay))))
                (entmod
                    (vl-list*
                        (cons -1 ent)
                       '(000 . "LAYER")
                       '(100 . "AcDbSymbolTableRecord")
                       '(100 . "AcDbLayerTableRecord")
                       '(070 . 0)
                        lay
                    )
                )
            )
            (princ
                (strcat
                    "\n"
                    (itoa (+ cn1 cn2))
                    " overlapping dimension"
                    (if (= 1 (+ cn1 cn2)) " was" "s were")
                    " found and moved to the \"" (cdr (assoc 2 lay)) "\" layer."
                    (if (< 0 cn2)
                        (strcat
                            "\n"
                            (itoa cn2)
                            (if (= 1 cn2) " was in a block." " were in blocks.")
                        )
                        ""
                    )
                )
            )
        )
        (princ "\nNo overlapping dimensions were found.")
    )
    (princ)
)

(defun dimoverlap:processselection ( sel lay tol / itm lst )
    (repeat (setq idx (sslength sel))
        (if (setq itm (dimoverlap:getdimdata (entget (ssname sel (setq idx (1- idx))))))
            (setq lst (cons itm lst))
        )
    )
    (dimoverlap:checkoverlap lst lay tol)
)

(defun dimoverlap:processblock ( blk lay tol / itm lst )
    (vlax-for obj blk
        (if (and (wcmatch  (vla-get-objectname obj) "AcDbRotatedDimension,AcDbAlignedDimension")
                 (setq itm (dimoverlap:getdimdata (entget (vlax-vla-object->ename obj))))
            )
            (setq lst (cons itm lst))
        )
    )
    (dimoverlap:checkoverlap lst lay tol)
)

(defun dimoverlap:getdimdata ( enx / g10 g13 g14 int ocs vec )
    (setq ocs (cdr (assoc 210 enx))
          g10 (trans (cdr (assoc 10 enx)) 0 ocs)
          g13 (trans (cdr (assoc 13 enx)) 0 ocs)
          g14 (trans (cdr (assoc 14 enx)) 0 ocs)
    )
    (cond
        (   (equal g10 g14 1e-8)
            nil
        )
        (   (setq vec (mapcar '- g10 g14)
                  int (inters
                          g10 (mapcar '+ g10 (list (- (cadr vec)) (car vec) 0.0))
                          g13 (mapcar '+ g13 vec)
                          nil
                      )
            )
            (list g10 int enx)
        )
    )
)

(defun dimoverlap:checkoverlap ( lst lay tol / cnt dm1 dm2 tmp )
    (setq cnt 0)
    (while (setq dm1 (car lst))
        (setq lst (cdr lst)
              tmp lst
        )
        (while
            (not
                (or (null (setq dm2 (car tmp)))
                    (vl-some
                       '(lambda ( a b c d )
                            (cond
                                (   (equal a c tol)
                                    (cond
                                        (   (dimoverlap:online-p b c d tol)
                                            (dimoverlap:flagdim (caddr dm2) lay)
                                            (setq cnt (1+ cnt))
                                        )
                                        (   (dimoverlap:online-p d a b tol)
                                            (dimoverlap:flagdim (caddr dm1) lay)
                                            (setq cnt (1+ cnt))
                                        )
                                    )
                                )
                                (   (and (dimoverlap:online-p c a b tol)
                                         (dimoverlap:online-p a c d tol)
                                    )
                                    (foreach dim (list dm1 dm2)
                                        (dimoverlap:flagdim (caddr dim) lay)
                                        (setq cnt (1+ cnt))
                                    )
                                )
                            )
                        )
                        (list (car  dm1) (car  dm1) (cadr dm1) (cadr dm1))
                        (list (cadr dm1) (cadr dm1) (car  dm1) (car  dm1))
                        (list (car  dm2) (cadr dm2) (car  dm2) (cadr dm2))
                        (list (cadr dm2) (car  dm2) (cadr dm2) (car  dm2))
                    )
                )
            )
            (setq tmp (cdr tmp))
        )
    )
    cnt
)

(defun dimoverlap:online-p ( p a b f )
    (and (not (equal a p f))
         (not (equal b p f))
         (equal (distance a b) (+ (distance a p) (distance b p)) f)
    )
)

(defun dimoverlap:flagdim ( x l )
    (entmod (subst (cons 8 l) (assoc 8 x) x))
)

;; Start Undo  -  Lee Mac
;; Opens an Undo Group.

(if (null LM:startundo)
    (defun LM:startundo ( doc )
        (LM:endundo doc)
        (vla-startundomark doc)
    )
)

;; End Undo  -  Lee Mac
;; Closes an Undo Group.

(if (null LM:endundo)
    (defun LM:endundo ( doc )
        (while (= 8 (logand 8 (getvar 'undoctl)))
            (vla-endundomark doc)
        )
    )
)

;; Active Document  -  Lee Mac
;; Returns the VLA Active Document Object

(if (null LM:acdoc)
    (defun LM:acdoc nil
        (eval (list 'defun 'LM:acdoc 'nil (vla-get-activedocument (vlax-get-acad-object))))
        (LM:acdoc)
    )
)

;;----------------------------------------------------------------------;;

(vl-load-com)
(princ
    (strcat
        "\n:: DimensionOverlap.lsp | Version 1.3 | \\U+00A9 Lee Mac "
        (menucmd "m=$(edtime,0,yyyy)")
        " www.lee-mac.com ::"
        "\n:: Type \"dimoverlap\" to Invoke ::"
    )
)
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;