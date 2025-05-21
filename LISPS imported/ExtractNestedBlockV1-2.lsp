;; -----------------------------=={ ExtractNestedBlock.lsp }==----------------------------- ;;
;;                                                                                         ;;
;;  Author   : Lee Mac                                                                     ;;
;;  Website  : www.lee-mac.com                                                             ;;
;;  Version  : 1.2                                                                          ;;
;;  Updated  : 19-May-2020                                                                 ;;
;;                                                                                         ;;
;;  Description:                                                                            ;;
;;  This routine extracts a selected nested block (inserted within another block)          ;;
;;  and creates top-level block references at the same location, rotation, and scale.      ;;
;;                                                                                         ;;
;;  Notes:                                                                                  ;;
;;  - Ignores blocks nested in xrefs or dynamic blocks.                                     ;;
;;  - Skips non-uniformly scaled references.                                                ;;
;;  - Deletes the nested instance after extraction.                                         ;;
;; --------------------------------------------------------------------------------------- ;;

(defun c:enb ( / sel )
  ;; Main command function to extract nested block
  (while
    ;; Prompt for nested block selection
    (progn
      (setvar 'errno 0)
      (setq sel (nentselp "\nSelect nested block: "))
      (cond
        ;; User missed selection
        ((= 7 (getvar 'errno))
         (princ "\nMissed, try again.")
        )
        ;; Nothing selected (exit loop)
        ((null sel) nil)

        ;; Not a nested block reference (missing parent)
        ((null (cadr (setq sel (last sel))))
         (princ "\nObject is not a nested block.")
        )

        ;; Block is on a locked layer (ignore)
        ((= 4 (logand 4 (cdr (assoc 70 (tblsearch "layer" (cdr (assoc 8 (entget (car sel)))))))))
         (princ "\nThe selected block is on a locked layer.")
        )

        ;; Block is nested inside an XREF (not supported)
        ((vl-some 'enb:isxref (cdr sel))
         (princ "\nThis program is not compatible with blocks nested within xrefs - sorry.")
        )

        ;; Block is nested inside a dynamic block (not supported)
        ((vl-some 'enb:isdynamic (cdr sel))
         (princ "\nThis program is not compatible with blocks nested within dynamic blocks - sorry.")
        )
      )
    )
  )
  ;; Proceed if a valid selection was made
  (if sel
    (progn
      ;; Get all references of the nested block
      (foreach lst (enb:getreferences (cdr (assoc 2 (entget (cadr sel)))))
        ;; Apply transformation to each reference (deep nested)
        (apply
         '(lambda (mat vec / obj)
            ;; Accumulate transformation matrices up the nesting chain
            (foreach ent (cdr (reverse lst))
              (apply
                (function
                  (lambda (m v)
                    (setq vec (mapcar '+ vec (mxv mat v))
                          mat (mxm mat m)
                    )
                  )
                )
                (refgeom ent)
              )
            )
            ;; Copy the selected nested block to target location
            (if
              (vl-catch-all-error-p
                (vl-catch-all-apply 'vla-transformby
                  (list (setq obj (enb:copy (car sel) (last lst)))
                    (vlax-tmatrix
                      (append
                        (mapcar '(lambda (m v) (append m (list v))) mat vec)
                        '((0.0 0.0 0.0 1.0))
                      )
                    )
                  )
                )
              )
              ;; If transform fails, delete the copy
              (vla-delete obj)
            )
          )
          (refgeom (last lst))
        )
      )
      ;; Delete the original nested instance from the parent block
      (vla-delete (vlax-ename->vla-object (car sel)))
      ;; Regenerate viewport to reflect changes
      (vla-regen (LM:acdoc) acactiveviewport)
    )
  )
  (princ)
)

;; -------------------------=={ Helper Functions }==------------------------- ;;

(defun enb:copy (ent par)
  ;; Copies a nested block into the parent context (paper/model space)
  (eval
    (list 'defun 'enb:copy '(ent par)
      (list 'car
        (list 'vlax-invoke (LM:acdoc) ''copyobjects
          '(list (vlax-ename->vla-object ent))
          (if (vlax-method-applicable-p (LM:acdoc) 'objectidtoobject32)
            (list 'vla-objectidtoobject32 (LM:acdoc) '(vla-get-ownerid32 (vlax-ename->vla-object par)))
            (list 'vla-objectidtoobject   (LM:acdoc) '(vla-get-ownerid   (vlax-ename->vla-object par)))
          )
        )
      )
    )
  )
  (enb:copy ent par)
)


(defun enb:getreferences (blk / ent enx lst)
  ;; Recursively collects all reference paths of the specified block name.
  ;; Each reference path is a list of entities from the top-level down to the nested block.

  (if (setq ent (tblobjname "block" blk)) ; Get the entity name of the block definition
    (foreach dxf (entget (cdr (assoc 330 (entget ent)))) ; Loop through the owners of the block definition
      ;; DXF group 330 holds the owner handle of the block. This could be multiple in some cases.

      (if
        (and
          (= 331 (car dxf))                 ; Check if this is a hard pointer reference (group 331)
          (setq ent (cdr dxf))              ; Set ent to the referenced entity
          (setq enx (entget ent))           ; Get the entity data of this reference
          ;; Get the block reference's owner (usually a parent block definition)
          (setq enx (entget (cdr (assoc 330 (reverse enx)))))
        )

        ;; At this point, enx is the DXF data of the parent block reference

        (if (wcmatch (strcase (setq blk (cdr (assoc 2 enx)))) "`**_SPACE")
          ;; If the block name matches *MODEL_SPACE or *PAPER_SPACE (top-level usage)
          (setq lst (cons (list ent) lst)) ; Store this reference as a new path (single-level)

          ;; Else, it's nested inside another block - recurse deeper
          (setq lst
            (append
              ;; For each path found in recursive call,
              ;; add current ent at the beginning (cons) to build the path
              (mapcar '(lambda (l) (cons ent l)) (enb:getreferences blk))
              lst
            )
          )
        )
      )
    )
  )
  lst ; Return the list of reference paths
)

(defun enb:isxref (ent)
  ;; Returns T if the block is an XREF
  (= 4 (logand 4 (cdr (assoc 70 (tblsearch "block" (cdr (assoc 2 (entget ent))))))))
)

(defun enb:isdynamic (ent / obj)
  ;; Returns T if the block is dynamic
  (and (vlax-property-available-p (setq obj (vlax-ename->vla-object ent)) 'isdynamicblock)
       (= :vlax-true (vla-get-isdynamicblock obj))
  )
)

(defun refgeom (ent / ang enx mat ocs)
  ;; Returns a list containing:
  ;; 1. A 3x3 transformation matrix (scale, rotation, orientation).
  ;; 2. A displacement vector (from block origin to insertion point in parent context).

  ;; Get the DXF data for the block reference
  (setq enx (entget ent)
        ang (cdr (assoc 050 enx))  ;; Rotation angle (in radians)
        ocs (cdr (assoc 210 enx))  ;; Object Coordinate System (OCS) vector
  )

  (list
    ;; ---------------------------- 1. TRANSFORMATION MATRIX ----------------------------
    ;; This matrix transforms the local coordinate system of the block
    ;; by applying scale, then rotation, then orientation (OCS).

    (setq mat
      (mxm
        ;; Step 3: OCS Transformation - transform local axes from OCS to WCS
        ;; Convert unit vectors from local to global using (trans)
        (mapcar '(lambda (v) (trans v 0 ocs t))
          '((1.0 0.0 0.0)  ; X-axis unit vector
            (0.0 1.0 0.0)  ; Y-axis unit vector
            (0.0 0.0 1.0)  ; Z-axis unit vector
          )
        )

        ;; Result of multiplying:
        ;; Step 1: Rotation matrix
        (mxm
          (list
            (list (cos ang) (- (sin ang)) 0.0) ; Rotates about Z axis
            (list (sin ang) (cos ang)     0.0)
            '(0.0 0.0 1.0)
          )

          ;; Step 2: Scaling matrix
          (list
            (list (cdr (assoc 41 enx)) 0.0 0.0) ; X scale
            (list 0.0 (cdr (assoc 42 enx)) 0.0) ; Y scale
            (list 0.0 0.0 (cdr (assoc 43 enx))) ; Z scale
          )
        )
      )
    )

    ;; ---------------------------- 2. TRANSLATION VECTOR ----------------------------
    ;; This vector adjusts the transformed origin of the block definition
    ;; to match the insertion point in the parent block or model space.

    ;; Calculate:
    ;; Insertion Point (in WCS) - (Matrix * Base Point of Block Definition)

    ;; (assoc 10 enx) = block reference insertion point (in OCS)
    ;; (assoc 10 block-def) = base point of block definition (local origin)

    (mapcar '-
      ;; Translate insertion point of the block from OCS to WCS
      (trans (cdr (assoc 10 enx)) ocs 0)

      ;; Apply the transformation matrix to the base point of the block definition
      (mxv mat
        (cdr (assoc 10
          (tblsearch "block" (cdr (assoc 2 enx))) ; Get the block definition for this block name
        ))
      )
    )
  )
)

;; RefGeom (gile)


;; Matrix Transpose - Transforms rows to columns
(defun trp (m)
  (apply 'mapcar (cons 'list m))
)

;; Matrix x Matrix multiplication
(defun mxm (m n)
  ((lambda (a) (mapcar '(lambda (r) (mxv a r)) m)) (trp n))
)

;; Matrix x Vector multiplication
(defun mxv (m v)
  (mapcar '(lambda (r) (apply '+ (mapcar '* r v))) m)
)

;; Returns the VLA Active Document Object
(defun LM:acdoc nil
  (eval (list 'defun 'LM:acdoc 'nil (vla-get-activedocument (vlax-get-acad-object))))
  (LM:acdoc)
)

;; -------------------------=={ Program Startup Message }==------------------------- ;;

(vl-load-com)
(princ
  (strcat
    "\n:: ExtractNestedBlock.lsp | Version 1.2 | \\U+00A9 Lee Mac "
    (menucmd "m=$(edtime,0,yyyy)")
    " www.lee-mac.com ::"
    "\n:: Type \"ENB\" to Invoke ::"
  )
)
(princ)

;; -------------------------=={ End of File }==------------------------- ;;