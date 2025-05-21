;;---------------------=={ Add LWPolyline Vertex }==--------------------;;
;;                                                                      ;;
;;  Adds a new vertex to an LWPolyline at a point specified by the      ;;
;;  user; compatible with LWPolylines at any orientation, with varying  ;;
;;  width and arc segments.                                             ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright © 2012  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.0    -    2012-12-17                                      ;;
;;                                                                      ;;
;;  - First release.                                                    ;;
;;----------------------------------------------------------------------;;
;;  Version 1.1    -    2022-08-29                                      ;;
;;                                                                      ;;
;;  - Fixed bug where adding a vertex to the final arc segment of a     ;;
;;    closed polyline caused the final segment to become linear.        ;;
;;----------------------------------------------------------------------;;

(defun c:apv ( / *error* a b e h l n p r v w x z )

    (defun *error* ( msg )
        (LM:endundo (LM:acdoc))
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )
    
    (while
        (progn (setq p (getpoint "\nSpecify point for new vertex: "))
            (cond
                (   (null p)
                    nil
                )
                (   (null (setq e (nentselp p)))
                    (princ "\nThe specified point does not lie on a polyline.")
                )
                (   (= 4 (length e))
                    (princ "\nThis program is not compatible with nested objects.")
                )
                (   (/= "LWPOLYLINE" (cdr (assoc 0 (entget (setq e (car e))))))
                    (princ "\nThe specified point does not lie on a polyline.")
                )
            )
        )
    )
    (if (and p e
            (setq p (vlax-curve-getclosestpointto e (trans p 1 0))
                  n (vlax-curve-getparamatpoint e p)
            )
        )
        (if (not (equal n (fix n) 1e-8))
            (progn
                (setq e (entget e)
                      h (reverse (member (assoc 39 e) (reverse e)))
                      v (assoc 90 h)
                      l (LM:LWVertices e)
                      z (assoc 210 e)
                )
                (repeat (fix n)
                    (setq a (cons (car l) a)
                          l (cdr l)
                    )
                )
                (setq x (car l)
                      r (- n (fix n))
                      w (cdr (assoc 40 x))
                      w (+ w (* r (- (cdr (assoc 41 x)) w)))
                      b (atan (cdr (assoc 42 x)))
                )
                (LM:startundo (LM:acdoc))
                (entmod
                    (append
                        (subst (cons 90 (1+ (cdr v))) v h)
                        (apply 'append (reverse a))
                        (list
                            (assoc 10 x)
                            (assoc 40 x)
                            (cons  41 w)
                            (cons  42 (tan (* r b)))
                            (cons  10 (trans p 0 (cdr z)))
                            (cons  40 w)
                            (assoc 41 x)
                            (cons  42 (tan (* (- 1.0 r) b)))
                        )
                        (apply 'append (cdr l))
                        (list z)
                    )
                )
                (LM:endundo (LM:acdoc))
            )
        )
    )
    (princ)
)

;; Tangent  -  Lee Mac
;; Args: x - real

(defun tan ( x )
    (if (not (equal 0.0 (cos x) 1e-10))
        (/ (sin x) (cos x))
    )
)

;; LW Vertices  -  Lee Mac
;; Returns a list of lists in which each sublist describes
;; the position, starting width, ending width and bulge of the
;; vertex of a supplied LWPolyline

(defun LM:LWVertices ( e )
    (if (setq e (member (assoc 10 e) e))
        (cons
            (list
                (assoc 10 e)
                (assoc 40 e)
                (assoc 41 e)
                (assoc 42 e)
            )
            (LM:LWVertices (cdr e))
        )
    )
)

;; Start Undo  -  Lee Mac
;; Opens an Undo Group.

(defun LM:startundo ( doc )
    (LM:endundo doc)
    (vla-startundomark doc)
)

;; End Undo  -  Lee Mac
;; Closes an Undo Group.

(defun LM:endundo ( doc )
    (while (= 8 (logand 8 (getvar 'undoctl)))
        (vla-endundomark doc)
    )
)

;; Active Document  -  Lee Mac
;; Returns the VLA Active Document Object

(defun LM:acdoc nil
    (eval (list 'defun 'LM:acdoc 'nil (vla-get-activedocument (vlax-get-acad-object))))
    (LM:acdoc)
)

;;----------------------------------------------------------------------;;

(vl-load-com)
(princ
    (strcat
        "\n:: AddLWPolylineVertex.lsp | Version 1.1 | \\U+00A9 Lee Mac "
        ((lambda ( y ) (if (= y (menucmd "m=$(edtime,0,yyyy)")) y (strcat y "-" (menucmd "m=$(edtime,0,yyyy)")))) "2012")
        " www.lee-mac.com ::"
        "\n:: Type \"apv\" to Invoke ::"
    )
)
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;