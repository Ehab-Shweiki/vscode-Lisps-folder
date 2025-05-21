(defun c:get-end-vertex ( / ent obj end-pt)
  ;; Prompt user to select a polyline
  (setq ent (car (entsel "\nSelect a polyline: ")))
  
  ;; Check if the selected entity is a polyline
  (if (and ent (= (cdr (assoc 0 (entget ent))) "LWPOLYLINE"))
    (progn
      ;; Convert the entity to a VLA object
      (setq obj (vlax-ename->vla-object ent))
      
      ;; Get the last point of the polyline (endpoint)
      (setq end-pt (vlax-curve-getpointatparam obj (vlax-curve-getendparam obj)))
      
      ;; Display the endpoint
      (princ (strcat "\nEnd point of polyline: " (rtos (car end-pt) 2 3) ", " (rtos (cadr end-pt) 2 3)))
    )
    (princ "\nThe selected entity is not a polyline.")
  )
  (princ)
)

(defun c:get-first-last-vertices ( / ent obj pt1 pt2)
  ;; Prompt user to select a polyline
  (setq ent (car (entsel "\nSelect a polyline: ")))
  
  ;; Check if the selected entity is a polyline
  (if (and ent (= (cdr (assoc 0 (entget ent))) "LWPOLYLINE"))
    (progn
      ;; Convert the entity to a VLA object
      (setq obj (vlax-ename->vla-object ent))
      
      ;; Get the first point of the polyline
      (setq pt1 (vlax-curve-getpointatparam obj 0))
      
      ;; Get the last point of the polyline (endpoint)
      (setq pt2 (vlax-curve-getpointatparam obj (vlax-curve-getendparam obj)))
      
      ;; Display the first and last points
      (princ (strcat "\nFirst point of polyline (pt1): " 
                     (rtos (car pt1) 2 3) ", " (rtos (cadr pt1) 2 3)))
      )
      (princ (strcat "\nLast point of polyline (pt2): " 
                     (rtos (car pt2) 2 3) ", " (rtos (cadr pt2) 2 3)))
    )
    (princ "\nThe selected entity is not a polyline.")
  )
  (princ)
)


(defun c:get-first-second-vertex ( / ent obj obj_nam n pt1 pt2)
  ;; Prompt user to select a polyline
  (setq ent (car (entsel "\nSelect a polyline: ")))

  ;; Ensure the selected entity is a polyline
  (if (and ent
           (= (setq obj_nam (vla-get-objectname (vlax-ename->vla-object ent))) "AcDbPolyline"))
    (progn
      ;; Convert the entity to a VLA object
      (setq obj (vlax-ename->vla-object ent))

      ;; Get the number of vertices (vertices count)
      (setq n (vlax-curve-getnumvertices obj))

      ;; Get the first vertex coordinates
      (setq pt1 (vlax-curve-getpointatparam obj 0))

      ;; Get the second vertex coordinates (at parameter 1 if polyline has more than one segment)
      (if (> n 1)
        (setq pt2 (vlax-curve-getpointatparam obj 1))
        (princ "\nThe polyline does not have enough segments.")
      )

      ;; Display the first and second vertex coordinates
      (princ (strcat "\nFirst vertex: " (rtos (car pt1) 2 3) ", " (rtos (cadr pt1) 2 3)))
      (if pt2
        (princ (strcat "\nSecond vertex: " (rtos (car pt2) 2 3) ", " (rtos (cadr pt2) 2 3)))
      )
    )
    (princ "\nThe selected entity is not a polyline.")
  )
  (princ)
)