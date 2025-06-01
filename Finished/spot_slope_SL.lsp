(defun c:SL (/ entity pt1 pt2 height basept slope ang slope-text offset-point text-entity obj_nam polyline-entity)
  
  ;; Ask the user to pick a line or a polyline segment
  (setq entity (car (entsel "\nPick a line or a polyline segment: ")))

  ;; Get the object name of the selected entity
  (setq obj_nam (vla-get-objectname (vlax-ename->vla-object entity)))

  ;; Check if the entity is either a line or a polyline
  (if (not (or (= obj_nam "AcDbLine")
               (= obj_nam "AcDbPolyline")))

    (progn
      (princ "\nThe selected entity is neither a line nor a straight polyline segment.")
      (exit)
    )
  )

  ;; Extract points based on the object type (LINE or LWPOLYLINE)
  (cond
    ;; If the entity is a line
    ((= obj_nam "AcDbLine")
     (setq pt1 (cdr (assoc 10 (entget entity))))  ;; Start point of the line
     (setq pt2 (cdr (assoc 11 (entget entity))))  ;; End point of the line
    )

    ;; If the entity is a polyline (handle vertices)
    ((= obj_nam "AcDbPolyline")
     ;; Convert the polyline entity to a VLA object
     (setq polyline-entity (vlax-ename->vla-object entity))
	 ;; Get the last point of the polyline (endpoint)
	 (setq pt1 (vlax-curve-getpointatparam polyline-entity 0))
     (setq pt2 (vlax-curve-getpointatparam polyline-entity (vlax-curve-getendparam polyline-entity)))
	 ; or:
     ; ;; Get the number of vertices in the polyline
     ; (setq num-vertices (vla-get-count polyline-entity))

     ; ;; Check if the polyline has at least 2 vertices (start and end points)
     ; (if (> num-vertices 1)
       ; (progn
         ; (setq pt1 (vlax-curve-getpointatparam polyline-entity 0))  ;; First vertex (start point)
         ; (setq pt2 (vlax-curve-getpointatparam polyline-entity (1- num-vertices)))  ;; Last vertex (end point)
       ; )
       ; (progn
         ; (princ "\nThe polyline does not have enough segments.")
         ; (exit)
       ; )
     ; )
	 
    )
  )

  ;; Set the default height for the text to 25 (no user input needed)
  (setq height 25)

  ;; Calculate the delta values for slope
  (setq deltaY (- (cadr pt2) (cadr pt1)))
  (setq deltaX (- (car pt2) (car pt1)))

  ;; Check for horizontal or vertical lines (angles of 0 or 90 degrees)
  (cond
    ;; If the line is horizontal (0 angle)
    ((= deltaY 0)
     (setq slope 0)
     (setq ang 0)  ;; Horizontal line has an angle of 0
    )

    ;; If the line is vertical (90 degrees or pi/2 radians)
    ((= deltaX 0)
     (setq slope "Inf")  ;; Vertical line has infinite slope (undefined)
     (setq ang (/ pi 2))  ;; Vertical line has an angle of 90 degrees (pi/2 radians)
    )

    ;; If the line is neither horizontal nor vertical
    (t
     (setq slope (/ deltaY deltaX))  ;; Calculate the slope as percentage
     (setq slope (* slope 100))  ;; Convert to percentage
     (setq ang (atan (/ deltaY deltaX)))  ;; Calculate the angle
    )
  )

  ;; Ask for the base point of the text (this is where the text will be placed)
  (setq basept (getpoint "\nPick base point for text: "))

  ;; Adjust the base point slightly for better positioning
  (setq offset-point (list (+ (car basept) 10) (+ (cadr basept) 10)))

  ;; Create the text with slope percentage
  (setq slope-text (if (= slope "Inf") "∞" (strcat (rtos slope 2 2) "%")))
  (princ "\nstage-3: completed: Create the text with slope percentage")

  ;; Create the text entity at the base point, rotated to be parallel to the line
  (setq text-entity (entmakex
                      (list (cons 0 "TEXT")
                            (cons 10 offset-point)  ;; Base point for the text
                            (cons 40 height)        ;; Text height
                            (cons 1 slope-text)     ;; The text content (slope percentage)
                            (cons 50 ang) 		    ;; Rotate the text to be parallel to the line
							)))     

  ;; Finish the command and display a success message
  (princ "\nSlope text placed successfully.")

  (princ)  ;; Exit quietly
)

;; Prompt for the command in the AutoCAD command line
(princ "\nType 'SL' to run the command.")
(princ)