(defun c:LabelSlope ( / ent obj pt1 pt2 mid dx dy slope angle offset offsetVec textStr textHeight textAngle textPt)
  (setq ent (car (entsel "\nSelect a line or polyline segment: ")))

  (if ent
    (progn
      (setq obj (vlax-ename->vla-object ent))
      (cond
        ;; Line
        ((= (vla-get-objectname obj) "AcDbLine")
          (setq pt1 (vlax-get obj 'StartPoint))
          (setq pt2 (vlax-get obj 'EndPoint))
        )
        ;; Polyline (use first segment only)
        ((= (vla-get-objectname obj) "AcDbPolyline")
          (setq pt1 (vlax-curve-getpointatparam obj 0))
          (setq pt2 (vlax-curve-getpointatparam obj 1))
        )
        (T (prompt "\nSelected object is not a line or polyline."))
      )

      (if (and pt1 pt2)
        (progn
          ;; Calculate slope
          (setq dx (- (car pt2) (car pt1)))
          (setq dy (- (cadr pt2) (cadr pt1)))
          (if (not (equal dx 0.0 1e-10))
            (setq slope (strcat "Slope = " (rtos dy 2 2) " / " (rtos dx 2 2)))
            (setq slope "Slope = VERTICAL")
          )

          ;; Midpoint
          (setq mid (mapcar '(lambda (a b) (/ (+ a b) 2.0)) pt1 pt2))

          ;; Angle of line
          (setq angle (angle pt1 pt2))

          ;; Offset vector (perpendicular to line)
          (setq offset 2.0) ; adjust offset distance as needed
          (setq offsetVec (list
                            (* offset (cos (+ angle (/ pi 2))))
                            (* offset (sin (+ angle (/ pi 2))))
                          ))

          ;; Final text position
          (setq textPt (mapcar '+ mid offsetVec))

          ;; Create text
          (setq textHeight 1.0) ; adjust text height if needed
          (setq textAngle angle)

          (entmakex
            (list
              (cons 0 "TEXT")
              (cons 8 (getvar "CLAYER"))
              (cons 10 textPt)
              (cons 40 textHeight)
              (cons 1 slope)
              (cons 50 textAngle)
              (cons 7 "Standard")
              (cons 72 1) ; center horz
              (cons 73 1) ; center vert
            )
          )

        )
        (prompt "\nCouldn't get points.")
      )
    )
    (prompt "\nNothing selected.")
  )
  (princ)
)