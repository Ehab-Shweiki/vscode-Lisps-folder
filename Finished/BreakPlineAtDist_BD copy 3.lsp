(defun c:BD ( / ent ename pickpt obj param pickDist endDist option offset breakDist brkpt breakFromStart minOffset maxOffset done oldSnap)
  (vl-load-com)

  ;; Select polyline and point
  (setq ent (entsel "\nSelect polyline and click a point: "))
  (if (and ent (setq ename (car ent)) (setq pickpt (cadr ent)))
    (progn
      (setq obj (vlax-ename->vla-object ename))

      ;; Validate it's a polyline
      (if (wcmatch (vla-get-objectname obj) "*Polyline")
        (progn
          ;; Get picked distance and total length
          (setq param     (vlax-curve-getParamAtPoint ename (vlax-curve-getClosestPointTo ename pickpt)))
          (setq pickDist  (vlax-curve-getDistAtParam ename param))
          (setq endDist   (vlax-curve-getDistAtParam ename (vlax-curve-getEndParam ename)))
          
          ;; Default mode: Auto-nearest end
          (setq option    nil)
          (setq done      nil)

          ;; Begin interactive prompt loop
          (while (not done)
            ;; Determine working mode
            (cond
              ;; Option 1: Auto or by Start/End
              ((or (not option) (eq option "START") (eq option "END"))
               (setq breakFromStart
                 (if (eq option "START")
                   T
                   (if (eq option "END")
                     nil
                     (< pickDist (/ endDist 2.0)) ; auto if nil
                     )
                   )
                 )

               ;; Define offset limits
               (setq minOffset 0.0)
               (setq maxOffset endDist)

               ;; Initget with keywords
               (initget 0 "Start End Pick")
               (setq offset
                 (getdist
                   (strcat "\nEnter distance from "
                           (if breakFromStart "START" "END")
                           " [Start/End/Pick] (0.0 to "
                           (rtos maxOffset 2 2) "): ")))

               ;; Handle keyword switch
               (cond
                 ((= offset "Start") (setq option "START"))
                 ((= offset "End")   (setq option "END"))
                 ((= offset "Pick")  (setq option "PICK"))
                 ((numberp offset)
                  (if (and (>= offset minOffset) (<= offset maxOffset))
                    (progn
                      (setq breakDist (if breakFromStart offset (- endDist offset)))
                      (setq done T))
                    (princ "\nInvalid distance. Please enter a value within the allowed range.")
                  ))
               )
              )

              ;; Option 2: Pick mode (toward farthest end)
              ((= option "PICK")
               (if (< pickDist (/ endDist 2.0))
                 ;; Toward END
                 (progn
                   (setq minOffset (* -1 pickDist))
                   (setq maxOffset (- endDist pickDist))
                   (initget 0 "Start End")
                   (setq offset 
                     (getdist
                       (strcat "\nEnter distance from picked point toward END [Start/End] ("
                               (rtos minOffset 2 2) " to " (rtos maxOffset 2 2) "): ")))
                   (cond
                     ((= offset "Start") (setq option "START"))
                     ((= offset "End")   (setq option "END"))
                     ((numberp offset)
                      (if (and (>= offset minOffset) (<= offset maxOffset))
                        (progn
                          (setq breakDist (+ pickDist offset))
                          (setq done T))
                        (princ "\nInvalid distance. Please enter a value within the allowed range.")
                      )))
                 )
                 ;; Toward START
                 (progn
                   (setq minOffset (* -1 (- endDist pickDist)))
                   (setq maxOffset pickDist)
                   (initget 0 "Start End")
                   (setq offset
                     (getdist
                     (strcat "\nEnter distance from picked point toward START [Start/End] ("
                               (rtos minOffset 2 2) " to " (rtos maxOffset 2 2) "): ")
                     )
                   )
                   (cond
                     ((= offset "Start") (setq option "START"))
                     ((= offset "End")   (setq option "END"))
                     ((numberp offset)
                      (if (and (>= offset minOffset) (<= offset maxOffset))
                        (progn
                          (setq breakDist (+ pickDist offset))
                          (setq done T))
                        (princ "\nInvalid distance. Please enter a value within the allowed range.")
                      )))
                 )
               )
              )
            )
          )

          ;; Final break execution
          (setq oldSnap (getvar "OSMODE"))
          (setvar "OSMODE" 0)
          (setq brkpt (vlax-curve-getPointAtDist ename breakDist))
          (if brkpt
            (command "_.BREAK" ename "_F" brkpt brkpt)
            (princ "\nCould not compute break point."))
          (setvar "OSMODE" oldSnap)
        )
        (princ "\nSelected object is not a polyline.")
      )
    )
    (princ "\nNo valid polyline selected.")
  )
)
