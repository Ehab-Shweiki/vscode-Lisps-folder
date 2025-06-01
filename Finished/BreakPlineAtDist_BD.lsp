(defun c:BD ( / ent ename pickpt obj param pickDist endDist option offset breakDist brkpt breakFromStart minOffset maxOffset)
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
          (setq param (vlax-curve-getParamAtPoint ename (vlax-curve-getClosestPointTo ename pickpt)))
          (setq pickDist (vlax-curve-getDistAtParam ename param))
          (setq endDist (vlax-curve-getDistAtParam ename (vlax-curve-getEndParam ename)))

          ;; Prompt for option
          (initget "Start End Pick") ;; <--- here is your initget
          (setq option (getkword "\nBreak from [Start/End/Pick] <Auto nearest end>: "))
		  (setq option (if option (strcase option) nil))

          (cond
            ;; Option 1: Auto-detect or choose Start or End
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

             ;; Prompt and validate offset
             (while
               (progn
                 (setq offset (getreal
                   (strcat "\nEnter distance from "
                           (if breakFromStart "START" "END")
                           " (0.0 to "
                           (rtos maxOffset 2 2)
                           "): ")))
                 (or (< offset minOffset) (> offset maxOffset))
               )
               (princ "\nInvalid distance. Please enter a value within the allowed range.")
             )

             ;; Calculate breakDist
             (setq breakDist (if breakFromStart offset (- endDist offset)))
            )

            ;; Option 2: From pick point toward farthest end
            ((= option "PICK")
             (if (< pickDist (/ endDist 2.0))
               ;; Toward END
               (progn
                 (setq minOffset (* -1 pickDist))
                 (setq maxOffset (- endDist pickDist))
                 (while
                   (progn
                     (setq offset (getreal
                       (strcat "\nEnter distance from picked point toward END ("
                               (rtos minOffset 2 2) " to "
                               (rtos maxOffset 2 2) "): ")))
                     (or (< offset minOffset) (> offset maxOffset))
                   )
                   (princ "\nInvalid distance. Please enter a value within the allowed range.")
                 )
                 (setq breakDist (+ pickDist offset))
               )
               ;; Toward START
               (progn
                 (setq minOffset (* -1 (- endDist pickDist)))
                 (setq maxOffset pickDist)
                 (while
                   (progn
                     (setq offset (getreal
                       (strcat "\nEnter distance from picked point toward START ("
                               (rtos minOffset 2 2) " to "
                               (rtos maxOffset 2 2) "): ")))
                     (or (< offset minOffset) (> offset maxOffset))
                   )
                   (princ "\nInvalid distance. Please enter a value within the allowed range.")
                 )
                 (setq breakDist (+ pickDist offset))
               )
             )
            )

            ;; Invalid option
            (T (princ "\nInvalid option."))
          )

          ;; Get break point
          (setq brkpt (vlax-curve-getPointAtDist ename breakDist))

          ;; Break it
          (if brkpt
            (progn
              (setq oldSnap (getvar "OSMODE"))
			  (setvar "OSMODE" 0)  ;; turn off snaps temporarily  
              (command "_.BREAK" ename brkpt brkpt)
			  (setvar "OSMODE" oldSnap)
              (princ "\nPolyline broken at specified location."))
            (princ "\nCould not compute break point.")
          )
        )
        (princ "\nSelected object is not a polyline.")
      )
    )
    (princ "\nNo valid polyline selected.")
  )
)

(princ "\nType BD to run the BreakPlineAtDist command.")
(princ)