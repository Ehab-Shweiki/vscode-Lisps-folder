 ;; Select polyline and pick point
(setq ent (entsel "\nSelect polyline and click near one end: "))
(setq ename (car ent))
(setq pickpt (cadr ent))
(setq obj (vlax-ename->vla-object ename))

;; Verify polyline
(wcmatch (vla-get-objectname obj) "*Polyline")
        
;; Get parameter and distance of picked point
(setq param (vlax-curve-getParamAtPoint ename (vlax-curve-getClosestPointTo ename pickpt)))
(setq pickDist (vlax-curve-getDistAtParam ename param))

;; Compare distance from both ends
(setq startDist 0.0)
(setq endDist (vlax-curve-getDistAtParam ename (vlax-curve-getEndParam ename)))

;; Determine closest end and get offset
(setq closestEnd (if (< pickDist (/ endDist 2.0)) T nil))