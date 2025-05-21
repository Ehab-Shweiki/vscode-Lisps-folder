;; ---------------------------------------------
;; Main Functions
;; ---------------------------------------------
(defun FilterByType (ss etypes / ss i ename obj objname result)
  ;; etypes: list of VLA object names (e.g., '("AcDbLine" "AcDbPolyline"))
  ;; Returns filtered pre-selected entities that match given types

  (setq result '())	     ; Initialize empty result list
  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq ename (ssname ss i))

        ;; Check if the entity type is in the allowed type list
        (if (IsEntityOfType-DXF ename targetTypes)
          (setq result (cons ename result))
        )
        ; ;; Check if the object name is in the allowed type list
        ; (if (IsEntityOfType-VLA ename targetTypes)
        ;   (setq result (cons ename result))
        ; )

        (setq i (1+ i))
      )
    )
  )

  result ; Return list of matching entities
)


;; ---------------------------------------------
;; Sub Functions
;; ---------------------------------------------
(defun IsEntityOfType-DXF (ename targetTypes / ent etype)
  ;; ename: entity name (ename)
  ;; targetTypes: list of DXF entity type names, e.g. '("LINE" "LWPOLYLINE")
  ;; Returns T if entity type matches one in the list
  (setq ent (entget ename))
  (setq etype (cdr (assoc 0 ent)))
  (member etype targetTypes)
)

(defun IsEntityOfType-VLA (ename targetTypes / obj objname)
  ;; ename: entity name
  ;; targetTypes: list of VLA object names, e.g. '("AcDbLine" "AcDbPolyline")
  ;; Returns T if VLA object name matches one in the list
  (setq obj (vlax-ename->vla-object ename))
  (setq objname (vla-get-objectname obj))
  (member objname targetTypes)
)