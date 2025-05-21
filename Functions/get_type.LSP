(defun c:Get_Object_Type (/ entity entity-type vla-obj vla-type)
  ;; Prompt user to select an object
  (setq entity (car (entsel "\nPick an object: ")))

  ;; Check if selection was made
  (if entity
    (progn
      ;; Get DXF type (e.g., "LINE", "LWPOLYLINE", etc.)
      (setq entity-type (cdr (assoc 0 (entget entity))))

      ;; Get VLA type (e.g., "AcDbLine", "AcDbPolyline", etc.)
      (setq vla-obj (vlax-ename->vla-object entity))
      (setq vla-type (vla-get-objectname vla-obj))

      ;; Print both types
      (princ "\n---------------------")
      (princ (strcat "\nDXF Type: " entity-type))
      (princ (strcat "\nVLA Object Type: " vla-type))
      (princ "\n---------------------")
    )
    (princ "\nNo object selected.")
  )

  (princ)
)

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

(princ "\nType Get_Object_Type to run Get_Object_Type function.")
(princ)
;---------------
; example:
; (defun c:example1 (/ e IsEntityOfType-DXF IsEntityOfType-VLA)
; 	(setq e (car (entsel "\nSelect an object: ")))

; 	;; Check using DXF
; 	(if (IsEntityOfType-DXF e '("LINE" "LWPOLYLINE"))
; 	(princ "\nIt's a line or lwpolyline (DXF).")
; 	)

; 	;; Check using VLA
; 	(if (IsEntityOfType-VLA e '("AcDbLine" "AcDbPolyline"))
; 	(princ "\nIt's a line or polyline (VLA).")
; 	)
; )
;---------------
