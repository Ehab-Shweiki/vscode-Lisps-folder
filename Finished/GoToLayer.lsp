(defun c:GL ( / ent obj layer)
  (vl-load-com)
  ;; Ask user to select an entity
  (setq ent (car (entsel "\nSelect object to open its layer: ")))
  (if ent
    (progn
      ;; Get the VLA object and layer name
      (setq obj (vlax-ename->vla-object ent))
      (setq layer (vla-get-layer obj))
      (command "_.LAYER" "_M" layer "") ; Make the layer current to focus it
      (command "LAYER") ; Open Layer Properties Manager
      (princ (strcat "\nLayer \"" layer "\" opened in Layer Manager."))
    )
    (princ "\nNo object selected.")
  )
  (princ)
)