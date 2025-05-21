(defun c:MyCommand ( / option)
  ;; Set valid keywords (options) to appear in the prompt
  (initget "Option1 Option2 Option3")
  
  ;; Prompt the user to choose one
  (setq option (getkword "\nChoose an option [Option1/Option2/Option3]: "))
  
  ;; Perform logic based on selected option
  (cond
    ((= option "Option1")
     (princ "\nYou selected Option 1")
     ;; Call function or logic for Option1
    )
    ((= option "Option2")
     (princ "\nYou selected Option 2")
     ;; Call function or logic for Option2
    )
    ((= option "Option3")
     (princ "\nYou selected Option 3")
     ;; Call function or logic for Option3
    )
    (T
     (princ "\nNo valid option selected.")
    )
  )
  (princ)
)


(defun c:MySmartCommand1 ( / pt )
  ;; Define keywords (shortcuts), e.g., S for Square, C for Circle
  (initget "S C") ; these are the allowed keyword shortcuts

  ;; Ask for a point or keyword
  (setq pt (getpoint "\nSpecify point or [S/C]: "))

  ;; Check if user entered a keyword instead of point
  (cond
    ((= pt "S")
     (princ "\nOption S (Square) selected.")
     ;; call your square logic here
    )
    ((= pt "C")
     (princ "\nOption C (Circle) selected.")
     ;; call your circle logic here
    )
    ((and pt (listp pt))
     ;; A point was picked
     (princ (strcat "\nPoint selected: " (rtos (car pt) 2 2) ", " (rtos (cadr pt) 2 2)))
     ;; default behavior with point input
    )
    (T
     (princ "\nInvalid input.")
    )
  )
  
  (princ)
)

(defun c:MySmartCommand2 ( / sel pt )

  ;; Step 1: Ask the user to select object(s)
  (princ "\nSelect object(s): ")
  (setq sel (ssget))

  (if (not sel)
    (progn
      (princ "\nNo objects selected. Cancelled.")
      (exit)
    )
  )

  ;; Step 2: Prompt for point input OR shortcut option
  (initget "S C") ; allowed shortcut keys
  (setq pt (getpoint "\nSpecify base point or [S/C]: "))

  ;; Step 3: Handle user input
  (cond
    ((= pt "S")
     (princ "\nOption S: Do something like draw a square.")
     ;; example logic here
    )

    ((= pt "C")
     (princ "\nOption C: Do something like draw a circle.")
     ;; example logic here
    )

    ((and pt (listp pt))
     (princ (strcat "\nPoint selected: " (rtos (car pt) 2 2) ", " (rtos (cadr pt) 2 2)))
     ;; default action here with selected objects
    )

    (T
     (princ "\nInvalid input.")
    )
  )

  (princ)
)


; ;Prompt for point input OR shortcut option 
;   (initget "R") ; for ReverseX
;   (setq pt (getpoint "\nSpecify base point or [ReverseX]: ")) 

;   ;; Step 3: Handle user input
;   (cond
;     ((= pt "R") 
;      (princ "\nOption S: Do something like draw a square.")
;      ;;
;     )
;     ((and pt (listp pt))
;      (princ (strcat "\nPoint selected: " (rtos (car pt) 2 2) ", " (rtos (cadr pt) 2 2)))
;      ;; default action here
;     )
;     (T
;      (princ "\nInvalid input.")
;     )
;   )