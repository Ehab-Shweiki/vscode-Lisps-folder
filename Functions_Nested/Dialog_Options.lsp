(defun c:ChooseFromDialog ( / dcl_id dialog_status selected_option )
  (setq dcl_id (load_dialog "options1.dcl"))
  (if (not (new_dialog "OptionsDialog" dcl_id))
    (exit)
  )

  ;; Set default selection
  (start_list "option_list")
  (add_list "Option1")
  (add_list "Option2")
  (add_list "Option3")
  (end_list)

  (action_tile "option_list" "(setq selected_option $value)")
  (action_tile "accept" "(done_dialog 1)")
  (action_tile "cancel" "(done_dialog 0)")

  (setq dialog_status (start_dialog))
  (unload_dialog dcl_id)

  (if (= dialog_status 1)
    (princ (strcat "\nYou selected: " selected_option))
    (princ "\nDialog canceled.")
  )
  (princ)
)

(defun c:CustomActionUI ( / dcl_id status action param1 )

  (setq dcl_id (load_dialog "advanced_options.dcl"))
  (if (not (new_dialog "AdvancedOptions" dcl_id))
    (exit)
  )

  ;; Fill dropdown list
  (start_list "main_action")
  (add_list "Add Circle")
  (add_list "Add Text")
  (end_list)

  ;; Default input handling
  (action_tile "main_action"
    "(setq action (nth (atoi $value) '(\"Add Circle\" \"Add Text\")))
     (if (= action \"Add Circle\")
         (mode_tile \"param1\" 0)
         (mode_tile \"param1\" 0))" ; both enabled here, can hide/show
  )

  ;; Save text field
  (action_tile "param1" "(setq param1 $value)")
  (action_tile "accept" "(done_dialog 1)")
  (action_tile "cancel" "(done_dialog 0)")

  (setq status (start_dialog))
  (unload_dialog dcl_id)

  (if (= status 1)
    (progn
      (princ (strcat "\nAction: " action))
      (princ (strcat "\nInput: " param1))
      ;; Optional: call functions here
      (cond
        ((= action "Add Circle")
         (command "._CIRCLE" pause param1)
        )
        ((= action "Add Text")
         (command "._TEXT" pause "" "0" param1)
        )
      )
    )
    (princ "\nCanceled.")
  )
  (princ)
)