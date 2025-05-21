(defun c:DelDimsWithRefEdit ( / ent ename)
  (prompt "\nSelect a block reference to edit:")
  (setq ent (entsel "\nPick a block to edit: "))
  (if ent
    (progn
      (setq ename (car ent))
      (command "_.REFEDIT" ename) ; enter refedit
      (command "_.ERASE" "_ALL")  ; erase all — can be filtered if needed
      (command "_.REFCLOSE" "_SAVECHANGES") ; save and exit
    )
    (prompt "\nNothing selected.")
  )
  (princ)
)
(princ "\nType DelDimsWithRefEdit to run.")
(princ)