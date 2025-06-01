(defun heb-to-eng (str / hebrew-english-map result ch pair)
  ;; Hebrew to English character mapping
  (setq hebrew-english-map
    '(
      ("ש" . "a") ("נ" . "b") ("ב" . "c") ("ג" . "d") ("ק" . "e") ("כ" . "f")
      ("ע" . "g") ("י" . "h") ("ן" . "i") ("ח" . "j") ("ל" . "k") ("ך" . "l")
      ("צ" . "m") ("מ" . "n") ("ם" . "o") ("פ" . "p") ("/" . "q") ("ר" . "r")
      ("ד" . "s") ("א" . "t") ("ו" . "u") ("ה" . "v") ("'" . "w") ("ס" . "x")
      ("ט" . "y") ("ז" . "z")
      (";" . "`") ("1" . "1") ("2" . "2") ("3" . "3") ("4" . "4") ("5" . "5")
      ("6" . "6") ("7" . "7") ("8" . "8") ("9" . "9") ("0" . "0") ("-" . "-")
      ("=" . "=") ("]" . "[") ("[" . "]") ("\\" . "\\") ("ף" . ";") ("," . "'")
      ("ת" . ",") ("ץ" . ".") ("." . "/") (":" . ";") (")" . "(") ("(" . ")")
    )
  )

  ;; Convert each character
  (setq result "")
  (foreach c (vl-string->list str)
    (setq ch (vl-list->string (list c))) ; convert code to char
    (setq pair (assoc ch hebrew-english-map))
    (setq result (strcat result (if pair (cdr pair) ch)))
  )
  result
)

(defun c:HebToEngText ( / ent obj str newstr)
  (prompt "\nSelect TEXT or MTEXT entity to convert Hebrew to English...")
  (setq ent (car (entsel)))
  (if (and ent (setq obj (vlax-ename->vla-object ent)))
    (cond
      ((= (vla-get-objectname obj) "AcDbText")
       (setq str (vla-get-TextString obj))
       (setq newstr (heb-to-eng str))
       (vla-put-TextString obj newstr)
       (princ (strcat "\nConverted text: " newstr))
      )
      ((= (vla-get-objectname obj) "AcDbMText")
       (setq str (vla-get-TextString obj))
       (setq newstr (heb-to-eng str))
       (vla-put-TextString obj newstr)
       (princ (strcat "\nConverted MText: " newstr))
      )
      (T
       (prompt "\nSelected entity is not TEXT or MTEXT.")
      )
    )
    (prompt "\nNo valid entity selected.")
  )
)

(princ "\nType 'HebToEngText' to run .")
(princ)