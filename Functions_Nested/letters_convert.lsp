;; -------- Convert English to Hebrew --------
(defun eng-to-heb (str / english-hebrew-map result ch pair)
  ;; English to Hebrew character mapping
  (setq english-hebrew-map
    '(
      ("a" . "ש") ("b" . "נ") ("c" . "ב") ("d" . "ג") ("e" . "ק") ("f" . "כ")
      ("g" . "ע") ("h" . "י") ("i" . "ן") ("j" . "ח") ("k" . "ל") ("l" . "ך")
      ("m" . "צ") ("n" . "מ") ("o" . "ם") ("p" . "פ") ("q" . "/") ("r" . "ר")
      ("s" . "ד") ("t" . "א") ("u" . "ו") ("v" . "ה") ("w" . "'") ("x" . "ס")
      ("y" . "ט") ("z" . "ז")
      ("A" . "ש") ("B" . "נ") ("C" . "ב") ("D" . "ג") ("E" . "ק") ("F" . "כ")
      ("G" . "ע") ("H" . "י") ("I" . "ן") ("J" . "ח") ("K" . "ל") ("L" . "ך")
      ("M" . "צ") ("N" . "מ") ("O" . "ם") ("P" . "פ") ("Q" . "/") ("R" . "ר")
      ("S" . "ד") ("T" . "א") ("U" . "ו") ("V" . "ה") ("W" . "'") ("X" . "ס")
      ("Y" . "ט") ("Z" . "ז")
      ("`" . ";") ("1" . "1") ("2" . "2") ("3" . "3") ("4" . "4") ("5" . "5")
      ("6" . "6") ("7" . "7") ("8" . "8") ("9" . "9") ("0" . "0") ("-" . "-")
      ("=" . "=") ("[" . "]") ("]" . "[") ("\\" . "\\") (";" . "ף") ("'" . ",")
      ("," . "ת") ("." . "ץ") ("/" . ".") (":" . "ף") ("(" . ")") (")" . "(")
    )
  )
  
  ;; converting proccess
  (setq result "") ; initialize result string
  (foreach c (vl-string->list str)
    (setq ch (vl-list->string (list c))) ; Convert code to single-char string
    (setq pair (assoc ch english-hebrew-map))
    (setq result (strcat result (if pair (cdr pair) ch)))
  )
  result
)

(defun heb-to-eng (str / hebrew-english-map result ch pair)
  ;; Hebrew to English character mapping (reverse of original)
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

  ;; Convert string
  (setq result "")
  (foreach c (vl-string->list str)
    (setq ch (vl-list->string (list c))) ; Convert code to single-char string
    (setq pair (assoc ch hebrew-english-map))
    (setq result (strcat result (if pair (cdr pair) ch)))
  )
  result
)

;; testing
(setq txt_converted1 (eng-to-heb txt))
(setq txt_converted2 (heb-to-eng txt))
