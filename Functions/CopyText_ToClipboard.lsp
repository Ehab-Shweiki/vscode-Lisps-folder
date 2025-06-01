;; -------- Convert English to Hebrew --------
(defun eng-to-heb (str / english-hebrew-map result c ch pair)
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

;; -------- Smart Clipboard Handler --------
(defun copy-to-clipboard-smart (txt / file f contains-hebrew)
  ;; Check for Hebrew Unicode range (U+0590 to U+05FF)
  (setq contains-hebrew nil)
  (foreach c (vl-string->list txt)
    (if (and (>= c 1424) (<= c 1535)) ; Hebrew Unicode
      (setq contains-hebrew T)
    )
  )

  ;; Write the text to a temp file
  (setq file (vl-filename-mktemp "clip.txt"))
  (setq f (open file "w"))
  (write-line txt f)
  (close f)

  ;; Use different copy methods based on content
  (if contains-hebrew
    ;; PowerShell for UTF-8 Hebrew
    (startapp "cmd.exe"
      (strcat "/c powershell -command \"Get-Content -Path '" file "' | Set-Clipboard\"")
    )
    ;; clip.exe for plain text (fast)
    (startapp "cmd.exe"
      (strcat "/c type \"" file "\" | clip")
    )
  )
)

;; -------- General Copy Function --------
(defun copy-text-from-selected (convertHebrew / ent obj txt finalTxt)
  (vl-load-com)
  (setq ent (car (entsel "\nSelect a TEXT or MTEXT object: ")))
  (if ent
    (progn
      (setq obj (vlax-ename->vla-object ent))
      (cond
        ((= (vla-get-objectname obj) "AcDbText") (setq txt (vla-get-textstring obj)))
        ((= (vla-get-objectname obj) "AcDbMText") (setq txt (vla-get-textstring obj)))
        (T (setq txt nil))
      )
      (if txt
        (progn
          (setq finalTxt (if convertHebrew (eng-to-heb txt) txt))
          (copy-to-clipboard-smart finalTxt)
          (princ (strcat "\nCopied to clipboard: \"" finalTxt "\""))
        )
        (princ "\nSelected object is not TEXT or MTEXT.")
      )
    )
    (princ "\nNothing selected.")
  )
  (princ)
)

;; -------- Command: CT (no conversion) --------
(defun c:CT () (copy-text-from-selected nil) (princ))

;; -------- Command: CTH (with Hebrew conversion) --------
(defun c:CTH () (copy-text-from-selected T) (princ))

;; -------- Help Message --------
(princ "\nType CT to copy original text to clipboard.")
(princ "\nType CTH to copy Hebrew-transformed text to clipboard.")
(princ)
