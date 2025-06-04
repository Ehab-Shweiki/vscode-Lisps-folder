(defun c:DP ( / table1 table2 table tableNum S_input S N dir width pt1 pt2 rectLen
               W e blkName blkExists blkEnt matchedS width100 rectLen100
               i row match-row dim1 dim2 inspt
               PLine_layer Dim_layer Dim_style Text_style
               prevLayer prevDimStyle prevTextStyle prevOsMode)

  ;; --- Define desired settings ---
  (setq PLine_layer "MyBlockLayer")
  (setq Dim_layer   "MyDimLayer")
  (setq Dim_style   "MyDimStyle")
  (setq Text_style  "MyTextStyle")
  (setq Text_style  "MyTextStyle")

  ;; --- Save current system settings ---
  (setq prevLayer     (getvar 'CLAYER))
  (setq prevDimStyle  (getvar 'DIMSTYLE))
  (setq prevTextStyle (getvar 'TEXTSTYLE))
  (setq prevOsMode    (getvar 'OSMODE))

  ;; --- Turn off OSNAP ---
  (setvar 'OSMODE 0)

  ;; --- Define Tables ---
  (setq table1
    '((6.20 2.40 0.300) (6.00 2.45 0.325) (5.80 2.50 0.350) (5.65 2.55 0.375)
      (5.50 2.60 0.400) (5.35 2.65 0.425) (5.20 2.70 0.450) (5.10 2.75 0.475)
      (4.95 2.80 0.500) (4.85 2.85 0.525) (4.75 2.90 0.550) (4.60 2.95 0.575)
      (4.50 3.00 0.600)))
  (setq table2
    '((5.90 2.30 0.250) (5.75 2.35 0.275) (5.55 2.40 0.300) (5.40 2.45 0.325)
      (5.25 2.50 0.350) (5.10 2.55 0.375) (4.95 2.60 0.400) (4.85 2.65 0.425)
      (4.70 2.70 0.450) (4.60 2.75 0.475) (4.45 2.80 0.500) (4.35 2.85 0.525)
      (4.25 2.90 0.550) (4.15 2.95 0.575) (4.05 3.00 0.600)))

  ;; --- Get inputs ---
  (initget 1 "1 2")
  (setq tableNum (getkword "\nSelect table [1/2]: "))
  (setq table (if (= tableNum "1") table1 table2))

  (setq S_input (getreal "\nEnter S (e.g. 550 for 5.50): "))
  (setq S (/ S_input 100.0))

  (initget 1 "0 1 2")
  (setq N (atoi (getkword "\nEnter number of e's [0/1/2]: ")))
  (initget 1 "V H")
  (setq dir (getkword "\nInsert direction [V/H]: "))

  ;; --- Search table ---
  (setq match-row nil)
  (setq i 0)
  (while (and (< i (length table)) (null match-row))
    (setq row (nth i table))
    (if (<= (car row) S)
      (setq match-row row))
    (setq i (1+ i))
  )

  (if (null match-row)
    (progn
      (princ "\nNo matching row found.")
      (setvar 'OSMODE prevOsMode)
      (exit)))

  ;; --- Extract row values ---
  (setq matchedS (car match-row))
  (setq W (nth 1 match-row))
  (setq e (nth 2 match-row))
  (setq width (+ W (* e N)))
  (setq rectLen 500.0)
  (setq width100 (* width 100.0))
  (setq rectLen100 (* rectLen 100.0))

  ;; --- Build block name ---
  (setq blkName (strcat "par t" tableNum " "
                        (itoa (fix (+ (* matchedS 100) 0.5)))
                        "e" (itoa N)
                        " " dir))

  ;; --- Ask user for insertion point ---
  (setq inspt (getpoint "\nSpecify insertion point: "))

  ;; --- Check if block exists ---
  (setq blkExists (tblsearch "BLOCK" blkName))

  (if (not blkExists)
    (progn
      ;; --- Draw rectangle ---
      (setq pt1 '(0.0 0.0))
      (setq pt2 (if (= dir "H") (list rectLen100 width100) (list width100 rectLen100)))

      (setvar 'CLAYER PLine_layer)
      (command "_.PLINE" pt1
               (list (car pt2) (cadr pt1))
               pt2
               (list (car pt1) (cadr pt2))
               "C")
      (setq rectEnt (entlast))

      ;; --- Draw dimensions ---
      (setvar 'CLAYER Dim_layer)
      (command "-DIMSTYLE" "R" Dim_style "")
      (setvar 'TEXTSTYLE Text_style)

      ;; Horizontal (bottom)
      (command "_.DIMLINEAR" pt1 (list (car pt2) (cadr pt1))
               (list (/ (car pt2) 2.0) (- (cadr pt1) 200.0)))
      (setq dim1 (entlast))

      ;; Vertical (left)
      (command "_.DIMLINEAR" pt1 (list (car pt1) (cadr pt2))
               (list (- (car pt1) 200.0) (/ (cadr pt2) 2.0)))
      (setq dim2 (entlast))

      ;; --- Create block with rect + dims ---
      (command "_BLOCK" blkName '(0 0 0) rectEnt dim1 dim2 "")
    )
  )

  ;; --- Restore previous settings ---
  (setvar 'CLAYER prevLayer)
  (setvar 'TEXTSTYLE prevTextStyle)
  (setvar 'OSMODE prevOsMode)
  (command "-DIMSTYLE" "R" prevDimStyle "")

  ;; --- Insert block at chosen point ---
  (command "_-INSERT" blkName inspt 1 1 0)

  (princ (strcat "\nBlock inserted: " blkName))
  (princ)
)
