(defun DP_CreateBlock (tableNum S N dir / table match-row W e width rectLen width100 rectLen100
                                  table1 table2 i row blkName pt1 pt2 rectEnt dim1 dim2)
  
  ;; Define target layers and styles
  (setq PLine_layer "$M-hnaya mtoknnat")
  (setq Dim_layer   "$m-dimintion")
  (setq Dim_style   "abed")
  (setq Text_style  "ITALIC")
  (setq Dim_offset  50.0)

  ;; Save current system settings
  (setq prevLayer     (getvar 'CLAYER))
  (setq prevDimStyle  (getvar 'DIMSTYLE))
  (setq prevTextStyle (getvar 'TEXTSTYLE))
  (setq prevOsMode    (getvar 'OSMODE))

  ;; Turn off OSNAP temporarily
  (setvar 'OSMODE 0)

  ;; Define tables
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

  (setq table (if (= tableNum "1") table1 table2))

  ;; Find matching row
  (setq match-row nil i 0)
  (while (and (< i (length table)) (null match-row))
    (setq row (nth i table))
    (if (<= (car row) S) (setq match-row row))
    (setq i (1+ i))
  )

  (if (null match-row)
    (progn (princ "\nNot enough S.") nil)
    (progn
      ;; Values
      (setq W (nth 1 match-row))
      (setq e (nth 2 match-row))
      (setq width (+ W (* e N)))
      (setq rectLen 5.00)
      (setq width100 (* width 100.0))
      (setq rectLen100 (* rectLen 100.0))

      ;; Block name
      (setq blkName (strcat "par t" tableNum " "
                            (itoa (fix (+ (* (car match-row) 100) 0.5)))
                            " e" (itoa N) " " dir))

      ;; If block doesn't exist, create it
      (if (not (tblsearch "BLOCK" blkName))
        (progn
          (setq pt1 '(0.0 0.0))
          (setq pt2 (if (= dir "H")
                      (list rectLen100 width100)
                      (list width100 rectLen100)))
          ;; Draw pline
          (setvar 'CLAYER "MyBlockLayer")
          (command "_.PLINE" pt1
                   (list (car pt2) (cadr pt1))
                   pt2
                   (list (car pt1) (cadr pt2))
                   "C")
          (setq rectEnt (entlast))

          ;; Dimensions
          (setvar 'CLAYER "MyDimLayer")
          (command "-DIMSTYLE" "R" "MyDimStyle" "")
          (setvar 'TEXTSTYLE "MyTextStyle")

          ;; Horizontal
          (command "_.DIMLINEAR" pt1 (list (car pt2) (cadr pt1))
                   (list (/ (car pt2) 2.0) (+ (cadr pt1) Dim_offset)))
          (setq dim1 (entlast))

          ;; Vertical
          (command "_.DIMLINEAR" pt1 (list (car pt1) (cadr pt2))
                   (list (+ (car pt1) Dim_offset) (/ (cadr pt2) 2.0)))
          (setq dim2 (entlast))

          ;; Create block
          (command "_BLOCK" blkName '(0 0 0) rectEnt dim1 dim2 "")
        )
      )
      blkName ; return name
    )
  )
)


(defun c:DPG ( / S_input S tableNum dir option basept blk1 blk2 offset pt2)

  ;; Get inputs
  (initget 1 "1 2")
  (setq tableNum (getkword "\nSelect table [1/2]: "))

  (setq S_input (getreal "\nEnter S (e.g. 550 for 5.50): "))
  (setq S (/ S_input 100.0))

  (initget 1 "W+2e 2B 2B+W")
  (setq option (getkword "\nSelect grouping [W+2e / 2B / 2B+W]: "))

  (initget 1 "V H")
  (setq dir (getkword "\nPlace group vertically or horizontally [V/H]: "))

  (setq basept (getpoint "\nSpecify base point: "))

  ;; Direction offset
  (setq offset (cond
    ((= dir "H") '(50000.0 0.0 0.0))
    ((= dir "V") '(0.0 50000.0 0.0))))

  ;; Insert blocks
  (cond
    ((= option "W+2e")
     (setq blk1 (DP_CreateBlock tableNum S 2 dir))
     (command "_-INSERT" blk1 basept 1 1 0))

    ((= option "2B")
     (setq blk1 (DP_CreateBlock tableNum S 1 dir))
     (setq blk2 blk1)
     (command "_-INSERT" blk1 basept 1 1 0)
     (setq pt2 (mapcar '+ basept offset))
     (command "_-INSERT" blk2 pt2 1 1 0))

    ((= option "2B+W")
     (setq blk1 (DP_CreateBlock tableNum S 1 dir))
     (setq blk2 (DP_CreateBlock tableNum S 0 dir))
     (command "_-INSERT" blk1 basept 1 1 0)
     (setq pt2 (mapcar '+ basept offset))
     (command "_-INSERT" blk2 pt2 1 1 0))
  )

  (princ "\nGrouped blocks inserted.")
  (princ)
)