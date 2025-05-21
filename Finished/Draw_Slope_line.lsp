;; Helper to find an existing 'spot_level' block instance in the drawing
(defun find-block-instance ( blkName / e data )
(setq e (entnext)
		found nil)
(while (and e (not found))
	(setq data (entget e))
	(if (and (= (cdr (assoc 0 data)) "INSERT")
			(= (strcase (cdr (assoc 2 data))) (strcase blkName)))
	(setq found (vlax-ename->vla-object e))
	)
	(setq e (entnext e))
)
found
)

;---------------------------------------------------------
(defun c:DSLine ( / pt1 xLen slope yLen pt2 midPt ang slopeStr offset textHeight
                  blkName blkScale blkRef blkCopy ent attEnt attTag fieldStr blkID oldOsmode )

  ;; === Settings ===
  (setq blkName "spot_level") ; change as needed
  (setq offset 10.0)
  (setq textHeight 25.0)
  (setq blkScale 1.0)

  ;; === Disable OSNAP ===
  (setq oldOsmode (getvar "OSMODE"))
  (setvar "OSMODE" 0)

  ;; === Find Block Instance ===
  (setq blkRef (find-block-instance blkName))
  (if (null blkRef)
    (progn
      (prompt (strcat "\nNo instance of block '" blkName "' found in the drawing."))
      (setvar "OSMODE" oldOsmode)
      (exit)
    )
  )

  ;; === User Inputs ===
  (setq pt1 (getpoint "\nStart point: "))
  (setq xLen (getreal "\nHorizontal length (X) "cm": "))
  (setq slope (getreal "\nSlope (Y/X): "))
  (setq slope (* slope -0.01))              ; percent to slope (negative direction)
  (setq yLen (* xLen slope))
  (setq pt2 (list (+ (car pt1) xLen) (+ (cadr pt1) yLen)))

  ;; === Draw the LINE ===
  (command "_.LINE" pt1 pt2 "")

  ;; === Midpoint + Rotation ===
  (setq midPt (list (/ (+ (car pt1) (car pt2)) 2.0)
                    (/ (+ (cadr pt1) (cadr pt2)) 2.0)))
  (setq ang (atan yLen xLen))
  (setq midPt (polar midPt (+ ang (/ pi 2)) offset))

  ;; === Add Slope Text ===
  (setq slopeStr (strcat (rtos (* slope 100.0) 2 1) "%"))
  (entmake
    (list
      '(0 . "TEXT")
      (cons 10 midPt)
      (cons 40 textHeight)
      (cons 1 slopeStr)
      (cons 7 "Standard")
      (cons 50 ang)
      (cons 72 1)
      (cons 73 1)
      (cons 11 midPt)
    )
  )

  ;; === Copy Block to pt2 ===
  (setq blkCopy (vla-Copy blkRef))
  (vla-Move blkCopy
    (vlax-3d-point (vlax-get blkRef 'InsertionPoint))
    (vlax-3d-point pt2)
  )

  ;; === Update LEVEL Attribute with FIELD (Y Position) ===
  (setq ent (vlax-vla-object->ename blkCopy))
  (setq blkID (vla-get-ObjectID blkCopy))
  (setq fieldStr (strcat "%<\\AcObjProp Object(%<\\_ObjId " (itoa blkID) ">%).Position \f \"%tc1\">%"))
  (setq attEnt (entnext ent))
  (while (and attEnt (/= (cdr (assoc 0 (entget attEnt))) "SEQEND"))
    (setq attTag (cdr (assoc 2 (entget attEnt))))
    (if (equal (strcase attTag) "LEVEL")
      (entmod (subst (cons 1 fieldStr) (assoc 1 (entget attEnt)) (entget attEnt)))
    )
    (setq attEnt (entnext attEnt))
  )

  ;; === Restore OSNAP ===
  (setvar "OSMODE" oldOsmode)
  (princ)
)


(defun c:DS ( / pt xLen slope yLen ptList continue oldOsmode midPt offset slopeStr ang ptNew
                               textHeight blkScale blkCopy *spotBlockRef* blkRef insertPt ent attEnt attTag
                               fieldStr blkName )

  

  ;; Settings
  (setq offset 10.0)
  (setq textHeight 25.0)
  (setq blkScale 1.0)
  (setq blkName "spot_level")

  ;; Try to find the block automatically
  (setq *spotBlockRef* (find-block-instance blkName))
  (if (null *spotBlockRef*)
    (progn
      (prompt (strcat "\nNo instance of block '" blkName "' found in the drawing."))
      (exit)
    )
  )

  ;; Get start point
  (setq pt (getpoint "\nStart point: "))
  (setq ptList (list pt))
  (setq continue T)

  ;; Disable OSNAP temporarily
  (setq oldOsmode (getvar "OSMODE"))
  (setvar "OSMODE" 0)

  ;; Main loop
  (while continue
    (initget 0)
    (setq xLen (getreal (strcat "\nSegment " (itoa (length ptList)) " - Horizontal length (X) <Enter to finish>: ")))
    (if (null xLen)
      (progn
        (prompt "\nFinished input by X length.")
        (setq continue nil))
      (progn
        (initget 0)
        (setq slope (getreal (strcat "\nSegment " (itoa (length ptList)) " - Slope (Y/X) <Enter to finish>: ")))
        (setq slope (* slope -0.01))              ; percent to slope (negative direction)
        (if (null slope)
          (progn
            (prompt "\nFinished input by slope.")
            (setq continue nil))
          (progn
            (setq yLen (* xLen slope))
            (setq ptNew (list (+ (car pt) xLen) (+ (cadr pt) yLen)))
            (setq ptList (append ptList (list ptNew)))

            ;; Midpoint
            (setq midPt (list (/ (+ (car pt) (car ptNew)) 2.0)
                              (/ (+ (cadr pt) (cadr ptNew)) 2.0)))
            (setq ang (atan yLen xLen))
            (setq midPt (polar midPt (+ ang (/ pi 2)) offset))

            ;; Format slope as percent string
            (setq slopeStr (strcat (rtos (* slope 100.0) 2 1) "%"))

            ;; Create slope label
            (entmake
              (list
                '(0 . "TEXT")
                (cons 10 midPt)
                (cons 40 textHeight)
                (cons 1 slopeStr)
                (cons 7 "Standard")
                (cons 50 ang)
                (cons 72 1)
                (cons 73 1)
                (cons 11 midPt)
              )
            )

            ;; Copy block and move to ptNew
            (setq blkCopy (vla-Copy *spotBlockRef*))
            (vla-Move blkCopy
              (vlax-3d-point (vlax-get *spotBlockRef* 'InsertionPoint))
              (vlax-3d-point ptNew)
            )

            ; ;; Optional: auto-fill "level" attribute with FIELD for its Y-position
            ; (setq ent (vlax-vla-object->ename blkCopy))
            ; (setq blkID (vla-get-ObjectID blkCopy))
            ; (setq fieldStr (strcat "%<\\AcObjProp Object(%<\\_ObjId " (itoa blkID) ">%).Position \f \"%tc1\">%"))
            ; (setq attEnt (entnext ent))
            ; (while (and attEnt (/= (cdr (assoc 0 (entget attEnt))) "SEQEND"))
            ;   (setq attTag (cdr (assoc 2 (entget attEnt))))
            ;   (if (equal (strcase attTag) "LEVEL")
            ;     (entmod (subst (cons 1 fieldStr) (assoc 1 (entget attEnt)) (entget attEnt)))
            ;   )
            ;   (setq attEnt (entnext attEnt))
            ; )
            
			;;
            (setq pt ptNew)
          )
        )
      )
    )
  )

  ;; Draw polyline
  (if (> (length ptList) 1)
    (progn
      (princ "\n")
      (command "_.PLINE")
      (foreach p ptList (command p))
      (command "")
    )
    (prompt "\nNot enough points to draw a polyline.")
  )
  
  ;;; Finishing   
  ;; === Restore OSNAP ===
  (setvar "OSMODE" oldOsmode)

  ;; === Force REGEN to update fields and attributes ===
  (command "_.REGEN")
  (princ)
)


;; Prompt for the command in the AutoCAD command line
(princ "\nType 'DS' to run the command 'Draw_SLope_PLine'.")
(princ)