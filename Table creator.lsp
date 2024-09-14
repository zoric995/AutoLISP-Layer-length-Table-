(defun C:tablelayoutlist ( / *error* acdoc ss p i e a d l) 
  (vl-load-com)
  (setq acdoc (vla-get-activedocument (vlax-get-acad-object)))
  (vla-startundomark acdoc)

  (defun *error* (msg)
    (and
      msg
      (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*BREAK*,*EXIT*"))
      (princ (strcat "\nError: " msg))
    )
    (if
      (= 8 (logand (getvar 'undoctl) 8))
      (vla-endundomark acdoc)
    )
    (princ)
    )
  
  (if
    (and
      (setq ss (ssget ":L" '((0 . "LINE,POLYLINE,LWPOLYLINE,ARC,CIRCLE,ELLIPSE,SPLINE"))))
      (setq p (getpoint "\nSpecify table insert point: ")) ; Prompt for the table insertion point
      )
    (progn
      (setq l nil) ; Initialize the list to store individual entity data
      (repeat
        (setq i (sslength ss))
        (setq e (ssname ss (setq i (1- i)))
              a (cdr (assoc 8 (entget e))) ; Layer name
              d (vlax-curve-getdistatparam e (vlax-curve-getendparam e)) ; Length
        )
        ; Add each entity's layer and length as a separate entry
        (setq l (cons (list a d) l))
      )
      (setq l (vl-sort l '(lambda (a b) (< (car a) (car b)))))
      (insert_table l p)
      )
    )
  (*error* nil)
  (princ)
  )

(defun insert_table (lst pct / tab row col ht i n space colWidths)
  (setq space (vlax-get acDoc (if (= 1 (getvar 'cvport)) 'PaperSpace 'ModelSpace))
        ht  (/ 2.5 (cond ((getvar 'cannoscalevalue)) (1.0)))
        pct (trans pct 1 0)
        n   (trans '(1 0 0) 1 0 T)
        tab (setq tab (vla-addtable space (vlax-3d-point pct) (+ 2 (length lst)) 2 (* 2.5 ht) ht)) ; Adjust column count to 2
        )
  (vlax-put tab 'direction n)
  
  (mapcar
    (function
      (lambda (rowType)
        (vla-SetTextStyle  tab rowType (getvar 'textstyle))
        (vla-SetTextHeight tab rowType ht)
      )
    )
   '(2 4 1)
  )
  
  (vla-put-HorzCellMargin tab (* 0.14 ht))
  (vla-put-VertCellMargin tab (* 0.14 ht))

  ; Header row for the table
  (vla-SetText tab 0 0 "Layer Name")
  (vla-SetText tab 0 1 "Length")

  ; Calculate column widths based on the widest text in each column
  (setq colWidths
        (mapcar
          '(lambda (col)
             (apply 'max
               (mapcar
                 '(lambda (x)
                    ((lambda (txb)
                       (+ (abs (- (caadr txb) (caar txb))) (* 2.0 ht)))
                      (textbox (list (cons 1 (vl-princ-to-string x)) (cons 7 (getvar 'textstyle)) (cons 40 ht)))
                    )
                  )
                col
              )
            )
          )
          (apply 'mapcar (cons 'list (cons '("Layer Name" "Length") lst)))
        )
  )

  ; Set column widths
  (setq col 0)
  (foreach width colWidths
    (vla-SetColumnWidth tab col width)
    (setq col (1+ col))
  )

  ; Loop through the list and fill the table
  (setq row 1)
  (foreach item lst
    (vla-SetRowHeight tab row (* 1.5 ht))
    (vla-SetText tab row 0 (vl-princ-to-string (car item))) ; Layer name
    (vla-SetText tab row 1 (rtos (cadr item))) ; Length
    (setq row (1+ row))
  )
)