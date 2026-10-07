;;; ==========================================================================
;;;  MEPCOUNT.lsp  -  MEP Block Counter                      by BuildMEP.com
;;; --------------------------------------------------------------------------
;;;  Counts block references (sprinklers, diffusers, light fittings, devices,
;;;  valves ...) in a selection or in the whole current space and reports the
;;;  quantities grouped by block name, or by block name + layer.
;;;
;;;  - Dynamic blocks are counted under their real name, not *U123.
;;;  - Xref references are skipped.
;;;  - AutoCAD's internal anonymous blocks (*X, *U, *T ...) that are not
;;;    dynamic blocks are skipped and reported as a count.
;;;  - Only top-level blocks are counted (blocks nested inside other blocks
;;;    are not opened).
;;;
;;;  Output options:
;;;    1. Report in the command line (always)
;;;    2. AutoCAD table placed in the drawing (optional)
;;;    3. CSV file for Excel / BOQ (optional)
;;;
;;;  Command:  MEPCOUNT
;;;  Free to use. Test on a copy of your drawing first.
;;; ==========================================================================

(vl-load-com)

;; Real name of a block reference (handles dynamic blocks)
(defun mepc:name (ent / obj)
  (setq obj (vlax-ename->vla-object ent))
  (if (vlax-property-available-p obj 'EffectiveName)
    (vla-get-EffectiveName obj)
    (cdr (assoc 2 (entget ent)))
  )
)

;; T if the INSERT refers to an xref
(defun mepc:xrefp (ent / def)
  (setq def (tblsearch "BLOCK" (cdr (assoc 2 (entget ent)))))
  (and def (= 4 (logand 4 (cdr (assoc 70 def)))))
)

;; Add one to the counter stored under KEY
(defun mepc:add (key lst / pair)
  (if (setq pair (assoc key lst))
    (subst (cons key (1+ (cdr pair))) pair lst)
    (cons (cons key 1) lst)
  )
)

;; Pad a string with spaces to width W
(defun mepc:pad (s w)
  (while (< (strlen s) w) (setq s (strcat s " ")))
  s
)

;; Quote a value for CSV
(defun mepc:csv (s / i out ch)
  (setq i 1 out "")
  (while (<= i (strlen s))
    (setq ch (substr s i 1))
    (setq out (strcat out (if (= ch "\"") "\"\"" ch)))
    (setq i (1+ i))
  )
  (strcat "\"" out "\"")
)

;; Is model space the active space?
(defun mepc:modelp ()
  (or (= (getvar "TILEMODE") 1) (/= (getvar "CVPORT") 1))
)

(defun c:MEPCOUNT (/ *error* doc ss mode i ent key lst total rows anon bname
                     maxw fname fh ans pt space tbl th row ncol)

  (defun *error* (msg)
    (if (and fh) (close fh))
    (if doc (vla-EndUndoMark doc))
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*EXIT*")))
      (princ (strcat "\nMEPCOUNT error: " msg))
    )
    (princ)
  )

  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (vla-StartUndoMark doc)

  ;; 1. Selection -----------------------------------------------------------
  (princ "\nSelect blocks to count, or press Enter to count ALL blocks in the current space.")
  (setq ss (ssget '((0 . "INSERT"))))
  (if (not ss)
    (setq ss
      (ssget "_X"
        (list '(0 . "INSERT")
              (cons 410 (if (mepc:modelp) "Model" (getvar "CTAB"))))
      )
    )
  )

  (if (not ss)
    (princ "\nNo blocks found.")
    (progn

      ;; 2. Grouping --------------------------------------------------------
      (initget "Name Layer")
      (setq mode (getkword "\nGroup by [Name/Layer] <Name>: "))
      (if (not mode) (setq mode "Name"))

      ;; 3. Count -----------------------------------------------------------
      (setq i 0 lst nil total 0 anon 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i))
        (if (not (mepc:xrefp ent))
          (progn
            (setq bname (mepc:name ent))
            ;; skip AutoCAD's internal anonymous blocks (*X, *U, *T, *D ...)
            ;; that are not dynamic blocks - they are not real symbols
            (if (wcmatch bname "`**")
              (setq anon (1+ anon))
              (progn
                (setq key
                  (if (= mode "Layer")
                    (list bname (cdr (assoc 8 (entget ent))))
                    (list bname)
                  )
                )
                (setq lst (mepc:add key lst))
                (setq total (1+ total))
              )
            )
          )
        )
        (setq i (1+ i))
      )
      (if (> anon 0)
        (princ (strcat "\n" (itoa anon)
                       " anonymous block(s) (*X, *U ...) skipped - not real symbols."))
      )

      (if (= total 0)
        (princ "\nNo named blocks found (only xrefs or anonymous blocks) - nothing to count.")
        (progn

          ;; sort alphabetically by block name, then layer
          (setq lst
            (vl-sort lst
              '(lambda (a b)
                 (if (= (strcase (car (car a))) (strcase (car (car b))))
                   (< (strcase (if (cadr (car a)) (cadr (car a)) ""))
                      (strcase (if (cadr (car b)) (cadr (car b)) "")))
                   (< (strcase (car (car a))) (strcase (car (car b))))
                 )
               )
            )
          )

          ;; 4. Command-line report -------------------------------------------
          (setq maxw 10)
          (foreach r lst
            (setq maxw (max maxw (strlen (car (car r)))))
          )
          (setq maxw (min (+ maxw 2) 50))
          (princ "\n\n---------------- MEPCOUNT  (BuildMEP.com) ----------------")
          (princ (strcat "\n" (mepc:pad "Block" maxw)
                         (if (= mode "Layer") (mepc:pad "Layer" 26) "")
                         "Qty"))
          (foreach r lst
            (princ (strcat "\n" (mepc:pad (car (car r)) maxw)
                           (if (= mode "Layer") (mepc:pad (cadr (car r)) 26) "")
                           (itoa (cdr r))))
          )
          (princ (strcat "\n" (mepc:pad "TOTAL" maxw)
                         (if (= mode "Layer") (mepc:pad "" 26) "")
                         (itoa total)))
          (princ "\n-----------------------------------------------------------")

          ;; 5. Table in drawing (optional) -----------------------------------
          (initget "Yes No")
          (setq ans (getkword "\nPlace a table in the drawing? [Yes/No] <No>: "))
          (if (= ans "Yes")
            (if (setq pt (getpoint "\nTable insertion point (top-left): "))
              (vl-catch-all-apply
                '(lambda ()
                   (setq space (if (mepc:modelp)
                                 (vla-get-ModelSpace doc)
                                 (vla-get-PaperSpace doc)))
                   (setq th (getvar "TEXTSIZE"))
                   (setq ncol (if (= mode "Layer") 3 2))
                   (setq rows (+ (length lst) 3)) ; title + header + data + total
                   (setq tbl (vla-AddTable space (vlax-3d-point (trans pt 1 0))
                                           rows ncol (* th 2.0) (* th 12.0)))
                   (vla-put-RegenerateTableSuppressed tbl :vlax-true)
                   (vla-SetTextHeight tbl 7 th)
                   (vla-SetColumnWidth tbl 0 (* th 22.0))
                   (if (= mode "Layer") (vla-SetColumnWidth tbl 1 (* th 16.0)))
                   (vla-SetText tbl 0 0 "BLOCK COUNT")
                   (vla-SetText tbl 1 0 "Block")
                   (if (= mode "Layer")
                     (progn (vla-SetText tbl 1 1 "Layer") (vla-SetText tbl 1 2 "Qty"))
                     (vla-SetText tbl 1 1 "Qty")
                   )
                   (setq row 2)
                   (foreach r lst
                     (vla-SetText tbl row 0 (car (car r)))
                     (if (= mode "Layer")
                       (progn
                         (vla-SetText tbl row 1 (cadr (car r)))
                         (vla-SetText tbl row 2 (itoa (cdr r))))
                       (vla-SetText tbl row 1 (itoa (cdr r)))
                     )
                     (setq row (1+ row))
                   )
                   (vla-SetText tbl row 0 "TOTAL")
                   (vla-SetText tbl row (1- ncol) (itoa total))
                   (vla-put-RegenerateTableSuppressed tbl :vlax-false)
                 )
              )
            )
          )

          ;; 6. CSV export (optional) -----------------------------------------
          (initget "Yes No")
          (setq ans (getkword "\nExport to CSV for Excel? [Yes/No] <No>: "))
          (if (= ans "Yes")
            (if (setq fname
                  (getfiled "Save block count as CSV"
                    (strcat (getvar "DWGPREFIX")
                            (vl-filename-base (getvar "DWGNAME"))
                            "-block-count.csv")
                    "csv" 1))
              (if (setq fh (open fname "w"))
                (progn
                  (write-line (if (= mode "Layer") "Block,Layer,Qty" "Block,Qty") fh)
                  (foreach r lst
                    (write-line
                      (strcat (mepc:csv (car (car r)))
                              (if (= mode "Layer")
                                (strcat "," (mepc:csv (cadr (car r))))
                                "")
                              "," (itoa (cdr r)))
                      fh)
                  )
                  (write-line (strcat "\"TOTAL\"" (if (= mode "Layer") ",\"\"" "")
                                      "," (itoa total)) fh)
                  (close fh)
                  (setq fh nil)
                  (princ (strcat "\nSaved: " fname))
                )
                (princ "\nCould not write the file (is it open in Excel?).")
              )
            )
          )
          (princ (strcat "\n" (itoa total) " block(s) counted in "
                         (itoa (length lst)) " group(s)."))
        )
      )
    )
  )
  (vla-EndUndoMark doc)
  (princ)
)

(princ "\nMEPCOUNT loaded (BuildMEP.com). Type MEPCOUNT to count blocks.")
(princ)
