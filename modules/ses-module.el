(use-package ses)
(use-package csv)
(use-package csv-mode)

;; Should write some code to convert to/from csv

(defun ses-edit-cell-text (string)
  "Prompt for a string, propertize it, and insert it into the buffer."
  (interactive "sEnter the string to insert: ")
  (let* ((propertized (propertize string
				  'line-prefix ""
				  'wrap-prefix ""
				  'fontified t))
	 (rowcol  (ses-sym-rowcol ses--curcell))
	 (row (car rowcol))
	 (col (cdr rowcol)))
    (ses-edit-cell row col propertized)))


