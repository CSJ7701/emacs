;; ==== VARIABLES ====
(setq ledger-accounts-file "~/Personal/Ledger/Accounts.ledger")
(setq ledger-file "~/Personal/Ledger/Main.ledger")


;; ==== FUNCTIONS ====
(defun cj/ledger-get-accounts ()
  "Extract all ledger accounts from =ledger-accounts-file="
  (let (accounts)
    (with-temp-buffer
      (insert-file-contents ledger-accounts-file)
      (goto-char (point-min))
      (while (re-search-forward "^account \\(.+\\)" nil t)
	(let ((account (match-string 1)))
	  (setq account (car (split-string account ";" t)))
	  (setq account (string-trim account))
	  (push account accounts))))
    (sort (nreverse accounts) #'string<)))

(defun cj/ledger-select-account ()
  (let* ((accounts (cj/ledger-get-accounts))
	(selection (ivy-read "Select Account: " accounts :require-match t)))
    selection))

(defun cj/ledger-insert-account ()
  (interactive)
  (let ((account (cj/ledger-select-account)))
    (insert account)))

(defun cj/ledger-rename-account (old)
  (interactive "sOld Name: ")
  (let ((new (cj/ledger-select-account)))
    (ledger-rename-account old new)))

(defun cj/ledger-get-tags ()
  "Extract tags from =ledger-account-file="
  (let (tags)
    (with-temp-buffer
      (insert-file-contents ledger-accounts-file)
      (goto-char (point-min))
      (while (re-search-forward "^tag \\(.+\\)" nil t)
	(let ((tag (match-string 1)))
	  (setq tag (car (split-string tag ";" t)))
	  (setq tag (string-trim tag))
	  (push tag tags))))
    (nreverse tags)))

(defun cj/ledger-select-tag ()
  (let* ((tags (cj/ledger-get-tags))
	 (selection (ivy-read "Select Tag: " tags :require-match t)))
    selection))

(defun cj/ledger-insert-tag ()
  (interactive)
  (let* ((tag (cj/ledger-select-tag)))
    (if (string-match-p ":" tag)
	(insert (format "; %s" tag))
      (insert (format "; :%s:" tag)))))

(defun cj/ledger-report ()
  ; This function is no longer needed - use (ledger-report) instead.
  (ledger-report-select-report (ledger-report-read-name) nil))

(defun cj/ledger-insert-budget (prefix)
  "Insert a budget template.
With PREFIX, prompt for month/year from the last 6 months; otherwise use today's date."
  (interactive "P")
  (let* ((date-info
	  (if prefix
	      (let* ((months (cl-loop for i from 0 to 5
				      collect (format-time-string "%Y-%m" (time-subtract (current-time) (days-to-time (* 30 i))))))
		     (selected (completing-read "Choose month (YYYY-MM): " months))
		     (parts (split-string selected "-"))
		     (year (string-to-number (car parts)))
		     (month (string-to-number (cadr parts))))
		(list (format "%04d/%02d/01" year month)
		      (calendar-month-name month)))
	    ;; Else: use current data and month name
	    (list (format-time-string "%Y/%m/%d")
		  (format-time-string "%B"))))
	 (date (car date-info))
	 (month-name (cadr date-info))
	 (start (point))
         (accounts (cj/ledger-get-accounts))
         (excluded-accounts '("Budget:Unallocated")) ;; Add any specific accounts you want to exclude here
         (budget-accounts (sort
			   (seq-filter (lambda (acc)
                                         (and (string-prefix-p "Budget" acc)
                                              (not (member acc excluded-accounts))))
                                       accounts)
			   #'string<)))
    (insert (format "%s * %s Budget\n" date month-name))
    (dolist (account budget-accounts)
      (insert (format "    [%s]                          $0.00\n" account)))
    (insert "    [Budget:Unallocated]\n    ; :Budget:\n")
    (indent-region start (point))))

(defun cj/ledger-select-month ()
  "Prompt for a month and year. Mainly used to select a period for the ledger 'Budget' report."
  (let* ((current-year (string-to-number (format-time-string "%Y")))
	 (current-month (string-to-number (format-time-string "%m")))
	 (years (number-sequence 2024 current-year))
	 (months (number-sequence 1 12))
	 (date-options
	  (cl-loop for y in years append
		   (cl-loop for m in months
			    collect (format "%04d-%02d" y m))))
	 (selected-date (ivy-read "Select Date (YYYY-MM): " date-options)))
    selected-date))

;; Technical stuff - behind the scenes to make certain reports etc work better
; Define custom replacement variables for ledger commands
(setq ledger-report-format-specifiers
      '(("ledger-file" . ledger-report-ledger-file-format-specifier)
	("binary" . ledger-report-binary-format-specifier)
	("payee" . ledger-report-payee-format-specifier)
	("account" . ledger-report-account-format-specifier)
	("select-account" . ledger-report-select-account-format-specifier)
	("month" . ledger-report-month-format-specifier)
	("prevmonth" . ledger-report-previous-month-format-specifier)
	("othermonth" . ledger-report-other-month-format-specifier)
	("tagname" . ledger-report-tagname-format-specifier)
	("tagvalue" . ledger-report-tagvalue-format-specifier)))

(defun ledger-report-previous-month-format-specifier ()
  "Substitute previous month."
  (with-current-buffer (or ledger-report-buffer-name (current-buffer))
    (let* ((month (or ledger-report-current-month (ledger-report--current-month)))
           (year (car month))
           (month-index (cdr month))
	   (previous-month-index (if (= month-index 1)
				     12
				   (1- month-index)))
	   (previous-year (if (= month-index 1)
			      (1- year)
			    year)))
      (format "%s-%s" previous-year previous-month-index))))

(defun ledger-report-other-month-format-specifier ()
  "Select and substitute other month."
  (let* ((selected-date (cj/ledger-select-month))
	 (date-parts (split-string selected-date "-"))
	 (year (car date-parts))
	 (month (cadr date-parts)))
    (format "%s-%s" year month)))

(defun ledger-report-select-account-format-specifier ()
  "Select and substitute an account from 'ledger-accounts-file'"
  (cj/ledger-select-account))
