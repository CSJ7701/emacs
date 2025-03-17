
(use-package ledger-mode)

(setq ledger-accounts-file "~/Personal/Ledger/Accounts.ledger")
(setq ledger-file "~/Personal/Ledger/Main.ledger")
(setq ledger-reports
      '(("Balance" "%(binary) -f ~/Personal/Ledger/Main.ledger bal not \\(Budget or Equity\\)")
  	("Monthly Balance" "%(binary) -f ~/Personal/Ledger/Main.ledger bal -p %(month) -S amount")
	("2 Month Expenses" "python ~/Personal/Ledger/Commands/Two_Month.py")
	("Net Worth" "%(binary) -f ~/Personal/Ledger/Main.ledger bal Assets Liabilities")))

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
    (nreverse accounts)))

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
  (interactive)
  (ledger-report-select-report (ledger-report-read-name) nil))


(defun cj/ledger-insert-budget ()
  "Insert a budget template with the current date and month name, and lines for each Budget account, excluding specific ones."
  (interactive)
  (let* ((current-date (format-time-string "%Y/%m/%d"))
         (current-month (format-time-string "%B"))
         (accounts (cj/ledger-get-accounts))
         (excluded-accounts '("Budget:Unallocated")) ;; Add any specific accounts you want to exclude here
         (budget-accounts (seq-filter (lambda (acc)
                                        (and (string-prefix-p "Budget" acc)
                                             (not (member acc excluded-accounts))))
                                      accounts)))
    (insert (format "%s * %s Budget\n" current-date current-month))
    (dolist (account budget-accounts)
      (insert (format "    [%s]                          $0.00\n" account)))
    (insert "    [Budget:Unallocated]\n    ; :Budget:\n")))
