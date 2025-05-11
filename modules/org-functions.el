
;; configures text faces and heading size
(defun cj/org-font-setup ()
  ;; Configure org-mode fonts. Set variable width font in most cases.

  ;; Replace list hyphen with dot
      (font-lock-add-keywords 'org-mode
                            '(("^ *\\([-]\\) "
                               (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
    ;; Set faces for heading levels
    (dolist (face '((org-level-1 . 1.3)
                    (org-level-2 . 1.2)
                    (org-level-3 . 1.2)
                    (org-level-4 . 1.2)
                    (org-level-5 . 1.2)
                    (org-level-6 . 1.2)
                    (org-level-7 . 1.2)
                    (org-level-8 . 1.2)))
      (set-face-attribute (car face) nil :font "JetBrainsMono Nerd Font" :weight 'bold :height (cdr face)))
    ;; Ensure that anything that should be fixed-pitch in Org files appears that way
    (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-block-begin-line nil :foreground nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-formula nil :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-date nil :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

;; Fix for angle bracket matching
(defun cj/org-syntax-table-modify ()
  (modify-syntax-entry ?< "." org-mode-syntax-table)
  (modify-syntax-entry ?> "." org-mode-syntax-table))


;; Initial org-mode options

(defun cj/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1)
  (setq org-hide-emphasis-markers t)
  )


;; Org-QL agenda views

(defun cj/project-agenda ()
  (org-ql-search (cj/org-roam-list-notes-by-tag "Project" "Archived")
    '(and (todo)
  	  (not (done)))
    :sort '(priority todo)
    :super-groups '((:auto-category t))))

(defun cj/school-agenda ()
  (org-ql-search "~/org/Tasks-School.org"
    '(and (todo)
	  (not (done))
	  (ts :to +8)
	  )
    :sort '(priority deadline scheduled)
    :super-groups '((:name "Overdue"
			   :and (:todo ("HOMEWORK" "QUIZ" "PROEJCT" "TEST")
				       :deadline past)
			   :and (:todo ("HOMEWORK" "QUIZ" "PROJECT" "TEST")
				       :scheduled past)
			   :and (:todo ("HOMEWORK" "QUIZ" "PROJECT" "TEST")
				       :deadline today)
			   :and (:todo ("HOMEWORK" "QUIZ" "PROJECT" "TEST")
				       :scheduled today)
			   :order 0)
		    (:name "Study"
			   :todo ("QUIZ" "TEST")
			   :order 2)
		    (:name "Next"
			   :order 1
			   :anything))))

(defun cj/dashboard-agenda ()
  (org-ql-search org-agenda-files
    '(and (or (todo)
	      (habit))
	  (not (done))
	  (not (tags "Appointment"))
	  (or (ts :to +10)
	      (not (ts))))
    :sort '(priority deadline scheduled date)
    :super-groups '((:name "Habit"
			   :habit)
		    (:name "Today"
			   :time-grid t
			   :deadline past
			   :scheduled past
			   :deadline today
			   :scheduled today
			   )
		    (:name "Next"
			   :deadline future
			   :scheduled future)
		    (:name "General")
		    (:name "Events"
			   :todo nil
			   :date today
		    ))))




;; Org-Capture helper views

    (defun org-ask-location ()
      (let* ((org-refile-targets '((nil :maxlevel . 9)))
    	 (hd (condition-case nil
    		 (car (org-refile-get-location nil nil t t))
                   (error (car org-refile-history)))))
        (goto-char (point-min))
        (outline-next-heading)
        (if (re-search-forward
    	 (format org-complex-heading-regexp-format (regexp-quote hd))
    	 nil t)
    	(goto-char (point-at-bol))
          (goto-char (point-max))
          (or (bolp) (insert "\n"))
          (insert "* " hd "\n")))
      (end-of-line))

;; Automatically archive todo entries when marked as done.

(defun cj/auto-archive-done-todos ()
  "Archive the current subtree if its state is 'DONE'.
The archive location is specified by 'org-archive-location'.
Uses a half second timer to introduce a delay."
  (interactive)
  (if (and (eq major-mode 'org-mode)
	   (not (eq major-mode 'org-agenda-mode))
	   (or (string= (org-get-todo-state) "DONE")
	       (string= (org-get-todo-state) "CANC")))
      (let ((org-archive-location (format "Archive::%s" (format-time-string "%Y-%m-%d"))))
	(run-with-timer 0.7 nil 'org-archive-subtree))))

(defun cj/auto-archive-agenda-todos ()
  "Archive the todo at point if its state is 'DONE'. Uses a half second timer to introduce a delay."
  (interactive)
  (if (and (eq major-mode 'org-agenda-mode)
	   (or (string= org-state "DONE")
	       (string= (cj/org-agenda-get-todos) "DONE")
	       (string= org-state "CANC")))
      (run-with-timer 0.7 nil 'org-agenda-archive)))

(defun cj/archive-todos-ql ()
  "Uses org-ql to search agenda-files for any todos with the state 'DONE' then archives them."
  (interactive)
  (org-ql-select (org-agenda-files)
		 '(todo "DONE")
		 :action '(org-archive-subtree)))

(defun cj/auto-archive-todos-ql ()
  "Waits 0.5 seconds, runs 'cj/archive-todos-ql' then updates the agenda buffer"
  (interactive)
  (run-with-timer 0.5 nil 'cj/archive-todos-ql)
  (sleep-for 0.5)
  (org-agenda-redo))

(defun cj/launch-excalidraw ()
  (interactive)
  (progn
    (browse-url-firefox "https://excalidraw.com/")))
(defcustom org-image-function-default-directory
  "~/roam/Attachments"
  "Default directory for images for org link insertion."
  :type 'string
  :group 'org-image-function)
(defcustom org-image-function-extensions
  '("xcf" "png" "jpg" "svg" "pdf")
  "List of image extensions allowed for org link insertion."
  :type '(repeat string)
  :group 'org-image-function)
(defun cj/org-insert-image-link (img)
  "Insert an org image link, choosing with completion from 'org-image-function-default-directory'."
  (interactive
   (list (read-file-name
	  "Image: " org-image-function-default-directory nil t nil
	  (lambda (name)
	    (or (directory-name-p name)
		(member (file-name-extension name)
			org-image-function-extensions))))))
  (insert "#+ATTR_LATEX: :caption \\bicaption{---}")
  (newline)
  (insert (format "[[file:%s]]" img)))

(defun org-insert-heading-link ()
  "Insert a link to an org heading in the current file."
  (interactive)
  (save-excursion
    (let ((headings (org-map-entries (lambda ()
                                       (cons (nth 4 (org-heading-components))
                                             (point)))
                                     t 'file)))
      (if headings
          (let* ((chosen-heading (completing-read "Choose heading: " (mapcar #'car headings))))
            (insert (format "[[*%s][%s]]" chosen-heading chosen-heading))
            (message "Link inserted to heading: %s" chosen-heading))
        (message "No headings found in the current file.")))))

; Used in cooking.el for org-chef
(defun cj/get-org-headline-titles (elements)
  "Takes a list of org elements \"ELEMENTS\", and returns a list of their titles"
  (mapcar (lambda (element)
	    (plist-get (cadr element) :raw-value))
	  elements))

(defun cj/get-org-headline-tags (elements)
  (mapcar (lambda (element)
	    (let ((tags (plist-get (cadr element) :tags)))
	    (if tags
		(mapcar (lambda (tag)
			  (substring-no-properties tag))
			tags)
	      '()))) ; Return an empty list if no tags
  elements))

