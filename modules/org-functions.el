
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



;; Org-QL agenda views and helper functions

(defun cj/org-ql-indent-subtask (entry)
  (let ((level (org-element-property :level entry)))
    (concat (make-string (* 2 (1- level)) ?\s)
	    (org-element-property :raw-value entry))))

(defun cj/project-agenda ()
  (org-ql-search (cj/org-roam-list-notes-by-tag "Project" "Archived")
    '(and (todo)
  	  (not (done)))
    :sort '(priority todo)
    :super-groups '((:auto-category t))))

(defun cj-archive/school-agenda ()
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
    '(and (or (todo) (habit))
	  (not (done))
	  (not (tags "Appointment"))
	  (or (ts :to +10)
	      (not (ts))))
    :sort '(priority deadline scheduled date)
    :super-groups '((:name "Habits" :habit)
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
		    (:name "General Tasks"
			   :todo ("TODO" "NEXT"))
		    (:name "Events"
			   :todo nil
			   :date today
			   ))
    ))

(defun cj/next-agenda ()
  (org-ql-search org-agenda-files
    '(and (not (done))
	  (todo "NEXT"))
    :sort '(priority deadline scheduled)
    :super-groups '((:auto-tags t))))

(defun cj/waiting-agenda ()
  (org-ql-search org-agenda-files
    '(todo "WAITING" "DEFERRED")
    :super-groups '((:auto-tags t))))



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

(defun cj/capture-project-file ()
  "Return a filename for a roam project file"
  (let* (
	 (file (org-roam-node-file (org-roam-node-read nil (cj/org-roam-filter-by-tag "Project")))))
    (set-buffer (org-capture-target-buffer file))
    (goto-char (point-min))
    (if (re-search-forward
	 (format org-complex-heading-regexp-format (regexp-quote "Inbox")) nil t)
	(goto-char (point-at-bol))
      (goto-char (point-max)))))

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





;; Helper functions for inserting calendar events

;; ;; Like org-read-date, but org-read-date doesn't handle time ranges... which is dumb.
(defun org-read-date-string ()
  "Prompt user for an Org timestamp, supporting time ranges via calendar."
  (with-temp-buffer
    ;; use Org’s built-in interactive timestamp function
    (org-mode)
    (call-interactively #'org-time-stamp)
    ;; return the inserted timestamp string
    (string-trim (substring-no-properties (buffer-string)))))

(defun same-day-p (t1 t2)
  "Return non-nil if T1 and T2 fall on the same calendar day."
  (let ((d1 (decode-time t1))
	(d2 (decode-time t2)))
    (and (= (nth 3 d1) (nth 3 d2)) ; Day
	 (= (nth 4 d1) (nth 4 d2)) ; Month
	 (= (nth 5 d1) (nth 5 d2)) ; Year
	 )))

(defun org-insert-repeated-timestamps (start end repeater)
  "Insert Org timestamps from START to END separated by REPEATER.
START and END are full Org timestamps, possibly with a time range.
REPEATER is a string like '+1w', '+2d', '+1m', or '+1y'."
  (interactive
   (list (org-read-date-string)
         (org-read-date-string)
         (read-string "Repeater (e.g. +1w, +2d, +1m, +1y): ")))
  (unless (string-match "^\\+\\([0-9]+\\)\\([dwmy]\\)$" repeater)
    (user-error "Invalid repeater syntax (expected +<num><unit>, e.g. +1w)"))
  (let* ((n (string-to-number (match-string 1 repeater)))
         (unit (match-string 2 repeater))
         ;; detect if range (HH:MM-HH:MM)
         (range-match (string-match
                       "\\([0-9][0-9]:[0-9][0-9]\\)-\\([0-9][0-9]:[0-9][0-9]\\)"
                       start))
         (start-has-time (or range-match (string-match-p "[0-9]:[0-9]" start)))
         ;; extract the end-of-range time string
         (end-of-range (when range-match (match-string 2 start)))
         ;; base times for iteration
         (base-start (org-read-date nil t start))
         (base-end   (org-read-date nil t end)))

    ;; Manually correct `base-start`’s time to match user input exactly
    (when (string-match "\\([0-9][0-9]\\):\\([0-9][0-9]\\)" start)
      (let* ((dt (decode-time base-start))
             (hour (string-to-number (match-string 1 start)))
             (min  (string-to-number (match-string 2 start))))
        (setq base-start (apply #'encode-time
                                (list (nth 0 dt) min hour (nth 3 dt)
                                      (nth 4 dt) (nth 5 dt) (nth 8 dt))))))

    (let* ((ts-format (cond
                       (range-match "<%Y-%m-%d %a %H:%M")
                       (start-has-time "<%Y-%m-%d %a %H:%M>")
                       (t "<%Y-%m-%d %a>")))
           (current base-start))
      (cl-labels
          ((step (ti)
                 (pcase unit
                   ("d" (time-add ti (days-to-time n)))
                   ("w" (time-add ti (days-to-time (* 7 n))))
                   ("m" (let* ((dt (decode-time ti)))
                          (apply #'encode-time
                                 (append (seq-subseq dt 0 3)
                                         (list (+ (nth 4 dt) n))
                                         (list (nth 5 dt))))))
                   ("y" (let* ((dt (decode-time ti)))
                          (apply #'encode-time
                                 (append (seq-subseq dt 0 3)
                                         (list (nth 4 dt))
                                         (list (+ (nth 5 dt) n)))))))))
        (while (or (time-less-p current base-end)
		   (same-day-p current base-end))
          (insert (format-time-string ts-format current))
          (when end-of-range
            (insert "-" end-of-range ">"))
          (insert "\n")
          (setq current (step current)))))))
