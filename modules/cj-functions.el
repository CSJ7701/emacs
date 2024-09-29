
(defhydra hydra-text-scale (:timeout 4)
  "Scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(defun my-calendar ()
  (interactive)
  (cfw:open-calendar-buffer
   :view 'week
   :contents-sources
   (list
    (cfw:org-create-file-source "Personal" "~/org/Tasks-Personal.org" "#186A3B")
    (cfw:org-create-file-source "Work" "~/org/Tasks-Work.org" "#3498DB")
    (cfw:org-create-file-source "School" "~/org/Tasks-School.org" "#9B59B6")
    (cfw:org-create-file-source "Habit" "~/org/Habit.org" "darkseagreen"))))

(defun cj/space-menu-timeblock ()
  (interactive)
  (if (eq major-mode 'org-timeblock-mode)
      (major-mode-hydra)
    (org-timeblock)))



(defun cj/open-textbook ()
  (interactive)
  "Select textbook from '*cj/textbook-list*'. Uses 'alt-completing-read'"
  (let ((class (alt-completing-read "Class: " *cj/textbook-list*))
	(display-buffer-alist (list (cons "\\*Async Shell Command\\*.*"
					  (cons #'display-buffer-no-window nil)))))
    (async-shell-command (format "zathura %s" class))))

(defun cj/daily-capture-properties ()
  "Function to be used with =org-capture= and 'function' to find today's property drawer location."
  (interactive)
  (let* ((today (format-time-string "%Y-%m-%d %A"))
	 (year (format-time-string "%Y"))
	 (month (format-time-string "%Y-%m %B")))
    (find-file cj/journal-file)
    (with-current-buffer (find-file cj/journal-file)
      (goto-char (point-min))
      (unless (re-search-forward (format "^* %s" year) nil t)
	(goto-char (point-max))
	(insert (format "* %s\n" year)))
      (unless (re-search-forward (format "^** %s" month) nil t)
	(re-search-forward (format "^* %s" year) nil t)
	(forward-line)
	(insert (format "** %s\n" month)))
      (unless (re-search-forward (format "^*** %s" today) nil t)
	(re-search-forward (format "^** %s" month) nil t)
	(forward-line)
	(insert (format "*** %s\n" today)))
      (re-search-forward (format "^*** %s" today) nil t)
      (unless (re-search-forward ":PROPERTIES:" nil t)
	(end-of-line)
	(newline)
	(insert ":PROPERTIES:\n\n:END:")
	(previous-line))
      (if (re-search-forward ":Rating:" nil t)
	  (progn
	    (kill-whole-line)
	    (previous-line)
	    (newline)))
	
      )))


	  
