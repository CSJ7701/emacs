;;; name: Random
;;; depends:
;;; conflicts:
;;; description: Random functions and variables that don't fit anywhere else


(defun cj/open-textbook ()
  (interactive)
  "Select textbook from '*cj/textbook-list*'. Uses 'alt-completing-read'"
  (let ((class (alt-completing-read "Class: " *cj/textbook-list*))
	(display-buffer-alist (list (cons "\\*Async Shell Command\\*.*"
					  (cons #'display-buffer-no-window nil)))))
    (async-shell-command (format "zathura %s" class))))
