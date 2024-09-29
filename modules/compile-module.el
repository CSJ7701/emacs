
(setq compilation-scroll-output t)

(require 'ansi-color) ;; Show terminal colors in compile
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)


(defun python-compile ()
  "Set the defaule compile-command to run the current file with python"
  (setq-local compile-command
	      (concat "python "
		      (when buffer-file-name
			(shell-quote-argument buffer-file-name)))))
(add-hook 'python-mode-hook 'python-compile)

(defun latex-compile ()
  "Set the default compile-command to export the current latex file to pdf"
  (setq-local compile-command
	      (concat "latexmk -pdf "
		      (when buffer-file-name
			(shell-quote-argument buffer-file-name)))))
(add-hook 'latex-mode-hook 'latex-compile)
(add-hook 'LaTeX-mode-hook 'latex-compile)

(defun compile-send-input (input &optional nl)
  "Send INPUT to the current process.
Interactively also sends a terminatine newline."
  (interactive "MInput: \nd")
  (let ((string (concat input (if nl "\n"))))
    ;; Visual Feedback
    (let ((inhibit-read-only t))
      (insert-before-markers string))
    ;; Important Part
    (process-send-string
     (get-buffer-process (current-buffer))
     string)))
