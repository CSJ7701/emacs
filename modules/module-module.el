
(setq user-modules-dir (expand-file-name "modules" user-emacs-directory))
(defvar cj/all-modules '())
(defvar cj/loaded-modules '())
(defvar cj/unloaded-modules '())
(defvar cj/module-conflicts (make-hash-table :test 'equal)
  "Hash table mapping modules to list of conflicting modules.")
(defvar cj/init-file "~/.emacs.d/init.el")

(defun load-module (&optional module-name)
  "Load the specified module or prompt for a module to load."
  (interactive)
  (unless module-name
    (setq module-name (completing-read "Load module: " (mapcar (lambda (file) (replace-regexp-in-string "-module\\.el$" "" file)) (directory-files user-modules-dir nil "-module\\.el$")))))
  (let ((module-file (expand-file-name (format "%s-module.el" module-name) user-modules-dir)))
    (when (file-exists-p module-file)
      (load module-file)
      (add-to-list 'cj/loaded-modules module-name))))

(defun module-conflict (module conflict)
  "Record a conflict between MODULE and CONFLICT symettrically.
When a conflict with MODULE and CONFLICT is recorded, a conflict between CONFLICT and MODULE is also recorded."
  (when (and (member module cj/all-modules)
	     (member conflict cj/all-modules))
    (let ((module-conflicts (gethash module cj/module-conflicts nil))
	  (conflict-conflicts (gethash conflict cj/module-conflicts nil)))
      (unless (member conflict module-conflicts)
	(puthash module (cons conflict module-conflicts) cj/module-conflicts))
      (unless (member module conflict-conflicts)
	(puthash conflict (cons module conflict-conflicts) cj/module-conflicts)))))

(defun cj/open-module (&optional module-name)
  "Open the specified module file or prompt for a module to open."
  (interactive)
  (unless module-name
    (setq module-name (completing-read "Open module: " (mapcar (lambda (file) (replace-regexp-in-string "-module\\.el$" "" file)) (directory-files user-modules-dir nil "-module\\.el$")))))
  (let ((module-file (expand-file-name (format "%s-module.el" module-name) user-modules-dir)))
    (when (file-exists-p module-file)
      (find-file module-file))))

(defun cj/open-init ()
  (interactive)
  "Open the init.el file, specified by 'cj/init-file'"
  (find-file cj/init-file))

(defun cj/create-module (&optional module-name)
  "Create a module with the specified name or prompt for a module to create."
  (interactive)
  (unless module-name
    (setq module-name (read-string "Create module: ")))
  (let ((module-file (expand-file-name (format "%s-module.el" module-name) user-modules-dir)))
    (if (not (file-exists-p module-file))
	(find-file module-file)
      (message "Module \"%s\" already exists" module-name))))

(defun cj/insert-module (&optional module-name)
  "Insert the init string for the specified module, or prompt for a module."
  (interactive)
  (unless module-name
   (setq module-name (completing-read "Insert module: " (mapcar (lambda (file) (replace-regexp-in-string "-module\\.el$" "" file)) (directory-files user-modules-dir nil "-module\\.el$")))))
  (let ((module-file (expand-file-name (format "%s-module.el" module-name) user-modules-dir)))
    (when (file-exists-p module-file)
      (insert (format "(load-module \"%s\")" module-name)))))

(defun cj/init-module (&optional module-name)
  "Insert the init string for the specified module, then create that module. Prompts for module name if not passed"
  (interactive)
  (unless module-name
    (setq module-name (read-string "Create module: ")))
  (let ((module-file (expand-file-name (format "%s-module.el" module-name) user-modules-dir)))
    (if (not (file-exists-p module-file))
	(progn
	  (insert (format "(load-module \"%s\")" module-name))
	  (save-buffer)
	  (find-file module-file))
      (if (y-or-n-p "Module already exists. Insert anyway?")
	  (insert (format "(load-module \"%s\")" module-name))))))

(defun cj/calc-unused-modules ()
    "Fills variable =cj/all-modules= then calculates difference between =cj/all-modules= and =cj/loaded-modules=. =cj/loaded-modules= is filled on each call of (init-module)"
  (setq cj/all-modules (mapcar (lambda (file) (replace-regexp-in-string "-module\\.el$" "" file)) (directory-files user-modules-dir nil "-module\\.el$")))
  (setq cj/unloaded-modules (-difference cj/all-modules cj/loaded-modules)))



;; === Visualization ===
(require 'tabulated-list)

(defvar cj/module-table-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'cj/module-conflict-view)
    (define-key map (kbd "q") #'cj/quit-module-table)
    (define-key map (kbd "C-c C-c") #'cj/module-table-load-module)
    map)
  "Keymap for `cj/module-table-mode'.")

(defvar cj/module-conflict-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'kill-buffer-and-window)
    map)
  "Keymap for `cj/module-conflict-view-mode'.")

(define-derived-mode cj/module-table-mode tabulated-list-mode "Module Table"
  "Major mode for viewing loaded modules and their conflicts."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'cj/module-table-mode)
  (setq mode-name "Module Table")
  ;; Define the format of the table: 3 columns
  (setq tabulated-list-format
        [("Module" 20 t)        ;; Module name column, width 20
         ("Loaded" 10 t)        ;; Loaded status column, width 10
         ("Conflicts" 20 nil)]) ;; Conflicts column, width 20
  (setq tabulated-list-padding 2)
  (read-only-mode 1)
  (use-local-map cj/module-table-mode-map)
  (tabulated-list-init-header))

(define-derived-mode cj/module-conflict-view-mode special-mode "Conflict Sidebar" 
  "Major mode for a module's conflict view."
  (read-only-mode 1))
  
(defun cj/module-table-refresh ()
  "Refresh the module table, updating its entries."
  (setq-local tabulated-list-entries
              (mapcar (lambda (module)
                        (let* ((name module)
                               (loaded (if (member name cj/loaded-modules) "Yes" "No"))
                               (conflicts (gethash name cj/module-conflicts))
                               (conflict-count (length (or conflicts '())))
                               ;; Display plain text for the table, no properties
			       (conflict-summary (if conflicts
						     (if (get-text-property 0 'expanded (mapconcat #'identity conflicts ", "))
							 (mapconcat #'identity conflicts ", ")
						       (format "%d conflicts" conflict-count))
						   "0")))
			  (put-text-property 0 (length conflict-summary) 'expanded nil conflict-summary)
			  (list name (vector name loaded conflict-summary))))
		      cj/all-modules))
  (tabulated-list-print t))

(defun cj/module-conflict-view ()
  "Show a side window with detailed conflict information for the selected module."
  (interactive)
  (if (get-buffer "*Conflict Details*")
      (kill-buffer "*Conflict Details*"))
  (let* ((entry (tabulated-list-get-entry))
         (module-name (aref entry 0))
         (conflicts (gethash module-name cj/module-conflicts)))
    (if conflicts
        (let ((buf (get-buffer-create (format "*Conflict Details*"))))
          (with-current-buffer buf
            (erase-buffer)
            (insert (format "Module: %s\n\n" module-name))
            (insert "Conflicts:\n")
            (dolist (conflict conflicts)
              (insert (format "- %s\n" conflict)))
	    (goto-char (point-min))
	    (cj/module-conflict-view-mode))
          ;; Display the buffer in a side window.
          (display-buffer-in-side-window buf '((side . right)
                                                 (window-width . 40))))
      (message "Module %s has no conflicts." module-name))))

(defun cj/module-table ()
  "Display the module table to visualized loaded and unloaded modules."
  (interactive)
  (let ((buf (get-buffer-create "*Modules*")))
    (with-current-buffer buf
      (cj/module-table-mode)
      (cj/module-table-refresh)
      (switch-to-buffer buf))))

(defun cj/module-table-load-module ()
  "Load the module selected in the table."
  (interactive)
  (let* ((module (tabulated-list-get-id)))
    (if module
        (load-module module)
      (message "No module selected."))))

(defun cj/quit-module-table ()
  "Quit the module table buffer, and close the conflict sidebar if open."
  (interactive)
  (if (get-buffer "*Conflict Details*")
      (kill-buffer "*Conflict Details*"))
  (kill-buffer))

;; TODO:
;; Make this into a true package?
;; Possible option to display follow up message instead of error on conflict - although error may just be better (makes the conflict seem more meaningful, harder to miss)
