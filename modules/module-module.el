
(setq user-modules-dir (expand-file-name "modules" user-emacs-directory))
(defvar cj/all-modules '())
(defvar cj/loaded-modules '())
(defvar cj/unloaded-modules '())
(defvar cj/init-file "~/.dotfiles/.emacs.d/init.el")

(defun load-module (&optional module-name)
  "Load the specified module or prompt for a module to load."
  (interactive)
  (unless module-name
    (setq module-name (completing-read "Load module: " (mapcar (lambda (file) (replace-regexp-in-string "-module\\.el$" "" file)) (directory-files user-modules-dir nil "-module\\.el$")))))
  (let ((module-file (expand-file-name (format "%s-module.el" module-name) user-modules-dir)))
    (when (file-exists-p module-file)
      (load module-file)
      (add-to-list 'cj/loaded-modules module-name))))

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


;; TODO
;; Write a function that lets you visualize what modules are loaded vs unloaded
;; You could do this with 2 alists, "cj/modules-loaded" and "cj/modules-unloaded"
;; modules unloaded starts off as checking all properly named files in module directory and adding all of them, then you could add a line to the load function that adds each file/module to the "loaded" variable and checks the difference between the two variables (removing common elements from the unloaded var)
;; You could then create a 3rd variable that just lists ALL modules, and see if you can write a function that creates a temp org buffer with a table displaying module name, file path, loaded or not, and selecting can expand into a list of packages in that module plus variable definitions and functions defined in the function file.
