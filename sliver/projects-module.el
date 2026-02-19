;;; name: Projects
;;; depends: minibuffer
;;; conflicts:
;;; description: Built in projects.el. Integrates with consult (if 'minibuffer' module is loaded or 'consult' is instlalled)

(require 'project)

(setq project-switch-commands
      '((project-file-file "Find file")
	(project-dired "Dired")))

;; Default keybinds
(global-set-key (kbd "C-x p p") #'project-switch-project)
(global-set-key (kbd "C-x p f") #'project-find-file)
(global-set-key (kbd "C-x p b") #'project-switch-to-buffer)
(global-set-key (kbd "C-x p D") #'project-dired)
(global-set-key (kbd "C-x p k") #'project-kill-buffers)

(with-eval-after-load 'vc
  (add-to-list 'vc-directory-exclusion-list "__pycache__")
  (add-to-list 'vc-directory-exclusion-list ".venv")
  (add-to-list 'vc-directory-exclusion-list "node_modules"))


;; Consult Integration
(with-eval-after-load 'consult
  (global-set-key (kbd "C-x p b") #'consult-project-buffer))
