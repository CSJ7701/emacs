
(use-package projectile
  :diminish projectile-mode
  :custom ((projectile-completion-system 'ivy)))
(use-package counsel-projectile)

(add-hook 'after-init-hook #'projectile-global-mode)

(general-def
  "C-c p" 'projectile-command-map)

(projectile-mode)
(counsel-projectile-mode)

(when (file-directory-p "~/projects/")
  (setq projectile-project-search-path '("~/projects/")))
(setq projectile-switch-project-action #'projectile-dired)