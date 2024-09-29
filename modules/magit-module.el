
(use-package magit
  ;; :custom
  ;; (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :straight t)

(setq magit-section-inhibit-markers nil)
(setq magit-section-preserve-visibility nil)
(setq magit-section-insert-in-reverse nil)
(setq magit-repository-directories
      '(("~/roam" . 1)
	("~/org" . 1)
	("~/.dotfiles" . 1)
	("~/class" . 1)
	("~/Projects" . 1)
	("~/Server" . 1)))
(setq magit-repolist-columns
      '(("Name" 25 magit-repolist-column-indent ())
	("  " 3 magit-repolist-column-unpulled-from-upstream ((:right-align t)))
	("  " 3 magit-repolist-column-unpushed-to-upstream ((:right-align t)))
	("" 99 magit-repolist-column-path nil)))

(use-package forge)

;; (setq package-install-upgrade-built-in t) 
