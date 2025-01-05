
(use-package magit
  ;; :custom
  ;; (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :straight t)
(use-package forge
  :after magit)

(setq magit-section-inhibit-markers nil)
(setq magit-section-preserve-visibility nil)
(setq magit-section-insert-in-reverse nil)
(setq magit-repository-directories
      '(("~/roam" . 1)
	("~/org" . 1)
	("~/.dotfiles" . 1)
	("~/.emacs.d" . 1)
	("~/class" . 1)
	("~/Projects" . 1)
	("~/Server" . 1)
	("~/School" . 1)
	("~/Personal" . 1)))

;; Update magit flag icons.
;; Original
;; '((magit-untracked-files . "N")
;;   (magit-unstaged-files . "U")
;;   (magit-staged-files . "S"))
(setq magit-repolist-column-flag-alist
      '((magit-modified-files . "  ")
	))

(setq magit-repolist-columns
      '(
	(" 󰈻 " 3 magit-repolist-column-flags ())
	("Name" 25 magit-repolist-column-ident ())
	;("Version" 25 magit-repolist-column-version ())
	("  " 3 magit-repolist-column-unpulled-from-upstream
	 ((:right-align t)
	  (:help-echo "Upstream changes not in branch")))
	("  " 3 magit-repolist-column-unpushed-to-upstream
	 ((:right-align t)
	  (:help-echo "Local changes not in upstream")))
	("  " 25 magit-repolist-column-branch
	 ((:right-aligh t)))
	("  " 99 magit-repolist-column-path ())))

(use-package forge)

;; (setq package-install-upgrade-built-in t) 
