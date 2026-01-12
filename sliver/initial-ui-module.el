;;; name: Initial UI
;;; depends:
;;; conflicts:
;;; description: Basic initial UI setup



;; Initial UI setup
(setq inhibit-startup-message t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 3)
(menu-bar-mode -1)
(setq visible-bell t)
(column-number-mode)
(global-display-line-numbers-mode 1)
(defalias 'yes-or-no-p 'y-or-n-p)

;;;; Transparency
(set-frame-parameter (selected-frame) 'alpha '(100 . 100))
(add-to-list 'default-frame-alist '(alpha . (100 . 100)))
(set-frame-parameter (selected-frame) 'fullscreen 'maximised)
(add-to-list 'default-frame-alist '(fullscreen . maximised))
(add-to-list 'default-frame-alist '(background-mode . dark))
(setq frame-alpha-lower-limit 50)

;;;; Window Dividers
(window-divider-mode)
(setq window-divider-default-places t
      window-divider-default-bottom-width 3
      window-divider-default-right-width 3)

;;;; Disable line numbers in some modes
(dolist (mode '(org-mode-hook
		org-agenda-mode
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook
		treemacs-mode-hook
		darkroom-mode-hook
		xwidget-webkit-mode-hook
		doc-view-mode
		mu4e-headers-mode
		cfw:calendar-mode-hook
		tabulated-list-mode))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


;; Fonts
(defun cj/set-font-faces ()
  (message "Setting Font Faces...")
  (set-face-attribute 'default nil :font "JetBrainsMono Nerd Font" :weight 'semibold :height cj/default-font-size)
  (set-face-attribute 'fixed-pitch nil :font "JetBrains Mono Nerd Font" :weight 'light :height cj/default-fixed-font-size)
  (set-face-attribute 'variable-pitch nil :font "JetBrains Mono Nerd Font" :weight 'regular :height cj/default-variable-font-size))

(if (daemonp)
    (add-hook 'after-make-frame-functions
	      (lambda (frame)
		(setq doom-modeline-icon t) ;; FIX ME
		(setq dashboard-set-heading-icons t)
		(setq dashboard-set-file-icons t)
		(dolist (mode '(org-mode-hook
				term-mode-hook
				shell-mode-hook
				eshell-mode-hook
				treemacs-mode-hook))
		  (add-hook mode (lambda () (display-line-numbers-mode 0))))
		(with-selected-frame frame
		  (cj/set-font-faces))))
  (cj/set-font-faces))

(setq next-line-add-newlines t) ;; C-n etc add newlines at end of buffer
