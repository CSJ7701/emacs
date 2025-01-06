;; -*- lexical-binding: t; -*-

(setq debug-on-error t)

;; Initial Variable Declarations
(defvar cj/default-font-size 120)
(defvar cj/default-variable-font-size 120)
(defvar cj/default-fixed-font-size 100)
(defvar cj/frame-active-transparency 100)
(defvar cj/frame-inactive-transparency 100)
(defvar cj/module-system-init-file "/home/csj7701/.emacs.d/modules/module-module.el")

;; Package System
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(setq package-install-upgrade-built-in t)

;; Initialize module system
(load-file cj/module-system-init-file)

;; Custom Variables
(load-module "variables")

;; Straight
(load-module "straight")

;; Automatically upgrade packages
(load-module "auto-package-update")

;; Autosaves and Backups
(setq backup-directory-alist '(("." . "~/.emacs.d/tmp/backups/")))
(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)
(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))

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
		cfw:calendar-mode-hook))
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

(setq nex-line-add-newlines t) ;; C-n etc add newlines at end of buffer

(load-module "general")
;; (load-module "evil")

;; Keybindings
(general-define-key
  ;; Global bindings
  "<escape>" 'keyboard-escape-quit
  "M-TAB" 'completion-at-point
  "C-h C-f" 'find-function-at-point
  "C-H-s-<left>" 'windmove-left
  "C-s-<left>" 'windmove-left
  "C-H-s-<right>" 'windmove-right
  "C-s-<right>" 'windmove-right
  "C-H-s-<up>" 'windmove-up
  "C-s-<up>" 'windmove-up
  "C-H-s-<down>" 'windmove-down
  "C-s-<down>" 'windmove-down
  "M-x" 'counsel-M-x
  "C-s" 'swiper
  "C-x b" 'counsel-switch-buffer
  "C-x C-f" 'counsel-find-file
  "M-/" 'evilnc-comment-or-uncomment-lines
  "C-SPC" 'space-menu/body
  "C-." 'set-mark-command
  "C-r" 'ivy-bibtex
  )

(general-define-key
  "C-w" 'kill-region
  "M-w" 'kill-ring-save
  "C-y" 'yank
  "M-y" 'counsel-yank-pop)

;; Mode Specific Keybindings
(general-define-key
 :keymaps 'org-capture-mode-map
 "C-c C-t" 'counsel-org-tag)
		    

;; Customizing the UI
(load-module "themes") 
(load-theme 'doom-nord-aurora t)
;(load-theme 'Pywal t)

;; Modeline and Minibuffer Improvements
(load-module "modeline")
(load-module "ivy")
(load-module "help")
(load-module "hydra")

;; Org
;;; org, org-roam
(load-module "org")
(load-module "roam")
(load-module "org-ranker")

;; Development
(load-module "undo-tree")
;; (load-module "lsp")
(load-module "yuck")
(load-module "python")
(load-module "web")
(load-module "lua")
(load-module "java")
(load-module "matlab")
(load-module "yuck")
(load-module "company")
(load-module "projectile")
(load-module "prescient")
(load-module "sqlite")
(load-module "magit")
(load-module "json")
(load-module "lisp")
(load-module "eshell")
(load-module "compile")
;; (load-module "github-copilot")
(load-module "ellama")
(load-module "csv")


;; Peripherals
;;; scratch, treemacs, dashboard, etc
(load-module "dashboard")
(load-module "sidebar")
(load-module "scratch")
(load-module "calfw")
(load-module "spellcheck")
(load-module "pdf-tools")
(load-module "space-menu")
(load-module "evil-nerd-commenter")
(load-module "rainbow-delimiters")
(load-module "browsers")
;; (load-module "mail")
(load-module "chat")
(load-module "dired")
(load-module "rss")
(load-module "ledger")
(load-module "music")
(load-module "ssh")
(load-module "cooking")
(load-module "authentication")

;; Functions
(load-file "/home/csj7701/.emacs.d/modules/cj-functions.el")
(load-file "/home/csj7701/.emacs.d/modules/emacs-functions.el")

(cj/calc-unused-modules)

(load-module "org-ranker")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("571661a9d205cb32dfed5566019ad54f5bb3415d2d88f7ea1d00c7c794e70a36" default))
 '(safe-local-variable-values
   '((python-shell-interpreter . "/home/csj7701/.virtualenvs/ServerHub/bin/")
     (eval progn
	   (org-babel-goto-named-src-block "startup")
	   (org-babel-execute-src-block)
	   (outline-hide-sublevels 1))
     (org-refile-targets
      (nil :tag . "Header"))
     (org-outline-path-complete-in-steps)
     (org-refile-use-outline-path . t)
     (org-refile-allow-creating-parent-nodes quote confirm)
     (eval add-hook 'after-save-hook
	   (lambda nil
	     (org-html-export-to-html t))
	   t t)))
 '(warning-suppress-log-types '((pdf-view)))
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil))))
 '(cfw:face-header ((t (:weight bold))))
 '(cfw:face-saturday ((t (:weight bold))))
 '(cfw:face-sunday ((t (:weight bold))))
 '(cfw:face-title ((t (:weight bold :height 2.0 :inherit variable-pitch))))
 '(cfw:face-toolbar-button-off ((t (:weight bold))))
 '(cfw:face-toolbar-button-on ((t (:weight bold))))
 '(org-agenda-date-today ((t (:foreground "SeaGreen2" :weight bold))))
 '(org-agenda-structure ((t (:foreground "#2e8b57"))))
 '(org-column ((t (:background "gray0" :strike-through nil :underline nil :slant normal :weight normal))))
 '(org-imminent-deadline ((t (:foreground "MediumPurple3" :weight bold :slant italic))))
 '(org-upcoming-deadline ((t (:foreground "LightPink" :slant oblique)))))
(put 'narrow-to-region 'disabled nil)
