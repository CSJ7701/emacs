;; -*- lexical-binding: t; -*-

(setq debug-on-error t)

;;; Load Straight and Sliver

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
                         user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; TODO: arbitrary 'if' predicate. Pass a method that must evaluate to nil or non-nil
;; TODO: sliver-run method. Same conditional logic as sliver load, but for running encapsulated method (potentially unneeded? Basically the same concept as loading a file... but maybe if you don't want to make a file for a small piece of code?
;; TODO: sliver-init-module needs to call sliver-create-module
;; TODO: Update readme to show lists for conditions
(use-package sliver
  :straight (sliver :type git :host github :repo "CSJ7701/Sliver"))
(setq sliver-modules-dir (expand-file-name "sliver" user-emacs-directory))

(setq sliver-machine-profiles
      '(("Desktop" . (:hostname ("Glamdring" "Anduril")))
	("Server" . (:hostname ("Aiglos")))
	("Guix" . (:hostname ("Aiglos")))
	("Ellama-Machines" . (:hostname ()))
	))

;; Initial Setup
(sliver-load "variables")
(sliver-load "autosaves")
(sliver-load "initial-ui")
(sliver-load "keybindings")
(sliver-load "themes")
(sliver-load "emacs-functions")

;; Modeline and Minibuffer
(sliver-load "modeline")
(sliver-load "minibuffer")
(sliver-load "hydra")

;; Org
;;; Come back to this. This will probably need to be refactored pretty significantly
(sliver-load "org")
(sliver-load "org-functions")
(sliver-load "org-extras")
(sliver-load "org-latex-extra")
(sliver-load "org-ref")

;; Org Roam
(sliver-load "org-roam")

;; Org Peripherals
(sliver-load "org-protocol" :profile "Desktop")
(sliver-load "org-habit")
(sliver-load "org-youtube" :profile "Desktop")
(sliver-load "org-toc")
(sliver-load "org-notifications" :profile "Desktop") ;; Empty.
;; Run at startup
(cj/org-font-setup)


;; Development Utilities
(sliver-load "dired")
(sliver-load "undo-tree" :profile "Desktop")
(sliver-load "lsp" :profile "Desktop")
(sliver-load "corfu")
(sliver-load "projects")
(sliver-load "magit")
(sliver-load "eshell")
(sliver-load "eat")
(sliver-load "compile")
(sliver-load "ssh")
(sliver-load "authentication")
(sliver-load "rainbow-delimiters")
(sliver-load "ellama" :profile "Ellama-Machines")

;; Language configurations
(sliver-load "python") ;;; Redo this module
(sliver-load "web")
(sliver-load "docker")
(sliver-load "csv")
(sliver-load "ledger")
(sliver-load "ledger-functions")
(sliver-load "guile" :profile "Guix")
;;; (sliver-load "matlab")
;;; (sliver-load "tridactyl")
;;; (sliver-load "java")
;;; (sliver-load "lua")
;;; (sliver-load "yuck")

;; Productivity
;;; Dashboard (productivity)
(sliver-load "sidebar")
(sliver-load "scratch")
(sliver-load "calfw" :profile "Desktop")
(sliver-load "spellcheck")
(sliver-load "pdf-tools" :profile "Desktop")

;; Peripherals
(sliver-load "chat" :profile "Desktop")
(sliver-load "rss" :profile "Desktop")
(sliver-load "cooking" :profile "Desktop")
(sliver-load "random")

;; NEXT

;;; Look at 'functions' files
;;; Look at unused modules in current config



;; REFACTOR INTO A MODULE


(custom-set-faces
 '(org-agenda-date-today               ((t (:foreground "SeaGreen2" :weight bold))))
 '(org-agenda-structure                ((t (:foreground "#2e8b57"))))
 '(org-imminent-deadline               ((t (:foreground "MediumPurple3" :weight bold :slant italic))))
 '(org-upcoming-deadline               ((t (:foreground "LightPink" :slant oblique)))))

					; (define-key org-capture-mode-map (kbd "C-c C-t") 'counsel-org-tag)




(use-package citar
  :after vertico
  :bind
  (("C-c b" . citar-insert-citation)) ;; insert citation into buffer
   ;; optionally: find references
  :custom
  (citar-bibliography '("~/path/to/your/library.bib"))
  (citar-library-paths '("~/path/to/your/pdfs"))
  (citar-notes-paths '("~/path/to/your/notes")))

