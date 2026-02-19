;;; name: Minibuffer
;;; depends:
;;; conflicts:
;;; description: Configure the minibuffer (vertico + consult + additions)



(use-package vertico
  :init (vertico-mode))

(use-package orderless
  :init (setq completion-styles '(orderless basic)
	      completion-category-defaults nil
	      completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :init (marginalia-mode))

(use-package consult
  :bind
  (("C-s" . consult-line)
   ("C-x b" . consult-buffer)
   ("M-y" . consult-yank-pop)))

(use-package embark
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim))
  :init (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :after (embark consult))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config (setq which-key-idle-delay 1))

;; Completion / command keys
;(global-set-key (kbd "C-s") 'consult-line)           ;; replaces swiper
;(global-set-key (kbd "C-x b") 'consult-buffer)      ;; replaces counsel-switch-buffer
;(global-set-key (kbd "M-y") 'consult-yank-pop)      ;; replaces counsel-yank-pop
