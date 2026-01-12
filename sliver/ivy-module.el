
(use-package ivy
  :diminish
  :config (ivy-mode 1))
;(use-package ivy-rich
;  :init (ivy-rich-mode 1))
(use-package counsel
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-pretty)
  :config
  (counsel-mode 1))
(use-package ivy-prescient
  :after counsel
  :config (ivy-prescient-mode 1)
  (prescient-persist-mode 1)
  (setq prescient-filter-method '(literal regexp initialism)))

;; Remove the leading '^' from the initial input of counsel-M-x
;; (setcdr (assoc 'counsel-M-x ivy-initial-inputs-alist) "")
