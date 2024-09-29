
(use-package company
  :after lsp-mode
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))
(use-package company-box)
(use-package company-prescient
  :after company
  :config (company-prescient-mode 1))

(add-hook 'lsp-mode 'company-mode)
(add-hook 'company-mode 'company-box-mode)

