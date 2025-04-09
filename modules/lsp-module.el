
(use-package lsp-mode
  :commands (lsp lsp-deferred))
(use-package eglot)
(use-package lsp-ui)
(use-package lsp-pyright)

(defun cj/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode)
  (lsp-ui-mode))

(add-hook 'lsp-after-open-hook 'cj/lsp-mode-setup)
