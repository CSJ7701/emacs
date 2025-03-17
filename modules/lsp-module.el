
; These 2 lines fix an issue with 'project' and 'eglot'.
(straight-use-package '(project :type built-in))
(straight-use-package '(xref :type built-in))

(use-package lsp-mode
  :commands (lsp lsp-deferred))
(use-package eglot)
(use-package lsp-ui)

(defun cj/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode)
  (lsp-ui-mode))

(add-hook 'lsp-after-open-hook 'cj/lsp-mode-setup)
