;;; name: lsp
;;; depends:
;;; conflicts:
;;; description:


(use-package lsp-mode
  :commands (lsp lsp-deferred))
(use-package lsp-ui)
;(use-package lsp-pyright)
(use-package lsp-treemacs)

(defun cj/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-enable t)
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (setq lsp-headerline-breadcrumb-icons-enable t)
  ; (lsp-headerline-breadcrumb-mode)
  (lsp-ui-mode))

(add-hook 'lsp-after-open-hook 'cj/lsp-mode-setup)
(add-hook 'python-mode-hook 'lsp-mode)
