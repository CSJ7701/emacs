;;; name: Java
;;; depends:
;;; conflicts:
;;; description:

(use-package groovy-mode)
(use-package lsp-java)
(add-hook 'java-mode-hook #'lsp)
(add-to-list 'auto-mode-alist '("\\.java\\'" . java-mode))
