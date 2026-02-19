;;; name: undo-tree
;;; dependency:
;;; conflict:
;;; description:


(use-package undo-tree)

;; Do not need a dependency here, since add-hook doesn't fail if the mode doesn't exist
(add-hook 'lsp-mode-hook #'undo-tree-mode)
