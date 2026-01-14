;;; name: clippety
;;; depends:
;;; conflicts:
;;; description: Support clipboard over SSH

(use-package clipetty
  :ensure t
  :hook (after-init . global-clipetty-mode))



