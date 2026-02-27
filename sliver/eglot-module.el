;;; name: eglot
;;; depends:
;;; conflicts:
;;; description:

;; Fix issues with 'project' and 'eglot'
(straight-use-package '(project :type built-in))
(straight-use-package '(xref :type built-in))

(use-package eglot
  :hook (python-mode . eglot-ensure)
  )

(with-eval-after-load 'eglot
  (push '(python-mode "pylsp") eglot-server-programs))


