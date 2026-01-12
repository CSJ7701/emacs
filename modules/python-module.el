
(use-package python-mode
  :custom (python-shell-interpreter "python3"))
;(use-package lsp-pyright
;  :hook (python-mode . (lambda ()
;			 (require 'lsp-pyright)
;                         (lsp))))

(use-package pyvenv
  :ensure t
  :init
  (setenv "WORKON_HOME" "~/.virtualenvs/")
  :config
  (setq pyvenv-post-activate-hooks
	(list (lambda ()
		(setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python")))))
  (setq pyvenv-post-deactivate-hooks
	(list (lambda ()
		(setq python-shell-interpreter "python3")))))

(use-package poetry)


;(add-hook 'python-mode 'lsp-mode)
;(add-hook 'python-mode '(lambda () (require 'lsp-pyright) (lsp)))
(add-hook 'python-mode 'pyvenv-mode)
(add-hook 'python-mode 'eglot-ensure)

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
	       '(python-mode . ("jedi-language-server"))))
