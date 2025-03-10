
(use-package eshell)
(use-package eshell-git-prompt)

(when (require 'eat nil 'noerror)
  (add-hook 'eshell-load-hook #'eat-eshell-mode))

(with-eval-after-load 'esh-opt
  (setq eshell-destroy-buffer-when-process-dies t)
  (setq eshell-visual-commands '("htop" "zsh" "vim")))
(eshell-git-prompt-use-theme 'powerline)
