
(use-package eshell)
(use-package eshell-git-prompt)

(with-eval-after-load 'esh-opt
  (setq eshell-destroy-buffer-when-process-dies t)
  (setq eshell-visual-commands '("htop" "zsh" "vim")))
(eshell-git-prompt-use-theme 'powerline)
