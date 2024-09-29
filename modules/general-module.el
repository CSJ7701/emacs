
(use-package general
  :config
  (general-create-definer rune/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    ;; :global-prefix "C-SPC" ;; This line is for the original space menu
    ))
