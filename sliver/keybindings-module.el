;;; name: Keybindings
;;; depends:
;;; conflicts:
;;; description: Setting vanilla keybindings.


;; --- Global keybindings ---
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "M-TAB") 'completion-at-point)
(global-set-key (kbd "C-h C-f") 'find-function-at-point)
(global-set-key (kbd "C-x C-f") 'find-file)

;; Windmove bindings (supporting multiple modifier combos)
(global-set-key (kbd "C-S-<left>")  'windmove-left)
(global-set-key (kbd "C-s-<left>")  'windmove-left)
(global-set-key (kbd "C-S-<right>") 'windmove-right)
(global-set-key (kbd "C-s-<right>") 'windmove-right)
(global-set-key (kbd "C-S-<up>")    'windmove-up)
(global-set-key (kbd "C-s-<up>")    'windmove-up)
(global-set-key (kbd "C-S-<down>")  'windmove-down)
(global-set-key (kbd "C-s-<down>")  'windmove-down)

;; Kill/yank standard keys
(global-set-key (kbd "C-w") 'kill-region)
(global-set-key (kbd "M-w") 'kill-ring-save)
(global-set-key (kbd "C-y") 'yank)
