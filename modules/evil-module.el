
(use-package evil
  :init (setq evil-want-keybinding nil))

(setq evil-want-integration t)
(setq evil-want-C-u-scroll t)
(setq evil-want-C-i-jump nil)
(setq-default evil-cross-lines t)

(evil-mode 1)

(general-def 'insert
  "C-g" 'evil-normal-state)

(general-def 'insert 'normal
  "C-." 'set-mark-command)

(general-def 'motion
  "j" 'evil-next-visual-line
  "k" 'evil-previous-visual-line)

(general-define-key
 :states 'normal
 "<remap> <evil-next-line>" 'evil-next-visual-line
 "<remap> <evil-previous-line>" 'evil-previous-visual-line
 :states 'motion
 "<remap> <evil-next-line>" 'evil-next-visual-line
 "<remap> <evil-previous-line>" 'evil-previous-visual-line)

(evil-set-initial-state 'messages-buffer-mode 'normal)
(evil-set-initial-state 'dashboard-mode 'normal)

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))
