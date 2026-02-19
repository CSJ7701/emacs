;;; name: Corfu
;;; depends:
;;; conflicts:
;;; description: Popup completions. 


(use-package corfu
  :init (global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.0)
  (corfu-auto-prefix 1)
  (corfu-cycle t)
  (corfu-preselect 'prompt)
  (corfu-quit-at-boundary t)
  (corfu-quit-no-match t)
  (corfu-preview-current nil)
  :bind (
	 :map corfu-map
	 ("TAB" . corfu-next)
	 ("<tab>" . corfu-next)
	 ("S-TAB" . corfu-previous)
	 ("<backtab>" . corfu-previous)
	 ))

(use-package nerd-icons-corfu
  :config (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))


