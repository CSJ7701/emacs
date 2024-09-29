(use-package copilot)

(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)

(add-hook 'prog-mode-hook 'copilot-mode)

  
