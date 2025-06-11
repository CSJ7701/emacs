(use-package paredit)
(use-package geiser)
(use-package geiser-guile)

(add-hook 'scheme-mode-hook 'paredit-mode)
(add-hook 'scheme-mode-hook 'geiser-mode)
(add-hook 'scheme-mode-hook 'company-mode)

