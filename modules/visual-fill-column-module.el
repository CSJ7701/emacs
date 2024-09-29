(use-package visual-fill-column
  :after org
  :hook (org-mode . cj/org-mode-visual-fill)
  :config (add-hook 'org-mode-hook #'cjorg-mode-visual-fill))
