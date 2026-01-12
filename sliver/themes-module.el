;;; name: Themes
;;; depends:
;;; conflicts:
;;; description: Install and set themes


(use-package doom-themes)
(use-package sublime-themes)
(use-package kaolin-themes)
(use-package autothemer)
(use-package ewal
  :init (ewal-load-colors))

(load-theme 'kaolin-valley-dark t)
