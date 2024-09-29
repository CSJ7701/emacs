
(use-package doom-themes)
(use-package sublime-themes)
(use-package kaolin-themes)
(use-package autothemer)
(use-package ewal
  :init (ewal-load-colors))

(custom-set-faces
 '(org-agenda-date-today               ((t (:foreground "SeaGreen2" :weight bold))))
 '(org-agenda-structure                ((t (:foreground "#2e8b57"))))
 '(org-imminent-deadline               ((t (:foreground "MediumPurple3" :weight bold :slant italic))))
 '(org-upcoming-deadline               ((t (:foreground "LightPink" :slant oblique)))))

(custom-set-faces
 '(cfw:face-title                ((t (:weight bold :height 2.0 :inherit variable-pitch))))
 '(cfw:face-header               ((t (:weight bold))))
 '(cfw:face-sunday               ((t (:weight bold))))
 '(cfw:face-saturday             ((t (:weight bold))))
 '(cfw:face-toolbar-button-off   ((t (:weight bold))))
 '(cfw:face-toolbar-button-on    ((t (:weight bold)))))
