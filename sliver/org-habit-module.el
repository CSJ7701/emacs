;;; name: Org Habit
;;; depends: org
;;; conflicts:
;;; description:

(require 'org-habit)
(add-to-list 'org-modules 'org-habit)

(setq org-habit-graph-column 60)
(setq org-habit-show-habits t)
(setq org-habit-show-habits-only-for-today nil)
(setq org-habit-completed-glyph 10003)
(setq org-habit-today-glyph 9671)
(setq org-habit-show-done-always-green t)
