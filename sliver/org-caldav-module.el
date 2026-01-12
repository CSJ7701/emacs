;;; name: Org-Caldav
;;; depends: org
;;; conflicts:
;;; description: Deprecated.

(use-package org-caldav)

;; Export TODOs
(setq org-icalendar-include-todo 'all
      org-caldav-sync-todo t)

;; URL of the caldav server
(setq org-caldav-url "http://192.168.1.172:5232/dav.php/calendars/shipley7701")

(setq org-caldav-calendars
      '((:calendar-id  "default" :inbox "/home/csj7701/org/Tasks-Personal.org" :caldav-todo-percent-states ((0 "TODO") (1 "GOAL") (100 "DONE")))
	(:calendar-id  "school" :inbox "/home/csj7701/org/Tasks-School.org" :caldav-todo-percent-states ((0 "TODO") (1 "HOMEWORK") (2 "STUDY") (3 "PROJECT") (4 "QUIZ") (5 "TEST") (100 "DONE")))
	(:calendar-id  "work" :inbox "/home/csj7701/org/Tasks-Work.org" :caldav-todo-percent-states ((0 "TODO") (100 "DONE")))
	))


