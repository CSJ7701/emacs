;;; name: Dashboard
;;; depends:
;;; conflicts:
;;; description:



(use-package page-break-lines)
(use-package dashboard)

(setq dashboard-startup-banner 'logo
      dashboard-banner-logo-title "Christian Johnson"
      dashboard-set-heading-icons t
      dashboard-set-file-icons t
      dashboard-set-navigator t
      dashboard-center-content t
      dashboard-icon-type 'nerd-icons
      dashboard-items '((recents . 5)
			(agenda . 5)
			(projects . 5))
      dashboard-week-agenda t)

;; Format: "(icon title help action face prefix suffix)"
(setq dashboard-navigator-buttons
      `(
	;; Line 1
	((,(all-the-icons-material "mail" :height 1.1 :v-adjust 0.0) "Mail" "Open Org Mail" (lambda (&rest _) (find-file "~/org/Mail.org")) )
	 (,(all-the-icons-material "fitness_center" :height 1.0 :v-adjust -0.2) "Fitness" "Open Org Fitness" (lambda (&rest _) (find-file "~/org/fitness.org")) )) 
	;; Line 2
	((,(all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0)
	  "Homepage" "Browse Homepage" (lambda (&rest _) (browse-url "homepage")))
	 ("?" "" "?/h" #'show-help nil "<" ">"))
	))
