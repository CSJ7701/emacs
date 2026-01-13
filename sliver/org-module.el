;;; name: Org
;;; depends:
;;; conflicts:
;;; description: Org mode core module

(use-package org
  :straight (:type built-in))

(use-package org-ql)
(use-package org-bullets)

(if (member "hydra" sliver--loaded-modules)
    (pretty-hydra-define+ space-menu
      (:foreign-keys warn :title "󰘧" :quit-key ("<escape>" "C-g"))
      (
       "Frequent"
       (("o" space-menu-org/body "   Org" :exit t))))
  )

(with-eval-after-load 'ivy
  (define-key org-capture-mode-map (kbd "C-c C-t") 'counsel-org-tag))

;; Standard Org Settings
(setq org-ellipsis " ▾")
(setq org-startup-with-inline-images t)
(setq org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●"))
(setq org-columns-default-format "%10TODO %50ITEM(Task) %20DEADLINE %20SCHEDULED %TAGS")
(setq org-image-actual-width nil)
(add-hook 'org-mode-hook #'org-bullets-mode)
(add-hook 'org-mode-hook #'cj/org-mode-setup)

;; Org Agenda Settings
(setq org-agenda-start-with-log-mode t)
(setq org-log-done 'time)
(setq org-log-into-drawer t)

;; Fix org mode angle bracket issue
(add-hook 'org-mode-hook #'cj/org-syntax-table-modify)

;; Save Org buffers after refiling and archiving!
(advice-add 'org-refile :after (lambda (&rest _) (org-save-all-org-buffers)))
(advice-add 'org-archive-subtree :after 'org-save-all-org-buffers)

;; Automatically archive done todos
(add-hook 'org-after-todo-state-change-hook 'cj/auto-archive-todos-ql)
(add-hook 'org-after-todo-state-change-hook 'org-save-all-org-buffers)


(setq org-tag-alist
      '((:startgrouptag)
	("Categories")
	(:grouptags)
	("Personal" . ?p)
	("Work" . ?w)
	("School" . ?s)
	(:endgroup)
	))

(setq org-archive-location "~/org/Archive.org::* From %s")

(setq org-refile-targets
      '(("Archive.org" :maxlevel . 1)
	(nil :maxlevel . 1)))

(setq org-agenda-files
      '("~/org/Mail.org"
	"~/org/Tasks.org"
	"~/org/Calendar.org"))

(setq org-todo-keywords ;; ! (log time), @ (add note), ^ (???)
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)" "CANC(c@)")
	(sequence "WAITING(W@)" "DEFERRED(D@)" "|" "CANC(c@)")
	(sequence "GOAL(g)" "ACTIVE(a)" "|" "DONE(d!)")
	))


(setq org-agenda-custom-commands
      '(("d" "Dashboard"
	 ((agenda ""
		  ((org-agenda-span 'day)
		   (org-agenda-start-on-weekday nil)
		   (org-agenda-overriding-header "Today")))
	  (todo "TODO|NEXT"
		((org-agenda-overriding-header "General")
		 ))
	  ))

	("n" "Next Actions"
	 ((tags-todo "+NEXT")
	  (tags-todo "+TODO")))

	("w" "Waiting/Deferred"
	 ((tags-todo "+WAITING")
	  (tags-todo "+DEFERRED")))

	("W" "Weekly Review"
	 ((agenda ""
		  ((org-agenda-overriding-header "Completed Tasks")
		   (org-agenda-skip-function '(org-agenda-skip-entry-if 'nottodo 'done))
		   (org-agenda-span 'week)))
	  (agenda ""
		  ((org-agenda-overriding-header "Scheduled/Unfinished")
		   (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
		   (org-agenda-span 'week)))
		  
	   ))
	))

(setq org-capture-templates
      `(
	("i" "Inbox" entry
	 (file+headline "~/org/Tasks.org" "Inbox")
	 "** TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n")

	("w" "Work" entry
	 (file+headline "~/org/Tasks.org" "Work")
	 "** TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n")

	("p" "Personal" entry
	 (file+headline "~/org/Tasks.org" "Personal")
	 "** TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n")

	("s" "School" entry
	 (file+headline "~/org/Tasks.org" "School")
	 "** TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n")

	("P" "Project" entry (function cj/capture-project-file)
	 "** TODO %?")

	("j" "Journal")
	("jt" "Today" plain (file+olp+datetree "~/org/Journal.org") "- %?" :tree-type day)
	("jd" "Select Date" plain (file+olp+datetree "~/org/Journal.org") "- %i %?" :time-prompt t)
	("jf" "Today's Rating" plain (function cj/daily-capture-properties) ":Rating: %^{How was today? (-3 to +3)}" :immediate-finish t)

	("C" "Cookbook")
	("Ci" "Internet Recipe" entry (file "~/org/cookbook.org")
	 "%(org-chef-get-recipe-from-url)"
	 :empty-lines 1)
	("cm" "Manual Recipe" entry (file "~/org/cookbook.org")
	 "* %^{Recipe title: }%(org-set-tags (read-string \"Tags (seperated with solon): \"))\n  :PROPERTIES:\n  :source-url:\n  :servings:\n  :prep-time:\n  :cook-time:\n  :ready-in:\n  :END:\n** Ingredients\n  %?\n** Directions\n\n")
	))



