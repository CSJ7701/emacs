(defvar org--inhibit-version-check t)
(load-file (expand-file-name "org-functions.el" user-modules-dir))

(use-package org
  :straight (:type built-in)
  :config
  (add-hook 'org-mode-hook #'cj/org-mode-setup)
  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  )
(use-package org-ql)
(use-package org-bullets)
(require 'org-protocol)
					;(use-package org-agenda-property)

(load-module "org-contrib")
(load-module "org-babel")
(load-module "org-latex")
(load-module "org-yt")
(load-module "org-notifications")
(load-module "org-ref")
(load-module "org-caldav")
(load-module "org-toc")

(setq org-ellipsis " ▾")
(setq org-startup-with-inline-images t)
(setq org-agenda-start-with-log-mode t)
(setq org-log-done 'time)
(setq org-log-into-drawer t)
(setq org-habit-graph-column 60)
(setq org-habit-show-habits t)
(setq org-habit-show-habits-only-for-today nil)
(setq org-habit-completed-glyph 10003)
(setq org-habit-today-glyph 9671)
(setq org-habit-show-done-always-green t)
(setq org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●"))
(setq org-columns-default-format "%10TODO %50ITEM(Task) %20DEADLINE %20SCHEDULED %TAGS")
;(setq org-agenda-property-list '("DEADLINE" "SCHEDULED"))
(setq org-image-actual-width nil)

;; Save Org buffers after refiling and archiving!
(advice-add 'org-refile :after (lambda (&rest _) (org-save-all-org-buffers)))
(advice-add 'org-archive-subtree :after 'org-save-all-org-buffers)
;; Automatically archive done todos
(add-hook 'org-after-todo-state-change-hook 'cj/auto-archive-todos-ql)
(add-hook 'org-after-todo-state-change-hook 'org-save-all-org-buffers)
(add-hook 'org-mode-hook #'org-bullets-mode)


(setq org-tag-alist
      '((:startgrouptag)
	("S24")
	(:grouptags)
	("EngineeringEthics" . ?E)
	("Professional Maritime Officer" . ?M)
	("IndustrialControls" . ?I)
	("GlobalChallenges" . ?G)
	("Capstone" . ?C)
        (:endgroup)
	
        ("@errand" . ?e)
        ("@home" . ?h)
        ("@work" . ?w)
        ("agenda" . ?a)
        ("planning" . ?p)
        ("publish" . ?P)
        ("batch" . ?b)
        ("note" . ?n)
        ("idea" . ?i)))


(setq org-refile-targets
      '(("Archive.org" :maxlevel . 1)
        ("Tasks-Personal.org" :maxlevel . 1)
        ("Tasks-School.org" :maxlevel . 1)
        ("Tasks-Work.org" :maxlevel . 1)
        (nil :maxlevel . 1)))


(setq org-agenda-files
      '("~/org/Mail.org"
        "~/org/Tasks-Personal.org"
        "~/org/Tasks-School.org"
        "~/org/Tasks-Work.org"
        "~/org/Habit.org"))


(setq org-todo-keywords  ;; ! means log time , @ means add note , ^ means ???
      '((sequence "TODO(t)" "NEXT(n)" "LOW(l)" "DEFER(F@)" "|" "DONE(d!)" "WAITING(w@)" "CANC(c@)") ;;This is for actual tasks, stuff that I need to complete
        (sequence "HOMEWORK(h)" "STUDY(s)" "PROJECT(p)" "|" "DONE(d!)" "MISSED(m!)") ;; Homework stuff, add as needed
    	(sequence "QUIZ(q)" "TEST(T)" "|" "DONE(d!)")
        (sequence "DUTY(u)" "|" "DONE(d)") ;; CG Stuff
        (sequence "EVENT(e@)" "RESCHEDULE(r@)" "FOLLOW UP(f@)" "|" "DONE(d!)" "CANC(c!)")
        (sequence "GOAL(G)" "SHORT-TERM(S)" "|" "DONE(d!)")))


(setq org-todo-keyword-faces
      '(("HOMEWORK" . (:foreground "rebeccapurple"))
        ("STUDY" . (:foreground "mediumpurple" :weight bold))
        ("PROJECT" . (:foreground "purple")) ;; Homework, theme color is purple
        ("TODO" . (:foreground "darkseagreen")) 
        ("INBOX" . (:foreground "darkseagreen")) ;; General entries
        ("NEXT" . (:foreground "yellow" :weight bold))
        ("FOLLOW UP" . (:foreground "yellow")) ;; Immediate Steps
        ("MISSED" . (:foreground "red" :weight bold))
        ("RESCHEDULE" . (:foreground "red" :weight bold))
        ("DEFER" . (:foreground "red")) ;; Urgent - re-evaluate
        ("WAITING" . (:foreground "pink"))
        ("DUTY" . (:foreground "royalblue"))
        ("EVENT" . (:foreground "royalblue"))
        ("DONE" . (:foreground "slategrey" :weight bold))
        ("CANC" . (:foreground "slategrey"))
        ("GOAL" . (:foreground "#2e8b57"))
        ("SHORT-TERM" . (:foreground "springgreen")) ;; Goals
        ))



;; Configure custom agenda views
(setq org-agenda-block-separator "══════════════════════")
(setq org-agenda-compact-blocks t)
 

           
(setq org-agenda-custom-commands
      '(
        ("s" "School Planner"
         ((todo "HOMEWORK"
    		((org-agenda-todo-ignore-deadlines 'future)
    		 (org-agenda-overriding-header "Overdue")
		 (org-agenda-sorting-strategy '(deadline-up)))
    	  (tags-todo "DEADLINE>=\"<today>\"+DEADLINE<=\"<+10d>\"/!+HOMEWORK|+STUDY|+PROJECT|+DUTY|+QUIZ|+TEST"
    		     ((org-agenda-overriding-header "Due Next")
    		      (org-agenda-sorting-strategy '(deadline-up))))
          (todo "STUDY|QUIZ|TEST"
                ((org-agenda-overriding-header "Study")))
    	  (todo "HOMEWORK"
                ((org-agenda-overriding-header "All Homework"))))))
	
        ("p" "Projects"
         ((agenda ""
                  ((org-agenda-time-grid nil)
                   (org-agenda-show-all-dates nil)
                   (org-agenda-files (cj/org-roam-list-notes-by-tag "Project"))))
  	  (todo ""
  		((org-agenda-files (cj/org-roam-list-notes-by-tag "Project")))))
        (todo ""
              ((org-agenda-overriding-header "\n Dotfiles")
               (org-agenda-files '("/home/csj7701/roam/20230729081016-dotfiles.org"))
               (org-agenda-prefix-format "%i  ")))
        (todo ""
              ((org-agenda-overriding-header "\n Startpage")
               (org-agenda-files '("/home/csj7701/roam/20230729080958-emacs_startpage.org"))
               (org-agenda-prefix-format "%i  ")))
        (todo ""
              ((org-agenda-overriding-header "\n Other Projects")
               (org-agenda-files (seq-difference (cj/org-roam-list-notes-by-tag "Project") '("/home/csj7701/roam/20230729080958-emacs_startpage.org" "/home/csj7701/roam/20230729081016-dotfiles.org" "/home/csj7701/roam/20230729080922-jarvis.org"))))))

	("d" "Dashboard"
	 ((agenda "-Linear-ProbTheory-Antennas"
                  ((org-agenda-time-grid nil)
                   (org-agenda-span 'day)
                   (org-agenda-skip-scheduled-if-deadline-is-shown 'repeated-after-deadline)
                   (org-agenda-time-grid (quote ((daily today remove-match) (0800 1200 1600 2000) "╌╌╌╌╌╌╌╌╌╌  " "")))
                   (org-agenda-current-time-string " ◄───── Now ")
                   (org-agenda-time-leading-zero t)
                   (org-agenda-prefix-format "  %?-12t")
                   (org-agenda-overriding-header "Today")
    		   (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("HOMEWORK" "STUDY")))))
          (tags-todo "DEADLINE>=\"<today>\"+DEADLINE<=\"<+10d>\"/!+HOMEWORK|+STUDY|+PROJECT|+DUTY|+QUIZ|+TEST"
                     ((org-agenda-overriding-header "\n School")
                      (org-agenda-prefix-format "%i ")
   		      (org-agenda-sorting-strategy '(deadline-up))
   		      (org-agenda-remove-tags t)))
          (todo "TODO|NEXT|EVENT|RESCHEDULE|FOLLOW UP"
                ((org-agenda-overriding-header "\n General")
                 (org-agenda-prefix-format "%i   ")
                 (org-agenda-tag-filter-preset '("-project")) ;; Add excluded tags here (project tags likely best)
                 )))
    	 ((org-agenda-tag-filter-preset '("-Appointment"))))

        (".z" "Startpage Deadline View"
         ((org-ql-block '(and (todo)
                              (deadline :to -1))
                        ((org-ql-block-header "  󰪻")))
          (org-ql-block '(and (or (todo "NEXT")
                                  (todo "RESCHEDULE")
                                  (todo "FOLLOW UP")
                                  (deadline :from 0 :to 7))
                              (not (or (todo "DUTY")
    				       (todo "EVENT")
    				       (todo "HOMEWORK")
    				       (todo "STUDY")
    				       (todo "PROJECT"))))
                        ((org-ql-block-header "  󰳹  ")))
          (org-ql-block '(or (todo "DUTY")
                             (todo "EVENT"))
                        ((org-ql-block-header "  ")))
          (org-ql-block '(and (or (todo "HOMEWORK")
    				  (todo "STUDY")
    				  (todo "PROJECT"))
    			      (deadline :from 0 :to 10))
    			((org-ql-block-header "  󰑴")))
          (org-ql-block '(or (todo "GOAL")
                             (todo "SHORT-TERM"))
                        ((org-ql-block-header "  ")))))
	
        (".Z" "Startpage All View"
         ((org-ql-block '(and (todo)
                              (not (todo "GOAL"))
                              (not (todo "SHORT-TERM"))
    			      (not (todo "HABIT"))
    			      (not (todo "MONTHLY"))
                              (not (deadline)))
                        ((org-ql-block-header "  ")))))
	
    	("S" "Class Schedule" agenda ""
    	 ((org-agenda-span 8)
    	  (org-agenda-start-on-weekday nil)
    	  (org-agenda-skip-scheduled-if-done t)
    	  (org-agenda-start-day "-1d")
    	  (org-agenda-prefix-format " %i %-12:c%?-12t% s")
    	  (org-agenda-time-grid nil)
    	  (org-agenda-property-list '("LOCATION" "TEACHER"))
	  (org-agenda-overriding-columns-format "%ITEM %SCHEDULED %LOCATION %TEACHER")
    	  (org-agenda-remove-tags t)
    	  (org-scheduled-past-days 0)
    	  (org-agenda-tag-filter-preset '("+Appointment"))
    	  (org-agenda-sorting-strategy '(timestamp-up))))

        ("q" "Appointments"
    	 ((agenda ""
    		  ((org-agenda-span 'week)
    		   (org-agenda-start-on-weekday nil)
    		   (org-agenda-start-day "-1d")
    		   (org-agenda-time-grid nil)
    		   (org-agenda-tag-filter-preset '("+Appointment"))
    		   (org-agenda-view-columns-initially t)
    		   (org-agenda-overriding-columns-format "%ITEM %SCHEDULED")
    		   (org-agenda-sorting-strategy '(timestamp-up))
    		   (org-agenda-columns-active nil)))))))


;; Capture Template Helper
;;; Now defined in variable module
;; (defconst *cj/ledger-file* (format-time-string "~/Personal/Ledger/Year/%y.ledger"))
(defun cj/capture-project-file ()
  "Return a filename for a roam project file"
  (let* (
	 (file (org-roam-node-file (org-roam-node-read nil (cj/org-roam-filter-by-tag "Project")))))
    (set-buffer (org-capture-target-buffer file))
    (goto-char (point-min))
    (if (re-search-forward
	 (format org-complex-heading-regexp-format (regexp-quote "Inbox")) nil t)
	(goto-char (point-at-bol))
      (goto-char (point-max)))))
    

;; (defvar cj/journal-file "~/org/Journal.org")
(setq org-capture-templates
      `(
    	("p" "Personal")
    	("pt" "Task" entry (file+olp "~/org/Tasks-Personal.org" "Inbox")
    	 "* TODO %?\n %a\n %i" :empty-lines 1)
    	("pr" "Reminder" entry (file+olp "~/org/Tasks-Personal.org" "Reminders")
    	 "* TODO %?\n %^t \n :PROPERTIES: \n :ORG-TIMED-ALERTS: \n :CATEGORY: Personal \n :END: \n %i" :empty-lines 1)
    	("pa" "Appointment" entry (file+olp "~/org/Tasks-Personal.org" "Appointments")
    	 "* %? \n SCHEDULED: %^t")
	
	("pp" "Project" entry (function cj/capture-project-file)
	 "** TODO %?")

	("j" "Journal")
	("jt" "Today" plain (file+olp+datetree "~/org/Journal.org") "- %?" :tree-type day)
	("jd" "Select Date" plain (file+olp+datetree "~/org/Journal.org") "- %i %?" :time-prompt t)
	("jf" "Today's Rating" plain (function cj/daily-capture-properties) ":Rating: %^{How was Today? (-3 to +3)}" :immediate-finish t)

    	("w" "Work")
    	("wt" "Task" entry (file+olp "~/org/Tasks-Work.org" "Inbox")
    	 "* TODO %?\n %a\n %i" :empty-lines 1)
    	             
    	("s" "School")
	("sT" "Task" entry (file+olp "~/org/Tasks-School.org" "Homework")
	 "* TODO %?")
    	("sh" "Homework" entry (file+olp "~/org/Tasks-School.org" "Homework")
    	 "* HOMEWORK %?\n DEADLINE: %^t")
    	("sp" "Project" entry (file+olp "~/org/Tasks-School.org" "Project")
    	 "* PROJECT %?\n DEADLINE: %^t")
    	("ss" "Study" entry (file+olp "~/org/Tasks-School.org" "Study")
    	 "* STUDY %?\n SCHEDULED: %^t")
    	("st" "Test" entry (file+olp "~/org/Tasks-School.org" "Study")
    	 "* TEST %?\n DEADLINE: %^t")
    	("sq" "Quiz" entry (file+olp "~/org/Tasks-School.org" "Study")
    	 "* QUIZ %?\n DEADLINE: %^t")
    	("sa" "Appointment" entry (file+olp "~/org/Tasks-School.org" "Appointments")
    	 "* %? \n SCHEDULED: %^t")
	
        ("m" "Email Workflow")
        ("mf" "Follow Up" entry (file+olp "~/org/Mail.org" "Follow Up")
         "* TODO Follow up on %a \n SCHEDULED:%t \n DEADLINE:%(org-insert-time-stamp (org-read-date nil t \"+2d\")) \n From: %:fromname On: %:date \n\n %i" :immediate-finish t)
        ("mr" "Read Later" entry (file+olp "~/org/Mail.org" "Read Later")
         "* TODO Read %a \n SCHEDULED:%t \n DEADLINE:%(org-insert-time-stamp (org-read-date nil t \"+2d\")) \n From: %:fromname On: %:date \n\n%i" :immediate-finish t)
	
        ("l" "Ledger")
	("lu" "USAA" plain (file ,(format "~/Personal/Ledger/Year/%s.ledger" (format-time-string "%y")))
	 "\n\n%(org-read-date) * %^{Payee} \n    Expenses:%^{Account}     $%^{Amount} \n    Assets:USAA")
	("ln" "Navy Fed" plain (file ,(format "~/Personal/Ledger/Year/%s.ledger" (format-time-string "%y")))
	 "\n\n%(org-read-date) * %^{Payee} \n    Expenses:%^{Account}     $%^{Amount} \n    Assets:Navy Federal")
	("la" "AMEX" plain (file ,(format "~/Personal/Ledger/Year/%s.ledger" (format-time-string "%y")))
	 "\n\n%(org-read-date) * %^{Payee} \n    Expenses:%^{Account}     $%^{Amount} \n    Liabilities:AMEX")
	
        ("i" "Org Protocol")
        ("is" "Web site" entry (file "~/roam/inbox.org")
         "* %:description
      :PROPERTIES:
      :Title:  %:description
      :Link:  %:link
      :END:

      %:initial" :empty-lines 1)
    	("ib" "Bookmark" entry (file+headline "~/org/org-linkz/index.org" "INBOX")
    	 "* %a %U"
    	 :immediate-finish t)
	("c" "Cookbook")
	("ci" "Internet Recipe" entry (file "~/org/cookbook.org")
	 "%(org-chef-get-recipe-from-url)"
	 :empy-lines 1)
	("cm" "Manual Recipe" entry (file "~/org/cookbook.org")
	 "* %^{Recipe title: }%(org-set-tags (read-string \"Tags (seperated with colon): \"))\n  :PROPERTIES:\n  :source-url:\n  :servings:\n  :prep-time:\n  :cook-time:\n  :ready-in:\n  :END:\n** Ingredients\n   %?\n** Directions\n\n")
	))

(cj/org-font-setup)


