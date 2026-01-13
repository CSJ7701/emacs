;;; name: hydra
;;; depends:
;;; conflicts:
;;; description: Hydra, Major Mode Hydra, and my custom space menu


(use-package hydra)
(use-package major-mode-hydra)

(global-set-key (kbd "C-SPC") 'space-menu/body)

;;; ============================================================================
;;; Basic Space Menu
;;; ============================================================================

;(cj/space-menu-bulk-add
; '(;; Always available entries
;   (t ("Menus" "SPC" major-mode-hydra "   Major Mode" :exit t)
;      ("Menus" "C" space-menu-config/body "   Config" :exit t)
;      ("Menus" "G" space-menu-goto/body "   Goto" :exit t))
   
;   ;; Feature-specific entries
;   (org ("Frequent" "o" space-menu-org/body "   Org" :exit t))
;   (org-roam ("Frequent" "r" space-menu-roam/body "   Roam" :exit t))
;   (magit ("Frequent" "g" space-menu-git/body "  󰊢 Git" :exit t))
;   (mu4e ("Utilities" "m" space-menu-mu4e/body "  󰊫 Mu4E" :exit t))
;   (elfeed ("Utilities" "e" space-menu-elfeed/body "   Elfeed" :exit t))
;   (ellama ("Utilities" "A" space-menu-ellama/body "   Ellama" :exit t))
					;   ))

(pretty-hydra-define space-menu
  (:foreign-keys warn :title "󰘧" :quit-key ("<escape>" "C-g"))
  (
   "Frequent"
   ()
   "Menus"
   (("SPC" major-mode-hydra " Major Mode" :exit t)
    ("C" space-menu-config/body "   Config" :exit t)
    ("G" space-menu-goto/body "   Goto" :exit t))
   "Utilities"
   ()))
    

;;; ============================================================================
;;; Sub-Menu Definitions - Org/Roam
;;; ============================================================================

(pretty-hydra-define space-menu-org 
  (:foreign-keys warn :title "󰘧 Org" :quit-key ("<escape>" "C-g"))
  ("Frequent Actions"
   (("c" org-capture "Capture" :exit t)
    ("i" cj/org-insert-image-link "Image Insert" :exit t)
    ("E" cj/open-excalidraw "Excalidraw" :exit t)
    ("s" org-store-link "Store Link" :exit t)
    ("l" org-insert-link "Insert Link" :exit t))

   "Utilities"
   (("a" org-agenda "Agenda" :exit t)
    ("A" space-menu-org-agenda/body "Custom Agenda" :exit t)
    ("t" cj/space-menu-timeblock "Timeblock" :exit t))

   "References"
   (("R" org-ref-insert-link-hydra/body "Org Ref Link Hydra" :exit t)
    ("r" org-ref-insert-link "Org Ref Link" :exit t))))

(pretty-hydra-define space-menu-org-agenda 
  (:foreign-keys warn :title "󰘧 Org Agenda" :quit-key ("<escape>" "C-g"))
  ("Frequent"
   (("d" (cj/dashboard-agenda) "Dashboard" :exit t)
    ("n" (cj/next-agenda) "Next" :exit t)
    ("w" (cj/waiting-agenda) "Waiting" :exit t)
    ("p" (cj/project-agenda) "Projects" :exit t))))


(pretty-hydra-define space-menu-roam 
  (:foreign-keys warn :title "󰘧 Roam" :quit-key ("<escape>" "C-g"))
  ("Frequent"
   (("F" cj/roam-find-file-layout "Open in Roam tab" :exit t)
    ("f" org-roam-node-find "Open node" :exit t)
    ("t" org-roam-tag-add "Add Tag" :exit t)
    ("a" org-roam-alias-add "Alias Add" :exit t))
   
   "Utilities"
   (("T" cj/capture-daily-node "Capture today" :exit t)
    ("p" cj/org-roam-find-project "Find project" :exit t))))

;;; ============================================================================
;;; Sub-Menu Definitions - Git
;;; ============================================================================

(pretty-hydra-define space-menu-git 
  (:foreign-keys warn :title "󰘧 Git" :quit-key ("<escape>" "C-g"))
  ("Frequent"
   (("s" magit-status "Status" :exit t)
    ("S" magit-list-repositories "Status All" :exit t)
    ("c" magit-commit "Commit" :exit t)
    ("p" magit-push "Push" :exit t)
    ("f" magit-pull-from-pushremote "Pull" :exit t))
   
   "Utilities"
   (("R" magit-remote-add "Add Remote" :exit t))))

;;; ============================================================================
;;; Sub-Menu Definitions - Elfeed
;;; ============================================================================

(defun cj/elfeed-filter-helper (filter)
  "Wrapper around unpackaged/elfeed-search-filter-toggle-component."
  (elfeed-search-set-filter
   (unpackaged/elfeed-search-filter-toggle-component
    elfeed-search-filter filter)))

(pretty-hydra-define space-menu-elfeed 
  (:foreign-keys warn :title "󰘧 Elfeed" :quit-key ("<escape>" "C-g"))
  ("Views"
   (("d" cj/elfeed-default "Default" :exit t)
    ("a" cj/elfeed-academic "Academic" :exit t))
   
   "Status"
   (("u" (cj/elfeed-filter-helper "+unread") "Unread")
    ("r" (cj/elfeed-filter-helper "+read-later") "Read Later"))))

;;; ============================================================================
;;; Sub-Menu Definitions - Mu4e
;;; ============================================================================

(pretty-hydra-define space-menu-mu4e 
  (:foreign-keys warn :title "󰘧 MU4E" :quit-key ("<escape>" "C-g"))
  ("Jump"
   (("d" mu4e "Dashboard" :exit t))
   
   "Actions"
   (("c" mu4e-compose-new "Compose" :exit t)
    ("S" mu4e-search "Manual Search" :exit t)
    ("s" mu4e-search-query "Search" :exit t)
    ("x" mu4e-context-switch "Switch Context" :exit t)
    ("u" mu4e-update-mail-and-index "Update" :exit t))))

;;; ============================================================================
;;; Sub-Menu Definitions - Ellama (AI)
;;; ============================================================================

(pretty-hydra-define space-menu-ellama 
  (:foreign-keys warn :title "󰘧 Ellama" :quit-key ("<escape>" "C-g"))
  ("Code"
   (("cc" ellama-code-complete "Complete" :exit t)
    ("ca" ellama-code-add "Add" :exit t)
    ("ce" ellama-code-edit "Edit" :exit t)
    ("ci" ellama-code-improve "Improve" :exit t)
    ("cr" ellama-code-review "Review" :exit t)
    ("cm" ellama-generate-commit-message "Commit Message" :exit t))
   
   "Chat"
   (("a" ellama-ask-about "About" :exit t)
    ("i" ellama-chat "Chat" :exit t)
    ("l" ellama-ask-line "Line" :exit t)
    ("s" ellama-ask-selection "Selection" :exit t))
   
   "Text"
   (("tc" ellama-complete-text "Complete" :exit t)    
    ("ts" ellama-summarize "Summarize" :exit t)
    ("ti" ellama-improve-wording "Improve Wording" :exit t)
    ("tg" ellama-improve-grammar "Improve Grammar" :exit t)
    ("tC" ellama-improve-conciseness "Improve Conciseness" :exit t)
    ("tw" ellama-summarize-webpage "Summarize Webpage" :exit t)
    ("tk" ellama-summarize-killring "Summarize Killring" :exit t))
   
   "Session"
   (("Sl" ellama-load-session "Load" :exit t)
    ("Sr" ellama-session-rename "Rename" :exit t)
    ("Sd" ellama-session-remove "Delete" :exit t)
    ("Sa" ellama-session-switch "Switch" :exit t))
   
   "Context"
   (("xb" ellama-context-add-buffer "Add Buffer" :exit t)
    ("xf" ellama-context-add-file "Add File" :exit t)
    ("xs" ellama-context-add-selection "Add Selection" :exit t)
    ("xi" ellama-context-add-info-node "Add Info Node" :exit t))
   
   "Utility"
   (("up" ellama-provider-select "Select Provider" :exit t)
    ("uw" ellama-define-word "Define Word" :exit t)
    ("ut" ellama-translate-text "Translate Selection" :exit t)
    ("ub" ellama-translate-buffer "Translate Buffer" :exit t)
    ("uc" ellama-chat-translation-enable "Translate Chat" :exit t)
    ("uC" ellama-char-translation-disable "Stop Translating Chat" :exit t)
    ("ul" ellama-make-list "Make List" :exit t)
    ("uT" ellama-make-table "Make Table" :exit t)
    ("uf" ellama-make-format "Make Format" :exit t))))

;;; ============================================================================
;;; Sub-Menu Definitions - Config & Goto
;;; ============================================================================

(pretty-hydra-define space-menu-config 
  (:foreign-keys warn :title "󰘧 Config" :quit-key ("<escape>" "C-g"))
  ("Frequent"
   (("i" sliver-open-init "Open Init" :exit t)
    ("o" sliver-open-module "Open Modules" :exit t))
   
   "Utilities"
   (("c" sliver-create-module "Create Module" :exit t)
    ("m" sliver-insert-module "Insert Module" :exit t)
    ("I" sliver-init-module "Initialize Module" :exit t))))

(pretty-hydra-define space-menu-goto 
  (:foreign-keys warn :title "󰘧 Goto" :quit-key ("<escape>" "C-g"))
  ("Directories"
   (("o" (dired org-directory) "Org" :exit t)
    ("e" (dired user-emacs-directory) "Emacs" :exit t)
    ("d" (dired "~/.dotfiles") "Dotfiles" :exit t)
    ("p" (dired "~/Projects") "Projects" :exit t))
   
   "Personal"
   (("l" (find-file *cj/ledger-file*) "Ledger" :exit t)
    ("j" (find-file *cj/journal-file*) "Journal" :exit t)
    ("f" (find-file *cj/fitness-file*) "Fitness" :exit t))))

;;; ============================================================================
;;; Major Mode Hydras - Org
;;; ============================================================================

(defun cj/space-menu-timeblock ()
  (interactive)
  (if (eq major-mode 'org-timeblock-mode)
      (major-mode-hydra)
    (org-timeblock)))

(major-mode-hydra-define org-timeblock-mode 
  (:foreign-keys warn :quit-key ("<escape>" "C-g"))
  ("Org"
   (("n" org-timeblock-new-task "Add Todo")
    ("s" org-timeblock-reschedule "Schedule")
    ("d" org-timeblock-set-duration "Duration"))

   "View"
   (("1" org-timeblock-day-earlier "Previous Day")
    ("2" org-timeblock-day-later "Next Day")
    ("l" org-timeblock-toggle-todo-list "Open List")
    ("v" org-timeblock-switch-view "View")
    ("j" org-timeblock-jump-to-day "Jump to Date"))

   "Actions"
   (("a" org-timeblock-todo "Todo")
    ("b" org-timeblock-done "Done"))))

(major-mode-hydra-define org-mode 
  (:foreign-keys warn :quit-key ("<escape>" "C-g"))
  ("Editing"
   (("l" org-insert-link "Link" :exit t)
    ("r" org-refile "Refile" :exit t)
    ("p" org-set-property "Property" :exit t)
    ("i" org-roam-node-insert "Roam - Insert node" :exit t)
    ("I" org-roam-node-insert-immediate "Roam - Insert immediate" :exit t)
    ("b" org-roam-buffer-toggle "Roam - Node info" :exit t))

   "Agenda"
   (("t" org-todo "Toggle TODO" :exit t)
    ("s" org-schedule "Schedule" :exit t)
    ("d" org-deadline "Deadline" :exit t)
    ("m" org-timestamp "Timestamp" :exit t))
    
   "Utilities"
   (("z" org-mode-restart "Restart" :exit t)
    ("R" org-ranker-hydra/body "Org Ranker" :exit t) 
    ("L" org-lint "Check file for errors" :exit t))))

;;; ============================================================================
;;; Major Mode Hydras - Programming
;;; ============================================================================

(major-mode-hydra-define emacs-lisp-mode 
  (:quit-key ("<escape>" "C-g"))
  ("References"
   (("r" elisp-refs-function "Function refs" :exit t))))

(major-mode-hydra-define python-mode 
  (:quit-key ("<escape>" "C-g"))
  ("General"
   (("p" python "Python" :exit t))
   
   "Utility"
   (("c" compile "Compile" :exit t)
    ("v" pyvenv-workon "Virtualenv" :exit t))))

(major-mode-hydra-define latex-mode 
  (:quit-key ("<escape>" "C-g"))
  ("Utility"
   (("c" compile "Compile" :exit t))))

(major-mode-hydra-define compilation-mode 
  (:quit-key ("<escape>" "C-g"))
  ("Utility"
   (("y" (compile-send-input "y" t) "Yes" :exit t)
    ("n" (compile-send-input "n" t) "No" :exit t)
    ("<RETURN>" (compile-send-input "" t) "Enter" :exit t)
    ("i" (compile-send-input (ivy-read) t) "Input" :exit t))))

;;; ============================================================================
;;; Major Mode Hydras - Ledger
;;; ============================================================================

(major-mode-hydra-define ledger-mode 
  (:quit-key ("<escape>" "C-g"))
  ("General"
   (("a" cj/ledger-insert-account "Account" :exit t)
    ("t" cj/ledger-insert-tag "Tag" :exit t)
    ("r" cj/ledger-rename-account "Rename Account" :exit t)
    ("b" cj/ledger-insert-budget "Budget" :exit t)
    ("B" (cj/ledger-insert-budget t) "Budget (Other month)" :exit t))
   
   "Reports"
   (("R" cj/ledger-report "Ledger Reports" :exit t))))

;;; ============================================================================
;;; Major Mode Hydras - Dired
;;; ============================================================================

(major-mode-hydra-define dired-mode 
  (:hint nil :color pink :quit-key ("<escape>" "C-g"))
  ("Marking"
   (("m" dired-mark "Mark" :exit t)
    ("t" dired-toggle-marks "Toggle Marks" :toggle t)
    ("u" dired-unmark "Unmark" :exit t)
    ("U" dired-unmark-all-marks "Unmark all" :exit t)
    ("E" dired-mark-extension "Extension Mark" :exit t))
   
   "Create"
   (("f" dired-create-empty-file "File" :exit t)
    ("d" dired-create-directory "Directory" :exit t))
   
   "File Operations"
   (("C" dired-do-copy "Copy" :exit t)
    ("D" dired-do-delete "Delete" :exit t)
    ("R" dired-do-rename "Rename" :exit t)
    ("S" dired-do-symlink "Symlink" :exit t)
    ("Y" dired-do-relsymlink "Rel. Symlink" :exit t)
    ("z" diredp-compress-this-file "Compress This" :exit t)
    ("Z" dired-do-compress "Compress" :exit t)
    ("G" dired-do-chgrp "chgrp" :exit t)
    ("M" dired-do-chmod "chmod" :exit t))
   
   "Viewing"
   (("O" dired-display-file "View Other" :exit t)
    ("v" dired-view-file "View" :exit t)
    ("o" dired-find-file-other-window "Open Other" :exit t)
    ("i" dired-maybe-insert-subdir "Insert Subdir" :exit t)
    ("w" dired-kill-subdir "Kill Subdir" :exit t)
    ("l" dired-do-redisplay "Redisplay" :exit t))
   
   "Search and Replace"
   (("A" dired-do-find-regexp "Find Regexp" :exit t)
    ("Q" dired-do-find-regexp-and-replace "Repl Regexp" :exit t))
   
   "Comparison"
   (("e" dired-ediff-files "ediff" :exit t)
    ("=" diredp-ediff "pdiff" :exit t))
   
   "Details"
   (("(" dired-hide-details-mode "Details" :toggle t)
    (")" dired-omit-mode "Omit Mode" :toggle t)
    ("$" diredp-hide-subdir-nomove "Hide Subdir" :toggle t))
   
   "Misc"
   (("\\" dired-do-ispell "Flyspell" :toggle t)
    ("?" dired-summary "Summary" :exit t)
    ("s" dired-sort-toggle-or-edit "Sort" :exit t)
    ("g" revert-buffer "Revert Buffer" :exit t)
    ("F" dired-do-find-marked-files "Find Marked" :exit t))))

;;; ============================================================================
;;; Initialization
;;; ============================================================================

;; Build and evaluate the space menu definition
; (eval (cj/build-space-menu))
