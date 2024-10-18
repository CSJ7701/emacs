
(pretty-hydra-define space-menu (:foreign-keys warn :title "󰘧" :quit-key ("<escape>" "C-g"))
  ("Frequent"
   (("o" space-menu-org/body "  Org" :exit t)
    ("r" space-menu-roam/body "  Roam" :exit t)
    ("g" space-menu-git/body " 󰊢 Git" :exit t)
    )
   "Utilities"
   (("m" space-menu-mu4e/body " 󰊫 Mu4E" :exit t)
    ("e" space-menu-elfeed/body "  Elfeed" :exit t)
    )
   "Menus"
   (
    ;; ("n" hydra-dunstctl/body "Notifications")
    ("SPC" major-mode-hydra "  Major Mode" :exit t)
    ("C" space-menu-config/body "    Config" :exit t)
    ("G" space-menu-goto/body "    Goto" :exit t)
    )))


(pretty-hydra-define space-menu-org (:foreign-keys warn :title "󰘧 Org" :quit-key ("<escape>" "C-g"))
  ("Frequent Actions"
   (("c" org-capture "Capture" :exit t)
    ("i" cj/org-insert-image-link "Image Insert" :exit t)
    ("E" cj/open-excalidraw "Excalidraw" :exit t)
    ("s" org-store-link "Store Link" :exit t)
    ("l" org-insert-link "Insert Link" :exit t)
    )

   "Utilities"
   (("a" org-agenda "Agenda" :exit t)
    ("A" space-menu-org-agenda/body "Custom Agenda (migrate)" :exit t)
    ("t" cj/space-menu-timeblock "Timeblock" :exit t)
    )

   "References"
   (("R" org-ref-insert-link-hydra/body "Org Ref Link Hydra" :exit t)
    ("r" org-ref-insert-link "Org Ref Link" :exit t))
    
   ))

(pretty-hydra-define space-menu-org-agenda (:foreign-keys warn :title "󰘧 Org Agenda" :quit-key ("<escape>" "C-g"))
  ("Frequent"
   (("d" (cj/dashboard-agenda) "Dashboard" :exit t)
    ("s" (cj/school-agenda) "School Planner" :exit t)
    ("p" (cj/project-agenda) "Projects" :exit t)
    )
   ))

(pretty-hydra-define space-menu-roam (:foreign-keys warn :title "󰘧 Roam" :quit-key ("<escape>" "C-g"))
  ("Frequent"
   (("F" cj/roam-find-file-layout "Open in Roam tab" :exit t)
    ("f" org-roam-node-find "Open node" :exit t)
    ("t" org-roam-tag-add "Add Tag" :exit t)
    ("a" org-roam-alias-add "Alias Add" :exit t))
   "Utilities"
   (("T" cj/capture-daily-node "Capture today" :exit t)
   ("p" cj/org-roam-find-project "Find project" :exit t)
   )))

(pretty-hydra-define space-menu-git (:foreign-keys warn :title "󰘧 Git" :quit-key ("<escape>" "C-g"))
  ("Frequent"
   (("s" magit-status "Status" :exit t)
    ("S" magit-list-repositories "Status All" :exit t)
    ("c" magit-commit "Commit" :exit t)
    ("p" magit-push "Push" :exit t)
    ("f" magit-pull-from-pushremote "Pull" :exit t))
   "Utilities"
   (("R" magit-remote-add "Add Remote" :exit t))
   ))

(pretty-hydra-define space-menu-config (:foreign-keys warn :title "󰘧 Config" :quit-key ("<escape>" "C-g"))
  ("Frequent"
   (("i" cj/open-init "Open Init" :exit t)
    ("o" cj/open-module "Open Modules" :exit t))
   "Utilities"
   (("c" cj/create-module "Create Module" :exit t)
    ("m" cj/insert-module "Insert Module" :exit t)
    ("I" cj/init-module "Initialize Module" :exit t))
   ))

(pretty-hydra-define space-menu-goto (:foreign-keys warn :title "󰘧 Goto" :quit-ket ("<escape>" "C-g"))
  ("Directories"
   (("o" (dired org-directory) "Org" :exit t)
    ("e" (dired user-emacs-directory) "Emacs" :exit t)
    ("d" (dired "~/.dotfiles") "Dotfiles" :exit t)
    ("p" (dired "~/Projects") "Projects" :exit t))
   "Personal"
   (("l" (find-file *cj/ledger-file*) "Ledger" :exit t)
    ("j" (find-file cj/journal-file) "Journal" :exit t))
   ))

(defun cj/elfeed-filter-helper (filter)
  "Wrapper around unpackaged/elfeed-search-filter-toggle-component"
  (elfeed-search-set-filter
   (unpackaged/elfeed-search-filter-toggle-component
    elfeed-search-filter filter)))
(pretty-hydra-define space-menu-elfeed (:foreign-keys warn :title "󰘧 Elfeed" :quit-key ("<escape>" "C-g"))
  ("Views"
   (("d" cj/elfeed-default "Default" :exit t)
    ("a" cj/elfeed-academic "Academic" :exit t))
   "Status"
   (("u" (cj/elfeed-filter-helper "+unread") "Unread")
    ("r" (cj/elfeed-filter-helper "+read-later") "Read Later")
    )))

(pretty-hydra-define space-menu-mu4e (:foreign-keys warn :title "󰘧 MU4E" :quit-key ("<escape>" "C-g"))
  ("Jump"
   (("d" mu4e "Dashboard" :exit t)
    )
   "Actions"
   (("c" mu4e-compose-new "Compose" :exit t)
    ("S" mu4e-search "Manual Search" :exit t)
    ("s" mu4e-search-query "Search" :exit t)
    ("x" mu4e-context-switch "Switch Context" :exit t)
    ("u" mu4e-update-mail-and-index "Update" :exit t)
    )
   ))


;;; Major Mode Hydras

(major-mode-hydra-define org-timeblock-mode (:foreign-keys warn :quit-key ("<escape>" "C-g"))
  ("Org"
   (("n" org-timeblock-new-task "Add Todo")
    ("s" org-timeblock-reschedule "Schedule")
    ("d" org-timeblock-set-duration "Duration")
    )

   "View"
   (("1" org-timeblock-day-earlier "Previous Day")
    ("2" org-timeblock-day-later "Next Day")
    ("l" org-timeblock-toggle-todo-list "Open List")
    ("v" org-timeblock-switch-view "View")
    ("j" org-timeblock-jump-to-day "Jump to Date")
    )

   "Unknown"
   (("a" org-timeblock-todo "Todo")
    ("b" org-timeblock-done "Done")
    )
   ))

(major-mode-hydra-define org-mode (:foreign-keys warn :quit-key ("<escape>" "C-g"))
  ("Editing"
   (("l" org-insert-link "Link" :exit t)
    ("r" org-refile "Refile" :exit t)
    ("p" org-set-property "Property" :exit t)
    ("i" org-roam-node-insert "Roam - Insert node" :exit t)
    ("I" org-roam-node-insert-immediate "Roam - Insert immediate" :exit t)
    ("b" org-roam-buffer-toggle "Roam - Node info" :exit t)
    )

   "Agenda"
   (("t" org-todo "Toggle TODO" :exit t)
    ("s" org-schedule "Schedule" :exit t)
    ("d" org-deadline "Deadline" :exit t)
    ("m" org-timestamp "Timestamp" :exit t)
    )
    "Utilities"
    (("z" org-mode-restart "Restart" :exit t)
     ("L" org-lint "Check file for errors" :exit t)
     )))

(major-mode-hydra-define emacs-lisp-mode (:quit-key ("<escape>" "C-g"))
  ("test"
   (("t" elisp-refs-function "refs" :exit t))))

(major-mode-hydra-define latex-mode (:quit-key ("<escape>" "C-g"))
  ("General"
   ()
   "Utility"
   (("c" compile "Compile" :exit t))))

(major-mode-hydra-define python-mode (:quit-key ("<escape>" "C-g"))
  ("General"
   (("p" python "Python" :exit t)
    )
   "Utility"
   (("c" compile "Compile" :exit t)
    ("v" pyvenv-workon "Virtualenv" :exit t)
    )
   ))

(major-mode-hydra-define ledger-mode (:quit-key ("<escape>" "C-g"))
  ("General"
   (("a" cj/ledger-insert-account "Account" :exit t)
    ("t" cj/ledger-insert-tag "Tag" :exit t)
    ("r" cj/ledger-rename-account "Rename Account" :exit t)
    )
   "Reports"
   ()
   ))

(major-mode-hydra-define compilation-mode (:quit-key ("<escape>" "C-g"))
  ("Utility"
   (("y" (compile-send-input "y" t) "Yes" :exit t)
    ("n" (compile-send-input "n" t) "No" :exit t)
    ("<RETURN>" (compile-send-input "" t) "Enter" :exit t)
    ("i" (compile-send-input (ivy-read) t) "Input" :exit t)
   )
   ))

(major-mode-hydra-define dired-mode (:hint nil :color pink :quit-key ("<escape>" "C-g"))
  ("Marking"
   (("m" dired-mark "Mark" :exit t)
    ("t" dired-toggle-marks "Toggle Marks" :toggle t)
    ("u" dired-unmark "Unmark" :exit t)
    ("U" dired-unmark-all-marks "Unmark all" :exit t)
    ("E" dired-mark-extension "Extension Mark" :exit t))
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
    ("?" dired-summart "Summary" :exit t)
    ("s" dired-sort-toggle-or-edit "Sort" :exit t)
    ("g" revert-buffer "Revert Buffer" :exit t)
    ("F" dired-do-find-marked-files "Find Marked" :exit t))
   ))

  "
_+_ mkdir          _v_iew           _m_ark             _(_ details        _i_nsert-subdir    wdired
_C_opy             _O_ view other   _U_nmark all       _)_ omit-mode      _$_ hide-subdir    C-x C-q : edit
_D_elete           _o_pen other     _u_nmark           _l_ redisplay      _w_ kill-subdir    C-c C-c : commit
_R_ename           _M_ chmod        _t_oggle           _g_ revert buf     _e_ ediff          C-c ESC : abort
_Y_ rel symlink    _G_ chgrp        _E_xtension mark   _s_ort             _=_ pdiff
_S_ymlink          ^ ^              _F_ind marked      _._ toggle hydra   \\ flyspell
_r_sync            ^ ^              ^ ^                ^ ^                _?_ summary
_z_ compress-file  _A_ find regexp
_Z_ compress       _Q_ repl regexp

T - tag prefix
"
