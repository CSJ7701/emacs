
(use-package calfw)
(use-package calfw-cal)
(use-package calfw-org)
(setq calendar-week-start-day 0
      cfw:org-afenda-schedule-args '(:scheduled :sexp :closed :deadline :todo :timestamp))
(setq cfw:fchar-vertical-line    ?┃
      cfw:fchar-horizontal-line  ?━
      cfw:fchar-junction         ?╋
      cfw:fchar-left-junction    ?┣
      cfw:fchar-right-junction   ?┫
      cfw:fchar-top-junction     ?┳
      cfw:fchar-bottom-junction  ?┻
      cfw:fchar-top-left-corner  ?┏
      cfw:fchar-top-right-corner ?┓)

;; (evil-set-initial-state 'cfw:details-mode 'emacs)

(general-define-key
 :keymaps 'cfw:details-mode-map
  "q" 'cfw:details-kill-buffer-command)

