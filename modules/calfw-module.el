
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

;; Fix for calfw not displaying multi-day events
(defun cfw:org-get-timerange (text)
  "Return a range object (begin end text).
If TEXT does not have a range, return nil."
  (let* ((dotime (cfw:org-tp text 'dotime)))
    (and (stringp dotime) (string-match org-ts-regexp dotime)
	 (let* ((matches  (s-match-strings-all org-ts-regexp dotime))
           (start-date (nth 1 (car matches)))
           (end-date (nth 1 (nth 1 matches)))
	       (extra (cfw:org-tp text 'extra)))
	   (if (string-match "(\\([0-9]+\\)/\\([0-9]+\\)): " extra)
       ( list( calendar-gregorian-from-absolute
       (time-to-days
       (org-read-date nil t start-date))
       )
       (calendar-gregorian-from-absolute
       (time-to-days
       (org-read-date nil t end-date))) text))))))
