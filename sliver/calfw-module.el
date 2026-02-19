;;; name: Calfw
;;; depends: org
;;; conflicts:
;;; description: Clean calendar UI



(use-package calfw
  :bind (:map cfw:details-mode-map
	      ("q" . cfw:details-kill-buffer-command))
  )
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

(custom-set-faces
 '(cfw:face-title                ((t (:weight bold :height 2.0 :inherit variable-pitch))))
 '(cfw:face-header               ((t (:weight bold))))
 '(cfw:face-sunday               ((t (:weight bold))))
 '(cfw:face-saturday             ((t (:weight bold))))
 '(cfw:face-toolbar-button-off   ((t (:weight bold))))
 '(cfw:face-toolbar-button-on    ((t (:weight bold)))))



(defun my-calendar-old ()
  (interactive)
  (cfw:open-calendar-buffer
   :view 'week
   :contents-sources
   (list
    (cfw:org-create-file-source "Personal" "~/org/Tasks-Personal.org" "#186A3B")
    (cfw:org-create-file-source "Work" "~/org/Tasks-Work.org" "#3498DB")
    (cfw:org-create-file-source "School" "~/org/Tasks-School.org" "#9B59B6")
    (cfw:org-create-file-source "Habit" "~/org/Habit.org" "darkseagreen"))))

(defun my-calendar ()
  (interactive)
  (let ((sources '())
        (headings '(("Personal" "#186A3B")
                    ("Work"     "#3498DB")
                    ("School"   "#9B59B6")))
        (calendar-file "~/org/Calendar.org"))
    (with-current-buffer (find-file-noselect calendar-file)
      (dolist (heading headings)
        (let* ((name (car heading))
              (color (cadr heading))
              (tempfile (make-temp-file (concat name "-org-") nil ".org")))
          ;; Find the subtree
          (save-restriction
            (widen)
            (goto-char (point-min))
            (when (re-search-forward (concat "^* " (regexp-quote name)) nil t)
              (org-narrow-to-subtree)
              (write-region (point-min) (point-max) tempfile)))
          ;; Add to sources
          (push (cfw:org-create-file-source name tempfile color) sources))))
    ;; Open calendar
    (cfw:open-calendar-buffer
     :view 'week
     :contents-sources (nreverse sources))))
