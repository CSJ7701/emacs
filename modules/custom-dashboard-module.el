
  (defvar onetab-mode-hook nil) ;; Define a mode-hook
  (defvar onetab-mode-map
    (let ((map (make-sparse-keymap)))
      map)
    "Keymap for my custom startpage mode")

                                          ;  (defun startpage-mode ()
                                          ;     "Minor mode for defining certain actions within my startpage setup"
                                          ;     (interactive)
                                          ;     (use-local-map startpage-mode-map)
                                          ;     (setq minor-mode 'startpage-mode)
                                          ;     (setq mode-name "Startpage")
                                          ;     (run-hooks 'startpage-mode-hook)
                                          ;     (provide 'startpage-mode))


  ;; Darkroom
  (use-package darkroom
    :straight t
    :config
    (setq darkroom-text-scale-increase 0)
    (setq darkroom-margins (cons 0 0))
    )


  (define-minor-mode onetab-mode
    "Startpage mode. mimics single buffer behavior inside the multi-window tab that I designated as my startpage"
    :init-value nil ;; Initial value
    :lighter " Start " ;; Indicator on modeline
    )

  (defun cj/agenda-view ()
    (interactive)
    (org-ql-search (org-agenda-files) '(and (todo) (deadline :to -1)))
    )

  (defun cj/start-startpage ()
    (interactive)
                                          ;    (tab-bar-mode)
    (magit-list-repositories)
    (setq org-agenda-sticky t)
    (org-agenda 1 ".z")
    (org-agenda 1 ".Z")
    (my-calendar) ;; Open the necessary buffers
    (delete-other-windows) ;; Close any other windows
    (switch-to-buffer (get-buffer "*cfw-calendar*")) ;; Display calendar
    (onetab-mode)
                                          ;    (cfw:calendar-mode)
    (darkroom-mode)
    (split-window-below) ;; Make window below calendar
    (fit-window-to-buffer)
    (cfw:refresh-calendar-buffer nil)
    (other-window 1) ;; Switch to the other window
    (switch-to-buffer (get-buffer "*Magit Repositories*")) ;; Make that window show magit
    (onetab-mode)
    (darkroom-mode)
    (split-window-right) ;; Make new window to the right
    (shrink-window-horizontally 12)
    (other-window 1) ;; Switch to that window
    (switch-to-buffer (get-buffer "*Org Agenda(.z)*")) ;; Make it show one agenda
    (onetab-mode)
    (darkroom-mode)
    (split-window-right) ;; Make a new window to the right
    (shrink-window-horizontally 5)
    (other-window 1) ;; Switch to it
    (switch-to-buffer (get-buffer "*Org Agenda(.Z)*"))
    (onetab-mode)
    (darkroom-mode)
    (tab-rename "Startpage")
    (setq tab-bar-show nil)
    )



  (defun cj/startpage () ;; Command to manually switch to startpage
    (interactive)
    (tab-switch "Startpage")
    (tab-bar-close-other-tabs))

  (defvar cj/startpage-refresh-loop-count 4)
  (defun cj/startpage-refresh ()
    (interactive)
    (setq cj/startpage-refresh-loop-count 4)
    (while (> cj/startpage-refresh-loop-count 0)
      (other-window 1)
      (if (string-equal (buffer-name) "*cfw-calendar*")
          (progn (cfw:refresh-calendar-buffer nil) (onetab-mode 1))
        (if (string-equal (buffer-name) "*Magit Repositories*")
            ()
          (if (string-equal (buffer-name) "*Org Agenda(.z)*")
              (progn (org-agenda-redo-all) (darkroom-mode 1) (onetab-mode 1))
            (if (string-equal (buffer-name) "*Org Agenda(.Z)*")
                (progn (org-agenda-redo-all) (darkroom-mode 1) (onetab-mode 1))
              (message "No Matching Buffers")))))
      (setq cj/startpage-refresh-loop-count (- cj/startpage-refresh-loop-count 1))))







  (global-set-key (kbd "<home>") 'cj/startpage) ;; Open start page with home key
  (define-key global-map (kbd "<home>") 'cj/startpage)
  (define-key onetab-mode-map (kbd "C-x C-f") 'find-file-other-tab)
  (define-key onetab-mode-map (kbd "C-x b") 'switch-to-buffer-other-tab)
  (define-key cfw:calendar-mode-map (kbd "c") 'org-capture)
  (define-key onetab-mode-map (kbd "R") 'cj/startpage-refresh)

                                          ;(add-hook 'after-init-hook 'cj/start-startpage)
  (add-hook 'server-visit-hook 'cj/start-startpage)
  (add-hook 'onetab-mode-hook 'window-divider-mode)
                                          ;  (add-hook 'org-capture-after-finalize-hook 'cfw:refresh-calendar-buffer)



