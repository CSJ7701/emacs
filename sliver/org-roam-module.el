;; -*- lexical-binding:t -*-
;;; name: Org Roam
;;; depends: org
;;; conflicts:
;;; description:

;; Packages and init

(use-package org-roam
  :custom
  (org-roam-directory "~/roam")
  (org-roam-completion-everywhere t))
(use-package org-roam-ui
  :after org-roam)
(use-package org-roam-ql
  :straight (org-roam-ql :type git :host github :repo "ahmed-shariff/org-roam-ql"
			 :files (:defaults (:exclude "org-roam-ql-ql.el")))
  :after (org-roam))

(with-eval-after-load 'company
  (use-package company-org-roam))
(with-eval-after-load 'counsel
  (use-package consult-org-roam))

(require 'org-roam-dailies)
(require 'org-roam-protocol)

(if (member "hydra" sliver--loaded-modules)
    (pretty-hydra-define+ space-menu
      (:foreign-keys warn :title "󰘧" :quit-key ("<escape>" "C-g"))
      (
       "Frequent"
       (("r" space-menu-roam/body "   Roam" :exit t)))))

(org-roam-db-autosync-enable)

(setq org-roam-v2-ack t)
(setq org-roam-node-display-template
      (concat "${title:*} "
	      (propertize "${tags:10}" 'face 'org-tag)))

(setq org-roam-capture-templates
      '(("d" "default" plain "%?" :target
         (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
         :unnarrowed t)
        ("D" "My default" entry "* %<%I:%M %p>: %?"
         :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))
        ("b" "Books")
        ("bm" "Book MOC" plain "** TODO %(read-from-minibuffer \"Title: \")\n:PROPERTIES:\n:author: %(read-from-minibuffer \"Author: \")\n:genre: %(read-from-minibuffer \"Genre: \")\n:url: %(read-from-minibuffer \"URL: \")\n:pages: %(read-from-minibuffer \"Pages: \")\n:roam: %(cj/get-org-roam-link (read-from-minibuffer \"Title: \"))\n:END:" :target
         (file+olp "20230704164040-books.org" ("Inbox")))
        ("p" "project" plain "* Project Description\nWhat is this project?\n\n%?\n\n* Inbox\n\n** TODO Add initial tasks\n\n"
	 :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: ${title}\n#+filetags: Project")
	 :unnarrowed t)
        ))

(setq org-roam-capture-ref-templates
      '(("r" "ref" plain "%?" :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
						 "#+title: ${title}\n\n${body}"
                            )
	 :unnarrowed t)
        ("v" "video-ref" table-line
         "| ${body} | %? | |\n" :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"                       
						   "#+title: ${title}\n#+STARTUP: inlineimages\n#+Filetags: :Video:\n${ids}\n| Timestamp | Description | Link |\n|-----------+-------------+------|\n\n* Notes")
         :immediate-finish t :jump-to-captured t :unnarrowed t :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+STARTUP: inlineimages\n#+Filetags: :Video:\n${ids}\n| Timestamp | Description | Link |\n|-----------+-------------+------|") :prepend nil)
	("p" "project" plain "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
        :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: ${title}\n#+filetags: Project")
        :unnarrowed t)))
(setq org-roam-ui-open-on-start nil)
(setq org-roam-ui-browser-function #'browse-url-firefox)
(setq org-roam-ui-sync-theme t)


;;; ===================================================
;;; Many of the functions below are deprecated...
;;; Needs some significant going-through to restructure my roam setup.
;;; May want to start from scratch tbh.

;; Functions
(defun org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (cons arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

(defun cj/org-roam-node-find-tag-filter ()
  (interactive)
  (let ((tag (car (completing-read "Tag: "
				   (org-roam-tag-completions)))))
    (org-roam-node-find nil nil
			(lambda (node)
			  (member tag
				  (org-roam-node-tags node))))))

;; Org agenda with roam notes

(defun cj/org-roam-filter-by-tag (tagname &optional exclude)
    (lambda (node)
      (and (member tagname (org-roam-node-tags node))
	   (not (member exclude (org-roam-node-tags node))))))


;; There is an issue with the above function. It isn't recognizing the argument correctly. It needs to be set outside of the function, the 'tagname' argument isn't actually being passed into the 'lambda' statement

(defun cj/org-roam-list-notes-by-tag (tagname &optional exclude)
  (if exclude
      (mapcar #'org-roam-node-file
	      (seq-filter
	       (cj/org-roam-filter-by-tag tagname exclude)
	       (org-roam-node-list)))
    
    (mapcar #'org-roam-node-file
	    (seq-filter
	     (cj/org-roam-filter-by-tag tagname)
	     (org-roam-node-list))))
  )



;; Find file - limit to project notes. Also lets me create new with tempate
(defun cj/org-roam-project-finalize-hook ()
  "Adds the captures project file to 'org-agenda-files' if the capture was not aborted."
  ;; Remove hook, since it was added temporarily
  (remove-hook 'org-capture-after-finalize-hook #'cj/org-roam-project-finalize-hook)
  ;; Add project file to the agenda list if the capture was confirmed
  (unless org-note-abort
    (with-current-buffer (org-capture-get :buffer)
      (add-to-list 'org-agenda-files (buffer-file-name)))))

(defun cj/org-roam-find-project ()
  (interactive)
  ;; Add the project file to the agenda after capture was confirmed
  ;(add-hook 'org-capture-after-finalize-hook #'cj/org-roam-project-finalize-hook)
  ;; Select file to open, creating if necessary
  (org-roam-node-find
   nil
   nil
   (cj/org-roam-filter-by-tag "Project" "Archived") nil
   :templates
   '(("p" "project" plain "* Goal\n\n%?\n\n* Inbox\n\n** TODO Add initial tasks\n\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: ${title}\n#+filetags: Project")
      :unnarrowed t))))

(defun cj/roam-insert-link-other-file (&optional auto)
  "Inserts a roam link to LINK in a file, DESTINATION under HEADING, if defined."
  (interactive)
  (setq destination-node (org-roam-node-read nil nil nil nil "Destination: ")) ;; Replaced with let
  (setq link-node (org-roam-node-read nil nil nil nil "Link: ")) ;; Replaced with let
					;(setq destination-id (org-roam-node-id destination-node)) ;; Don't need, left for reference
  (setq link-id (org-roam-node-id link-node)) ;; Replaced with let
					;(setq destination-title (org-roam-node-file-title destination-node)) ;; Dont need, reference
  (setq link-title (org-roam-node-file-title link-node)) ;; Replaced with let
  (setq destination-file (org-roam-node-file destination-node)) ;; Replaced with let
					;(setq link-file (org-roam-node-file link-node)) ;; Don't need, reference
  (setq link-string (concat "[[id:" link-id "][" link-title "]]" "\n")) ;; Repalced with let
  (append-to-file link-string nil destination-file)
					;(insert link-string)
  (setq destination-node nil)
  (setq link-node nil)
  (setq link-id nil)
  (setq link-title nil)
  (setq destination-file nil)
  (setq link-string nil))

(defun cj/get-org-roam-node (note-title) ;; Intermediate function 
  "Get the Org-Roam node variable for a daily note with the specified title."
  (let* ((node (org-roam-node-from-title-or-alias note-title)))
    (if node
        node
      (message "Note with title %s not found." note-title)))) ;; If you ever feel like fixing this, instead of all the setq's you could have the intermediate functions return their values (like this one did here, with if node, node) then use local let expressions within the subsequent functions 


(defun cj/get-current-org-roam-daily-note-node () ;; Intermediate function
  "Get the Org-Roam node variable for the current daily note."
  (let* ((current-date (format-time-string "%Y-%m-%d"))
         (daily-note-title current-date)
         (node (cj/get-org-roam-node daily-note-title)))
    (if node
        (progn (setq cj/daily-note-node node) (message "%s" cj/daily-note-node))
      (message "Current daily note not found."))))

(defun cj/get-org-roam-link (node-title)
  (let* ((node (cj/get-org-roam-node node-title)))
    (if node
        (let* ((link-id (org-roam-node-id node))
               (link-string (concat "[[id:" link-id "][" node-title "]]")))
          link-string)
      (message "Node not found"))))


(defvar cj/roam-update-daily-moc-check-end "init" "Records the last time the roam daily moc was updated.")
(defun cj/roam-update-daily-moc ()
  "Runs automatically on roam capture today"

  ;;Goal now is to define an if function here to check if I've run this yet today. I'm going to do that by setting a variable to a datestring and checking if that variable equals todays datestring.

  (setq cj/roam-update-daily-moc-check (format-time-string "%Y-%m-%d"))


  (if (string= cj/roam-update-daily-moc-check cj/roam-update-daily-moc-check-end)
      (message "MOC update has already been run today - aborting")
    (progn
      (cj/get-current-org-roam-daily-note-node)
      (setq cj/daily-moc-node (cj/get-org-roam-node "Daily MOC"))
      (if (boundp 'cj/daily-moc-node)
          (progn
            ;; DO something
            (message "Found Daily MOC"))
        (message "Daily MOC not found"))
          ;;; I have the 2 nodes I want. Now just access the data for each, and we're in business.
      (setq link-id (org-roam-node-id cj/daily-note-node))
      (message "Link ID: %s" link-id)
      (setq link-title (org-roam-node-file-title cj/daily-note-node))
      (message "Link Title: %s" link-title)
      (setq destination-file (org-roam-node-file cj/daily-moc-node))
      (message "Destination File: %s" destination-file)
      (setq link-string (concat "[[id:" link-id "][" link-title "]]" "\n"))
      (message "Link String: %s" link-string)
      (append-to-file link-string nil destination-file)

      (setq cj/daily-moc-node nil)
      (setq link-id nil)
      (setq link-title nil)
      (setq destination-file nil)
      (setq link-string nil)
      (setq cj/daily-note-node nil)
      (setq cj/roam-update-daily-moc-check-end (format-time-string "%Y-%m-%d"))
      (message "MOC update has not been run today - running")
      ) ;; End of the progn that runs the actual function
    )
  )

(defun cj/capture-daily-node (&optional choice)
  (interactive "P")
  (if choice
      (progn (org-roam-dailies-capture-today nil choice) (run-with-timer 100 nil #'cj/roam-update-daily-moc)
             )
    (progn (setq choice (alt-completing-read "Capture: " '(("Default" . "d") ("Entry" . "e")) nil t "")) (org-roam-dailies-capture-today nil choice) (run-with-timer 100 nil #'cj/roam-update-daily-moc))
    )
  (setq choice nil)
  )

(defun cj/roam-update-daily-moc-quiet ()
  (ignore-errors (cj/roam-update-daily-moc))
  )

(add-hook 'org-capture-after-finalize-hook 'cj/roam-update-daily-moc-quiet)

(defun cj/org-roam-filter-book (node) ;; Example filter function for org roam node find
  (interactive)
  (let ((tags (org-roam-node-tags node)))
    (member "Book" tags))) ;; Example usage (org-roam-node-find nil nil 'cj/org-roam-filter-book)

;; Roam Protocol

(defun org-roam-protocol-open-ref (info)
  "Process an org-protocol://roam-ref?ref= style url with INFO.

It opens or creates a note with the given ref.

javascript:location.href = \\='org-protocol://roam-ref?template=r&ref=\\='+ \\encodeURIComponent(location.href) + \\='&title=\\=' + \\encodeURIComponent(document.title) + \\='&body=\\=' + \\encodeURIComponent(window.getSelection())

Originally defined in org-roam-protocol.el, redefined here to add the =id= field. Add more as needed."
  (unless (plist-get info :ref)
    (user-error "No ref key provided"))
  (org-roam-plist-map! (lambda (k v)
			 (org-link-decode
			  (if (equal k :ref)
			      (org-protocol-sanitize-uri v)
			    v))) info)
  (when org-roam-protocol-store-links
	(push (list (plist-get info :ref)
		    (plist-get info :title)) org-stored-links))
  (org-link-store-props :type (and (string-match org-link-plain-re
						 (plist-get info :ref))
				   (match-string 1 (plist-get info :ref)))
			:link (plist-get info :ref)
			:annotation (org-link-make-string (plist-get info :ref)
							  (or (plist-get info :title)
							      (plist-get info :ref)))
			:initial (or (plist-get info :body) ""))
  (raise-frame)
  (let ((org-capture-link-is-already-stored t))
    (org-roam-capture-
     :keys (plist-get info :template)
     :node (org-roam-node-create :title (plist-get info :title))
     :info (list :ref (plist-get info :ref)
		 :body (plist-get info :body)
		 :ids (plist-get info :ids))
     :templates org-roam-capture-ref-templates))
  nil)

;; Skeleton to insert book chapter template
(define-skeleton Book-Chapter-Skeleton
  "Add a chapter to an existing book note"
  "Chapter (# and name):"
  "*" str "\n"
  ":PROPERTIES:\n"
  ":begin:\n"
  ":length:\n"
  ":END:"
  "\n")

;(defun cj/org-roam-ui-open-external ()
;  (interactive)
;  (setq org-roam-ui-browser-function #'browse-url-firefox)
;  (org-roam-ui-open)
;  (setq org-roam-ui-browser-function #'xwidget-webkit-browse-url))

;; Opens the Roam ui, if im in a roam buffer, opens my sidebar layout instead.
(defun cj/org-roam-ui-show ()
  (interactive)
  (split-window-right)
  (enlarge-window-horizontally 30)
  (other-window 1)
  (if (get-buffer "*xwidget webkit: ORUI *")
      ((switch-to-buffer "*xwidget webkit: ORUI *")
       (org-roam-ui-open))
    (org-roam-ui-open))    
  (when (string-prefix-p "/home/csj7701/roam" (expand-file-name (buffer-file-name)))
    ((org-roam-ui-node-local)
     (org-roam-buffer-toggle)
     ))
  (hide-mode-line-mode)
  (other-window 1))


(defun cj/get-keyword-key-value (kwd)
  (let ((data (cadr kwd)))
    (list (plist-get data :key)
          (plist-get data :value))))

(defun cj/org-current-buffer-get-title ()
  (nth 1
       (assoc "TITLE"
              (org-element-map (org-element-parse-buffer 'greater-element)
                  '(keyword)
                #'cj/get-keyword-key-value))))

(defun cj/org-file-get-title (file)
  (with-current-buffer (find-file-noselect file)
    (cj/org-current-buffer-get-title)))

;; Opens a sidebar layout in an open roam node.
(defun cj/org-roam-side-windows ()
  (interactive) ;; For now
  (setq cj/roam-sidebar-main-display (current-buffer)) ;; Record the actual roam file I want to see
  (setq cj/roam-sidebar-file-name (org-roam-node-at-point))
  (unless (get-buffer "*Org Roam*")
    (org-roam-buffer-display-dedicated cj/roam-sidebar-file-name)) ;; Open roam buffer unless its already open
  (other-window 1)
  (setq cj/roam-buffer-name (current-buffer))
  (delete-window)
  (when (get-buffer "*xwidget webkit: ORUI *")
    (progn (switch-to-buffer "*xwidget webkit: ORUI *") (evil-collection-xwidget-webkit-close-tab)
           )) ;; If roam ui is open, close it
  (split-window-right) ;; Make the sidebar, still focusing main window
  (enlarge-window-horizontally 30)
  (other-window 1) ;; Focus sidebar
  (org-roam-ui-open) ;; Open graph
  (split-window-below) ;; Make new window below
  (other-window 1)
  (switch-to-buffer cj/roam-buffer-name)
  (other-window 1)
  (org-roam-ui-node-local) ;; Make graph local
  )

;; Lets me take an open roam node and move it to a new tab with sidebar layout.
(defun cj/roam-tabs ()
  (interactive)
  (tab-new)
  (condition-case nil
      (tab-bar-close-tab-by-name "Roam")
    (error nil))
  (delete-other-windows)
  (cj/org-roam-side-windows)
  (onetab-mode)
  (other-window 1)
  (onetab-mode)
  (other-window 1)
  (onetab-mode)
  (other-window 1)
  (tab-rename "Roam")
  )


;; Lets me pick a roam NODE and open in a new tab with a sidebar layout.
(defun cj/roam-find-file-layout ()
  (interactive)
  (org-roam-node-find)
  (other-window 1)
  (delete-other-windows)
  (cj/roam-tabs)
  )
