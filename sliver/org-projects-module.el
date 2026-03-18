;;; name: org-projects
;;; depends: org
;;; conflicts:
;;; description: Code for my custom projects workflow. May want to make into a package at some point if this is built-out enough.


(defgroup org-projects nil
  "Lightweight project tracking using one Org file per project."
  :group 'org)

;; Use "org-directory" later
(defcustom org-projects-directory
  (expand-file-name "Projects" org-directory)
  "Directory containing Org project files."
  :type 'directory
  :group 'org-projects)

(defcustom org-projects-status-values
  '("Open" "Paused" "Done" "Archived")
  "Allowed project status values."
  :type '(repeat string)
  :group 'org-projects)

(defcustom org-projects-default-status
  "Open"
  "Default status for new projects"
  :type 'string
  :group 'org-projects)

(defcustom org-projects-name-column-width 30
  "Width of the project name column in completion UI."
  :type 'integer
  :group 'org-projects)

(defcustom org-projects-template
  "#+TITLE: %s
#+STATUS: %s
#+FILETAGS: :Project:

* Overview

* Goals
- [ ]

* Tasks
** TODO Add Initial Tasks

* References
"
  "Template for new project files."
  :type 'string
  :group 'org-projects)

;; Functions

(defun org-projects--project-files ()
  "Returns a list of Org files in the 'org-projects-directory'."
  (directory-files-recursively
   org-projects-directory "\\.org$"))

(defun org-projects--file-property (property)
  "Return file-level PROPERTY value as a string or nil."
  (car (cdr (assoc property (org-collect-keywords (list property))))))

(defun org-projects--read-project (file)
  "Return an alist of metadata for project FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (delay-mode-hooks (org-mode))
    `((file . ,file)
      (title . ,(org-projects--file-property "TITLE"))
      (status . ,(org-projects--file-property "STATUS"))
      (filetags . ,(org-projects--file-property "FILETAGS")))))

(defun org-projects--all-projects ()
  "Return list of project metadata alists."
   (mapcar #'org-projects--read-project
	   (org-projects--project-files)))

;; Completing Read
(defun org-projects--candidates ()
  "Return list or propertized candidate strings."
  (mapcar
   (lambda (proj)
     (let ((s (alist-get 'title proj)))
       (propertize s 'org-project proj)))
   (org-projects--all-projects)))

(defun org-projects--status-sort (status)
  "Return numeric rank of STATUS based on the order of 'org-projects-status-values'."
  (or (seq-position org-projects-status-values status #'string=)
      most-positive-fixnum))

;;;; Completing Read Utilities, for "extra properties" predicates

(defun org-projects--display-sort (cands)
  "Sort CANDS by project status using 'org-projects-status-values'."
  (sort cands
	(lambda (a b)
	  (let* ((pa (get-text-property 0 'org-project a))
		 (pb (get-text-property 0 'org-project b))
		 (sa (alist-get 'status pa))
		 (sb (alist-get 'status pb)))
	    (< (org-projects--status-sort sa)
	       (org-projects--status-sort sb))))))

(defun org-projects--candidates-annotate (cands)
  "Add prefix (status) and postfix (tags) to CANDS."
  (mapcar
   (lambda (cand)
     (let* ((proj (get-text-property 0 'org-project cand))
	    (status (or (alist-get 'status proj) "??"))
	    (tags   (or (alist-get 'filetags proj) "No Tags"))
	    (name   (org-projects--fixed-width-name cand))
	    (prefix (propertize (format " %-8s" status)
				'face 'font-lock-keyword-face))
	    (postfix (propertize tags 'face 'font-lock-comment-face))
	    (padded-postfix (org-projects--right-justify-postfix prefix name postfix)))
       (list prefix name padded-postfix)))
   cands))

(defun org-projects--right-justify-postfix (prefix cand postfix)
  "Pad POSTFIX so it appears right-justified relative to PREFIX and CAND."
  (let* ((win-width (window-body-width (minibuffer-window)))
	 (used (+ (string-width prefix)
		  (string-width cand)
		  (string-width postfix)))
	 (spaces (max 1 (- win-width used))))
    (concat (make-string spaces ?\s) postfix)))

(defun org-projects--fixed-width-name (name)
  "Return NAME truncated or padded up to 'org-projects-name-column-width'."
  (string-pad
   (truncate-string-to-width name org-projects-name-column-width 0 nil t)
   org-projects-name-column-width))

(defun org-projects-open-project ()
  "Open or create a project using 'completing-read'."
  (interactive)
  (let* ((candidates (org-projects--candidates))
	 (completion-extra-properties
	  '(:affixation-function org-projects--candidates-annotate
				 :display-sort-function org-projects--display-sort))
	 (choice (completing-read "Project: " candidates nil nil))
	 (match (seq-find (lambda (c) (string= (downcase c) (downcase choice))) candidates)))
    (if match
	(let ((proj (get-text-property 0 'org-project match)))
	  (find-file (alist-get 'file proj)))
      (org-projects-new-project choice))))

(defun org-projects-new-project (name)
  "Create a new project called NAME."
  (let* ((filename (concat (replace-regexp-in-string " " "-" (downcase name)) ".org"))
	 (file (expand-file-name filename org-projects-directory)))
    (find-file file)
    (insert (format org-projects-template name org-projects-default-status))
    (save-buffer)))



