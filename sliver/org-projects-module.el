;;; name: org-projects
;;; depends: org
;;; conflicts:
;;; description: Code for my custom projects workflow. May want to make into a package at some point if this is built-out enough.


(defgroup org-projects nil
  "Lightweight project tracking using one Org file per project."
  :group 'org)

;; Use "org-directory" later
(defcustom org-projects-directory
  (expand-file-name "~/org/Projects")
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
