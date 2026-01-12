;;; name: Org-Extras
;;; depends: org
;;; conflicts:
;;; description: Essential add-ons for org, including latex and babel


(use-package org-contrib)

(require 'ox)
(require 'ox-latex)
(require 'ox-extra)
(require 'org-tempo)


;;; ====================================
;;; LATEX CONFIGURATION
;;; ====================================

(ox-extras-activate '(ignore-headlines))
(setq org-startup-with-latex-preview t
      org-latex-packages-alist '())
(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.4))
(add-to-list 'org-latex-packages-alist '("" "gensymb" t))
(add-to-list 'org-latex-packages-alist '("" "placeins" t))
(setq org-latex-src-block-backend 'verbatim)

(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

;;(setq org-latex-pdf-process
;;      '("xelatex -interaction nonstopmode -output-directory %o %f"
;;        "biber --output-directory %o $(basename %f .tex)"
;;        "xelatex -interaction nonstopmode -output-directory %o %f"
;;        "xelatex -interaction nonstopmode -output-directory %o %f"))

;;(setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))

(add-to-list 'org-latex-classes
             '("IEEEtran"
               "\\documentclass{IEEEtran}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


;;; ====================================
;;; BABEL CONFIGURATION
;;; ====================================


(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (matlab . t)
   ;; (ledger . t)
   (js . t)
   (latex . t)))

(push '("conf-unix" . conf-unix) org-src-lang-modes)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("cf" . "src conf"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("ml" . "src matlab"))
(add-to-list 'org-structure-template-alist '("le" . "src ledger"))
(add-to-list 'org-structure-template-alist '("tex" . "export latex"))
(add-to-list 'org-structure-template-alist '("jv" . "src java"))

;; Handle session blocks in a single buffer better

(defun src-block-in-session-p (&optional name)
  "Return if src-block is in a session of NAME.
NAME may be nil for unnamed sessions."
  (let* ((info (org-babel-get-src-block-info))
	 (lang (nth 0 info))
	 (body (nth 1 info))
	 (params (nth 2 info))
	 (session (cdr (assoc :session params))))
    (cond
     ;; Unnamed session, both name and session are nil
     ((and (null session)
	   (null name))
      t)
     ;; Matching name and session
     ((and
       (stringp name)
       (stringp session)
       (string= name session))
      t)
     ;; No match
     (t nil))))

(defun org-babel-restart-session-to-point (&optional arg)
  "Restart session up to the src-block in the current point.
Goes to beginning of buffer and executes each code block with
`org-babel-execute-src-block' that has the same language and
session as the current block. ARG has same meaning as in
`org-babel-execute-src-block'."
  (interactive "P")
  (unless (org-in-src-block-p)
    (error "You must be in a src-block to run this command"))
  (let* ((current-point (point-marker))
         (info (org-babel-get-src-block-info))
         (lang (nth 0 info))
         (params (nth 2 info))
         (session (cdr (assoc :session params))))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward org-babel-src-block-regexp nil t)
        ;; goto start of block
        (goto-char (match-beginning 0))
        (let* ((this-info (org-babel-get-src-block-info))
               (this-lang (nth 0 this-info))
               (this-params (nth 2 this-info))
               (this-session (cdr (assoc :session this-params))))
          (when
              (and
               (< (point) (marker-position current-point))
               (string= lang this-lang)
               (src-block-in-session-p session))
            (org-babel-execute-src-block arg)))
        ;; move forward so we can find the next block
        (forward-line)))))

(defun org-babel-kill-session ()
  "Kill session for current code block."
  (interactive)
  (unless (org-in-src-block-p)
    (error "You must be in a src-block to run this command"))
  (save-window-excursion
    (org-babel-switch-to-session)
    (kill-buffer)))
