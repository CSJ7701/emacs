
;;; Look at migrating this ALL to citar

(use-package org-ref)
(use-package org-roam-bibtex)

(require 'bibtex)

(with-eval-after-load 'ivy
  (use-package ivy-bibtex))
(with-eval-after-load 'consult
  (use-package consult-bibtex
    :straight '(:host github :repo "mohkale/consult-bibtex")))

(setq bibtex-completion-bibliography '("~/roam/Assets/references.bib")
      bibtex-completion-library-path '("~/roam/References")
      bibtex-completion-notes-path "~/roam/"
      bibtex-completion-notes-template-multiple-files "* ${author-or-editor}, ${title}, ${journal}, (${year}) :${=type=}: \n\nSee [[cite:&${=key=}]]\n"

      bibtex-completion-additional-search-fields '(keywords)
      bibtex-completion-display-formats
      '((article       . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${journal:40}")
	(inbook        . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} Chapter ${chapter:32}")
	(incollection  . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
	(inproceedings . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
	(t             . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*}"))
      bibtex-completion-pdf-field "file"
      bibtex-completion-pdf-open-function
      (lambda (fpath)
	(call-process "xdg-open" nil 0 nil fpath)))



(setq bibtex-autokey-year-length 4
      bibtex-autokey-name-year-separator "-"
      bibtex-autokey-year-title-separator "-"
      bibtex-autokey-titleword-separator "-"
      bibtex-autokey-titlewords 2
      bibtex-autokey-titlewords-stretch 1
      bibtex-autokey-titleword-length 5)

(org-roam-bibtex-mode 1)

(defun org-ref-insert-cite-type-link ()
  "Call 'org-ref-insert-cite-link' with the universal argument."
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively 'org-ref-insert-cite-link)))
(defvar org-ref-insert-cite-type-function 'org-ref-insert-cite-type-link)

(defhydra+ org-ref-insert-link-hydra () ("{" (funcall org-ref-insert-cite-type-function) "Citation /w type" :column "org-ref"))

				       
