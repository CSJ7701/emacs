(use-package org-fragtog)
(add-hook 'org-mode-hook 'org-fragtog-mode)
(require 'ox-latex)



(setq org-startup-with-latex-preview t
      org-latex-packages-alist '())
(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.4))
(add-to-list 'org-latex-packages-alist '("" "gensymb" t))
(add-to-list 'org-latex-packages-alist '("" "placeins" t))
;; (add-to-list 'org-latex-packages-alist '("" "minted" t))

;; (setq org-latex-src-block-backend 'minted)
(setq org-latex-src-block-backend 'verbatim)

(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))



;; Latex Templates

(define-skeleton latex-mla-skeleton
  "Insert MLA format for a latex document"
  "Prompt: "
  "#+latex_class: article\n"
  "#+latex_class_options: [12pt, a4paper]\n"
  "#+latex_header: \\usepackage[letterpaper]{geometry}\n"
  "#+latex_header: \\geometry{top=1.0in, bottom=1.0in, left=1.0in, right=1.0in}\n"
  "#+latex_header: \\usepackage{rotating}\n"
  "#+latex_header: \\usepackage{graphicx}\n"
  "#+latex_header: \\usepackage{pgfplots}\n"
  "#+latex_header: \\usepackage{filecontents}\n"
  "#+latex_header: \\usepackage{tikz}\n"
  "#+latex_header: \\usepackage{fancyhdr}\n"
  "#+latex_header: \\usepackage{enumitem}\n"
  "#+latex_header: \\pagestyle{fancy}\n"
  "#+latex_header: \\lhead{}\n"
  "#+latex_header: \\chead{}\n"
  "#+latex_header: \\rhead{Johnson \\thepage}\n"
  "#+latex_header: \\lfoot{}\n"
  "#+latex_header: \\cfoot{}\n"
  "#+latex_header: \\rfoot{}\n"
  "#+latex_header: \\renewcommand{\\headrulewidth}{0pt}\n"
  "#+latex_header: \\renewcommand{\\footrulewidth}{0pt}\n"
  "#+latex_header: \\setlength\\headsep{0.333in}\n"
  "#+latex_header: \\newcommand{\\bibent}{\\noindent \\hangindent 40pt}\n"
  "#+latex_header: \\newenvironment{workscited}{\\newpage \\begin{center} Works Cited \\end{center}}{\\newpage }\n"
  "#+latex_header: \\graphicspath{ {./attachments/} }\n"
  "#+options: toc:nil title:nil num:nil\n"
  "#+BEGIN_EXPORT latex\n"
  "\\begin{document}\n"
  "\\begin{flushleft}\n"
  (skeleton-read "Name: ") "\\" "\\" "\n"
  "\\vspace{2mm}"
  (skeleton-read "Teacher: ") "\\" "\\" "\n"
  "\\vspace{2mm}"
  (skeleton-read "Class: ") "\\" "\\" "\n"
  "\\vspace{2mm}"
  (format-time-string "%B %d %Y") "\\" "\\" "\n"
  "\\vspace{4mm}"
  "\\begin{center}\n"
  (skeleton-read "Title: ") "\n"
  "\\end{center}\n"
  "\\vspace{1mm}"
  "\\setlength{\\parindent}{0.5in}\n"
  "#+END_EXPORT"
  "\n\n"
  "# Essay Content goes here"
  "\n\n\n"
  "\n\n# Place /notes/ or /bib/ sections here if needed"
  "\n\n"
  "#+BEGIN_EXPORT latex\n"
  "\\newpage\n"
  "\\begin{center}\n"
  "Appendices\n"
  "\\end{center}\n"
  "\\begin{figure}[htb]\n"
  "\\centering\n"
  "\\includegraphics[width=0.7\\textwidth]{}\n"
  "\\caption{}\n"
  "\\end{figure}\n"
  "\\newpage\n"
  "#+END_EXPORT\n\n"

  "#+BEGIN_EXPORT latex\n"
  "\\begin{center}\n"
  "Lab Questions\n"
  "\\end{center}\n"
  "\\vspace{2mm}\n"
  "\\begin{enumerate}[label=\\textbf{\\arabic*.}]\n"
  "\\item text\n"
  "answer\n"
  "\\end{enumerate}\n"
  "#+END_EXPORT\n\n"
  "#+BEGIN_EXPORT latex\n"
  "\\end{document}\n"
  "#+END_EXPORT"
  )

(define-skeleton latex-mla-notes-skeleton
  "Insert MLA notes section for a latex document"
  "Prompt: "
  "#+BEGIN_EXPORT latex"
  "\n"
  "\\begin{center}\n"
  "Notes\n"
  "\\end{center}\n"
  "\\setlength{\\parindent}{0.5in}\n"
  "% Insert Notes in bullet-points"
  "\n"
  "#+END_EXPORT\n"
  _
  "\n")

(define-skeleton latex-mla-bibliography-skeleton
  "Insert MLA Bib section for a latex document"
  "Prompt: "
  "#+BEGIN_EXPORT latex"
  "\n"
  "\\begin{workscited}\n"
  "% Format: Last, F.M. TITLE. PUBLISHER, PUBDATE. PrintTYPE.\n"
  ("(RET when done, SPC-RET to continue) " "\\bibent\n" (skeleton-read "Author's Name (Last, F.M.): ")" " "\\textit{"(skeleton-read "Title: ")"}. "(skeleton-read"Publisher (PUB, PUByear): ")". " (skeleton-read "Location (Page #, URL, etc.): ")"." str "\n\n")
  "\\end{workscited}\n"
  "\\end{flushleft}\n"
  "#+END_EXPORT\n"
  )


  

