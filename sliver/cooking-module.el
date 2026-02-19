;;; name: Cooking
;;; depends:
;;; conflicts:
;;; description:

;;;;;;; POTENTIALLY REWRITE THIS WITH TYPST (better support for structured data, seemingly)
(use-package org-chef)

					; This is a rewrite of the original org-chef function. I have updated it to prompt for headline tags.
					; This is used in "org-chef-get-recipe-from-url" for the automated capture template
(defun org-chef-recipe-to-org-element (recipe)
  "Convert a RECIPE into an `org-element` AST, prompting the user for headline tags."
  (let* ((title (cdr (assoc 'name recipe)))
         (tags (read-string (format "Enter tags for the recipe '%s' (comma separated): " title)))
         ;; Split the tags input into a list and trim each tag
         (tag-list (mapcar 'string-trim (split-string tags ",\\s-*"))))
    `(headline (:title ,title :level 1 :tags ,tag-list)
               (property-drawer nil
                                ((node-property (:key "source-url" :value ,(cdr (assoc 'source-url recipe))))
                                 (node-property (:key "servings"   :value ,(cdr (assoc 'servings recipe))))
                                 (node-property (:key "prep-time"  :value ,(format "%s" (cdr (assoc 'prep-time recipe)))))
                                 (node-property (:key "cook-time"  :value ,(format "%s" (cdr (assoc 'cook-time recipe)))))
                                 (node-property (:key "ready-in"   :value ,(format "%s" (cdr (assoc 'ready-in recipe)))))))
               (headline (:title "Ingredients" :level 2 :pre-blank 1)
                         ,(org-chef-to-unordered-list (cdr (assoc 'ingredients recipe))))
               (headline (:title "Directions" :level 2 :pre-blank 1)
                         ,(org-chef-to-ordered-list (cdr (assoc 'directions recipe)))))))

(defun cj/org-chef-get-all ()
  "Get all recipes as org elements"
  (org-ql-select "~/org/cookbook.org"
    '(level 1)))

(defun cj/org-chef-get-search-string (elements)
  (let ((title (cj/get-org-headline-titles elements))
	(tags (cj/get-org-headline-tags elements)))
    (cl-mapcar (lambda (itemtitle itemtags)
		 (format "%-5s      :%s:"
		       itemtitle
		       (mapconcat #'identity itemtags ":")))
	       title tags)))

(defun cj/org-chef-recipe-properties (str pred _)
  "Associate headline title and tags with the full org element. Used in =cj/org-chef-search="
  (let* ((elements (cj/org-chef-get-all))
	 (strings (cj/org-chef-get-search-string elements)))
    (cl-mapcar (lambda (s e) (propertize s 'property e))
	       strings
	       elements)))

(defun cj/org-chef-show-entry (element)
  "Display the org entry content in a temporary buffer.
`element` is an org-element containing properties like :begin and :end."
  (let* ((begin (plist-get (nth 1 element) :begin))
         (end (plist-get (nth 1 element) :end))
	 (title (plist-get (nth 1 element) :raw-value))
         (buffer (generate-new-buffer (concat "*Recipe: " title "*")))) ;; Create a new buffer named *Org Entry*
    (with-current-buffer (find-file-noselect "~/org/cookbook.org") ;; Open the source org file
      (goto-char begin) ;; Go to the beginning of the entry
      (let ((entry-content (buffer-substring-no-properties begin end))) ;; Extract the content
        (with-current-buffer buffer ;; Switch to the new temporary buffer
          (insert entry-content) ;; Insert the content
          (org-mode) ;; Enable org-mode for proper formatting
          (goto-char (point-min))))) ;; Move cursor to the beginning
    (switch-to-buffer buffer))) ;; Display the buffer

(defun cj/org-chef-get-entry (element)
  "Return the org entry content. `element` is an org-element containing properties - requires :begin and :end."
  (let* ((begin (plist-get (nth 1 element) :begin))
	 (end (plist-get (nth 1 element) :end)))
    (with-current-buffer (find-file-noselect "~/org/cookbook.org")
      (goto-char begin)
      (buffer-substring-no-properties begin end))))	

(defun cj/org-chef-search-open ()
  (interactive)
  (ivy-read "Search for Recipe: "
	    #'cj/org-chef-recipe-properties
	    :action (lambda (x)
		      (cj/org-chef-export (get-text-property 0 'property x)
			       ))))


(defun cj/org-chef-get-title (element)
  "Get the =title= property from the provided element."
  (plist-get (nth 1 element) :raw-value))

(defun cj/org-chef-get-property (element property)
  (let* ((begin (org-element-property :begin element))
	 (end (org-element-property :end element))
	 (buffer (find-file-noselect "~/org/cookbook.org"))
	 (value nil))
    (with-current-buffer buffer
      (save-excursion
	(save-restriction
	  (narrow-to-region begin end)
	  (goto-char (point-min))
	  (setq value (org-entry-get nil property)))))))

(defun cj/org-chef-get-servings (element)
  (let ((servings (cj/org-chef-get-property element "servings")))
    servings))
(defun cj/org-chef-get-prep (element)
  (let ((prep (cj/org-chef-get-property element "prep-time")))
    prep))
(defun cj/org-chef-get-cook (element)
  (let ((cook (cj/org-chef-get-property element "cook-time")))
    cook))

(defun cj/org-chef-get-ingredients (element)
  "Get the list of ingredients from the provided element."
  (let* ((begin (plist-get (nth 1 element) :begin))
	 (end (plist-get (nth 1 element) :end))
	 (buffer (find-file-noselect "~/org/cookbook.org"))
	 (ingredients '()))
    (with-current-buffer buffer
      (save-excursion
	(save-restriction
	  (narrow-to-region begin end)
	  (goto-char (point-min))
	  (while (re-search-forward "^- \\(.+\\)" nil t)
	    (let ((ingredient (buffer-substring-no-properties (match-beginning 0) (match-end 0))))
	      (push ingredient ingredients))))))
    (nreverse ingredients)))

(defun cj/org-chef-get-formatted-ingredients (element)
  "Formats a list of ingredients into a string, each ingredient on a new line.
Uses =cj/org-chef-get-ingredients=."
  (mapconcat
   (lambda (ingredient)
     (when (string-match "- \\(.*\\)" ingredient)
       (format "\\\\item{%s}" (match-string 1 ingredient))))
   (cj/org-chef-get-ingredients element)
   "\n"))

(defun cj/org-chef-get-instructions (element)
  (let* ((begin (plist-get (nth 1 element) :begin))
	 (end (plist-get (nth 1 element) :end))
	 (buffer (find-file-noselect "~/org/cookbook.org"))
	 (instructions '()))
    (with-current-buffer buffer
      (save-excursion
	(save-restriction
	  (narrow-to-region begin end)
	  (goto-char (point-min))
	  (while (re-search-forward "^\\([0-9]+\\.\\) \\(.*\\)" nil t)
	    (let ((instruction (buffer-substring-no-properties (match-beginning 0) (match-end 0))))
	      (push instruction instructions))))))
    instructions))

(defun cj/org-chef-get-formatted-instructions (element)
  (mapconcat
   (lambda (instruction)
     (when (string-match "^[0-9]+\\. \\(.*\\)" instruction)
       (format "\\item %s" (match-string 1 instruction))))
   (reverse (cj/org-chef-get-instructions element))
   "\n"))

(defun cj/org-chef-get-tips (element)
  (let* ((begin (plist-get (nth 1 element) :begin))
	 (end (plist-get (nth 1 element) :end))
	 (buffer (find-file-noselect "~/org/cookbook.org"))
	 (tips '()))
    (with-current-buffer buffer
      (save-excursion
	(save-restriction
	  (narrow-to-region begin end)
	  (goto-char (point-min))
	  (while (re-search-forward "^+ \\(.+\\)" nil t)
	    (let ((tip (buffer-substring-no-properties (match-beginning 0) (match-end 0))))
	      (push tip tips))))))
    (if (null tips)
	(setq tips '("+ Enjoy! :)")))
    tips))

(defun cj/org-chef-get-formatted-tips (element)
  (mapconcat
   (lambda (tip)
     (when (string-match "^+ \\(.+\\)" tip)
       (format "\\item %s" (match-string 1 tip))))
   (cj/org-chef-get-tips element)
   "\n"))

;; This template references =RecipePage.cls= which is stored permanently in .dotfiles/.config/latex. To make this class available, =mkdir ~/texmf=, =cp .dotfiles/.config/latex=
(defun cj/org-chef-get-latex-recipe (element)
  "Generate and return the LaTeX recipe based on the given ELEMENT."
  (let ((latex-template
         "\\documentclass{RecipePage}
\\begin{document}
\\recipeTitle{TEMPLATE_TITLE}
\\recipeInfo{TEMPLATE_SERVINGS}{TEMPLATE_PREP}{TEMPLATE_COOK}
\\hbox to 0.9\\textwidth{
\\vbox{
\\hsize=0.4\\textwidth
\\begin{ingredients}
TEMPLATE_INGREDIENTS
\\end{ingredients}
}
\\vbox{
\\hsize=0.45\\textwidth
\\begin{instructions}
TEMPLATE_INSTRUCTIONS
\\end{instructions}
}
}
\\begin{tips}
TEMPLATE_TIPS
\\end{tips}
\\end{document}"))
    ;; Fetch dynamic content
    (let ((title (cj/org-chef-get-title element))
          (servings (cj/org-chef-get-servings element))
          (prep-time (cj/org-chef-get-prep element))
          (cook-time (cj/org-chef-get-cook element))
          (ingredients (cj/org-chef-get-formatted-ingredients element))
          (instructions (cj/org-chef-get-formatted-instructions element))
          (tips (cj/org-chef-get-formatted-tips element)))
      ;; Perform the replacements with case sensitivity
      (setq latex-template (replace-regexp-in-string "TEMPLATE_TITLE" title latex-template t t))
      (setq latex-template (replace-regexp-in-string "TEMPLATE_SERVINGS" servings latex-template t t))
      (setq latex-template (replace-regexp-in-string "TEMPLATE_PREP" prep-time latex-template t t))
      (setq latex-template (replace-regexp-in-string "TEMPLATE_COOK" cook-time latex-template t t))
      (setq latex-template (replace-regexp-in-string "TEMPLATE_INGREDIENTS" ingredients latex-template t))
      (setq latex-template (replace-regexp-in-string "TEMPLATE_INSTRUCTIONS" instructions latex-template t t))
      (setq latex-template (replace-regexp-in-string "TEMPLATE_TIPS" tips latex-template t t))
      ;; Return the final LaTeX document
      latex-template)))

(defun cj/save-latex-to-file (filename content)
  (with-temp-file filename
    (insert content)))

(defun cj/compile-latex-to-pdf (tex-file pdf-file)
  (with-current-buffer (find-file-noselect tex-file)
    (let ((command (format "latexmk -pdf -output-directory=%s %s -gg"
			   (file-name-directory pdf-file)
			   tex-file)))
      (shell-command command))))

(defun cj/org-chef-export (element)
  (let* ((pdf-filename "/home/csj7701/org/ChefExports/TempRecipe.pdf")
	 (latex-content (cj/org-chef-get-latex-recipe element))
	 (tex-filename (concat (file-name-sans-extension pdf-filename) ".tex")))
    (cj/save-latex-to-file tex-filename latex-content)
    (sleep-for 0.2)
    (cj/compile-latex-to-pdf tex-filename pdf-filename)
    (message "PDF generated: %s" pdf-filename)
    (async-shell-command (format "zathura %s" pdf-filename))))



;; Next Steps:
;; Write Latex Package to format recipe properly - XXX
;; Write function for latex module - skeleton to take different sections of text
;; Write functions for chef module - split recipe and pass different sections to the skeleton
;; Will probably need a series of functions - i.e. cj/org-chef-export-recipe-title, cj/org-chef-export-recipe-prep-time, etc
