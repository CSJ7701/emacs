
(use-package elfeed)
;; (use-package elfeed-webkit)
(use-package elfeed-goodies)
(use-package elfeed-web)
(use-package elfeed-protocol)
(use-package elfeed-tube
  :config (elfeed-tube-setup)
  :bind (:map elfeed-show-mode-map
	      ("F" . elfeed-tube-fetch)
	      ([remap save-buffer] . elfeed-tube-save)
	      :map elfeed-search-mode-map
	      ("F" . elfeed-tube-fetch)
	      ([remap save-buffer] . elfeed-tube-save)))

(setq elfeed-protocol-enabled-protocols '(ttrss))
(setq elfeed-protocol-ttrss-maxsize 200)
;; (setq elfeed-protocol-ttrss-fetch-category-as-tag t)
(setq elfeed-protocol-feeds '(("ttrss+http://shipley7701@192.168.1.172:8280/tt-rss"
			       :password "Chri$7701")))
(elfeed-protocol-enable)

(elfeed-goodies/setup)
(setq elfeed-db-directory
      (expand-file-name "elfeed" "/home/csj7701/.dotfiles/.emacs.d"))

(setq elfeed-search-print-entry-function #'elfeed-goodies/entry-line-draw)
(setq-default elfeed-search-filter "@6-weeks-ago +unread -academic")
(defvar cj/elfeed-state "*default*")

;; (setq elfeed-feeds
;;       '(
;; 	;; programming
;; 	("https://www.reddit.com/r/emacs/.rss" emacs)
;; 	("https://www.youtube.com/feeds/videos.xml?channel_id=UCJetJ7nDNLlEzDLXv7KIo0w" lisp code youtube)
;; 	("https://www.youtube.com/feeds/videos.xml?channel_id=UCAiiOTio8Yu69c3XnR7nQBQ" emacs lisp youtube code)
;; 	("https://www.youtube.com/feeds/videos.xml?channel_id=UC9x0AN7BWHpCDHSm9NiJFJQ" linux code youtube)
;; 	("https://sachachua.com/blog/feed/index.xml" emacs code lisp)
;; 	("https://planet.emacslife.com/atom.xml" emacs code lisp)
;; 	("https://www.reddit.com/r/orgmode/.rss" emacs org code)

;; 	;; Linux

;; 	;; News
	
;; 	;; Personal
;; 	("https://www.youtube.com/feeds/videos.xml?channel_id=UCTrSsPMmZavLbc3Ex7VhjDg" guns youtube recreation)
;; 	("https://www.youtube.com/feeds/videos.xml?channel_id=UCBvc7pmUp9wiZIFOXEp1sCg" guns youtube recreation)


;; 	;; Music
;; 	("https://www.youtube.com/feeds/videos.xml?channel_id=UCSp2nvM27htoOTD1Cq05N5g" music youtube)
;; 	("https://www.youtube.com/feeds/videos.xml?channel_id=UC40gs0opj389ohjLnJIAJzA" music youtube)

;; 	;; Papers
;; 	("http://export.arxiv.org/api/query?search_query=cat:cs.LG&start=0&max_results=500&sortBy=submittedDate&sortOrder=descending" academic CS MachineLearning)
;; 	("http://export.arxiv.org/api/query?search_query=cat:cs.AI&start=0&max_results=500&sortBy=submittedDate&sortOrder=descending" academic CS AI)
;; 	("http://export.arxiv.org/api/query?search_query=cat:cs.GL&start=0&max_results=500&sortBy=submittedDate&sortOrder=descending" academic CS GeneralLiterature)
;; 	("http://export.arxiv.org/api/query?search_query=cat:cs.MA&start=0&max_results=500&sortBy=submittedDate&sortOrder=descending" academic CS MultiAgent)
;; 	("http://export.arxiv.org/api/query?search_query=cat:cs.NE&start=0&max_results=500&sortBy=submittedDate&sortOrder=descending" academic CS NeuralNetworks)
;; 	("http://export.arxiv.org/api/query?search_query=cat:cs.OS&start=0&max_results=500&sortBy=submittedDate&sortOrder=descending" academic CS OperatingSystems)
;; 	("http://export.arxiv.org/api/query?search_query=cat:cs.PL&start=0&max_results=500&sortBy=submittedDate&sortOrder=descending" academic CS ProgrammingLanguages)
;; 	("http://export.arxiv.org/api/query?search_query=cat:eess.SP&start=0&max_results=500&sortBy=submittedDate&sortOrder=descending" academic EE SignalProcessing)
;; 	("http://export.arxiv.org/api/query?search_query=cat:eess.SY&start=0&max_results=500&sortBy=submittedDate&sortOrder=descending" academic EE ControlSystems)
;; 	))

(setq-default elfeed-title-max-width 100)
(setq-default elfeed-title-min-width 100)

(defface video-entry
  '((t :foreground "#eeeeaeaeeeee"))
  "Highlight Videos in Elfeed")
(defface unimportant-entry
  '((t :foreground "#424242424242"))
  "Highlight unimportant entries in Elfeed")
(defface important-entry
  '((t :foreground "#ffff00000000"))
  "Highlight important entries in Elfeed")
(defface read-later-entry
  '((t :foreground "#8787ceceffff"))
  "Highlight entries to read later in Elfeed")
(defface news-entry
  '((t :foreground "#f5f5dedeb3b3"))
  "Highlight news entries in Elfeed")
(defface academic-entry
  '((t :foreground "#2e2e8b8b5757"))
  "Highlight academic entries in Elfeed")


(push '(recreation unimportant-entry)
      elfeed-search-face-alist)
(push '(arch unimportant-entry)
      elfeed-search-face-alist)
(push '(music unimportant-entry)
      elfeed-search-face-alist)
(push '(youtube video-entry)
      elfeed-search-face-alist)
(push '(news news-entry)
      elfeed-search-face-alist)
(push '(academic academic-entry)
      elfeed-search-face-alist)
(push '(important important-entry)
      elfeed-search-face-alist)
(push '(read-later read-later-entry)
      elfeed-search-face-alist)

(setq elfeed-tube-auto-fetch-p t)
(setq elfeed-tube-captions-languages
      '("en" "english (auto generated)"))


;; Academic Paper Functions

(defun cj/concatenate-authors (authors-list)
  "Given AUTHORS-LIST, list of plists; return string of all authors
concatenated."
  (mapconcat
   (lambda (author) (plist-get author :name))
   authors-list ", "))

(defun cj/elfeed-academic-fn (entry)
  "Print ENTRY to the buffer."
  (let* ((date (elfeed-search-format-date (elfeed-entry-date entry)))
	 (title (or (elfeed-meta entry :title)
		    (elfeed-entry-title entry) ""))
	 (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
	 (feed (elfeed-entry-feed entry))
	 (feed-title
	  (when feed
	    (or (elfeed-meta feed :title) (elfeed-feed-title feed))))
	 (entry-authors (cj/concatenate-authors
			 (elfeed-meta entry :authors)))
	 (tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
	 (tags-str (mapconcat
		    (lambda (s) (propertize s 'face
					    'elfeed-search-tag-face))
		    tags ","))
	 (title-width (- (window-width) 10
			 elfeed-search-trailing-width))
	 (title-column (elfeed-format-column
			title (elfeed-clamp
			       elfeed-search-title-min-width
			       title-width
			       elfeed-search-title-max-width)
			:left))
	 (authors-width 135)
	 (authors-column (elfeed-format-column
			entry-authors (elfeed-clamp
			       elfeed-search-title-min-width
			       authors-width
			       131)
			:left)))

    (insert (propertize date 'face 'elfeed-search-date-face) " ")

    (insert (propertize title-column
			'face title-faces 'kbd-help title) " ")

    (insert (propertize authors-column
			'face 'elfeed-search-date-face
			'kbd-help entry-authors) " ")
    ;; (when feed-title
    ;;   (insert (propertize entry-authors
    ;; 'face 'elfeed-search-feed-face) " "))
    (when entry-authors
      (insert (propertize feed-title
			  'face 'elfeed-search-feed-face) " "))
    ;; (when tags
    ;;   (insert "(" tags-str ")"))

    ))

(defun cj/elfeed-academic ()
  (interactive)
  (setq elfeed-search-print-entry-function #'cj/elfeed-academic-fn)
  (setf elfeed-search-filter "@2-weeks-ago +academic")
  (if (eq major-mode "elfeed-search")
      (elfeed-update)
    (progn
      (elfeed)
      (elfeed-update))
    (setq cj/elfeed-state "*academic*")))

(defun cj/elfeed-default ()
  (interactive)
  (setq elfeed-search-print-entry-function #'elfeed-goodies/entry-line-draw)
  (setf elfeed-search-filter "@6-weeks-ago +unread -academic")
  (if (eq major-mode "elfeed-search")
      (elfeed-update)
    (progn
      (elfeed)
      (elfeed-update))
    (setq cj/elfeed-state "*default*")))



;; Code From Alphapapa's Unpackaged.el for Elfeed. https://github.com/alphapapa/unpackaged.el?tab=readme-ov-file#elfeed
;; This is used in the space menu - don't delete without making sure you fix the reference there first.
(cl-defun unpackaged/elfeed-search-filter-toggle-component (string component)
  "Return STRING (which should be `elfeed-search-filter') having toggled COMPONENT.
Tries to intelligently handle components based on their prefix:
+tag, =feed, regexp."
  (save-match-data
    (cl-labels ((toggle (component +prefix -prefix string)
                        (let ((+pat (rx-to-string `(seq (or bos blank)
                                                        (group ,+prefix ,component)
                                                        (or eos blank))))
                              (-pat (rx-to-string `(seq (group (or bos (1+ blank)) ,-prefix ,component)
                                                        (or eos blank)))))
                          ;; TODO: In newer Emacs versions, the `rx' pattern `literal'
                          ;; evaluates at runtime in `pcase' expressions.
                          (pcase string
                            ((pred (string-match +pat)) (rm (concat -prefix component) string))
                            ((pred (string-match -pat)) (rm "" string))
                            (_ (concat string " " +prefix component)))))
                (rm (new string) (replace-match new t t string 1)))
      (pcase component
        ((rx bos "+" (group (1+ anything)))
         (toggle (match-string 1 component) "+" "-" string))
        ((rx bos "=" (group (1+ anything)))
         (toggle (match-string 1 component) "=" "~" string))
        (_ (toggle component "" "!" string))))))

