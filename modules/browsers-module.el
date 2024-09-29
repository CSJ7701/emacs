
(use-package engine-mode)

(engine-mode t)

(defengine google
  "https://www.google.com/search?q=%s"
  :keybinding "g"
  :browser 'browse-url)
(defengine DuckDuckGo
  "https://duckduckgo.com/?t=ffab&q=%s"
  :keybinding "d")

;; Bookmark Launcher
(require 'org-element)
(require 'seq)
(use-package simple-httpd)

(defun browser-bookmarks (org-file)
  "Return all links from ORG-FILE."
  (with-temp-buffer
    (let (links)
      (insert-file-contents org-file)
      (org-mode)
      (org-element-map (org-element-parse-buffer) 'link
	(lambda (link)
	  (let* ((raw-link (org-element-property :raw-link link))
		 (content (org-element-contents link))
		 (title (substring-no-properties (org (seq-first content) raw-link))))
	    (push (concat title
			  "\n"
			  (propertize raw-link 'face 'whitespace-space)
			  "\n")
		  links)))
	nil nil 'link)
      (seq-sort 'string-greaterp links))))

(defun open-bookmark ()
  (interactive)
  (browse-url (seq-elt (split-string (completing-read "Open: " (browser-bookmarks "/home/csj7701/org/org-linkz/index.org")) "\n") 1)))

(defun present-open-bookmark-frame ()
  (present (browse-url (seq-elt (split-string (completing-read "Open: " (browser-bookmarks "/home/csj7701/org/org-linkz/index.org")) "\n") 1))))

(defun cj/serve-bookmarks ()
  "No longer needed. Occasionally buggy, actual webserver works better."
  (interactive)
  (httpd-serve-directory "~/org/org-linkz/"))
