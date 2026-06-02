;;; name: org-highlight
;;; depends: org
;;; conflicts:
;;; description: Custom highlight markup using font-lock keywords


(defface cj/hl-green
  '((t :background "#d4edda" :foreground "#155724" :weight bold))
  "Face for green custom tag highlighting.")

(defface cj/hl-red
  '((t :background "#f8d7da" :foreground "#721c24" :weight bold))
  "Face for red custom tag highlighting.")

(defface cj/hl-yellow
  '((t :background "#ffec8b" :foreground "#b8860b" :weight bold))
  "Face for yellow custom tag highlighting")

(defface cj/hl-orange
  '((t :background "#ffa54f" :foreground "#8b2500" :weight bold))
  "Face for orange custom tag highlighting")

(defface cj/hl-purple
  '((t :background "#ffbbff" :foreground "#551a8b" :weight bold))
  "Face for purple custom tag highlighting")

(defface cj/hl-blue
  '((t :background "#add8e6" :foreground "#191970" :weight bold))
  "Face for blue custom tag highlighting")

;; (font-lock-add-keywords 'org-mode
;; 			'(
;; 			  ("\\(@g\\[\\)\\([^]]+\\)\\(\\]\\)"
;; 			   (1 'org-hide prepend)
;; 			   (2 'cj/hl-green prepend)
;; 			   (3 'org-hide prepend))
;; 			  ("\\(@r\\[\\)\\([^]]+\\)\\(\\]\\)"
;; 			   (1 'org-hide prepend)
;; 			   (2 'cj/hl-red prepend)
;; 			   (3 'org-hide prepend))
;; 			  ("\\(@y\\[\\)\\([^]]+\\)\\(\\]\\)"
;; 			   (1 'org-hide prepend)
;; 			   (2 'cj/hl-yellow prepend)
;; 			   (3 'org-hide prepend))
;; 			  ("\\(@o\\[\\)\\([^]]+\\)\\(\\]\\)"
;; 			   (1 'org-hide prepend)
;; 			   (2 'cj/hl-orange prepend)
;; 			   (3 'org-hide prepend))
;; 			  ("\\(@p\\[\\)\\([^]]+\\)\\(\\]\\)"
;; 			   (1 'org-hide prepend)
;; 			   (2 'cj/hl-purple prepend)
;; 			   (3 'org-hide prepend))
;; 			  ("\\(@b\\[\\)\\([^]]+\\)\\(\\]\\)"
;; 			   (1 'org-hide prepend)
;; 			   (2 'cj/hl-blue prepend)
;; 			   (3 'org-hide prepend))			  
;; 			  )
;; 			)


(defun cj/register-custom-highlights ()
  "Add custom tag highlighting with collapsible delimiters to org-mode."
  (let ((highlights '(("g" . cj/hl-green)
                      ("r" . cj/hl-red)
                      ("y" . cj/hl-yellow)
                      ("o" . cj/hl-orange)
                      ("p" . cj/hl-purple)
                      ("b" . cj/hl-blue)))
        (keywords '()))
    
    (dolist (hl highlights)
      (let* ((char (car hl))
             (face (cdr hl))
             ;; e.g., "\\(@g\\[\\)\\([^]]+\\)\\(\\]\\)"
             (regex (format "\\(@%s\\[\\)\\([^]]+\\)\\(\\]\\)" char)))
        (push `(,regex
                (1 (if org-hide-emphasis-markers '(face org-hide invisible t) 'org-hide) prepend)
                (2 ',face prepend)
                (3 (if org-hide-emphasis-markers '(face org-hide invisible t) 'org-hide) prepend))
              keywords)))
    
    (font-lock-add-keywords 'org-mode keywords)))

(cj/register-custom-highlights)
