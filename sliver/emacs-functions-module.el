;;; name: Emacs Functions
;;; depends:
;;; conflicts:
;;; description: General purpose functions that can be used throughout emacs



  (defun alt-completing-read (prompt collection &optional predicate require-match initial-input hist def inherit-input-method)
    "Calls `completing-read' but returns the value from COLLECTION.

  Simple wrapper around the `completing-read' function that assumes
  the collection is either an alist, or a hash-table, and returns
  the _value_ of the choice, not the selected choice. For instance,
  give a variable of choices like:

      (defvar favorite-hosts '((\"Glamdring\" . \"192.168.5.12\")
                               (\"Orcrist\"   . \"192.168.5.10\")
                               (\"Sting\"     . \"192.168.5.220\")
                               (\"Gungnir\"   . \"192.168.5.25\")))

  We can use this function to `interactive' without needing to call
  `alist-get' afterwards:

      (defun favorite-ssh (hostname)
        \"Start a SSH session to a given HOSTNAME.\"
        (interactive (list (alt-completing-read \"Host: \" favorite-hosts)))
        (message \"Rockin' and rollin' to %s\" hostname))"

    ;; Yes, Emacs really should have an `alistp' predicate to make this code more readable:
    (cl-flet ((assoc-list-p (obj) (and (listp obj) (consp (car obj)))))

      (let* ((choice
              (completing-read prompt collection predicate require-match initial-input hist def inherit-input-method))
             (results (cond
                       ((hash-table-p collection) (gethash choice collection))
                       ((assoc-list-p collection) (alist-get choice collection def nil 'equal))
                       (t                         choice))))
        (if (listp results) (first results) results))))

  (defmacro present (&rest body)
    "Create a buffer with BUFFER-NAME and eval BODY in a basic frame"
    (declare (indent 1) (debug t))
    `(let* ((buffer (get-buffer-create (generate-new-buffer-name "*present*")))
  	  (frame (make-frame '((auto-raise . t)
  			       (font . "JetBrainsMono Nerd Font 15")
  			       (top . 200)
  			       (height . 20)
  			       (width . 110)
  			       (internal-border-width . 20)
  			       (left . 0.33)
  			       (left-fringe . 0)
  			       (line-spacing . 3)
  			       (menu-bar-lines . 0)
  			       (minibuffer . only)
  			       (right-fringe . 0)
  			       (tool-bar-lines . 0)
  			       (undecorated . t)
  			       (unsplittable . t)
  			       (vertical-scroll-bars . nil)))))
       (set-face-attribute 'ivy-current-match frame
  			 :background "#2a2a2a"
  			 :foreground 'unspecified)
       (select-frame frame)
       (select-frame-set-input-focus frame)
       (with-current-buffer buffer
         (condition-case nil
  	   (unwind-protect
  	       ,@body
  	     (delete-frame frame)
  	     (kill-buffer buffer))
  	 (quit (delete-frame frame)
  	       (kill-buffer buffer))))))
