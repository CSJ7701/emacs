(use-package mu4e
  :defer 1
  :load-path "/usr/share/emacs/site-lisp/mu4e/")
(require 'mu4e-org)
(use-package org-mime)
(use-package htmlize)
(use-package mu4e-alert)

(setq mu4e-alert-interesting-mail-query "flag:unread AND NOT flag:trashed AND NOT maildir:/spam/")
(setq mu4e-attachment-dir "/home/csj7701/Downloads/")

(setq cj/mail-enabled (string-equal system-name "archlinux")
      cj/mu4e-inbox-query "(maildir:/Personal/Inbox/) AND flag:unread"
      mu4e-change-filenames-when-moving t
      mu4e-update-interval (* 10 60)
      mu4e-get-mail-command "mbsync -a"
      mu4e-maildir "~/Mail"
      message-send-mail-function 'smtpmail-send-it
      mu4e-compose-format-flowed t
      mu4e-notification-support t
      mu4e-context-policy 'pick-first
      shr-color-visible-luminance-min 800
      org-mime-export-options '(:section-numbers nil :with-author nil :with-toc nil)
      mu4e-alert-set-default-style 'libnotify)
;;Dont know why the shr var is defined here. It has something to do with counsel.

(setq mu4e-contexts
      (list
       ;; Personal
       (make-mu4e-context
	:name "Personal"
	:match-func
	(lambda (msg)
	  (when msg
	    (string-prefix-p "/Personal" (mu4e-message-field msg :maildir))))
	:vars '((user-mail-address . "Shipley7701@gmail.com")
		(user-full-name . "Christian Johnson")
		(smtpmail-smtp-server . "smtp.gmail.com")
		(smtpmail-smtp-service . 465)
		(smtpmail-stream-type . ssl)
		(mu4e-drafts-folder . "/Personal/[Gmail]/Drafts")
		(mu4e-sent-folder . "/Personal/[Gmail]/Sent Mail")
		(mu4e-refile-folder . "/Personal/[Gmail]/All Mail")
		(mu4e-trash-folder . "/Personal/[Gmail]/Trash")))
       (make-mu4e-context
	:name "Work"
	:match-func
	(lambda (msg)
	  (when msg
	    (string-prefix-p "/Work" (mu4e-message-field msg :maildir))))
	:vars '((user-mail-address . "C.Ship.Johnson@gmail.com")
		(user-full-name . "Christian Johnson")
		(smtpmail-smtp-server . "smtp.gmail.com")
		(smtpmail-smtp-service . 465)
		(smtpmail-stream-type . ssl)
		(mu4e-drafts-folder . "/Work/[Gmail]/Drafts")
		(mu4e-sent-folder . "/Work/[Gmail]/Sent Mail")
		(mu4e-refile-folder . "/Work/[Gmail]/All Mail")
		(mu4e-trash-folder . "/Work/[Gmail]/Trash")))))

(setq mu4e-maildir-shortcuts
      '(("/Personal/Inbox" . ?i)
	("/Personal/[Gmail]/Sent Mail" . ?s)
	("/Personal/[Gmail]/Trash" . ?t)
	("/Personal/[Gmail]/Drafts" . ?d)
	("/Personal/[Gmail]/All Mail" . ?a)))



(add-to-list 'mu4e-bookmarks '("m:/Work/Inbox or m:/Personal/Inbox" "All Inboxes" ?I))
(add-to-list 'mu4e-bookmarks '("m:/Work/Inbox" "Work" ?w))
(add-to-list 'mu4e-bookmarks '("m:/Personal/Inbox" "Personal" ?p))
;;Add custom actions to mu4e for capture templates
(add-to-list 'mu4e-headers-actions
             '("follow up" . cj/capture-mail-follow-up) t)
(add-to-list 'mu4e-view-actions
             '("follow-up" . cj/capture-mail-follow-up) t)
(add-to-list 'mu4e-headers-actions
             '("read later" . cj/capture-mail-read-later) t)
(add-to-list 'mu4e-view-actions
             '("read later" . cj/capture-mail-read-later) t)
(add-to-list 'message-send-hook 'org-mime-htmlize)
(add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
(add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)

(defun cj/capture-mail-follow-up (msg)
  (interactive)
  (call-interactively 'org-store-link)
  (org-capture nil "mf"))
(defun cj/capture-mail-read-later (msg)
  (interactive)
  (call-interactively 'org-store-link)
  (org-capture nil "mr"))
;; Store a link for the mu4e query (go to a page and this finds the query to get you there)
(defun cj/store-link-to-mu4e-query ()
  (interactive)
  (let ((mu4e-org-link-query-in-headers-mode t))
    (call-interactively 'org-store-link)))

;; Customize headers columns
(setq mu4e-headers-fields
      '(;;(:empty . 2)
        (:human-date . 12)     ;; Date in human-readable format
        (:flags . 5)
        (:from . 22)           ;; Sender's name/email
        (:subject . nil)))     ;; Subject of the email
(setq mu4e-headers-thread-root-prefix                 '("" . "")
      mu4e-headers-thread-first-child-prefix          '("│  " . "│  ")
      mu4e-headers-thread-child-prefix                '("│  " . "│  ")
      mu4e-headers-thread-connection-prefix           '("│  " . "│  ")
      mu4e-headers-thread-last-child-prefix          '("╰─●    " . "╰─●    ")
      mu4e-headers-thread-blank-prefix               '("" . "")
      mu4e-headers-thread-orphan-prefix              '("orphan1" . "orphan2")
      mu4e-headers-thread-single-orphan-prefix       '("orphan3" . "orphan4")
      mu4e-headers-thread-duplicate-prefix           '("☰▶  " . "☰▶  "))
(setq mu4e-use-fancy-chars t)
(setq mu4e-headers-new-mark            '("N" . " ")
      mu4e-headers-passed-mark         '("P" . "")
      mu4e-headers-seen-mark           '("S" . "")
      mu4e-headers-unread-mark         '("u" . " ")
      mu4e-headers-flagged-mark        '("F" . "󰉀 ")
      mu4e-headers-attach-mark         '("a" . " ")
      mu4e-headers-replied-mark        '("R" . " ")
      mu4e-headers-signed-mark         '("s" . " ")
      mu4e-headers-list-mark           '("l" . "")
      mu4e-headers-calendar-mark       '("c" . "󰃭 ")
      mu4e-headers-personal-mark       '("p" . "")
      mu4e-headers-trashed-mark        '("T" . "󰩹 "))
(setq mu4e-modeline-all-clear      '("C" . "󰇯 ")
      mu4e-modeline-all-read       '("R:" . "󰪱 ")
      mu4e-modeline-unread-items   '("U:" . "󰛏 ")
      mu4e-modeline-new-items      '("N:" . "󰧫 "))
(setq mu4e-marks
      '((refile
         :char ("r" . " ")
         :prompt "refile"
         :dyn-target (lambda (target msg) (mu4e-get-refile-folder msg))
         :action (lambda (docid msg target) (mu4e--server-move docid (mu4e--mark-check-target target) "-N")))
        (delete :char ("D" . "󰆴 ")
                :prompt "Delete"
                :show-target
                (lambda
                  (target)
                  "delete")
                :action
                (lambda
                  (docid msg target)
                  (mu4e--server-remove docid)))
        (flag :char
              ("+" . "󰮚 ")
              :prompt "+flag"
              :show-target
              (lambda
                (target)
                "flag")
              :action
              (lambda
                (docid msg target)
                (mu4e--server-move docid nil "+F-u-N")))
        (move :char
              ("m" . "󱉆 ")
              :prompt "move"
              :ask-target mu4e--mark-get-move-target
              :action
              (lambda
                (docid msg target)
                (mu4e--server-move docid
                                   (mu4e--mark-check-target target)
                                   "-N")))
        (read :char
              ("!" . "󰑇 ")
              :prompt "!read"
              :show-target
              (lambda
                (target)
                "read")
              :action
              (lambda
                (docid msg target)
                (mu4e--server-move docid nil "+S-u-N")))
        (trash :char
               ("d" . "󰆴 ")
               :prompt "dtrash"
               :dyn-target
               (lambda
                 (target msg)
                 (mu4e-get-trash-folder msg))
               :action
               (lambda
                 (docid msg target)
                 (mu4e--server-move docid
                                    (mu4e--mark-check-target target)
                                    "+T-N")))
        (unflag :char
                ("-" . "󱣮 ")
                :prompt "-unflag"
                :show-target
                (lambda
                  (target)
                  "unflag")
                :action
                (lambda
                  (docid msg target)
                  (mu4e--server-move docid nil "-F-N")))
        (untrash :char
                 ("=" . "󱂧 ")
                 :prompt "=untrash"
                 :show-target
                 (lambda
                   (target)
                   "untrash")
                 :action
                 (lambda
                   (docid msg target)
                   (mu4e--server-move docid nil "-T")))
        (unread :char
                ("?" . " ")
                :prompt "?unread"
                :show-target
                (lambda
                  (target)
                  "unread")
                :action
                (lambda
                  (docid msg target)
                  (mu4e--server-move docid nil "-S+u-N")))
        (unmark :char " "
                :prompt "unmark"
                :action
                (mu4e-error "No action for unmarking"))
        (action :char
                ("a" . "󰐾 ")
                :prompt "action"
                :ask-target
                (lambda nil
                  (mu4e-read-option "Action: " mu4e-headers-actions))
                :action
                (lambda
                  (docid msg actionfunc)
                  (save-excursion
                    (when
                        (mu4e~headers-goto-docid docid)
                      (mu4e-headers-action actionfunc)))))
        (something :char
                   ("*" . " ")
                   :prompt "*something"
                   :action
                   (mu4e-error "No action for deferred mark"))))


(run-at-time
 "10 sec" nil (lambda ()
		(let ((current-prefix-arg '(4)))
		  (call-interactively 'mu4e)
		  (message nil))))

;; This 'run-at-time' statement is for eww, so that it can open mu4e on click.
