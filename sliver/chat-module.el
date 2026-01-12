;;; name: Chat
;;; depends:
;;; conflicts:
;;; description:


; IRC Client
(use-package erc)
(setq erc-server "irc.libera.chat"
      erc-nick "shipley7701"
      erc-user-full-name "Christian Johnson"
      erc-track-shorten-start 8
      erc-autojoin-channels-alist '(nil) ;("irc.libera.chat" "#systemcrafters" "#emacs")
      erc-kill-buffer-on-part t
      erc-auto-query 'bury)


;; Matrix Client
(use-package ement)
(setf use-default-font-for-symbols nil)
(set-fontset-font t 'unicode "Noto Emoji" nil 'append)

