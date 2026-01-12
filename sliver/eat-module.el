;;; name: Eat
;;; depends:
;;; conflicts:
;;; description:

(straight-use-package
 '(eat :type git
       :host codeberg
       :repo "akib/emacs-eat"
       :files ("*.el" ("term" "term/*.el") "*.texi"
               "*.ti" ("terminfo/e" "terminfo/e/*")
               ("terminfo/65" "terminfo/65/*")
               ("integration" "integration/*")
               (:exclude ".dir-locals.el" "*-tests.el"))))

(add-hook 'eshell-load-hook #'eat-eshell-mode)
(setq eat-default-cursor-type "visible")

