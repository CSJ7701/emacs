;;; name: Authentication
;;; depends:
;;; conflicts:
;;; description:

;; I believe this requires the bitwarden cli to be installed
(straight-use-package
 '(emacs-bitwarden :type git :host github :repo "seanfarley/emacs-bitwarden"))

(bitwarden-auth-source-enable)

(setq auth-sources
      '((:source "~/.authinfo")))
