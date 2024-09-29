
(straight-use-package
 '(emacs-bitwarden :type git :host github :repo "seanfarley/emacs-bitwarden"))

(bitwarden-auth-source-enable)

(setq auth-sources
      '((:source "~/.authinfo")))
