;;; name: autosaves
;;; depends:
;;; conflicts:
;;; description: Configures Emacs autosave and backup directories and settings


(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)
(make-directory (expand-file-name "tmp/backups/" user-emacs-directory) t)
(make-directory (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory) t)

(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory)))
      auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))
      
