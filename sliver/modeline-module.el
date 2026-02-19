;;; name: Modeline
;;; depends:
;;; conflicts:
;;; description: Configure the modeline


(use-package all-the-icons)
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(setq doom-modeline-major-mode-color-icon t
      doom-modeline-buffer-state-icon t
      doom-modeline-buffer-name t
      doom-modeline-modal t
      doom-modeline-modal-icon t
      doom-modeline-minor-modes nil
      doom-modeline-time-icon t
      doom-modeline-time t
      doom-modeline-github t
      doom-modeline-mu43 t
      doom-modeline-buffer-encoding nil)
