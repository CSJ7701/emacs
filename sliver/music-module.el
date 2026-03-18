;;; name: music
;;; depends:
;;; conflicts:
;;; description:

(require 'auth-source)
(use-package empv)
(global-set-key (kbd "C-c m") 'my-empv-hydra/body)

					; (with-eval-after-load 'embark (empv-embark-initialize-extra-actions))

(setq empv-subsonic-url "https://music.christiansjohnson.com")
(let ((match (car (auth-source-search :host empv-subsonic-url :max 1))))
      (when match
	(setq empv-subsonic-username (plist-get match :user))
	(setq empv-subsonic-password (let ((secret (plist-get match :secret)))
				       (if (functionp secret)
					   (funcall secret)
					 secret)))))


(pretty-hydra-define my-empv-hydra
  (:hint nil :color green :quit-key "q" :title "EMPV Media Control")
  ("Playback"
   (("SPC" empv-toggle "Play/Pause")
    ("f" (empv-seek 5) "Seek Forward")
    ("b" (empv-seek -5) "Seek Backward")
    ("-" empv-volume-down "Volume Down")
    ("+" empv-volume-up "Volume Up"))
   "Subsonic"
   (("s" empv-subsonic-songs "Songs")
   ("a" empv-subsonic-albums "Albums")
   ("A" empv-subsonic-artics "Artists")
   ("S" empv-subsonic-search "Search"))
   ))

;; === Player ===

(defun my-empv-refresh-ui (&rest _)
  "Refresh the player buffer if it is currently visible."
  (let ((buf (get-buffer "*empv-player*")))
    (when (and buf (get-buffer-window buf))
      (empv-player))))

;; Update UI automatically when media or playback state changes
(add-hook 'empv-media-title-changed-hook #'my-empv-refresh-ui)
(add-hook 'empv-player-state-changed-hook #'my-empv-refresh-ui)

(defun empv-status ()
  "Show current song and state in the minibuffer."
  (interactive)
  (if (and empv-media-title (not (eq empv-player-state 'stopped)))
      (message "%s: %s" empv-player-state empv-media-title)
    (message "empv is idle.")))

(defun empv-player ()
  "A dedicated buffer showing the current media information."
  (interactive)
  (with-current-buffer (get-buffer-create "*empv-player*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (propertize "EMPV PLAYER STATUS\n" 'face 'bold-alternate))
      (insert (make-string 20 ?-) "\n\n")
      (insert (format "State:  %s\n" empv-player-state))
      (insert (format "Media:  %s\n" (or empv-media-title "None")))
      (special-mode))
    (display-buffer (current-buffer))))
    

