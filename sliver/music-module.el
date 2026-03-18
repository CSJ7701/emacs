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
    

