(use-package dired-single)

(general-def 'dired-mode-map
  "h" 'dired-single-up-directory
  "l" 'dired-single-buffer)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-open
  :config
  (add-to-list 'dired-open-functions #'dired-open-xdg t)
  (setq dired-open-extensions '(("png" . "feh")
				("mkv" . "mpv"))))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (general-def 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

(use-package dired-rainbow)

(defconst dired-audio-files-extensions
  '("mp3" "MP3" "ogg" "OGG" "flac" "FLAC" "wav" "WAV")
  "Dired Audio files extensions")

(dired-rainbow-define audio "#329EE8" dired-audio-files-extensions)

(defconst dired-video-files-extensions
  '("vob" "VOB" "mkv" "MKV" "mpe" "mpg" "MPG" "mp4" "MP4" "ts" "TS"
    "m2ts" "M2TS" "avi" "AVI" "mov" "MOV" "wmv" "asf" "m2v" "m4v" "mpeg" "MPEG" "tp")
  "Dired Video files extensions")

(dired-rainbow-define video "#B3CCFF" dired-video-files-extensions)

(setq dired-listing-switches "-lXGh --group-directories-first"
      dired-recursive-copies 'always
      dired-recursive-deletes 'top
      global-auto-revert-non-file-buffers t
      auto-revert-verbose nil
      dired-dwim-target t
      wdired-allow-to-change-permissions t)

(use-package dired-filetype-face
  :ensure t
  :config (require 'dired-filetype-face))
(deffiletype-face "link" "snow4")
