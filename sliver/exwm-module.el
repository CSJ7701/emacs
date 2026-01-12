;;; name: EXWM
;;; depends:
;;; conflicts:
;;; description:

(defun cj/frame-visible ()
  (interactive)
  (set-frame-parameter nil 'alpha '(85 . 70)))
