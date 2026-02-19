;;; name: Sidebar
;;; depends:
;;; conflicts:
;;; description: Set up sidebar configuration


(setq window-sides-slots '(1 1 1 1))

(add-to-list 'display-buffer-alist
	     '("^side[-_]"
	       display-buffer-in-side-window
	       (side . left)
	       (slot . 0)
	       (window-parameters . ((no-delete-other-windows t)))
	       (window-width . 5)))
