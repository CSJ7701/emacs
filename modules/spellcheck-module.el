(use-package flyspell)
(use-package flyspell-correct-ivy)

(if (file-exists-p "/usr/bin/aspell")
    (progn
      (setq ispell-program-name "aspell")
      (eval-after-load "ispell"
        '(progn (defun ispell-get-coding-system () 'utf-8)))))

(define-key flyspell-mode-map "\M-\t"
	    'flyspell-correct-wrapper)
