;;; name: Org-Latex-Extra
;;; depends: org org-extras
;;; conflicts:
;;; description: Extra functionality and UX for org-latex

(use-package org-fragtog)
(use-package engrave-faces)
;; engrave-faces is used for with '(org-latex-src-block-backend 'engraved)'

(setq org-latex-src-block-backend 'engraved)
