;;; name: Org Roam
;;; depends: org
;;; conflicts:
;;; description:

(if (member "hydra" sliver--loaded-modules)
    (pretty-hydra-define+ space-menu
      (:foreign-keys warn :title "󰘧" :quit-key ("<escape>" "C-g"))
      (
       "Frequent"
       (("r" space-menu-roam/body "   Roam" :exit t))))
  )
