;;; name: Ellama
;;; depends:
;;; conflicts:
;;; description:


(use-package ellama)

(setopt ellama-language "English")
(require 'llm-ollama)

(setopt ellama-provider
	(make-llm-ollama
	 :chat-model "llama2"
	 :host "192.168.1.172"
	 :embedding-model "nomic-embed-text"))

(if (member "hydra" sliver--loaded-modules)
    (pretty-hydra-define+ space-menu
      (:foreign-keys warn :title "󰘧" :quit-key ("<escape>" "C-g"))
      (
       "Utilities"
       (("E" space-menu-ellama/body "   Ellama" :exit t))))
  )

