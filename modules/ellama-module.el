(use-package ellama)

(setopt ellama-language "English")
(require 'llm-ollama)

(setopt ellama-provider
	(make-llm-ollama
	 :chat-model "llama2"
	 :host "192.168.1.172"
	 :embedding-model "nomic-embed-text"))


	 
