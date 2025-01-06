(use-package ellama)

(setopt ellama-language "English")
(require 'llm-ollama)

(setopt ellama-provider
	(make-llm-ollama
	 :chat-model "llama3.2"
	 :embedding-model "nomic-embed-text"
	 :default-chat-non-standard-params '(("num_ctx" . 8192))))


	 
