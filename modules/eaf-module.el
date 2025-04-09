(use-package eaf
  :straight (eaf :type git :host github
		 :repo "emacs-eaf/emacs-application-framework"
		 :files ("*.el" "*.py" "core" "app" "*.json")
		 :includes (eaf-browser eaf-pdf-viewer)
		 :pre-build (("python" "install-eaf.py" "--install" "browser" "pdf-viewer"))))

(use-package eaf-pdf-viewer
  :after eaf)

