;;; llms.el --- Packages for large language models. -*- lexical-binding: t; -*-

(use-package gptel
  :commands (gptel)
  :autoload (gptel-make-openai gptel-make-ollama)
  :bind (:map gptel-mode-map
              ("C-c k" . gptel-abort)))
;; Please customize available models in local.el!

;;; llms.el ends here
