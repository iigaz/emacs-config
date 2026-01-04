;;; programming-python.el --- Utilities for programming in Python. -*- lexical-binding: t; -*-

;;; Compile
(add-hook 'python-mode-hook
          (lambda ()
            (unless (or (file-exists-p "makefile")
                        (file-exists-p "Makefile"))
              (set (make-local-variable 'compile-command)
                   (concat "python "
                           buffer-file-name)))))

;;; Virtual Environments
(use-package pyvenv
  :defer t
  :config
  ;; (setenv "WORKON_HOME" (expand-file-name "~/virtualenvs/"))
  (setq pyvenv-menu t)
  (add-hook 'pyvenv-post-activate-hooks (lambda ()
                                          (pyvenv-restart-python)))
  :hook (python-mode . pyvenv-mode))

;;; Lsp-mode
(with-eval-after-load 'lsp-mode
  (add-hook 'python-mode-hook 'lsp))

;;; Dap-mode
(with-eval-after-load 'dap-mode  
  (require 'dap-python)
  (setq dap-python-debugger 'debugpy))

;;; programming-python.el ends here
