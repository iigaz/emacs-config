;;; development.el --- Utilities for development. -*- lexical-binding: t; -*-
;; Contains things like terminals, version control systems, and modes
;; for configuration languages.

;;; Shell and Terminal
(use-package terminal-here
  :custom ((terminal-here-linux-terminal-command 'xfce4-terminal)))

(use-package bash-completion
  :init
  (autoload
    'bash-completion-dynamic-complete
    "bash-completion"
    "BASH completion hook")
  (add-hook
   'shell-dynamic-complete-functions
   #'bash-completion-dynamic-complete))

;; (use-package shell-command-x
;;   :init
;;   (shell-command-x-mode 1))

;;; Version Control
(use-package magit :defer t
  :config
  (setq magit-branch-read-upstream-first 'fallback))

(use-package magit-todos
  :after (magit)
  :init
  (magit-todos-mode)
  :config
  (setq magit-todos-branch-list nil)
  (define-key magit-todos-section-map "b" nil)
  (define-key magit-todos-item-section-map "b" nil)
  (define-key magit-todos-section-map "B" nil)
  (define-key magit-todos-item-section-map "B" nil))

(use-package diff-hl
  :init
  (global-diff-hl-mode)
  :config
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

;;; Modes for configuration files
(use-package yaml-mode
  :mode "\\.ya?ml\\'")

;;; development.el ends here
