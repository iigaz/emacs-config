;;; programming-general.el --- Utilities for programming in general. -*- lexical-binding: t; -*-

;;; In-buffer Completion
(use-package company :hook (prog-mode . company-mode)
  :bind (:map company-active-map
              ("M-k" . company-select-next)
              ("M-i" . company-select-previous)
              ("C-s" . save-buffer)))

(use-package company-quickhelp
  :after (company)
  :init (company-quickhelp-mode))

(use-package corfu
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  :custom
  (corfu-auto nil)
  (corfu-quit-no-match t)
  (corfu-quit-at-boundary t)
  (corfu-popupinfo-delay 1)
  (tab-always-indent 'complete)
  :config
  (setq completion-cycle-threshold 3)
  (add-hook 'minibuffer-setup-hook (lambda ()
                                     (when (memq #'completion-at-point
                                                 (flatten-tree
                                                  (current-local-map)))
                                       (corfu-mode)))))

;;; Editing
(use-package smart-semicolon
  :hook ((c-mode . smart-semicolon-mode)
         (c-mode-common . smart-semicolon-mode)
         (c++-mode . smart-semicolon-mode)
         (java-mode . smart-semicolon-mode)))

(use-package editorconfig
  :init
  (editorconfig-mode 1))

;;; Navigation
(use-package dumb-jump
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read))

;;; Formatting
(use-package apheleia
  :bind (:map prog-mode-map ("C-S-i" . apheleia-format-buffer))
  :init
  ;; (apheleia-global-mode +1) ; enables format-on-save
  :config
  (setf (alist-get 'clang-format apheleia-formatters)
        '("clang-format" "-style" "Microsoft" "-assume-filename"
          (or
           (buffer-file-name)
           (apheleia-formatters-mode-extension)
           ".c")))
  (push '(csharpier "dotnet" "csharpier") apheleia-formatters)
  (push '(csharp-mode . csharpier) apheleia-mode-alist))

;;; Highlighting
(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character))

(use-package hl-todo ; highlights TODO, FIXME, XXX, etc.
  :defer t
  :init
  (global-hl-todo-mode))

(use-package rainbow-mode ; highlights colour names
  :defer t
  :init
  (add-hook 'prog-mode-hook 'rainbow-mode))

(use-package rainbow-delimiters ; highlights parentheses
  :defer t
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;;; Syntax Checking
(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

;;; LSP
(progn
  (use-package lsp-mode
    :bind (:map lsp-mode-map
                ("C-." . lsp-execute-code-action)
                ("<f2>" . lsp-rename))
    :hook ((lsp-mode . lsp-enable-which-key-integration)
           (lsp-completion-mode . my/lsp-mode-setup-completion))
    :custom
    (lsp-headerline-breadcrumb-enable nil)
    (lsp-ui-doc-border "black")
    :commands lsp
    :config
    (setq lsp-modeline-code-actions-segments '(count icon name)
          lsp-keep-workspace-alive nil)
    :init
    (defun my/lsp-mode-setup-completion ()
      (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
            '(orderless))) ;; Configure orderless
    )

  (use-package lsp-ui
    :defer t
    :config
    (add-hook 'lsp-ui-doc-mode-hook
              (lambda ()
                (when lsp-ui-doc-mode
                  (remove-hook 'post-command-hook #'lsp-ui-doc--make-request t))))
    (setq lsp-ui-doc-show-with-mouse nil)
    (setq lsp-ui-doc-show-with-cursor t)
    :commands lsp-ui-mode)
  ) ; LSP

;;; DAP
(progn
  (use-package dap-mode
    :defer t
    :custom-face
    (dap-ui-pending-breakpoint-face ((t (:background "gray90"))))
    (dap-ui-verified-breakpoint-face ((t (:background "RosyBrown1"))))
    :config
    (add-hook 'dap-ui-mode-hook (lambda ()
                                  (define-key dap-ui-mode-map (kbd "<f10>") 'dap-next)
                                  (define-key dap-ui-mode-map (kbd "<f11>") 'dap-step-in)
                                  (define-key dap-ui-mode-map (kbd "S-<f11>") 'dap-step-out)
                                  (define-key dap-ui-mode-map (kbd "S-<f5>") 'dap-continue)))
    (setq dap-auto-show-output nil
          dap-output-window-max-height 10)
    (global-set-key (kbd "C-<f5>") 'dap-debug))

  (with-eval-after-load 'dap-ui
    (setq dap-ui-buffer-configurations
          `((,dap-ui--locals-buffer . ((side . left) (slot . 2) (window-width . ,treemacs-width)))
            (,dap-ui--expressions-buffer . ((side . right) (slot . 2) (window-width . 0.20)))
            (,dap-ui--sessions-buffer . ((side . right) (slot . 3) (window-width . 0.20)))
            (,dap-ui--breakpoints-buffer . ((side . left) (slot . 2) (window-width . ,treemacs-width)))
            (,dap-ui--debug-window-buffer . ((side . bottom) (slot . 3) (window-width . 0.20)))
            (,dap-ui--repl-buffer . ((side . bottom) (slot . 1) (window-height . 0.45))))))
  (setq dap-auto-configure-features '(locals controls tooltip))
  ) ; DAP

;;; programming-general.el ends here
