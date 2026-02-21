;;; general-packages.el --- Generally useful packages. -*- lexical-binding: t; -*-
;; Contains various packages, useful for just about any Emacs workflow.

;;; Undo-Redo
;; Visual undo-redo tree
(use-package vundo
  :bind (("C-M-z" . vundo))
  :custom
  (vundo-glyph-alist vundo-unicode-symbols))

;; Un-cycles undo and redo.
(use-package undo-fu :defer t)

;;; Package Management
;; Try packages before installing
(use-package try :commands (try))

;;; UI

(use-package mlscroll
  :custom
  (mlscroll-in-color "DeepSkyBlue")
  (mlscroll-out-color nil)
  (mlscroll-right-align nil)
  (mlscroll-alter-percent-position 'replace)
  :init
  (mlscroll-mode 1))

(use-package minions
  :config
  (setq minions-mode-line-lighter "🥞")
  :init
  (minions-mode 1))

;; Themes
(use-package auto-dark
  :defer t
  :custom
  (auto-dark-light-theme 'tsdh-light)
  (auto-dark-dark-theme 'modus-vivendi)
  :init (auto-dark-mode t))

;;; UX

(use-package which-key
  :init (which-key-mode))

(use-package vertico
  :init
  (vertico-mode)
  (vertico-mouse-mode)
  :bind (:map vertico-map
              ("C-<tab>" . vertico-next)
              ("C-<iso-lefttab>" . vertico-previous))
  :config
  (keymap-set vertico-map "RET" #'vertico-directory-enter)
  (keymap-set vertico-map "DEL" #'vertico-directory-delete-char)
  (keymap-set vertico-map "M-DEL" #'vertico-directory-delete-word)
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy))

(use-package savehist
  :ensure nil
  :init
  (savehist-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))))
  :init
  (define-advice company-capf (:around (orig-fun &rest args) set-completion-styles) 
    (let ((completion-styles '(basic partial-completion orderless)))
      (apply orig-fun args))))

(use-package marginalia
  :init
  (marginalia-mode))

;;; Navigation

;; Jump to character
(use-package avy
  :bind (("C-;" . avy-goto-char)))

;; Better than isearch
(use-package ctrlf
  :init
  (ctrlf-mode +1)
  :custom
  (ctrlf-default-search-style 'regexp)
  (ctrlf-alternate-search-style 'literal)
  :config
  (defun ig/ctrlf-forward-default (&optional arg)
    "A slight modification to `ctrlf-forward-default' to initiate search
with selected region. If no region is selected, continue as usual."
    (interactive "P")
    (let ((initial (when (use-region-p)
                     (buffer-substring (region-beginning) (region-end)))))
      (deactivate-mark)
      (ctrlf-forward ctrlf-default-search-style
                     (null arg) initial nil t)))
  (define-key ctrlf-mode-map [remap isearch-forward] #'ig/ctrlf-forward-default))

;; Better than plain query-replace
(use-package visual-replace
  :custom
  (visual-replace-default-to-full-scope t)
  :init
  (visual-replace-global-mode 1)

  (defun ig/visual-replace-from-ctrlf ()
    (interactive)
    (when (minibuffer-prompt)
      (let* ((query (minibuffer-contents))
             (args (visual-replace-make-args
                    :from query
                    :to ""
                    :regexp (eq ctrlf--style 'regexp)
                    :case-fold ctrlf--case-fold-search)))
        (run-at-time nil nil
                     (lambda ()
                       (apply #'visual-replace (visual-replace-read args))))
        (exit-minibuffer))))
  (define-key ctrlf-minibuffer-mode-map (kbd "C-r") #'ig/visual-replace-from-ctrlf))

;; Toolbox for navigation with preview
(use-package consult
  :bind (([remap switch-to-buffer] . consult-buffer)
         ("C-c f" . consult-line)
         ("C-<tab>" . consult-buffer)))

;;; Spell-check

(use-package flyspell
  :ensure nil
  :hook ((prog-mode . flyspell-prog-mode)
         (text-mode . flyspell-mode))
  :bind (:map flyspell-mode-map
              ("C-;" . nil))) ;; Used by avy

;;; Multiple cursors

(use-package multiple-cursors
  :config
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C->") 'mc/mark-more-like-this-extended)
  (global-set-key (kbd "C-c C-r") 'mc/mark-all-dwim)
  (define-key mc/keymap (kbd "<return>") nil)
  (define-key mc/keymap (kbd "<mouse-1>") 'mc/keyboard-quit)
  (global-unset-key (kbd "M-<down-mouse-1>"))
  (global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)
  (global-unset-key (kbd "<down-mouse-2>"))
  (global-set-key (kbd "<mouse-2>") 'mc/add-cursor-on-click))

;;; general-packages.el ends here
