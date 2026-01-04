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
  (ctrlf-alternate-search-style 'literal))

;; Toolbox for navigation with preview
(use-package consult
  :bind (([remap switch-to-buffer] . consult-buffer)
         ("C-c f" . consult-line)))

;;; Spell-check

;; TODO: Replace with jinx
(use-package flyspell
  :ensure nil
  :hook ((prog-mode . flyspell-prog-mode)
         (text-mode . flyspell-mode))
  :bind (:map flyspell-mode-map
              ("C-;" . nil))) ;; Used by avy

;;; Multiple cursors

;; TODO: Research other useful methods
(use-package multiple-cursors
  :defer t
  :config
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (define-key mc/keymap (kbd "<return>") nil)
  (define-key mc/keymap (kbd "<mouse-1>") 'mc/keyboard-quit)
  (global-unset-key (kbd "M-<down-mouse-1>"))
  (global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)
  (global-unset-key (kbd "<down-mouse-2>"))
  (global-set-key (kbd "<mouse-2>") 'mc/add-cursor-on-click))

;;; general-packages.el ends here
