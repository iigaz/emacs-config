;;; spell-checking.el --- Additional packages for spell checking. -*- lexical-binding: t; -*-

(use-package jinx
  :hook (emacs-startup . global-jinx-mode)
  :bind (("C-c ." . jinx-correct)
         ("C-c C-." . jinx-correct)
         ("C-c M-." . jinx-languages))
  :config
  (remove-hook 'prog-mode-hook #'flyspell-prog-mode)
  (remove-hook 'text-mode-hook #'flyspell-mode)
  :custom
  (jinx-languages "en_GB ru_RU"))

;;; spell-checking.el ends here
