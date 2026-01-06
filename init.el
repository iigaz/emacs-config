(setq gc-cons-threshold (* 100 1024 1024))
(setq read-process-output-max (* 1024 1024))

(let ((default-directory (expand-file-name "lisp" user-emacs-directory)))
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-subdirs-to-load-path))

(load "main-config")

(when (file-exists-p (expand-file-name "customize.el" user-emacs-directory))
  (load (expand-file-name "customize.el" user-emacs-directory)))
(when (file-exists-p (expand-file-name "local.el" user-emacs-directory))
  (load (expand-file-name "local.el" user-emacs-directory)))
