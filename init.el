(setq gc-cons-threshold (* 100 1024 1024))
(setq read-process-output-max (* 1024 1024))

(let
    ((default-directory (expand-file-name "lisp" user-emacs-directory)))
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-subdirs-to-load-path))

(require 'modular-config)
(setq modular-config-list '((none ())
                            (basic (basic-config))
                            (general
                             ((basic) general-packages development
                              text-processing icons spell-checking))
                            (programming
                             ((general) programming-general
                              programming-frontend programming-cpp))
                            (main
                             ((programming) games restclient llms test))))
(setq modular-config-default 'main)
(setq modular-config-use-packages-from-load-path 't)
(modular-config-command-line-args-process)

(when
    (file-exists-p
     (expand-file-name "customize.el" user-emacs-directory))
  (load (expand-file-name "customize.el" user-emacs-directory)))
(when
    (file-exists-p (expand-file-name "local.el" user-emacs-directory))
  (load (expand-file-name "local.el" user-emacs-directory)))
