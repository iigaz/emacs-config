;;; programming-flutter.el --- Utilities for programming in Dart with Flutter. -*- lexical-binding: t; -*-

;;; Dart
(use-package dart-mode
  :defer t
  :config
  (defun project-try-dart (dir)
    (let ((project (or (locate-dominating-file dir "pubspec.yaml")
                       (locate-dominating-file dir "BUILD"))))
      (if project
          (cons 'dart project)
        (cons 'transient dir))))
  (add-hook 'project-find-functions #'project-try-dart)
  (cl-defmethod project-roots ((project (head dart)))
    (list (cdr project)))
  (with-eval-after-load 'lsp-mode
    (add-hook 'dart-mode-hook 'lsp)))

;;; Flutter
(use-package flutter
  :after dart-mode
  :bind (:map dart-mode-map
              ("C-c C-c" . #'flutter-run-or-hot-reload))
  :hook (dart-mode . flutter-test-mode))

;;; Lsp-mode
(use-package lsp-dart
  :after lsp-mode)

;;; programming-flutter.el ends here
