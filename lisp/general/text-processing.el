;;; text-processing.el --- Markup languages and utilities for reading and editing text files. -*- lexical-binding: t; -*-

;;; Org
(use-package org
  :ensure nil
  :bind (:map org-mode-map
              ("C-p" . org-export-dispatch))
  :custom
  (org-src-window-setup 'current-window)
  :config
  ;; Enable structure templates and add one for elisp code.
  ;; To use, write "<el" in org mode and press tab.
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp")))

;;; Markdown
(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :custom-face
  (markdown-header-delimiter-face ((t (:inherit markdown-comment-face))))
  (markdown-header-face-1 ((t (:inherit (org-level-1 markdown-header-face) :height 1.8))))
  (markdown-header-face-2 ((t (:inherit (org-level-2 markdown-header-face) :height 1.4))))
  (markdown-header-face-3 ((t (:inherit (org-level-3 markdown-header-face) :height 1.2))))
  (markdown-header-face-4 ((t (:inherit (org-level-4 markdown-header-face) :height 1.15))))
  (markdown-header-face-5 ((t (:inherit (org-level-5 markdown-header-face) :height 1.1))))
  (markdown-header-face-6 ((t (:inherit (org-level-6 markdown-header-face) :height 1.05))))
  :custom
  (markdown-asymmetric-header t)
  (markdown-indent-on-enter 'indent-and-new-item)
  (markdown-enable-math t)
  (markdown-command '("pandoc" "-d" "html" "--from=markdown+emoji" "--to=html5"))
  :bind (:map markdown-mode-map
              ("C-1" . markdown-insert-header-atx-1)
              ("C-2" . markdown-insert-header-atx-2)
              ("C-3" . markdown-insert-header-atx-3)
              ("C-4" . markdown-insert-header-atx-4)
              ("C-5" . markdown-insert-header-atx-5)
              ("C-6" . markdown-insert-header-atx-6)
              ("C-c C-b" . markdown-insert-bold)
              ("C-c C-i" . markdown-insert-italic)
              ("C-c C-l" . markdown-insert-link)
              ("C-c C-f" . markdown-insert-image)
              ("C-c C-m" . markdown-insert-code)
              ("C-c C-s" . markdown-insert-strike-through)
              ("<tab>" . markdown-cycle)
              ("C-p" . markdown-preview)
              ("C-S-p" . markdown-export)
              ("C-M-p" . markdown-export))
  :hook ((markdown-mode . visual-line-mode)))

;;; "Zen-mode"
(use-package darkroom
  :bind (("C-c d" . darkroom-tentative-mode)))

;; ToC generator
(use-package toc-org
  :after (org markdown-mode)
  :hook ((org-mode markdown-mode) . toc-org-mode)
  :bind (:map markdown-mode-map
              ([remap markdown-follow-thing-at-point] . 'toc-org-markdown-follow-thing-at-point)))

;;; Images From Clipboard
(progn
  (defun ig/save-image-from-clipboard (filepath &optional mimetype)
    "Save an image from clipboard to the `filepath' with type `mimetype',
  or image/png if none provided. Only works on linux."
    (interactive
     (list
      (read-file-name "File to save to:")
      (read-string "Mimetype:" "image/png")))
    (let* ((mimetype (or mimetype "image/png"))
           (command (concat "xclip -selection clipboard -t " mimetype " -o > \"" filepath "\"")))
      (message (concat "Executing " command))
      (shell-command command)))

  (defun ig/paste-image-from-clipboard (folder-to-save-to/)
    "If clipboard contains image, try to save it to the same folder
  the current file is in and return the resulting filepath.
  If clipboard contains image in multiple formats,
  it will pick the first one it finds.
  Returns nil if clipboard doesn't contain any image.
  Uses `ig/save-image-from-clipboard', thus only works on linux."
    (interactive (list (read-directory-name "Save image to folder:")))
    (let* ((clipboard-contents-may-be-symbol (gui-get-selection 'CLIPBOARD 'TARGETS))
           (clipboard-contents (if (sequencep clipboard-contents-may-be-symbol) clipboard-contents-may-be-symbol
                                 `[,clipboard-contents-may-be-symbol])))
      (when (sequencep clipboard-contents)
        (let* ((image-types-list (seq-filter
                                  (lambda (type)
                                    (pcase-let ((`(,major ,minor) (split-string (symbol-name type) "/")))
                                      (if (equal major "image")
                                          type
                                        nil)))
                                  clipboard-contents))
               (image-mimetype (car-safe image-types-list)))
          (when image-mimetype
            (let* ((extension (cond ((eq image-mimetype 'image/png) ".png")
                                    ((or (eq image-mimetype 'image/jpeg) (eq image-mimetype 'image/jpg)) ".jpg")
                                    ((eq image-mimetype 'image/webp) ".webp")
                                    (t "")))
                   (filename (format-time-string (concat "%Y%m%d%H%M%S%3N" extension) (current-time)))
                   (filepath (concat folder-to-save-to/ filename)))
              (ig/save-image-from-clipboard filepath)
              filepath))))))

  (defun ig/markdown-paste-images-from-clipboard ()
    "Enables support for pasting images from the clipboard.
Only works on linux. Uses `yank'."
    (interactive)
    (let* ((using-temp-dir (not buffer-file-name))
           (current-folder (if using-temp-dir temporary-file-directory (file-name-directory buffer-file-name)))
           (pasted-image-filepath (ig/paste-image-from-clipboard current-folder)))
      (if pasted-image-filepath
          (if using-temp-dir
              (insert (concat "![](" pasted-image-filepath ")"))
            (insert (concat "![](./" (file-name-nondirectory pasted-image-filepath) ")")))
        (yank))))

  (put 'ig/markdown-paste-images-from-clipboard 'delete-selection 'yank)

  (defvar-keymap ig/markdown-paste-images-map
    :doc "Just remaps yank to custom function."
    "<remap> <yank>" #'ig/markdown-paste-images-from-clipboard)

  (define-minor-mode ig/markdown-paste-images
    "Allows you to paste images in markdown documents, straight from the clipboard.
Alas, only works on Linux."
    :lighter " MdPasteImg"
    :keymap ig/markdown-paste-images-map)

  (add-hook 'markdown-mode-hook 'ig/markdown-paste-images)
  ) ; Images From Clipboard

;;; text-processing.el ends here
