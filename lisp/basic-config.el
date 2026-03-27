;;; basic-config.el --- Basic Emacs configuration. -*- lexical-binding: t; -*-

;; Basic modules are considered essential and should contain as little
;; dependencies as possible while maintaining comfort. They should be
;; loaded first, before any other modules, so that if something went
;; wrong during start-up the user would be in a familiar environment.

;;; Emacs
(progn
  ;; Relocate backup files
  (setq backup-directory-alist `(("." . "~/.emacs-backups")))
  (setq backup-by-copying t)

  ;; File to store customizations done with 'customize'
  (setq custom-file (concat user-emacs-directory "customize.el"))
  ;; Load custom file
  (when (file-readable-p custom-file) (load custom-file))

  (recentf-mode t)
  (setq recentf-max-saved-items 50)
  ) ; Emacs

;;; Package Management
(progn
  (require 'package)

  ;; Enable MELPA - the largest repository of third-party packages.
  (add-to-list 'package-archives '("melpa" . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/melpa/") t)
  (package-initialize)

  ;; For old versions of Emacs; nowadays it's installed by default.
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

  ;; In use-package, ensure means installing a package if it's not
  ;; installed. This behaviour is desired for most packages, and without
  ;; the code below writing ":ensure t" would be required.
  (require 'use-package-ensure)
  (setq use-package-always-ensure t)
  ) ; Package Management

;;; UI
(progn
  ;; Remove scroll bar
  (scroll-bar-mode -1)
  ;; Remove tool bar (the one with big icons)
  (tool-bar-mode -1)

  ;; Highlight current line
  (global-hl-line-mode t)

  ;; Enable line numbers, but only in programming modes
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)

  ;; Enable column number in the mode line
  (column-number-mode 1) 

  ;; Thin cursor in text modes
  (add-hook 'text-mode-hook
            (lambda () (setq cursor-type 'bar)))

  ;; Remove right fringe and enlarge left fringe
  (fringe-mode '(12 . 0))

  ;; Set the strongest scratch buffer on start-up
  (setq initial-scratch-message "\n;;         /*\\           /*\\         \n;;        /---\\_       _/---\\        \n;;        (-\\---\\     /---/-)        \n;;        |--\\-\\-\\___/-/-/--|        \n;;        ]-\\-_--*****--_-/-[        \n;;        ]--/∙∙∙∙∙∙∙∙∙∙∙\\--[        \n;;        >-(∙∙∙∙∙∙∙∙∙∙∙∙∙)-<        \n;;        /_[∙∙∙∙∙∙∙∙∙∙∙∙∙]_\\        \n;;        * [∙∙,^∙∙∙∙∙^.∙∙] *        \n;;         ][∙(__\\_∙_/__)∙][         \n;;         ]\\∙|\\_/ v \\_/|∙/[         \n;;         </\\|    ‿    |/->         \n;;          \\_v--.___.--,^/          \n;;           /-~| >v< |~-_\\          \n;;         -- _\\]*\"-\"*[/_  -         \n;;        (__/-`---------`-_)        \n;;         _/\',~~~.--,/~~.~`\\        \n;;        (∙∙∙__∙∙∙\\/∙∙__∙∙∙)        \n;;        *--(..\\_____/..)--*        \n;;           \\__/     \\__/           \n")
  (setq inhibit-startup-screen t)

  ;; How to construct names to buffers with similar file name
  (setq uniquify-buffer-name-style 'forward)

  ;; Default frame (window) name
  (setq-default frame-title-format '("%b - GNU Emacs"))
  ) ; UI

;;; Editor
(progn
  ;; Enable horizontal scroll
  (setq mouse-wheel-tilt-scroll t)

  ;; Do not disable scroll-left
  (put 'scroll-left 'disabled nil)

  ;; Map cyrillic layout to qwerty
  (defun map-jtsuken-to-qwerty (jtsuken qwerty pos)
    (when (< pos (length jtsuken))
      (let ((from (aref jtsuken pos))
            (to (aref qwerty pos)))
        (define-key key-translation-map (kbd (concat "C-" (string from))) (kbd (concat "C-" (string to))))
        (define-key key-translation-map (kbd (concat "M-" (string from))) (kbd (concat "M-" (string to)))))
      (map-jtsuken-to-qwerty jtsuken qwerty (+ 1 pos))))
  (map-jtsuken-to-qwerty
   "йцукенгшщзхъфывапролджэячсмитьбюЙЦУКЕНГШЩЗХЪФЫВАПРОЛДЖ\ЭЯЧСМИТЬБЮ№"
   "qwertyuiop[]asdfghjkl;'zxcvbnm,.QWERTYUIOP{}ASDFGHJKL:\"ZXCVBNM<>#"
   0)

  ;; Auto-insert bracket pairs
  (setq-default electric-pair-pairs '((?\{ . ?\})
                                      (?\( . ?\))
                                      (?\[ . ?\])
                                      (?\" . ?\")))
  (add-hook 'prog-mode-hook 'electric-pair-local-mode)

  ;; Auto-complete xml tags
  (setq-default nxml-slash-auto-complete-flag t)

  ;; Delete selected region
  (delete-selection-mode 1)

  ;; Enable drag & drop
  (setq mouse-drag-and-drop-region t)

  ;; Move by word in camel and pascal cases
  (add-hook 'prog-mode-hook (lambda () (global-subword-mode 1)))

  ;; UTF-8
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)

  ;; Show character name on point with C-x =
  (setq what-cursor-show-names t)

  ;; Indentation
  (setq-default tab-width 4)
  (setq-default indent-tabs-mode nil) ; spaces instead of tabs
  (setq-default standard-indent 4)
  (setq-default electric-indent-inhibit t)
  (setq backward-delete-char-untabify-method 'nil) ; Do not turn tabs into spaces on backspace

  ;; Clickable URLs
  (global-goto-address-mode t)

  ;; Auto revert buffers on external change
  (global-auto-revert-mode t)

  ;; Increase undo limit
  (setq undo-limit (* 10 undo-limit))
  (setq undo-strong-limit (* 10 undo-strong-limit))
  (setq undo-outer-limit (* 10 undo-outer-limit))

  ;; Configure hide-show blocks
  (use-package hideshow
    :hook ((prog-mode . hs-minor-mode))
    :bind (:map hs-minor-mode-map
                ("C-{" . hs-hide-block)
                ("C-}" . hs-show-block)))
  ) ; Editor

;;; File Management
(progn
  (use-package dired
    :ensure nil
    :custom
    (dired-listing-switches "-hal")
    (delete-by-moving-to-trash t)
    (dired-recursive-deletes 'always)
    (dired-recursive-copies 'always)

    :functions (dired-current-directory
                dired-create-directory
                dired-create-empty-file)
    :config
    (defun ig/dired-create-directory-or-file (path)
      "Call dired-create-directory if PATH ends with path-separator.
If PATH contains dots in the last segment, call
  dired-create-empty-file.
Otherwise, prompt user to choose between creating a file or a
  directory."
      (interactive
       (list (read-file-name "Create directory or file: " (dired-current-directory)))
       dired-mode)
      (let ((last-segment (file-name-nondirectory path)))
        (cond
         ((= (length last-segment) 0) (dired-create-directory path))
         ((seq-contains-p last-segment ?.) (dired-create-empty-file path))
         ((y-or-n-p "Is it a file (y) or a directory (n)?")
          (dired-create-empty-file path))
         (t (dired-create-directory path)))))
    
    :bind (:map dired-mode-map
                ("<mouse-2>" . dired-mouse-find-file)
                ("+" . ig/dired-create-directory-or-file)))

  (use-package ls-lisp
    :ensure nil
    :config
    (setq ls-lisp-dirs-first t)
    (setq ls-lisp-use-insert-directory-program nil))

  ;; Source: https://superuser.com/a/132844
  (defadvice find-file (before make-directory-maybe (filename &optional wildcards) activate)
    "Create parent directory if not exists while visiting file."
    (unless (file-exists-p filename)
      (let ((dir (file-name-directory filename)))
        (unless (file-exists-p dir)
          (make-directory dir t)))))
  
  (defun ig/open-file-externally (&optional file)
    "A simple wrapper over `browse-url-of-file' to display FILE
  in default external program."
    (interactive)
    (let ((browse-url-default-handlers nil))
      (browse-url-of-file file)))

  (defun ig/open-folder-externally (&optional folder)
    "Use `browse-url-of-file' to display (current) FOLDER
  in default external program."
    (interactive)
    (or folder
        (setq folder (expand-file-name ""))
        (error "Current buffer has no default folder"))
    (let ((browse-url-default-handlers nil))
      (browse-url-of-file folder)))
  ) ; File Management

;;; Key Bindings
(progn
  ;; Essential keybindings that map only standard Emacs functions.
  ;; Keybindings for other packages or custom functions should be
  ;; defined in corresponding modules, unless they are considered
  ;; essential.

  (defun ig/copy-without-region ()
    "Copy region if there is any, and copy the whole line otherwise."
    (interactive)
    (if (use-region-p)
        (kill-ring-save (region-beginning) (region-end))
      (kill-whole-line -1)
      (yank)))

  (defun ig/cut-without-region ()
    "Cut region if there is any, and cut the whole line otherwise."
    (interactive)
    (when (use-region-p)
      (kill-region (region-beginning) (region-end))))

  (defvar ig/protected-buffer-list '("*scratch*" "*dashboard*" "*Messages*")
    "Buffers defined in this list won't be killed with `ig/protected-kill-buffer',
and will be buried instead.")

  (defun ig/protected-kill-buffer ()
    "Bury current buffer if it is in `ig/protected-buffer-list',
and kill it otherwise."
    (interactive)
    (if (member (buffer-name (current-buffer)) ig/protected-buffer-list)
        (bury-buffer)
      (kill-buffer (current-buffer))))

  (defun ig/forward-delete-word (arg)
    "Delete characters forward until encountering the end of a word.
    With argument, do this that many times.
    This command does not push text to `kill-ring'."
    (interactive "p")
    (delete-region
     (point)
     (progn
       (forward-same-syntax arg)
       (point))))

  (defun ig/backward-delete-word (arg)
    "Delete characters backward until encountering the beginning of a word.
    With argument, do this that many times.
    This command does not push text to `kill-ring'."
    (interactive "p")
    (ig/forward-delete-word (- arg)))

  (defun ig/forward-delete-line ()
    "Delete text from current position to end of line char.
    This command does not push text to `kill-ring'."
    (interactive)
    (delete-region
     (point)
     (progn (end-of-line 1) (point)))
    (delete-char 1))

  (defun ig/backward-delete-line ()
    "Delete text between the beginning of the line to the cursor position.
    This command does not push text to `kill-ring'."
    (interactive)
    (let (p1 p2)
      (setq p1 (point))
      (beginning-of-line 1)
      (setq p2 (point))
      (delete-region p1 p2)))

  (defun ig/delete-or-quit ()
    "Smart quit, takes windows and `daemonp' into an account."
    (interactive)
    (if (eq (selected-window) (window-main-window (selected-frame)))
        (if (daemonp)
            (delete-frame)
          (when (yes-or-no-p "Are you sure you want to quit GNU Emacs? ")
            (save-buffers-kill-terminal)))
      (ig/protected-kill-buffer)
      (delete-window)))

  (defun ig/async-shell-command (&optional ignore-project)
    "Calls `project-async-shell-command' if inside a project
and ignore-project is nil, `async-shell-command' otherwise."
    (interactive "P")
    (if (and (not ignore-project) (project-current))
        (let ((shell-command-prompt-show-cwd t))
          (call-interactively #'project-async-shell-command))
      (call-interactively #'async-shell-command)))

  (defun ig/beginning-of-line ()
    "Go to either the beginning of line or to the indentation.
If the point is at the beginning of line, go to the indentation.
Otherwise, go to whatever is closest to the left of the point."
    (interactive)
    (if (= 0 (current-column))
        (back-to-indentation)
      (let ((orig-point (point)))
        (back-to-indentation)
        (when (<= orig-point (point))
          (beginning-of-line)))))

  (defun ig/temp-buffer ()
    "Create temporary buffers with no auto-save."
    (interactive)
    (switch-to-buffer (generate-new-buffer "*temp*")))

  (defun ig/indent-buffer ()
    "Indent the entire buffer."
    (interactive)
    (indent-region (point-min) (point-max)))
  (defun format-buffer ()
    "Indent the entire buffer. Replace it with a real formatter.
When a real formatter is available, use [remap format-buffer] as a key
binding."
    (interactive)
    (ig/indent-buffer))

  (defun ig/split-window-right ()
    (interactive)
    (split-window-right)
    (windmove-right))
  (defun ig/merge-window-left ()
    (interactive)
    (let ((buffer (window-buffer)))
      (windmove-left)
      (windmove-delete-right)
      (set-window-buffer (selected-window) buffer)))
  (defun ig/split-window-down ()
    (interactive)
    (split-window-below)
    (windmove-down))
  (defun ig/merge-window-up ()
    (interactive)
    (let ((buffer (window-buffer)))
      (windmove-up)
      (windmove-delete-down)
      (set-window-buffer (selected-window) buffer)))

  (use-package rebound
    :load-path "lisp/packages/rebound"
    :demand t
    :commands rebound-mode
    :custom
    (rebound-cc-key "C-d")
    (rebound-cx-key "C-e")
    :init
    (unless overriding-terminal-local-map
      (setq overriding-terminal-local-map (make-sparse-keymap)))
    :config
    (rebound-mode t)
    :bind*
    (;; Common keys
     ("C-q" . ig/delete-or-quit)
     ("C-w" . ig/protected-kill-buffer)
     ("C-r" . query-replace-regexp)
     ("C-o" . find-file)
     ("C-s" . save-buffer)
     ("C-a" . mark-whole-buffer)
     ("C-f" . isearch-forward)
     ("C-b" . switch-to-buffer)
     ("C-." . "\C-d\C-c")

     ;; Annoyances
     ("C-<mouse-1>" . ignore)
     ("C-<mouse-2>" . ignore)
     ("C-<mouse-3>" . ignore)
     ("C-<down-mouse-1>" . ignore)
     ("C-<down-mouse-2>" . ignore)
     ("C-<down-mouse-3>" . ignore)

     :map overriding-terminal-local-map
     ;; Essential keys
     ("C-z" . undo-only)
     ("C-S-z" . undo-redo) ("C-M-z" . undo-redo)
     ("C-x" . ig/cut-without-region)
     ("C-c" . ig/copy-without-region)
     ("C-v" . yank)
     
     ;; Navigation
     ("M-j" . left-char)
     ("M-l" . right-char)
     ("M-i" . previous-line)
     ("M-k" . next-line)
     ("M-h" . backward-word)
     ("M-;" . forward-word)
     ("M-u" . ig/beginning-of-line)
     ("M-o" . move-end-of-line)
     ("M-e" . scroll-down-command)
     ("M-d" . scroll-up-command)
     ("M-s" . backward-paragraph)
     ("M-f" . forward-paragraph)
     ("M-m" . set-mark-command))

    :bind
    (;; Editing
     ("C-<backspace>" . ig/backward-delete-word)
     ("M-DEL" . ig/backward-delete-word)
     ("M-<backspace>" . ig/backward-delete-word)
     ("C-<delete>" . ig/forward-delete-word)
     ("M-<delete>" . ig/forward-delete-word)
     ("C-M-<backspace>" . ig/backward-delete-line)
     ("C-M-<delete>" . ig/forward-delete-line)
     
     ;; Windows
     ("C-4" . ig/split-window-right)
     ("C-M-4" . ig/split-window-down)
     ("C-M-q" . delete-window)
     ("C-M-d" . delete-other-windows)
     ("C-M-<left>" . windmove-left)
     ("C-M-<right>" . windmove-right)
     ("C-M-<up>" . windmove-up)
     ("C-M-<down>" . windmove-down)
     ("C-c C-<left>" . windmove-swap-states-left)
     ("C-c C-<right>" . windmove-swap-states-right)
     ("C-c C-<up>" . windmove-swap-states-up)
     ("C-c C-<down>" . windmove-swap-states-down)

     ;; Emacs-specific
     ("<f5>" . compile)
     ("C-/" . comment-line) ("C-_" . comment-line)
     ("C-M-;" . ig/async-shell-command)
     ("C-c C-;" . shell-command)
     ("C-:" . eval-expression)
     ("M-=" . count-words)
     ("C-p" . ig/open-file-externally) ; to be used as a "preview" button
     ("C-S-p" . ig/open-folder-externally)
     ("C-M-p" . ig/open-folder-externally)

     :map rebound-mode-map
     ;; Less common keys
     ("C-S-r" . query-replace) ("C-M-r" . query-replace)
     ("C-S-o" . revert-buffer) ("C-M-o" . revert-buffer)
     ("C-S-s" . write-file) ("C-M-s" . write-file)
     ("C-S-f" . isearch-backward) ("C-M-f" . isearch-backward)
     ("C-S-b" . ibuffer) ("C-M-b" . ibuffer)
     ("C-S-i" . format-buffer) ("C-M-i" . format-buffer)
     ("C-y" . undo-redo)
     ("C-n" . ig/temp-buffer)
     ("C-+" . text-scale-increase)
     ("C-=" . text-scale-increase)
     ("C--" . text-scale-decrease)))

  (use-package isearch
    :ensure nil
    :bind (:map isearch-mode-map
                ("C-f" . isearch-repeat-forward)
                ("C-s" . nil)
                ("C-S-f" . isearch-repeat-backward)
                ("C-r" . isearch-query-replace)))

  ;; Remap mouse to click links
  (setq mouse-1-click-follows-link nil)
  (setq goto-address-highlight-keymap
        (let ((m (make-sparse-keymap)))
          (define-key m (kbd "<mouse-3>") 'goto-address-at-point)
          (define-key m (kbd "C-<mouse-1>") 'goto-address-at-point)
          (define-key m (kbd "C-c C-o") 'goto-address-at-point)
          m))
  ) ; Key Bindings

;;; Shell and Terminal
(progn
  ;; Open bash in ansi-term by default
  (defvar custom-term-shell "/bin/bash")
  (defadvice ansi-term (before force-bash)
    (interactive (list custom-term-shell)))
  (ad-activate 'ansi-term)

  ;; Kill process when restarting async-shell-command
  (setq async-shell-command-buffer 'confirm-kill-process)

  ;; The following code was taken from
  ;; https://rtime.ciirc.cvut.cz/~sojka/blog/compile-on-save/

  (defun compile-on-save-start ()
    (let ((buffer (compilation-find-buffer)))
      (unless (get-buffer-process buffer)
        (recompile))))

  (define-minor-mode compile-on-save-mode
    "Minor mode to automatically call `recompile' whenever the
  current buffer is saved. When there is ongoing compilation,
  nothing happens."
    :lighter " CoS"
    (if compile-on-save-mode
        (progn  (make-local-variable 'after-save-hook)
                (add-hook 'after-save-hook 'compile-on-save-start nil t))
      (kill-local-variable 'after-save-hook)))
  ) ; Shell and Terminal


;;; Custom Functions
(use-package emacs
  :config
  (defun xah-change-bracket-pairs (*p1 *p2 *fromType *toType)
    "Change bracket pairs from one type to another on current line or selection.
  For example, change all parenthesis () to square brackets [].

  When called in lisp program, *p1 *p2 are region begin/end position, *fromType or *toType is a string of a bracket pair. ➢ for example: \"()\",  \"[]\", etc.
  URL `http://ergoemacs.org/emacs/elisp_change_brackets.html'
  Version 2015-04-12, modified by IG."
    (interactive
     (let ((-bracketsList
            '("()" "{}" "[]" "<>" "\"\"" "''" "“”" "‘’" "‹›" "«»" "「」" "『』" "【】" "〖〗" "〈〉" "《》" "〔〕" "⦅⦆" "〚〛" "⦃⦄" "〈〉" "⦑⦒" "⧼⧽" "⟦⟧" "⟨⟩" "⟪⟫" "⟮⟯" "⟬⟭" "❛❜" "❝❞" "❨❩" "❪❫" "❴❵" "❬❭" "❮❯" "❰❱")))
       (if (use-region-p)
           (progn (list
                   (region-beginning)
                   (region-end)
                   (completing-read "Replace this:" -bracketsList )
                   (completing-read "To:" -bracketsList )))
         (progn
           (list
            (line-beginning-position)
            (line-end-position)
            (completing-read "Replace this:" -bracketsList )
            (completing-read "To:" -bracketsList ))))))
    (let* (
           (-findReplaceMap
            (vector
             (vector (char-to-string (elt *fromType 0)) (char-to-string (elt *toType 0)))
             (vector (char-to-string (elt *fromType 1)) (char-to-string (elt *toType 1))))))
      (save-excursion
        (save-restriction
          (narrow-to-region *p1 *p2)
          (let ( (case-fold-search nil))
            (mapc
             (lambda (-x)
               (goto-char (point-min))
               (while (search-forward (elt -x 0) nil t)
                 (replace-match (elt -x 1) 'FIXEDCASE 'LITERAL)))
             -findReplaceMap))))))

  (defcustom ig/jot-config-path user-init-file
    "File to copy code snippets to."
    :type 'string
    :group 'ig/jot-config)
  (defun ig/jot-config ()
    "Append region to `ig/jot-config-path' and evaluate it."
    (interactive)
    (if (use-region-p)
        (let ((region-text (buffer-substring (region-beginning) (region-end))))
          (make-directory (file-name-directory ig/jot-config-path) t)
          (find-file ig/jot-config-path)
          (goto-char (point-max))
          (insert region-text)
          (eval-buffer))
      (message "Please select a region to add it to the config.")))
  (setq ig/jot-config-path (expand-file-name "local.el"
                                             user-emacs-directory))

  :bind (("C-c b" . xah-change-bracket-pairs)
         ("C-c c" . ig/jot-config))
  ) ; Custom Functions

;;; basic-config.el ends here
