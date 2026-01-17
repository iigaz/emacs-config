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
  ) ; Editor

;;; File Management
(progn
  (use-package dired
    :ensure nil
    :custom
    (dired-listing-switches "-hal1")
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

  ;; TODO: Get rid of wakib-keys or otherwise seriously reconsider the
  ;; bindings.
  ;; TODO: Actually, I need to try cua-mode again, with C-c C-c
  ;; (unless defined) for copy and C-c C-v for duplicate line.
  (use-package wakib-keys
    :init
    (defun ig/wakib-copy-without-region ()
      "Copy region if there is any, and copy the whole line otherwise."
      (interactive)
      (if (use-region-p)
          (kill-ring-save (region-beginning) (region-end))
        (kill-whole-line -1)
        (yank)))

    (defun ig/wakib-cut-without-region ()
      "Cut region if there is any, and cut the whole line otherwise."
      (interactive)
      (when (use-region-p)
        (kill-region (region-beginning) (region-end))))

    (defvar ig/wakib-protected-buffer-list '("*scratch*" "*dashboard*" "*Messages*")
      "Buffers defined in this list won't be killed with `ig/wakib-kill-buffer',
and will be buried instead.")

    (defun ig/wakib-kill-buffer ()
      "Bury current buffer if it is in `ig/wakib-protected-buffer-list',
and kill it otherwise."
      (interactive)
      (if (member (buffer-name (current-buffer)) ig/wakib-protected-buffer-list)
          (bury-buffer)
        (kill-buffer (current-buffer))))

    (defun ig/wakib-delete-word (arg)
      "Delete characters forward until encountering the end of a word.
    With argument, do this that many times.
    This command does not push text to `kill-ring'."
      (interactive "p")
      (delete-region
       (point)
       (progn
         (forward-same-syntax arg)
         (point))))

    (defun ig/wakib-backward-delete-word (arg)
      "Delete characters backward until encountering the beginning of a word.
    With argument, do this that many times.
    This command does not push text to `kill-ring'."
      (interactive "p")
      (ig/wakib-delete-word (- arg)))

    (defun ig/wakib-delete-line ()
      "Delete text from current position to end of line char.
    This command does not push text to `kill-ring'."
      (interactive)
      (delete-region
       (point)
       (progn (end-of-line 1) (point)))
      (delete-char 1))

    (defun ig/wakib-delete-line-backward ()
      "Delete text between the beginning of the line to the cursor position.
    This command does not push text to `kill-ring'."
      (interactive)
      (let (p1 p2)
        (setq p1 (point))
        (beginning-of-line 1)
        (setq p2 (point))
        (delete-region p1 p2)))

    (defun ig/wakib-quit ()
      "Smart quit, takes windows and daemonp into an account."
      (interactive)
      (if (eq (selected-window) (window-main-window (selected-frame)))
          (if (daemonp)
              (delete-frame)
            (when (yes-or-no-p "Are you sure you want to quit GNU Emacs? ")
              (save-buffers-kill-terminal)))
        (delete-window)))

    ;; TODO: cycle through buffers
    (defun ig/wakib-switch-to-last-buffer ()
      "Switches to last buffer."
      (interactive)
      (switch-to-buffer nil))

    (defun ig/async-shell-command (&optional ignore-project)
      "Calls `project-async-shell-command' if inside a project
and ignore-project is nil, `async-shell-command' otherwise."
      (interactive "P")
      (if (and (not ignore-project) (project-current))
          (let ((shell-command-prompt-show-cwd t))
            (call-interactively #'project-async-shell-command))
        (call-interactively #'async-shell-command)))

    (setq wakib-keylist
          `(("M-j" . left-char)
            ("M-l" . right-char)
            ("M-i" . previous-line)
            ("M-k" . next-line)
            ("M-u" . backward-word)
            ("M-o" . forward-word)
            ("M-a" . wakib-back-to-indentation-or-beginning) ;; reminiscent of regular Emacs C-a
            ("M-e" . move-end-of-line) ;; reminiscent of regular Emacs C-e
            ("M-[" . backward-paragraph)
            ("M-]" . forward-paragraph)
            ("M-S-i" . scroll-down-command)
            ("M-S-k" . scroll-up-command)
            ("M-n" . beginning-of-buffer)
            ("M-S-n" . end-of-buffer)
            ("C-n" . wakib-new-empty-buffer)
            ("C-o" . find-file)
            ("C-S-o" . revert-buffer)
            ("C-w" . ig/wakib-kill-buffer)
            ("C-q" . ig/wakib-quit)
            ("C-<next>" . next-buffer)
            ("C-<prior>" . previous-buffer)
            ("C-c" . ig/wakib-copy-without-region)
            ("C-x" . ig/wakib-cut-without-region)
            ("C-v" . yank)
            ("C-z" . undo-only)
            ("C-S-z" . undo-redo)
            ("C-y" . undo-redo)
            ("C-f" . isearch-forward)
            ("C-S-f" . isearch-backward)
            ("C-r" . query-replace)
            ("C-S-r" . query-replace-regexp)
            ("C-s" . save-buffer)
            ("C-S-s" . write-file)
            ("C-p" . switch-to-buffer)
            ("C-S-p" . ibuffer)
            ("C-a" . mark-whole-buffer)
            ("C-+" . text-scale-increase)
            ("C-=" . text-scale-increase)
            ("C--" . text-scale-decrease)
            ("C-/" . comment-line)
            ("C-:" . ig/async-shell-command)
            ("M-C-;" . eval-expression)
            ("M-h" . other-window)
            ("M-M" . goto-line)
            ("M-4" . split-window-right)
            ("M-$" . split-window-below)
            ("M-3" . delete-other-windows)
            ("M-#" . delete-window)
            ("M-r" . kill-word)
            ("M-E" . wakib-backward-kill-line)
            ("M-R" . kill-line)
            ("M-w" . kill-whole-line)
            ("M-<f4>" . save-buffers-kill-emacs)
            ("M-d" . delete-backward-char)
            ("M-f" . delete-char)
            ("M-s" . set-mark-command)
            ("M-S-s" . set-rectangular-region-anchor)
            ("M-=" . count-words)
            ("<C-return>" . wakib-insert-line-after)
            ("<C-S-return>" . wakib-insert-line-before)
            ("M-X" . pp-eval-expression)
            ("<C-backspace>" . ig/wakib-backward-delete-word)
            ("<C-delete>" . ig/wakib-delete-word)
            ("<escape>" . keyboard-quit)
            ;; ("<C-tab>" . ig/wakib-switch-to-last-buffer)
            ("C-{" . hs-hide-block)
            ("C-}" . hs-show-block)))

    :functions (wakib-define-keys)
    :defines (goto-address-highlight-keymap)
    :config
    (wakib-keys 1)

    ;; Set C-c and C-x overriding to work EVERYWHERE
    (unless overriding-terminal-local-map
      (setq overriding-terminal-local-map (make-sparse-keymap)))
    (wakib-define-keys overriding-terminal-local-map '(("C-c" . ig/wakib-copy-without-region)
                                                       ("C-x" . ig/wakib-cut-without-region)))

    ;; Other keybindings
    (global-set-key (kbd "C-c C-;") 'shell-command)

    ;; Remap mouse to click links
    (setq mouse-1-click-follows-link nil)
    (global-set-key (kbd "C-<down-mouse-1>") nil)
    (global-set-key (kbd "C-<mouse-1>") 'mouse-buffer-menu)
    (setq goto-address-highlight-keymap
          (let ((m (make-sparse-keymap)))
            (define-key m (kbd "<mouse-3>") 'goto-address-at-point)
            (define-key m (kbd "C-<mouse-1>") 'goto-address-at-point)
            (define-key m (kbd "C-d C-o") 'goto-address-at-point)
            m)))

  ;; | Keys       | Action                                       |
  ;; |------------+----------------------------------------------|
  ;; | C-arrows   | Move to adjacent window                      |
  ;; | M-S-arrows | Move current buffer to adjacent window       |
  ;; | C-s-up     | Merge current window with window above       |
  ;; | C-s-down   | Split current window down                    |
  ;; | C-s-left   | Merge current window with window on the left |
  ;; | C-s-right  | Split current window right                   |
  ;; | s-f        | Remove all windows except current            |

  ;; Use meta key for windmove
  (windmove-default-keybindings 'meta)

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

  (global-set-key (kbd "C-s-<right>") 'ig/split-window-right)
  (global-set-key (kbd "C-s-<left>") 'ig/merge-window-left)
  (global-set-key (kbd "C-s-<down>") 'ig/split-window-down)
  (global-set-key (kbd "C-s-<up>") 'ig/merge-window-up)
  (global-set-key (kbd "s-f") 'delete-other-windows)

  (global-set-key (kbd "M-S-<right>") 'windmove-swap-states-right)
  (global-set-key (kbd "M-S-<left>") 'windmove-swap-states-left)
  (global-set-key (kbd "M-S-<down>") 'windmove-swap-states-down)
  (global-set-key (kbd "M-S-<up>") 'windmove-swap-states-up)
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

  ;; Usually if I press F5 I want emacs to compile things;
  ;; I also want to be able to redefine it in some buffers.
  ;; If I put it in wakib-keys, it will take precedence over local bindings.
  (global-set-key (kbd "<f5>") 'compile)

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

;;; basic-config.el ends here
