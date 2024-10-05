(setq gc-cons-threshold (* 100 1024 1024))

(defun ig/load-org-from-user-emacs-directory (filename-without-extension)
  (let ((org-file (expand-file-name (concat filename-without-extension ".org") user-emacs-directory))
        (el-file (expand-file-name (concat filename-without-extension ".el") user-emacs-directory)))
    (when (file-readable-p org-file)
      (if (file-newer-than-file-p org-file el-file)
          (org-babel-load-file org-file)
        (load-file el-file)))))

(ig/load-org-from-user-emacs-directory "config")
