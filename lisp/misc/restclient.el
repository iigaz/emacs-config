;;; restclient.el --- Rest client. -*- lexical-binding: t; -*-

(use-package restclient
  :config
  (defun restclient ()
    "Open the restclient buffer, (re)creating it if not present."
    (interactive)
    (pop-to-buffer (get-buffer-create "*restclient*"))
    (unless (derived-mode-p 'restclient-mode)
      (restclient-mode)))
  :commands
  (restclient))

(use-package company-restclient
  :after (company restclient)
  :config
  (add-to-list 'company-backends 'company-restclient)
  (add-hook 'restclient-mode 'company-mode))

;;; restclient.el ends here
