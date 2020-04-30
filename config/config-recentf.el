;;; config-recentf.el --- recentf                    -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(require 'recentf)

;;;; record dired directory

(defun my-recentf-record-dired ()
  (when (stringp dired-directory)
    (recentf-add-file dired-directory)))

(add-hook 'dired-mode-hook #'my-recentf-record-dired)


(provide 'config-recentf)
;;; config-recentf.el ends here
