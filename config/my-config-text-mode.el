;;; my-config-text-mode.el --- text-mode                -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(require 'text-mode)

;;;; company

(my-with-package company)
(require 'company)

(defun my-text-mode-company-setup ()
  (setq-local company-backends
              `((company-yasnippet company-dabbrev) ,@company-backends)))

(add-hook 'text-mode-hook #'my-text-mode-company-setup)


(provide 'my-config-text-mode)
;;; my-config-text-mode.el ends here
