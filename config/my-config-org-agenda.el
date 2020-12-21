;;; my-config-org-agenda.el --- org-agenda              -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(require 'org-agenda)
(require 'my-init-path)

(setq org-agenda-tags-column -78
      org-agenda-span 'day
      org-agenda-files (list (my-path-org-agenda))
      ;; do not change window configuration
      org-agenda-window-setup 'current-window
      org-agenda-restore-windows-after-quit nil
      ;; do not delete agenda buffer
      org-agenda-sticky t)
(add-hook 'org-agenda-mode-hook #'hl-line-mode)

(provide 'my-config-org-agenda)
;;; my-config-org-agenda.el ends here
