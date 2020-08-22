;;; config-spacemacs-common.el --- spacemacs-common  -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(my-with-package spacemacs-theme)

(require 'spacemacs-common)

(setq spacemacs-theme-org-agenda-height nil
      spacemacs-theme-org-bold nil
      spacemacs-theme-org-height nil
      spacemacs-theme-org-priority-bold nil)


(provide 'config-spacemacs-common)
;;; config-spacemacs-common.el ends here
