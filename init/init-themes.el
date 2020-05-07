;;; init-themes.el --- themes                        -*- lexical-binding: t; -*-

(require 'my-bootstrap)

;;;; paper-theme

(use-package paper-theme)

;;;; spacemacs-common

(use-package spacemacs-common
  :ensure spacemacs-theme
  :config
  (setq spacemacs-theme-org-agenda-height nil
        spacemacs-theme-org-bold nil
        spacemacs-theme-org-height nil
        spacemacs-theme-org-priority-bold nil))

;;;; enable theme

(load-theme 'spacemacs-light)


(provide 'init-themes)
;;; init-themes.el ends here
