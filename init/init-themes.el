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

;;;; solarized

(use-package solarized
  :ensure solarized-theme
  :config
  (setq solarized-use-variable-pitch  nil
        solarized-use-less-bold       t
        solarized-height-minus-1      1.0
        solarized-height-plus-1       1.0
        solarized-height-plus-2       1.0
        solarized-height-plus-3       1.0
        solarized-height-plus-4       1.0
        solarized-scale-org-headlines nil))

;;;; enable theme

(load-theme 'spacemacs-light)


(provide 'init-themes)
;;; init-themes.el ends here
