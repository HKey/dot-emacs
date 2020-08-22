;;; config-eyebrowse.el --- eyebrowse                -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(my-with-package eyebrowse)

(require 'eyebrowse)

;;; key binding

(require 'lib-util)

(my-define-key eyebrowse-mode-map
  "C-t n" #'eyebrowse-next-window-config
  "C-t p" #'eyebrowse-prev-window-config
  "C-t w" #'eyebrowse-switch-to-window-config)

;;; config

(setq eyebrowse-keymap-prefix (kbd "C-t")
      eyebrowse-wrap-around t)


(provide 'config-eyebrowse)
;;; config-eyebrowse.el ends here
