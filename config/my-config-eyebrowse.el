;;; my-config-eyebrowse.el --- eyebrowse                -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(my-with-package eyebrowse)

(require 'eyebrowse)

;;; key binding

(require 'my-util)

(my-define-key eyebrowse-mode-map
  ;; Change prefix key
  "C-t" (lookup-key eyebrowse-mode-map eyebrowse-keymap-prefix)
  eyebrowse-keymap-prefix nil

  ;; Additional key bindings
  "C-t n" #'eyebrowse-next-window-config
  "C-t p" #'eyebrowse-prev-window-config
  "C-t w" #'eyebrowse-switch-to-window-config)

;;; config

(setq eyebrowse-wrap-around t)


(provide 'my-config-eyebrowse)
;;; my-config-eyebrowse.el ends here
