;;; my-config-iflipb.el --- iflipb                      -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(my-with-package iflipb)

(require 'iflipb)

(setq
 ;; don't skip buffers that start with "*"
 iflipb-ignore-buffers nil
 ;; vertical style
 iflipb-format-buffers-function #'iflipb-format-buffers-vertically
 iflipb-other-buffer-template "  %s"
 iflipb-current-buffer-template "* %s")

;; mini-window height
(require 'my-init-configuration)
(setq iflipb-format-buffers-height 9)
(when (> iflipb-format-buffers-height max-mini-window-height)
  (warn "`max-mini-window-height' is smaller than `iflipb-format-buffers-height'"))

;;; my-always-recenter

(require 'my-always-recenter)

(cl-callf append my-always-recenter-ignore-commands
  (list #'iflipb-next-buffer
        #'iflipb-previous-buffer))


(provide 'my-config-iflipb)
;;; my-config-iflipb.el ends here
