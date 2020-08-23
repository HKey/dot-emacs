;;; config-iflipb.el --- iflipb                      -*- lexical-binding: t; -*-

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


(provide 'config-iflipb)
;;; config-iflipb.el ends here
