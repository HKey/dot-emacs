;;; config-pocket-reader.el --- pocket-reader        -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(my-with-package pocket-reader)

(require 'pocket-reader)

;;;; basic config

(setq
 ;; use eww as default browser
 pocket-reader-open-url-default-function #'eww
 ;; do not archive by opening a page
 pocket-reader-archive-on-open nil)


(provide 'config-pocket-reader)
;;; config-pocket-reader.el ends here
