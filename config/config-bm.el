;;; config-bm.el --- bm                              -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(my-with-package bm)

(require 'bm)
(require 'lib-util)

(my-define-key bm-show-mode-map
  "n" #'next-line
  "p" #'previous-line)


(provide 'config-bm)
;;; config-bm.el ends here
