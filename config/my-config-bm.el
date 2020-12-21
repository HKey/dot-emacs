;;; my-config-bm.el --- bm                              -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(my-with-package bm)

(require 'bm)
(require 'my-util)

(my-define-key bm-show-mode-map
  "n" #'next-line
  "p" #'previous-line)


(provide 'my-config-bm)
;;; my-config-bm.el ends here
