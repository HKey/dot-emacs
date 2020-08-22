;;; config-ztree.el --- ztree                        -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(my-with-package ztree)

(require 'ztree)
(require 'lib-util)

(my-define-key ztree-mode-map
  "p" #'previous-line
  "n" #'next-line
  "h" #'ztree-move-up-in-tree)


(provide 'config-ztree)
;;; config-ztree.el ends here
