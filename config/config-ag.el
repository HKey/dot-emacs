;;; config-ag.el --- ag                              -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(my-with-package ag)

(require 'ag)

(setq ag-highlight-search t)

;;;; wgrep-ag

(my-with-package wgrep-ag)


(provide 'config-ag)
;;; config-ag.el ends here
