;;; config-org-ql.el --- org-ql                      -*- lexical-binding: t; -*-

(require 'my-bootstrap)

(my-with-package org-ql)
(require 'org-ql)

;;; Show result in the same window

(require 'org-ql-view)

(setq org-ql-view-display-buffer-action
      (cons #'display-buffer-same-window nil))


(provide 'config-org-ql)
;;; config-org-ql.el ends here
