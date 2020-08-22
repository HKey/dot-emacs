;;; config-org-download.el --- org-download          -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(my-with-package org-download)

(require 'org-download)

(setq org-download-method 'attach)


(provide 'config-org-download)
;;; config-org-download.el ends here
