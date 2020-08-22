;;; config-org-id.el --- org-id                      -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(my-with-package f)

(require 'org-id)
(require 'f)

(setq org-id-extra-files
      (f-files org-directory (lambda (f) (f-ext-p f "org")) t))


(provide 'config-org-id)
;;; config-org-id.el ends here
