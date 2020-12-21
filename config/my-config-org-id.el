;;; my-config-org-id.el --- org-id                      -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(my-with-package f)

(require 'org-id)
(require 'f)
(require 'my-init-path)

(setq org-id-extra-files
      (f-files my-path-org (lambda (f) (f-ext-p f "org")) t))


(provide 'my-config-org-id)
;;; my-config-org-id.el ends here
