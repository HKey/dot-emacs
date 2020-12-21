;;; my-config-company-dabbrev.el --- company-dabbrev    -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(my-with-package company)

(require 'company-dabbrev)

(setq company-dabbrev-other-buffers nil
      company-dabbrev-downcase nil)


(provide 'my-config-company-dabbrev)
;;; my-config-company-dabbrev.el ends here
