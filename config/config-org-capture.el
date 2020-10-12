;;; config-org-capture.el --- org-capture            -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(require 'org-capture)


;;;; `org-capture-templates'

(require 'init-path)

(setq org-capture-templates
      `(("i" "Inbox" entry
         (file+datetree ,(lambda () (my-path-org-agenda "inbox.org")))
         (file ,(my-path-org-capture-templates "inbox.org"))
         :empty-lines 1)
        ("c" "Inbox from clipboard" entry
         (file+datetree ,(lambda () (my-path-org-agenda "inbox.org")))
         (file ,(my-path-org-capture-templates "inbox-clipboard.org"))
         :empty-lines 1)))


(provide 'config-org-capture)
;;; config-org-capture.el ends here
